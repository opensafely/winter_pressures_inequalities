library(here)
library(broom)
library(dplyr)
library(arrow)
library(survival)
library(rlang)
library(purrr)
library(tibble)
library(car)

# Shared building blocks for the Cox PH model scripts:
#   1_3_cox_models.R, 1_4_cox_models_stratified.R,
#   1_5_cox_models_time.R, 1_6_cox_models_int.R

# Outcome definitions used by every Cox model script.
# `stop_var` is the numeric day-offset from each patient's own fu_start used
# in the Surv() formula; `date_stop_var` is the actual calendar-date
# equivalent (kept alongside it in the arrow file), needed to restrict
# follow-up to a calendar period.
COX_OUTCOMES <- tibble::tribble(
  ~outcome_name,     ~stop_var,      ~date_stop_var,      ~binary_var,       ~strata_var,
  "gp_binary",       "stop_gp",      "date_stop_gp",      "gp_binary",       "year_end_gp",
  "gp_sens_binary",  "stop_gp_sens", "date_stop_gp_sens", "gp_sens_binary",  "year_end_gp",
  "sc_binary",       "stop_sc",      "date_stop_sc",      "sc_binary",       "year_end_sc",
  "emerg_binary",    "stop_emerg",   "date_stop_emerg",   "emerg_binary",    "year_end_emerg"
)

# Read the per-disease outcome dataset
read_outcome_data <- function(disease) {
  file_path <- paste0("an_outcome_first_", disease, ".arrow")
  read_feather(here("output", file_path), as_data_frame = TRUE)
}

print_stop_summary <- function(data_df) {
  print("stop check")
  print("GP");      print(summary(data_df$stop_gp))
  print("GP sens"); print(summary(data_df$stop_gp_sens))
  print("SC");      print(summary(data_df$stop_sc))
  print("A & E");   print(summary(data_df$stop_emerg))
}

# Sets the reference level for the exposure ("difference") variable.
# If the reference level isn't present in the data (e.g. after subsetting
# to a stratum), returns ok = FALSE instead of erroring, so callers can
# redact that combination instead of failing.
set_difference_reference <- function(data_df, difference, context_label = "") {
  ref_levels <- c(
    ethnicity6_combined  = "White",
    ethnicity16_combined = "White British",
    imd_quintile         = "5"
  )

  if (!difference %in% names(ref_levels)) {
    return(list(ok = TRUE, data = data_df))
  }

  ref <- ref_levels[[difference]]
  data_df[[difference]] <- factor(data_df[[difference]])

  if (!ref %in% levels(data_df[[difference]])) {
    message("Skipping: '", ref, "' reference level not present", context_label)
    return(list(ok = FALSE, data = data_df))
  }

  data_df[[difference]] <- relevel(data_df[[difference]], ref = ref)
  list(ok = TRUE, data = data_df)
}

# Common covariate prep: unordered factors with fixed reference levels,
# and dropping rows with unknown IMD/ethnicity.
prep_covariates <- function(data_df) {
  data_df %>%
    mutate(
      age_group        = relevel(factor(as.character(age_group), ordered = FALSE), ref = "adult_under_65"),
      sex              = factor(as.character(sex), ordered = FALSE),
      urban_rural_5    = relevel(factor(as.character(urban_rural_5), ordered = FALSE), ref = "Urban minor conurbation"),
      migrant          = relevel(factor(as.character(migrant), ordered = FALSE), ref = "Non-migrant"),
      english_language = relevel(factor(as.character(english_language), ordered = FALSE), ref = "TRUE")
    ) %>%
    filter(imd_quintile != "Unknown") %>%
    filter(ethnicity6_combined != "Unknown") %>%
    # filter() drops rows but not the now-unused "Unknown" factor level; left
    # in place, that phantom level gets its own (all-zero) dummy column in
    # any interaction term, which car::Anova(type = "III") then rejects
    # model-wide as "aliased coefficients".
    droplevels()
}

# Adjustment term sets shared by scripts that take an `adjustment` argument
build_adjustment_terms <- function(adjustment, difference) {
  switch(adjustment,
    "unadjusted"  = "",
    "age_sex"     = " + age_group + sex",
    "migration"   = " + age_group + sex + migrant + english_language",
    "area"        = if (difference == "imd_quintile") {
      " + age_group + sex + urban_rural_5"
    } else {
      " + age_group + sex + imd_quintile + urban_rural_5"
    },
    "health"      = " + age_group + sex + flu_atrisk_grp + covid_atrisk_grp + prior_flu_vacc_bin + prior_covid_vacc_bin + bmi_category",
    "health_2018" = " + age_group + sex + flu_atrisk_grp + prior_flu_vacc_bin + bmi_category",
    "all"         = " + age_group + sex + migrant + english_language + imd_quintile + urban_rural_5 + vacc_elig_covid + vacc_elig_flu + prior_flu_vacc_bin + prior_covid_vacc_bin + bmi_category",
    stop("Unknown adjustment: ", adjustment)
  )
}

make_cox_formula <- function(difference, rhs_extra, stop_var, binary_var, strata_var, start_var = "start") {
  as.formula(paste0("Surv(", start_var, ", ", stop_var, ", ", binary_var, ") ~ ",
                     difference, rhs_extra, " + strata(", strata_var, ")"))
}

# Restricts follow-up to a calendar period, right-censoring at period_end
# and left-truncating at period_start.
#
# `stop_var`/`start` are numeric day-offsets from each patient's own
# fu_start (not calendar dates), so the overlap with [period_start,
# period_end] has to be worked out on the calendar-date columns
# (`date_stop_var`, `fu_start_col`) and then converted back to that same
# per-patient numeric scale for Surv().
restrict_period <- function(df, stop_var, date_stop_var, binary_var, period_start, period_end,
                             fu_start_col = "fu_start") {
  df %>%
    filter(.data[[date_stop_var]] > period_start, .data[[fu_start_col]] < period_end) %>%
    mutate(
      !!binary_var := if_else(.data[[date_stop_var]] > period_end, 0L, .data[[binary_var]]),
      !!stop_var   := as.numeric(pmin(.data[[date_stop_var]], period_end) - .data[[fu_start_col]]),
      start         = pmax(start, as.numeric(period_start - .data[[fu_start_col]]))
    )
}

# For each outcome, checks whether every combination of levels of
# `group_vars` has > 2 events. Pass just `difference` for main-effects
# models, or c(difference, interaction_var) for interaction models - an
# interaction term can only be estimated if every difference x interaction
# cell has enough events, not just every difference level on its own.
check_sufficient_events <- function(data_df, group_vars) {
  data_df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      gp_binary      = sum(gp_binary),
      gp_sens_binary = sum(gp_sens_binary),
      sc_binary      = sum(sc_binary),
      emerg_binary   = sum(emerg_binary),
      .groups = "drop"
    ) %>%
    summarise(across(-all_of(group_vars), ~ all(.x > 2))) %>%
    as.list()
}

cox_redacted_row <- function(difference, outcome_name, disease, reason, extra_cols = list()) {
  tibble(
    term      = difference,
    estimate  = NA_real_,
    std.error = NA_real_,
    statistic = NA_real_,
    p.value   = NA_real_,
    conf.low  = NA_real_,
    conf.high = NA_real_,
    outcome   = outcome_name,
    by_var    = difference,
    disease   = disease,
    !!!extra_cols,
    n             = NA_real_,
    n_events      = NA_real_,
    redacted      = TRUE,
    redact_reason = reason
  )
}

# Redacts every outcome with the same reason - used when a whole stratum
# has to be skipped (e.g. no data, or the reference level is missing)
cox_redacted_grid <- function(difference, disease, reason, extra_cols = list()) {
  pmap_dfr(COX_OUTCOMES, function(outcome_name, stop_var, date_stop_var, binary_var, strata_var) {
    cox_redacted_row(difference, outcome_name, disease, reason, extra_cols)
  })
}

# Fits (or redacts) a single outcome's Cox model.
#   - min_events: total event threshold below which the outcome is redacted
#   - full_var: whether every level of `difference` has enough events
#     (set to TRUE to skip this check, e.g. for the stratified models)
#   - period: optional list(start=, end=) to restrict follow-up to
#     (a calendar date range - restriction is done via date_stop_var/fu_start,
#     then converted back to the numeric day-offset scale used by Surv())
#   - interaction_var: if set, runs a type III LRT for difference:interaction_var
#   - extra_cols: named list of additional columns to attach to every row
#     (e.g. adjustment, period, interactions, strat_var, strat_level)
fit_or_redact <- function(data_df, difference, outcome_name, stop_var, date_stop_var, binary_var, strata_var,
                           disease, rhs_extra, full_var = TRUE, min_events = 7,
                           period = NULL, interaction_var = NULL,
                           extra_cols = list(), context_label = "") {

  # Every row must carry the same columns, so redacted/failed rows get
  # NA placeholders for the LRT columns that a successful fit would add
  if (!is.null(interaction_var)) {
    extra_cols <- c(extra_cols, list(lrt_chisq = NA_real_, lrt_df = NA_real_, lrt_p = NA_real_))
  }

  n_events <- sum(data_df[[binary_var]] == 1, na.rm = TRUE)

  if (n_events <= min_events) {
    message("Redacted: ", min_events, " or fewer ", disease, " ", outcome_name, " events", context_label)
    return(cox_redacted_row(difference, outcome_name, disease, "min events not met", extra_cols))
  }
  if (!full_var) {
    cell_desc <- if (!is.null(interaction_var)) "exposure x interaction cell" else "exposure level"
    message("Redacted: 2 or fewer ", disease, " ", outcome_name, " events per ", cell_desc, context_label)
    return(cox_redacted_row(difference, outcome_name, disease, paste0("insufficient events per ", cell_desc), extra_cols))
  }

  df_fit <- data_df
  if (!is.null(period)) {
    df_fit <- restrict_period(df_fit, stop_var, date_stop_var, binary_var, period$start, period$end)
  }

  formula <- make_cox_formula(difference, rhs_extra, stop_var, binary_var, strata_var)
  # car::Anova() (used for the interaction LRT) refits reduced models by
  # re-evaluating the stored call in environment(formula(fit)). Since
  # make_cox_formula() is a top-level function (not nested inside this one,
  # unlike the pre-refactor scripts), its as.formula() call captures its own
  # environment rather than this one - re-point it here so df_fit is visible.
  environment(formula) <- environment()

  tryCatch({
    fit <- coxph(formula, data = df_fit)

    result <- tidy(fit, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>%
      mutate(
        outcome  = outcome_name,
        by_var   = difference,
        disease  = disease,
        !!!extra_cols,
        n        = fit$n,
        n_events = fit$nevent,
        redacted = FALSE
      )

    if (!is.null(interaction_var)) {
      int_test <- car::Anova(fit, type = "III")
      int_term <- paste0(difference, ":", interaction_var)
      result <- result %>%
        mutate(
          lrt_chisq = int_test[int_term, "Chisq"],
          lrt_df    = int_test[int_term, "Df"],
          lrt_p     = int_test[int_term, "Pr(>Chisq)"]
        )
    }
    result
  }, error = function(e) {
    message("Model failed for ", disease, " ", outcome_name, ": ", e$message)
    row <- cox_redacted_row(difference, outcome_name, disease, paste0("model failed: ", e$message), extra_cols)
    row$redacted <- FALSE
    row
  })
}

# Fits every outcome in COX_OUTCOMES for a single difference/adjustment/etc
# combination and row-binds the results
run_all_outcomes <- function(data_df, disease, difference, rhs_extra,
                              full_events_check = TRUE, period = NULL,
                              interaction_var = NULL, extra_cols = list(),
                              context_label = "") {

  # For interaction models, every difference x interaction_var cell needs
  # enough events (not just every difference level on its own), otherwise
  # the interaction term can't be estimated and car::Anova() errors out on
  # aliased coefficients rather than getting a clean redaction.
  group_vars <- if (!is.null(interaction_var)) c(difference, interaction_var) else difference
  events_ok <- if (full_events_check) check_sufficient_events(data_df, group_vars) else list()

  pmap_dfr(COX_OUTCOMES, function(outcome_name, stop_var, date_stop_var, binary_var, strata_var) {
    fit_or_redact(
      data_df, difference, outcome_name, stop_var, date_stop_var, binary_var, strata_var,
      disease, rhs_extra,
      full_var = if (full_events_check) events_ok[[binary_var]] else TRUE,
      period = period,
      interaction_var = interaction_var,
      extra_cols = extra_cols,
      context_label = context_label
    )
  })
}
