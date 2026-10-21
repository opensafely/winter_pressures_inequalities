library(arrow)
library(survival)
library(mediation)
library(tidyverse)
library(broom)
library(here)

outdir <- here("output", "tables")
fs::dir_create(outdir)

# Which mediator and time period to run - split out per-run via the yaml
# to keep memory down, since mediate()'s default 1000 simulations are
# computed over the whole (group vs White) subset for every one of the 36
# group x mediator combinations otherwise, in a single R session.
args <- commandArgs(trailingOnly = TRUE)
mediator_arg <- args[1]
period_arg <- args[2]
#ethnic_arg <- args[3]

set.seed(123)
# Same two calendar periods as 1_5_cox_models_time.R
periods <- list(
  period1 = list(start = as.Date("2021-09-01"), end = as.Date("2021-12-31")),
  period2 = list(start = as.Date("2022-09-01"), end = as.Date("2026-02-28"))
)
period <- periods[[period_arg]]
if (is.null(period)) {
  stop("Unknown period: ", period_arg, " (expected one of: ", paste(names(periods), collapse = ", "), ")")
}

# stop_sc/sc_binary are numeric day-offsets from each person's own
# fu_start, not calendar dates, and this script's survreg() call uses the
# plain 2-argument Surv(stop_sc, sc_binary) - no start/left-truncation
# support like coxph's 3-argument form. So rather than reusing
# cox_model_functions.R's restrict_period() (which measures time from the
# original fu_start, appropriate for left-truncated Cox models), time is
# re-anchored here at whichever is later: the person's actual entry, or
# the period start. Everyone then contributes only the portion of
# follow-up that falls inside the period, with their clock starting at 0
# for that window - no truncation handling needed for the 2-arg Surv().
restrict_period_reanchored <- function(df, stop_var, date_stop_var, binary_var, period_start, period_end,
                                        fu_start_col = "fu_start") {
  df %>%
    filter(.data[[date_stop_var]] > period_start, .data[[fu_start_col]] < period_end) %>%
    mutate(
      !!binary_var := if_else(.data[[date_stop_var]] > period_end, 0L, .data[[binary_var]]),
      !!stop_var   := as.numeric(pmin(.data[[date_stop_var]], period_end) - pmax(.data[[fu_start_col]], period_start))
    )
}

# use mediation package to do AFT model for COVID outcomes

data_covid <- read_feather(here("output/an_outcome_first_covid_2021.arrow"), as_data_frame = TRUE) 

data_covid <- data_covid %>% slice_sample(prop = 0.02) %>%
     droplevels() 

#print(levels(data_covid$ethnicity6_combined))

# Mediators with a natural order are modelled with an ordered logistic
# mediator model (MASS::polr) rather than binomial glm, since glm(family =
# binomial) silently collapses any factor with >2 levels into a "reference
# vs everything else" binary, which mediate() then can't reconcile with the
# real multi-level mediator used in the outcome model.
ordinal_levels <- list(
  imd_quintile  = c("1", "2", "3", "4", "5"),
  urban_rural_5 = c("Urban major conurbation", "Urban minor conurbation", "Urban city and town",
                     "Rural town and fringe", "Rural village and dispersed"),
  bmi_category  = c("underweight", "ideal", "overweight", "obese")
)

# Function to run mediation models
 run_mediation <- function(group, mediator) {
   data_df <- data_covid %>%
     filter(ethnicity6_combined %in% c(group, "White")) %>%
     restrict_period_reanchored("stop_sc", "date_stop_sc", "sc_binary", period$start, period$end) %>%
     # setting events on same day as follow up at half day - re-applied
     # after period restriction, since that can itself produce new
     # stop_sc == 0 values for people whose event/censoring lands right at
     # the start of their at-risk window within the period
     mutate(stop_sc = if_else(stop_sc == 0, 0.5, stop_sc))

   is_ordinal <- mediator %in% names(ordinal_levels)

   if (is_ordinal) {
     # Restrict to the well-defined ordered categories and set their order
     # (drops e.g. "Unknown"/"Missing"/"unknown or outlier")
     data_df <- data_df %>% filter(.data[[mediator]] %in% ordinal_levels[[mediator]])
     data_df[[mediator]] <- factor(data_df[[mediator]], levels = ordinal_levels[[mediator]], ordered = TRUE)
   } else {
     if ("Unknown" %in% levels(factor(data_df[[mediator]]))) {
       # Binary mediator: drop the "Unknown" category so both the mediator and
       # outcome model treat it as a genuine 2-level factor
       data_df <- data_df %>% filter(.data[[mediator]] != "Unknown")
     }
     # mediate()'s internal simulation for a glm(binomial) mediator draws
     # plain 0/1 values, but then maps them onto a factor mediator via
     # factor(x, levels = 1:m, ...) - i.e. it expects 1/2, not 0/1. Every
     # simulated 0 silently becomes NA, which is what was crashing pval()
     # with "missing value where TRUE/FALSE needed" regardless of how clean
     # the data/model fit was. Converting to plain numeric 0/1 (used
     # consistently in both the mediator and outcome models below) avoids
     # that factor-handling path entirely.
     if (!is.logical(data_df[[mediator]])) {
       data_df[[mediator]] <- as.numeric(factor(data_df[[mediator]])) - 1
     } else {
       data_df[[mediator]] <- as.numeric(data_df[[mediator]])
     }
   }
   data_df <- droplevels(data_df)

  print("Checking size dataset")
  print(table(data_df$ethnicity6_combined))

   # Mediator model
   med_formula <- as.formula(paste0(mediator, " ~ ethnicity6_combined + age_group + sex"))
   med_fit <- if (is_ordinal) {
    MASS::polr(med_formula, data = data_df, Hess = TRUE)
   } else {
     glm(med_formula, data = data_df, family = binomial(link = "logit"))
   }
   print(summary(med_fit))

   med_fit$data <- NULL 

   # Outcome model
   out_formula <- as.formula(paste0("Surv(stop_sc, sc_binary) ~ ethnicity6_combined + sex + age_group + ", mediator))
   out_fit <- survreg(out_formula, data = data_df, dist = "weibull", robust = TRUE)
   print(summary(out_fit))

   out_fit$data <- NULL 

   # Mediation
   med_out <- mediate(med_fit, out_fit, treat = "ethnicity6_combined", mediator = mediator,
                      treat.value = group, control.value = "White", sims = 500, long = FALSE)
   print(summary(med_out))
 
   med_tidy <- data.frame(
     term    = c("ACME", "ADE", "Total Effect", "Prop. Mediated"),
     estimate = c(med_out$d.avg,  med_out$z.avg,  med_out$tau.coef, med_out$n.avg),
     conf.low = c(med_out$d.avg.ci[1], med_out$z.avg.ci[1], med_out$tau.ci[1], med_out$n.avg.ci[1]),
     conf.high = c(med_out$d.avg.ci[2], med_out$z.avg.ci[2], med_out$tau.ci[2], med_out$n.avg.ci[2]),
     p.value  = c(med_out$d.avg.p, med_out$z.avg.p, med_out$tau.p, med_out$n.avg.p),
     group    = group,
     mediator = mediator,
     period   = period$end
   )
 
   return(med_tidy)
    rm(med_fit, out_fit, med_out)
    gc()
 }

group_vars <- c("Asian or Asian British")
mediation_vars <- c(mediator_arg)

param_grid <- expand.grid(
  group = group_vars, 
  mediator = mediation_vars,
  stringsAsFactors = FALSE 
)
print(param_grid)

mediation_outputs <- pmap(
  param_grid,
  function(group, mediator) run_mediation(group, mediator)
)

all_results <- bind_rows(mediation_outputs)
write.csv(all_results, file.path(outdir, paste0("mediation_covid_test_aft_", mediator_arg, "_", period_arg, ".csv")), row.names = FALSE)

# ---------------------------------------------------------------------------
# Mediation calculated over covariate patterns instead of per person, to
# avoid mediate() running out of memory (see mediation_collapsed_functions.R).
# Data preparation and models are the same as run_mediation() above.
# ---------------------------------------------------------------------------
source(here("analysis", "mediation_collapsed_functions.R"))

run_mediation_collapsed <- function(group, mediator) {
  data_df <- data_covid %>%
    filter(ethnicity6_combined %in% c(group, "White")) %>%
    restrict_period_reanchored("stop_sc", "date_stop_sc", "sc_binary", period$start, period$end) %>%
    mutate(stop_sc = if_else(stop_sc == 0, 0.5, stop_sc))

  is_ordinal <- mediator %in% names(ordinal_levels)

  if (is_ordinal) {
    data_df <- data_df %>% filter(.data[[mediator]] %in% ordinal_levels[[mediator]])
    data_df[[mediator]] <- factor(data_df[[mediator]], levels = ordinal_levels[[mediator]], ordered = TRUE)
  } else {
    if ("Unknown" %in% levels(factor(data_df[[mediator]]))) {
      data_df <- data_df %>% filter(.data[[mediator]] != "Unknown")
    }
    if (!is.logical(data_df[[mediator]])) {
      data_df[[mediator]] <- as.numeric(factor(data_df[[mediator]])) - 1
    } else {
      data_df[[mediator]] <- as.numeric(data_df[[mediator]])
    }
  }
  data_df <- droplevels(data_df)

  print("Checking size dataset")
  print(table(data_df$ethnicity6_combined))

  # Mediator model
  med_formula <- as.formula(paste0(mediator, " ~ ethnicity6_combined + age_group + sex"))
  med_fit <- if (is_ordinal) {
    MASS::polr(med_formula, data = data_df, Hess = TRUE)
  } else {
    glm(med_formula, data = data_df, family = binomial(link = "logit"))
  }
  # Outcome model
  out_formula <- as.formula(paste0("Surv(stop_sc, sc_binary) ~ ethnicity6_combined + sex + age_group + ", mediator))
  out_fit <- survreg(out_formula, data = data_df, dist = "weibull", robust = TRUE)

  # Mediation
  med_tidy <- mediate_collapsed(med_fit, out_fit, data = data_df,
                                treat = "ethnicity6_combined", mediator = mediator,
                                treat.value = group, control.value = "White", sims = 1000)
  print(med_tidy)

  rm(data_df, med_fit, out_fit)
  gc()

  med_tidy %>% mutate(group = group, mediator = mediator, period = period$end)
}

collapsed_outputs <- pmap(
  param_grid,
  function(group, mediator) run_mediation_collapsed(group, mediator)
)

all_results_collapsed <- bind_rows(collapsed_outputs)
write.csv(all_results_collapsed, file.path(outdir, paste0("mediation_covid_test_aft_collapsed_", mediator_arg, "_", period_arg, ".csv")), row.names = FALSE)
