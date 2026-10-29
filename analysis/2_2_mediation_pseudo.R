library(arrow)
library(survival)
library(mediation)
library(tidyverse)
library(broom)
library(here)

outdir <- here("output", "tables")
fs::dir_create(outdir)

# Which time period to run - passed in via the yaml
args <- commandArgs(trailingOnly = TRUE)
period_arg <- args[1]

# Same two calendar periods as 2_1_mediation.R. rmst_tau is the RMST horizon
# in days - period1 is only 548 days long, so it can't use the two-year
# horizon used for period2
periods <- list(
  period1 = list(start = as.Date("2021-03-01"), end = as.Date("2022-08-31"), rmst_tau = 365),
  period2 = list(start = as.Date("2022-09-01"), end = as.Date("2026-02-28"), rmst_tau = 730)
)
period <- periods[[period_arg]]
if (is.null(period)) {
  stop("Unknown period: ", period_arg, " (expected one of: ", paste(names(periods), collapse = ", "), ")")
}

# As in 2_1_mediation.R: restrict follow-up to the period and re-anchor time
# at whichever is later of the person's entry or the period start
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

data_covid <- read_feather(here("output/an_outcome_first_covid_2022.arrow"), as_data_frame = TRUE)

# Run e-value method in COVID cohort (https://arxiv.org/pdf/2411.17533)
data_df <- data_covid %>%
    restrict_period_reanchored("stop_sc", "date_stop_sc", "sc_binary", period$start, period$end) %>%
    # setting events on same day as follow up at half day - applied after
    # period restriction, which can itself produce new stop_sc == 0 values
    mutate(stop_sc = if_else(stop_sc == 0, 0.5, stop_sc)) %>%
    filter(ethnicity6_combined %in% c("Mixed", "White")) %>%
    filter(migrant != 'Unknown') %>% 
    mutate(migrant = as.numeric(migrant == "Non-migrant")) %>%
    droplevels()


med_fit <- glm(migrant ~ ethnicity6_combined + age_group + sex, data = data_df, family = binomial(link = "logit"))
print(summary(med_fit))

km_fit <- survfit(Surv(stop_sc,sc_binary) ~ 1, data = data_df) # Kaplan-Meier Estimator
# Fit pseudo values. Can fit targeting survival probability or RMST - chose RMST as gives absolute tome gained/lost
# RMST horizon set per period (see periods above)
data_df$pseudo <- pseudo(fit = km_fit,times = period$rmst_tau ,type = "rmst")

# Now fit linear mediation model
out_fit_pseudo <- lm(pseudo ~ ethnicity6_combined + sex + age_group + migrant, data = data_df)
print(summary(out_fit_pseudo))

med_out_pseudo <- mediate(med_fit, out_fit_pseudo, treat = "ethnicity6_combined", mediator = "migrant",
                           treat.value = "Mixed", control.value = "White")
print(summary(med_out_pseudo))

med_tidy <- data.frame(
  term      = c("ACME", "ADE", "Total Effect", "Prop. Mediated"),
  estimate  = c(med_out_pseudo$d.avg,     med_out_pseudo$z.avg,     med_out_pseudo$tau.coef, med_out_pseudo$n.avg),
  conf.low  = c(med_out_pseudo$d.avg.ci[1], med_out_pseudo$z.avg.ci[1], med_out_pseudo$tau.ci[1], med_out_pseudo$n.avg.ci[1]),
  conf.high = c(med_out_pseudo$d.avg.ci[2], med_out_pseudo$z.avg.ci[2], med_out_pseudo$tau.ci[2], med_out_pseudo$n.avg.ci[2]),
  p.value   = c(med_out_pseudo$d.avg.p,   med_out_pseudo$z.avg.p,   med_out_pseudo$tau.p,    med_out_pseudo$n.avg.p),
  group     = "Mixed",
  mediator  = "migrant",
  period    = period$end,
  rmst_tau  = period$rmst_tau
)

write.csv(med_tidy, file.path(outdir, paste0("mediation_covid_pseudo_", period_arg, ".csv")), row.names = FALSE)

# ---------------------------------------------------------------------------
# Mediation calculated over covariate patterns instead of per person, to
# avoid mediate() running out of memory (see mediation_collapsed_functions.R).
# Uses the same med_fit and out_fit_pseudo models fitted above.
# ---------------------------------------------------------------------------
source(here("analysis", "mediation_collapsed_functions.R"))

med_tidy_collapsed <- mediate_collapsed(med_fit, out_fit_pseudo, data = data_df,
                                        treat = "ethnicity6_combined", mediator = "migrant",
                                        treat.value = "Mixed", control.value = "White", sims = 1000) %>%
  mutate(
    group    = "Mixed",
    mediator = "migrant",
    period   = period$end,
    rmst_tau = period$rmst_tau
  )
print(med_tidy_collapsed)

write.csv(med_tidy_collapsed, file.path(outdir, paste0("mediation_covid_pseudo_collapsed_", period_arg, ".csv")), row.names = FALSE)