
library(here)
library(broom)
library(dplyr)
library(tidyverse)
library(arrow)
library(finalfit)
library(survival)
library(rlang)
library(purrr)
library(finalfit)
library(forcats)
library(gtsummary)
library(survminer)
library(glue)

## Import custom user functions
source(here::here("analysis", "redaction.R"))
source(here::here("analysis", "cox_model_functions.R"))

outdir <- here("output", "tables")
fs::dir_create(outdir)

args <- commandArgs(trailingOnly = TRUE)

disease <- args[1]
adj_yr <- args[2]

# # Cox PH models to measure differences in utilisation by ethnicity and IMD
run_cox_models <- function(disease, difference, adjustment, period_start, period_end) {
        data_df <- read_outcome_data(disease)

        message(paste0("Disease: ", disease, " | Difference: ", difference, " | Adjustment: ", adjustment))
        print_stop_summary(data_df)

        # Ensure factor + set reference
        ref <- set_difference_reference(data_df, difference)
        data_df <- ref$data

        # Make sure age and sex are unordered factors, drop unknown imd/ethnicity
        data_df <- prep_covariates(data_df)

        # Define adjustment terms - taking out rsv eligibility as is same as one of the age groups
        # taking out BMI for now as problems identifying
        rhs_extra <- build_adjustment_terms(adjustment, difference)

        run_all_outcomes(
                data_df, disease, difference, rhs_extra,
                period = list(start = period_start, end = period_end),
                extra_cols = list(adjustment = adjustment, period = period_end)
        )
}

by_vars <- c("imd_quintile", "ethnicity6_combined", "ethnicity16_combined")
adjustments_2021 <- c("unadjusted", "age_sex", "migration", "area", "health", "all")
# 2018 adjustments to be updated to add all when doing flu/RSV outcomes
adjustments_2018 <- c("unadjusted", "age_sex", "migration", "area", "health_2018")

adj <- get(paste0("adjustments_",adj_yr))

periods <- tibble(
  period_start = as.Date(c("2021-03-01", "2022-09-01")),
  period_end   = as.Date(c("2022-08-31", "2026-02-28"))
)

param_grid <- expand.grid(
  disease = disease,
  difference = by_vars,
  adjustment = adj,
  stringsAsFactors = FALSE
  ) %>%
  cross_join(periods)
print(param_grid[4, ])

cox_table <- pmap(
  param_grid,
  function(disease, difference, adjustment, period_start, period_end) run_cox_models(disease, difference, adjustment, period_start, period_end)) %>%
  list_rbind()

print(cox_table)

cox_table <- cox_table %>%
                mutate(across(c("estimate", "conf.low", "conf.high"), ~round(.x, 4))) %>%
                # don't need std.error or statistic
                select("term", "estimate", "conf.low", "conf.high", "outcome", "by_var", "disease", "adjustment", "period", "n", "n_events", "n_level", "n_events_level", "redacted")
file_name <- paste0("cox_models_time_",disease,".csv")
write_csv(cox_table, file.path(outdir, file_name))
