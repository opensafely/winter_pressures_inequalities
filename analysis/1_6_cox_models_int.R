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
library(car)

## Import custom user functions
source(here::here("analysis", "redaction.R"))
source(here::here("analysis", "cox_model_functions.R"))

outdir <- here("output", "tables")
fs::dir_create(outdir)

args <- commandArgs(trailingOnly = TRUE)

disease <- args[1]
# Which interaction variable to run (e.g. "sex" or "imd_quintile") - split
# out per-run via the yaml to keep memory down, since ethnicity16_combined's
# interaction with imd_quintile alone is ~80 terms under car::Anova type III.
interaction_arg <- args[2]

# # Cox PH models to measure differences in utilisation by ethnicity and IMD
run_cox_models <- function(disease, difference, interactions) {
        data_df <- read_outcome_data(disease)

        message(paste0("Disease: ", disease, " | Difference: ", difference, " | interactions: ", interactions))
        print_stop_summary(data_df)

        # Ensure factor + set reference
        ref <- set_difference_reference(data_df, difference)
        data_df <- ref$data

        # Make sure age and sex are unordered factors, drop unknown imd/ethnicity
        data_df <- prep_covariates(data_df)

        rhs_extra <- paste0(" * ", interactions, " + age_group + sex")

        run_all_outcomes(
                data_df, disease, difference, rhs_extra,
                interaction_var = interactions,
                extra_cols = list(interactions = interactions)
        )
}

by_vars <- c("ethnicity6_combined", "ethnicity16_combined")
int <- c(interaction_arg)

param_grid <- expand.grid(
  disease = disease,
  difference = by_vars,
  interactions = int,
  stringsAsFactors = FALSE
  )
print(param_grid[4, ])

cox_table <- pmap(
  param_grid,
  function(disease, difference, interactions) run_cox_models(disease, difference, interactions)) %>%
  list_rbind()

print(cox_table)

cox_table <- cox_table %>%
                mutate(across(c("estimate", "conf.low", "conf.high"), ~round(.x, 4))) %>%
                # don't need std.error or statistic
                select("term", "estimate", "conf.low", "conf.high", "outcome", "by_var", "disease", "interactions", "n", "n_events", "redacted", "lrt_chisq", "lrt_df", "lrt_p")
file_name <- paste0("cox_models_int_",disease,"_",interaction_arg,".csv")
write_csv(cox_table, file.path(outdir, file_name))
