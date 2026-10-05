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
strat <- args[2]


# Stratified models - adjusted for age_group and sex (matching
# 1_6_cox_models_int.R's covariate set), except whichever of those is the
# stratifying variable itself. age_group is also dropped when stratifying
# by age_infants, since the two are near-collinear (age_group has almost
# no variation within the infant/not-infant split).
run_cox_models_stratified <- function(disease, difference,
                                       strat_var, strat_level) {
        data_df <- read_outcome_data(disease)

        # Subset to stratum
        data_df <- data_df[data_df[[strat_var]] == strat_level, ]
        data_df <- droplevels(data_df)

        context_label <- paste0(" in ", strat_var, " = ", strat_level)
        extra_cols <- list(strat_var = strat_var, strat_level = strat_level)

        # Check data exists after subsetting
        if (nrow(data_df) == 0) {
                message("No data for ", strat_var, " = ", strat_level, " in ", disease)
                return(cox_redacted_grid(difference, disease, "no data in stratum", extra_cols))
        }

        # Ensure factor + set reference (skip stratum if reference level absent)
        ref <- set_difference_reference(data_df, difference, context_label)
        if (!ref$ok) {
                return(cox_redacted_grid(difference, disease, "reference level not present", extra_cols))
        }
        data_df <- ref$data

        # Check difference variable has at least 2 levels
        if (nlevels(data_df[[difference]]) < 2) {
                message("Skipping: ", difference, " has fewer than 2 levels in ",
                        strat_var, " = ", strat_level, " for ", disease)
                return(cox_redacted_grid(difference, disease, "fewer than 2 levels", extra_cols))
        }

        # Make sure age and sex are unordered factors, drop unknown imd/ethnicity
        data_df <- prep_covariates(data_df)

        adjustment_covariates <- c("age_group", "sex")
        if (strat_var == "age_infants") {
                adjustment_covariates <- setdiff(adjustment_covariates, "age_group")
        }
        adjustment_covariates <- setdiff(adjustment_covariates, strat_var)
        rhs_extra <- if (length(adjustment_covariates) > 0) {
                paste0(" + ", paste(adjustment_covariates, collapse = " + "))
        } else {
                ""
        }

        run_all_outcomes(
                data_df, disease, difference, rhs_extra,
                full_events_check = FALSE,
                extra_cols = extra_cols,
                context_label = context_label
        )
}

eth_vars <- c("ethnicity6_combined", "ethnicity16_combined")
strat_params <- bind_rows(
        expand.grid(strat_var = "sex",         strat_level = c("male", "female"),   stringsAsFactors = FALSE),
        expand.grid(strat_var = "imd_quintile", strat_level = c("1","2","3","4","5"), stringsAsFactors = FALSE)
)
# Need to stratify by age for RSV as most cases in infants
strat_params_rsv <- bind_rows(
        expand.grid(strat_var = "age_infants",         strat_level = c("infant", "not infant"),   stringsAsFactors = FALSE),
        expand.grid(strat_var = "sex",         strat_level = c("male", "female"),   stringsAsFactors = FALSE),
        expand.grid(strat_var = "imd_quintile", strat_level = c("1","2","3","4","5"), stringsAsFactors = FALSE)
)
strat <- get(strat)
param_grid_strat <- expand.grid(
        disease    = disease,
        difference = eth_vars,
        stringsAsFactors = FALSE
) %>% cross_join(strat)

cox_table_stratified <- pmap_dfr(param_grid_strat, function(disease, difference, strat_var, strat_level) {
        run_cox_models_stratified(disease, difference, strat_var, strat_level)
})

cox_table_stratified <- cox_table_stratified %>%
                mutate(across(c("estimate", "conf.low", "conf.high"), ~round(.x, 2))) %>%
                # don't need std.error or statistic
                select("term", "estimate", "conf.low", "conf.high", "outcome", "by_var", "disease", "strat_var", "strat_level")

file_name <- paste0("cox_models_stratified_",disease,".csv")

write_csv(cox_table_stratified, file.path(outdir, file_name))
