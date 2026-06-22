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

outdir <- here("output", "tables")
fs::dir_create(outdir)

args <- commandArgs(trailingOnly = TRUE)

disease <- args[1]


# Stratified models - unadjusted
run_cox_models_stratified <- function(disease, difference, 
                                       strat_var, strat_level) {
        # Build filename and read dataset
        file_path <- paste0("an_outcome_first_", disease, ".arrow")
        data_df <- read_feather(here("output", file_path), as_data_frame = TRUE)

        # Subset to stratum
        data_df <- data_df[data_df[[strat_var]] == strat_level, ]
        data_df <- droplevels(data_df)

        # Check data exists after subsetting
        if (nrow(data_df) == 0) {
                message("No data for ", strat_var, " = ", strat_level, " in ", disease)
                return(redacted_row(disease, difference, strat_var, strat_level, "no data in stratum"))
        }

        # Ensure factor + set reference
        if (difference == "ethnicity6_combined") {
                data_df[[difference]] <- factor(data_df[[difference]])
                if ("White" %in% levels(data_df[[difference]])) {
                data_df[[difference]] <- relevel(data_df[[difference]], ref = "White")
        } else {
                message("Skipping: 'White' reference level not present in ", 
                        strat_var, " = ", strat_level)
                return(redacted_row(disease, difference, strat_var, strat_level, "reference level not present"))
        }
}
        if (difference == "ethnicity16_combined") {
                data_df[[difference]] <- factor(data_df[[difference]])
                if ("White British" %in% levels(data_df[[difference]])) {
                data_df[[difference]] <- relevel(data_df[[difference]], ref = "White British")
        } else {
                message("Skipping: 'White British' reference level not present in ", 
                        strat_var, " = ", strat_level)
                return(redacted_row(disease, difference, strat_var, strat_level, "reference level not present"))
        }
}
#         if (difference == "imd_quintile") {
#                 data_df[[difference]] <- factor(data_df[[difference]])
#                 if ("5" %in% levels(data_df[[difference]])) {
#                 data_df[[difference]] <- relevel(data_df[[difference]], ref = "5")
#         } else {
#                 message("Skipping: IMD quintile '5' not present in ", 
#                         strat_var, " = ", strat_level)
#                 return(redacted_row(disease, difference, strat_var, strat_level, "reference level not present"))
#         }
# }
        # Check difference variable has at least 2 levels
        if (nlevels(data_df[[difference]]) < 2) {
                message("Skipping: ", difference, " has fewer than 2 levels in ",
                        strat_var, " = ", strat_level, " for ", disease)
                return(redacted_row(disease, difference, strat_var, strat_level, "fewer than 2 levels"))
        }
        print(class(data_df$age_group))
        print(table(data_df$age_group, data_df$imd_quintile))
        # Define Cox model formulas
        make_formula <- function(start, stop, event, strata_var) {
                as.formula(paste0("Surv(", start, ", ", stop, ", ", event, ") ~ ",
                                  difference, " + strata(", strata_var, ")"))
        }

        f_gp      <- make_formula("start", "stop_gp",      "gp_binary",      "year_end_gp")
        f_gp_sens <- make_formula("start", "stop_gp_sens", "gp_sens_binary", "year_end_gp")
        f_sc      <- make_formula("start", "stop_sc",      "sc_binary",      "year_end_sc")
        f_emerg   <- make_formula("start", "stop_emerg",   "emerg_binary",   "year_end_emerg")

        # Helper to either fit model or return redacted row
        fit_or_redact <- function(formula, binary_var, outcome_name) {
                n_events <- sum(data_df[[binary_var]] == 1, na.rm = TRUE)
                if (n_events > 7) {
                        coxph(formula, data = data_df) %>%
                                tidy(exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>%
                                mutate(outcome = outcome_name, by_var = difference,
                                       disease = disease, 
                                       strat_var = strat_var, strat_level = strat_level)
                } else {
                        message("Redacted: 7 or fewer ", disease, " ", outcome_name, 
                                " events in ", strat_var, " = ", strat_level)
                        tibble(
                                term        = difference,
                                estimate    = NA_real_,
                                std.error   = NA_real_,
                                statistic   = NA_real_,
                                p.value     = NA_real_,
                                conf.low    = NA_real_,
                                conf.high   = NA_real_,
                                outcome     = outcome_name,
                                by_var      = difference,
                                disease     = disease,
                                strat_var   = strat_var,
                                strat_level = strat_level,
                                redacted    = TRUE,
                                redact_reason = "fewer than 7 events"
                        )
                }
        }

        new_rows <- bind_rows(
                fit_or_redact(f_gp,      "gp_binary",      "gp_binary"),
                fit_or_redact(f_gp_sens, "gp_sens_binary", "gp_sens_binary"),
                fit_or_redact(f_sc,      "sc_binary",      "sc_binary"),
                fit_or_redact(f_emerg,   "emerg_binary",   "emerg_binary")
        )

        return(new_rows)
}

eth_vars <- c("ethnicity6_combined", "ethnicity16_combined")
strat_params <- bind_rows(
        expand.grid(strat_var = "sex",         strat_level = c("male", "female"),   stringsAsFactors = FALSE),
        expand.grid(strat_var = "imd_quintile", strat_level = c("1","2","3","4","5"), stringsAsFactors = FALSE)
)

param_grid_strat <- expand.grid(
        disease    = disease,
        difference = eth_vars,
        stringsAsFactors = FALSE
) %>% cross_join(strat_params)

cox_table_stratified <- pmap_dfr(param_grid_strat, function(disease, difference, strat_var, strat_level) {
        run_cox_models_stratified(disease, difference, strat_var, strat_level)
})

cox_table_stratified <- cox_table_stratified %>% 
                mutate(across(c("estimate", "conf.low", "conf.high"), ~round(.x, 2))) %>%
                # don't need std.error or statistic
                select("term", "estimate", "conf.low", "conf.high", "outcome", "by_var", "disease", "strat_var", "strat_level", "redacted", "redact_reason")

file_name <- paste0("cox_models_stratified_",disease,".csv")

write_csv(cox_table_stratified, file.path(outdir, file_name))
