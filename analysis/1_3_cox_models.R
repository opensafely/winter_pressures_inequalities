
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
adj_yr <- args[2]

# # Cox PH models to measure differences in utilisation by ethnicity and IMD
run_cox_models <- function(disease, difference, adjustment) {
        # Build filename and read dataset
        file_path <- paste0("an_outcome_first_",disease,".arrow")
        data_df <- read_feather(here("output", file_path), as_data_frame = TRUE)

        message(paste0("Disease: ", disease, " | Difference: ", difference, " | Adjustment: ", adjustment))
        #if (nrow(data_df) == 0) stop("Empty dataset for: ", file_path)

        # Ensure factor + set reference
        if (difference == "ethnicity6_combined") {
                data_df[[difference]] <- factor(data_df[[difference]])
                data_df[[difference]] <- relevel(data_df[[difference]], ref = "White")
        }

        if (difference == "ethnicity16_combined") {
                data_df[[difference]] <- factor(data_df[[difference]])
                data_df[[difference]] <- relevel(data_df[[difference]], ref = "White British")
        }

        if (difference == "imd_quintile") {
                data_df[[difference]] <- factor(data_df[[difference]])
                data_df[[difference]] <- relevel(data_df[[difference]], ref = "5") 
        }
        
        # Define adjustment terms - taking out rsv eligibility as is same as one of the age groups
        adj_vars <- switch(adjustment,
                "unadjusted"  = "",
                "age_sex"     = " + age_group + sex",
                "migration"        = " + age_group + sex + migrant + english_language",
                "area"  = " + age_group + sex + imd_quintile + urban_rural_5",
                "health" = " + age_group + sex + vacc_elig_covid + vacc_elig_flu + prior_flu_vaccination + prior_covid_vaccination + bmi_category",
                "health_2018" = " + age_group + sex + vacc_elig_flu + prior_flu_vaccination + bmi_category",
                stop("Unknown adjustment: ", adjustment)
        )

        # Define Cox model formulas
        make_formula <- function(start, stop, event, strata_var) {
                as.formula(paste0("Surv(", start, ", ", stop, ", ", event, ") ~ ", 
                                  difference, adj_vars, " + strata(", strata_var, ")"))
        }

        # define Cox model formulas
        f_gp     <- make_formula("start", "stop_gp",      "gp_binary",      "year_end_gp")
        f_gp_sens <- make_formula("start", "stop_gp_sens", "gp_sens_binary", "year_end_gp")
        f_sc     <- make_formula("start", "stop_sc",      "sc_binary",      "year_end_sc")
        f_emerg  <- make_formula("start", "stop_emerg",   "emerg_binary",   "year_end_emerg")

        # # Helper to either fit model or return redacted row
        fit_or_redact <- function(formula, binary_var, outcome_name) {
                n_events <- sum(data_df[[binary_var]] == 1, na.rm = TRUE)
                if (n_events > 7) {
                        coxph(formula, data = data_df) %>%
                                tidy(exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>%
                                mutate(outcome = outcome_name, by_var = difference, disease = disease, adjustment = adjustment)
                } else {
                        message("Redacted: 7 or fewer ", disease, " ", outcome_name, " events")
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
                                adjustment = adjustment,
                                redacted    = TRUE
                        )
                }
        }

        new_rows <- bind_rows(
                fit_or_redact(f_gp,    "gp_binary",    "gp_binary"),
                fit_or_redact(f_gp_sens,    "gp_sens_binary",    "gp_sens_binary"),
                fit_or_redact(f_sc,    "sc_binary",    "sc_binary"),
                fit_or_redact(f_emerg, "emerg_binary", "emerg_binary")
        )

        return(new_rows)
}

by_vars <- c("imd_quintile", "ethnicity6_combined", "ethnicity16_combined")
adjustments_2021 <- c("unadjusted", "age_sex", "migration", "area", "health")
adjustments_2018 <- c("unadjusted", "age_sex", "migration", "area", "health_2018")

adj <- get(paste0("adjustments_",adj_yr))

param_grid <- expand.grid(
  disease = disease, 
  difference = by_vars, 
  adjustment = adj,
  stringsAsFactors = FALSE
  )
print(param_grid)

cox_table <- pmap_dfr(
  param_grid,
  function(disease, difference, adjustment) run_cox_models(disease, difference, adjustment)
)
print(cox_table)

cox_table <- cox_table %>% 
                mutate(across(c("estimate", "conf.low", "conf.high"), ~round(.x, 2))) %>%
                # don't need std.error or statistic
                select("term", "estimate", "conf.low", "conf.high", "outcome", "by_var", "disease", "adjustment")
file_name <- paste0("cox_models_",disease,".csv")
write_csv(cox_table, file.path(outdir, file_name))