
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

        print("stop check")
        print("GP")
        print(summary(data_df$stop_gp))
        print("GP sens")
        print(summary(data_df$stop_gp_sens))
        print("SC")
        print(summary(data_df$stop_sc))
        print("A & E")
        print(summary(data_df$stop_emerg))

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
        
        # Make sure age and sex are unordered factors
        data_df <- data_df %>% 
                mutate(
                        age_group = relevel(factor(as.character(age_group), ordered = FALSE), ref = "adult_under_65"),
                        sex = factor(as.character(sex), ordered = FALSE),
                        urban_rural_5 = relevel(factor(as.character(urban_rural_5), ordered = FALSE), ref = "Urban minor conurbation"),
                        migrant = relevel(factor(as.character(migrant), ordered = FALSE), ref = "Non-migrant"),
                        english_language = relevel(factor(as.character(english_language), ordered = FALSE), ref = "TRUE"),
                        bmi_category = relevel(factor(as.character(bmi_category), ordered = FALSE), ref = "ideal")
                        )

        # Define adjustment terms - taking out rsv eligibility as is same as one of the age groups
        # taking out BMI for now as problems identifying
        adj_vars <- switch(adjustment,
                "unadjusted"  = "",
                "age_sex"     = " + age_group + sex",
                "migration"        = " + age_group + sex + migrant + english_language",
                "area"  = if (difference == "imd_quintile") {
                        " + age_group + sex + urban_rural_5"
                        } else{
                        " + age_group + sex + imd_quintile + urban_rural_5"
                        },
                "health" = " + age_group + sex + vacc_elig_covid + vacc_elig_flu + prior_flu_vacc_bin + prior_covid_vacc_bin + bmi_category",
                "health_2018" = " + age_group + sex + vacc_elig_flu + prior_flu_vacc_bin +bmi_category",
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

        ## Check if >2 events for each level of expo in full data
        check_events_full <- data_df %>%
        group_by(across(all_of(difference))) %>%
        summarise(
                total_events_gp = sum(gp_binary),
                total_events_gp_s = sum(gp_sens_binary),
                total_events_sc = sum(sc_binary),
                total_events_emerg = sum(emerg_binary),
                .groups = "keep") %>%
        ungroup() 
        full_gp = all(check_events_full$total_events_gp > 2) 
        full_gp_s = all(check_events_full$total_events_gp_s > 2) 
        full_sc = all(check_events_full$total_events_sc > 2) 
        full_emerg = all(check_events_full$total_events_emerg > 2) 

        # # Helper to either fit model or return redacted row
        fit_or_redact <- function(formula, binary_var, full_var, outcome_name) {
                n_events <- sum(data_df[[binary_var]] == 1, na.rm = TRUE)
                if (n_events > 7 & full_var) {
                        tryCatch({
                                coxph(formula, data = data_df) %>%
                                        tidy(exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>%
                                        mutate(outcome = outcome_name, by_var = difference, disease = disease, adjustment = adjustment)
                                }, error = function(e) {
                                message("Model failed for ", disease, " ", outcome_name, ": ", e$message)
                                tibble(
                                        term = difference, estimate = NA_real_, std.error = NA_real_,
                                        statistic = NA_real_, p.value = NA_real_, conf.low = NA_real_,
                                        conf.high = NA_real_, outcome = outcome_name, by_var = difference,
                                        disease = disease, adjustment = adjustment, n = NA_real_,
                                        n_events    = NA_real_,    
                                )
                                })
                } else if (n_events <= 7) {
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
                                n           = NA_real_,
                                n_events    = NA_real_,    
                                redacted    = TRUE
                        )
                } else {
                        message("Redacted: 2 or fewer ", disease, " ", outcome_name, " events per strata")
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
                                n           = NA_real_,
                                n_events    = NA_real_,    
                                redacted    = TRUE
                        )
                }
        }

        new_rows <- bind_rows(
                fit_or_redact(f_gp,    "gp_binary",  full_gp,  "gp_binary"),
                fit_or_redact(f_gp_sens,    "gp_sens_binary",  full_gp_s,  "gp_sens_binary"),
                fit_or_redact(f_sc,    "sc_binary",  full_sc,  "sc_binary"),
                fit_or_redact(f_emerg, "emerg_binary", full_emerg, "emerg_binary")
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
print(param_grid[4, ])

cox_table <- pmap(
  param_grid,
  function(disease, difference, adjustment) run_cox_models(disease, difference, adjustment)) %>%
  list_rbind()

print(cox_table)

cox_table <- cox_table %>% 
                mutate(across(c("estimate", "conf.low", "conf.high"), ~round(.x, 2))) %>%
                # don't need std.error or statistic
                select("term", "estimate", "conf.low", "conf.high", "outcome", "by_var", "disease", "adjustment", "n", "n_events", "redacted")
file_name <- paste0("cox_models_",disease,".csv")
write_csv(cox_table, file.path(outdir, file_name))