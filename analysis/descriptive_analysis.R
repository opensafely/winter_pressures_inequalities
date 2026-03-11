###################################################
# This script defines generates baseline tables 
# for both cohorts
#
# Author: Ruth Costello, LSHTM
#####################################################################
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
library(gtsummary)
library(survminer)
library(glue)

## Import custom user functions
source(here::here("analysis", "redaction.R"))

outdir <- here("output", "tables")
fs::dir_create(outdir)

#define function for midpoint 6 rounding
roundmid_any <- function(x, to = 6) {
  
  # like round_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x != 0))
  
}

#Creating baseline tables - first import data
baseline_covid <- read_feather(here("output/an_bl_data_covid.arrow")) %>% 
        mutate(imd_quintile = as.factor(imd_quintile),
        min_yr_start = as.factor(min_start_yr))
baseline_flu_rsv <- read_feather(here("output/an_bl_data_flu_rsv.arrow"))%>% 
        mutate(imd_quintile = as.factor(imd_quintile),
        min_yr_start = as.factor(min_start_yr))

var_labels_list <- list(
                min_start_yr ~ "Baseline year",
                age_group ~ "Age group",
                imd_quintile ~ "IMD quintile",
                ethnicity16_combined ~ "Ethnicity (16 groups)",
                sex ~ "Sex",
                urban_rural_5 ~ "Urban-rural classification",
                migrant ~ "Migrant",
                english_language ~ "English not main language spoken",
                flu_atrisk_grp ~ "Flu clinical risk group",
                covid_atrisk_grp ~ "COVID-19 clinical risk group"
                )


# COVID cohort baseline table by ethnicity
# desc_covid <- baseline_covid %>% 
#         select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
#         urban_rural_5, imd_quintile, migrant, english_language, 
#         flu_atrisk_grp, covid_atrisk_grp) %>% 
#         mutate(ethnicity6_combined = fct_explicit_na(
#                 ethnicity6_combined, 
#                 na_level = "Missing")) %>%
#         tbl_summary(
#                 missing = "no",
#                 by = ethnicity6_combined,
#                 label = var_labels_list,
#         ) %>% 
#         add_overall(last = FALSE)
# desc_covid$inputs$data <- NULL

# # Apply redactions to baseline table
# desc_covid_redact <- redact_tblsummary(desc_covid, 7) 

# #Apply midpoint 6 rounding
# raw_stats <- desc_covid_redact$meta_data %>%
#         select(var_label, df_stats) %>%
#         unnest(df_stats) %>%
#         select(-any_of("fmt_fn"))
# print(unique(raw_stats$by))
# raw_stats_midpoint <- raw_stats %>%
#   mutate(
#     n=roundmid_any(n),
#     N=roundmid_any(N),
#     p=round(100*n/N,1),
#     N_miss = roundmid_any(N_miss),
#     N_obs = roundmid_any(N_obs),
#     p_miss = round(100*N_miss/N_obs,1),
#     N_nonmiss = roundmid_any(N_nonmiss),
#     p_nonmiss = round(100*N_nonmiss/N_obs,1),
#     var_label = factor(var_label),
#     variable_levels = replace_na(as.character(variable_levels), "")
#   ) 

# desc_covid_redact_round <- raw_stats_midpoint %>%
#   rowwise() %>%
#   transmute(
#     var_label,
#     # gt creates a column called `label` when run locally, `variable_labels` 
#     # when run in opensafely (probs different versions)
#     # label,
#     variable_levels,
#     by,
#     value = glue::glue(stat_display, .envir = cur_data())
#   ) %>%
#   pivot_wider(
#     names_from = by,
#     values_from = value
#   )

# Grouped table
desc_covid_by <- baseline_covid %>% 
        select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
               urban_rural_5, imd_quintile, migrant, english_language, 
               flu_atrisk_grp, covid_atrisk_grp) %>% 
        mutate(ethnicity6_combined = fct_explicit_na(ethnicity6_combined, na_level = "Missing")) %>%
        tbl_summary(
                missing = "no",
                by = ethnicity6_combined,
                label = var_labels_list
        )

# Overall table (no by)
desc_covid_overall <- baseline_covid %>% 
        select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
               urban_rural_5, imd_quintile, migrant, english_language, 
               flu_atrisk_grp, covid_atrisk_grp) %>% 
        mutate(ethnicity6_combined = fct_explicit_na(ethnicity6_combined, na_level = "Missing")) %>%
        tbl_summary(
                missing = "no",
                label = var_labels_list
        )

# Extract and round both
extract_round_stats <- function(tbl) {
        tbl$meta_data %>%
                select(var_label, df_stats) %>%
                unnest(df_stats) %>%
                select(-any_of("fmt_fn")) %>%
                mutate(
                        n         = roundmid_any(n),
                        N         = roundmid_any(N),
                        p         = round(100*n/N, 1),
                        N_miss    = roundmid_any(N_miss),
                        N_obs     = roundmid_any(N_obs),
                        p_miss    = round(100*N_miss/N_obs, 1),
                        N_nonmiss = roundmid_any(N_nonmiss),
                        p_nonmiss = round(100*N_nonmiss/N_obs, 1),
                        var_label = factor(var_label),
                        variable_levels = replace_na(as.character(variable_levels), "")
                )
}

raw_stats_by      <- extract_round_stats(desc_covid_by)
raw_stats_overall <- extract_round_stats(desc_covid_overall) %>%
        mutate(by = "Overall")  # add by column so pivot_wider works

# Combine and pivot
desc_covid_redact_round <- bind_rows(raw_stats_overall, raw_stats_by) %>%
        rowwise() %>%
        transmute(
                var_label,
                variable_levels,
                by,
                value = glue::glue(stat_display, .envir = cur_data())
        ) %>%
        pivot_wider(
                names_from = by,
                values_from = value
        )
# saving both rounded/redacted and not rpound/redacted versions
write_csv(desc_covid_redact_round, file.path(outdir, "baseline_table_covid_r.csv"))
gt::gtsave(as_gt(desc_covid_by), file.path(outdir, "baseline_table_covid.html"))

# Flu/RSV cohort baseline table by ethnicity
# Grouped table
desc_flu_rsv_by <- baseline_flu_rsv %>% 
        select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
               urban_rural_5, imd_quintile, migrant, english_language, 
               flu_atrisk_grp, covid_atrisk_grp) %>% 
        mutate(ethnicity6_combined = fct_explicit_na(ethnicity6_combined, na_level = "Missing")) %>%
        tbl_summary(
                missing = "no",
                by = ethnicity6_combined,
                label = var_labels_list
        )

# Overall table (no by)
desc_flu_rsv_overall <- baseline_flu_rsv %>% 
        select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
               urban_rural_5, imd_quintile, migrant, english_language, 
               flu_atrisk_grp, covid_atrisk_grp) %>% 
        mutate(ethnicity6_combined = fct_explicit_na(ethnicity6_combined, na_level = "Missing")) %>%
        tbl_summary(
                missing = "no",
                label = var_labels_list
        )

raw_stats_by      <- extract_round_stats(desc_flu_rsv_by)
raw_stats_overall <- extract_round_stats(desc_flu_rsv_overall) %>%
        mutate(by = "Overall")  # add by column so pivot_wider works

# Combine and pivot
desc_flu_rsv_redact_round <- bind_rows(raw_stats_overall, raw_stats_by) %>%
        rowwise() %>%
        transmute(
                var_label,
                variable_levels,
                by,
                value = glue::glue(stat_display, .envir = cur_data())
        ) %>%
        pivot_wider(
                names_from = by,
                values_from = value
        )
# saving both rounded/redacted and not rpound/redacted versions
write_csv(desc_flu_rsv_redact_round, file.path(outdir, "baseline_table_flu_rsv_r.csv"))
gt::gtsave(as_gt(desc_flu_rsv_by), file.path(outdir, "baseline_table_flu_rsv.html"))

# generating tables with rounded weekly events, denominators and rates from measures
rates_covid <- read_csv(here("output/measures/measures_covid.csv")) %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_covid, file.path(outdir, "rounded_rates_covid.csv"))

rates_flu_18 <- read_csv(here("output/measures/measures_flu_rsv_2018.csv")) %>% 
        filter(measure=="flu_gp" | measure=="flu_admission") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_flu_18, file.path(outdir, "rounded_rates_flu_18.csv"))

rates_flu_21 <- read_csv(here("output/measures/measures_flu_rsv_2021.csv")) %>% 
        filter(measure=="flu_gp" | measure=="flu_admission") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_flu_21, file.path(outdir, "rounded_rates_flu_21.csv"))

rates_rsv_18 <- read_csv(here("output/measures/measures_flu_rsv_2018.csv")) %>% 
        filter(measure=="rsv_gp" | measure=="rsv_admission") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_rsv_18, file.path(outdir, "rounded_rates_rsv_18.csv"))

rates_rsv_21 <- read_csv(here("output/measures/measures_flu_rsv_2021.csv")) %>% 
        filter(measure=="rsv_gp" | measure=="rsv_admission") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_rsv_21, file.path(outdir, "rounded_rates_rsv_21.csv"))


# Making (not very good) line graphs of rates 
plot_line <- function(file, disease) {

        p <- ggplot(
                data = file,
                mapping = aes(
                x = interval_start,
                y = rate_mid6,
                group = interaction(ethnicity, measure),
                color = ethnicity,
                )
        ) +
                geom_line() +
                geom_point(size = 1) +
                facet_wrap(~measure, scales = "free_y") +
                labs(title = paste("Disease rates for", disease))

        ggsave(
                filename = paste0("plot_", disease,".svg"),
                plot = p,
                path = outdir
        )

        return(p)
}

disease_list <- list(
    covid = rates_covid,
    flu_18 = rates_flu_18,
    flu_21 = rates_flu_21,
    rsv_18 = rates_rsv_18,
    rsv_21 = rates_rsv_21
)

plots <- lapply(names(disease_list), function(d_name) {
    plot_line(disease_list[[d_name]], d_name)
})

# Table of outcomes - first event for Cox models
outcomes_covid <- read_feather(here("output/an_outcome_first_covid.arrow")) 
outcomes_flu <- read_feather(here("output/an_outcome_first_flu.arrow")) 
outcomes_rsv <- read_feather(here("output/an_outcome_first_rsv.arrow")) 

#Function to create table with number of outcomes, person-time, rate and flag for outcome type - no rounding yet
create_event_table <- function(data, time, status, outcome, group = NULL, rate_multiplier = 1000) {
  
  if (is.null(group)) {
    
    data %>%
      summarise(
        N = n(),
        Events = sum(.data[[status]], na.rm = TRUE),
        Person_time = sum(.data[[time]], na.rm = TRUE),
        Rate = (Events / Person_time) * rate_multiplier,
        Outcome = outcome
      )
    
  } else {
    
    data %>%
      group_by(.data[[group]]) %>%
      summarise(
        N = n(),
        Events = sum(.data[[status]], na.rm = TRUE),
        Person_time = sum(.data[[time]], na.rm = TRUE),
        Rate = (Events / Person_time) * rate_multiplier,
        Outcome = outcome,
        .groups = "drop"
      )
  }
}

# Run function for each outcome with GP, hospitalisations and both and create table
count_covid_gp <- create_event_table(outcomes_covid, time = "stop_gp", status = "gp_binary", outcome = "COVID - GP", group = "ethnicity6_combined")
count_covid_sc <- create_event_table(outcomes_covid, time = "stop_sc", status = "sc_binary", outcome = "COVID - SC", group = "ethnicity6_combined")
count_covid_all <- create_event_table(outcomes_covid, time = "stop", status = "binary_all", outcome = "COVID - all", group = "ethnicity6_combined")
count_covid <- bind_rows(count_covid_gp, count_covid_sc, count_covid_all)
write_csv(count_covid, file.path(outdir, "count_covid.csv"))

count_flu_gp <- create_event_table(outcomes_flu, time = "stop_gp", status = "gp_binary", outcome = "flu - GP", group = "ethnicity6_combined")
count_flu_sc <- create_event_table(outcomes_flu, time = "stop_sc", status = "sc_binary", outcome = "flu - SC", group = "ethnicity6_combined")
count_flu_all <- create_event_table(outcomes_flu, time = "stop", status = "binary_all", outcome = "flu - all", group = "ethnicity6_combined")
count_flu <- bind_rows(count_flu_gp, count_flu_sc, count_flu_all)
write_csv(count_flu, file.path(outdir, "count_flu.csv"))

count_rsv_gp <- create_event_table(outcomes_rsv, time = "stop_gp", status = "gp_binary", outcome = "rsv - GP", group = "ethnicity6_combined")
count_rsv_sc <- create_event_table(outcomes_rsv, time = "stop_sc", status = "sc_binary", outcome = "rsv - SC", group = "ethnicity6_combined")
count_rsv_all <- create_event_table(outcomes_rsv, time = "stop", status = "binary_all", outcome = "rsv - all", group = "ethnicity6_combined")
count_rsv <- bind_rows(count_rsv_gp, count_rsv_sc, count_rsv_all)
write_csv(count_rsv, file.path(outdir, "count_rsv.csv"))



# Cox PH models to measure differences in utilisation by ethnicity and IMD
run_cox_models <- function(disease, difference) {
        # Build filename and read dataset
        file_path <- paste0("an_outcome_first_",disease,".arrow")
        data_df <- read_feather(here("output", file_path), as_data_frame = TRUE)

        # Ensure factor + set reference
        if (difference == "ethnicity6_combined") {
                data_df[[difference]] <- factor(data_df[[difference]])
                data_df[[difference]] <- relevel(data_df[[difference]], ref = "White")
        }

        if (difference == "imd_quintile") {
                data_df[[difference]] <- factor(data_df[[difference]])
                data_df[[difference]] <- relevel(data_df[[difference]], ref = "1") 
        }
        
        # define Cox model formulas
        f_gp <- as.formula(paste0("Surv(start, stop_gp, gp_binary) ~ ", difference, " + strata(year_end_gp)"))
        f_sc <- as.formula(paste0("Surv(start, stop_sc, sc_binary) ~ ", difference, " + strata(year_end_sc)"))
        # Fit Cox models
        mod_gp <- coxph(f_gp, data = data_df)
        mod_sc <- coxph(f_sc, data = data_df)

        new_rows <- bind_rows(
                tidy(mod_gp, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% mutate(outcome = "gp_binary", by_var = difference, disease = disease),
                tidy(mod_sc, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% mutate(outcome = "sc_binary", by_var = difference, disease = disease)
        )

        return(new_rows)
}

 diseases <- c("covid", "flu", "rsv")
 by_vars <- c("imd_quintile", "ethnicity6_combined")

param_grid <- expand.grid(disease = diseases, difference = by_vars, stringsAsFactors = FALSE)

cox_table <- pmap_dfr(
  param_grid,
  function(disease, difference) run_cox_models(disease, difference)
)

write_csv(cox_table, file.path(outdir, "cox_models.csv"))

#test KM plot

# km_gp <- survfit(Surv(stop_gp, gp_binary) ~ ethnicity6_combined, data = outcomes_covid)
# # Generate GP plot 
# names(km_gp$strata) <- gsub("ethnicity6_combined=", "", names(km_gp$strata))
# gp_plot <- ggsurvplot(km_gp, data = outcomes_covid, risk.table = TRUE,
#         conf.int = TRUE, pval = FALSE, legend = "right", title = ("Kaplan-Meier: COVID-19 (GP)"),
#         ggtheme = theme_bw())

# ggsave(file.path(outdir, "plot_km_covid_gp.svg"), gp_plot$plot)

#Kaplan Meier plots - COVID
 run_km <- function(disease, difference) {
         disease <- as.character(disease)
         difference <- as.character(difference)
         # Build filename and read dataset
         file_path <- paste0("outcomes_",disease)
         data_df <- get(file_path)
         print(head(data_df))
#         # Check the variable exists
         if (!(difference %in% names(data_df))) {
         stop(paste("Variable", difference, "not found in data. Available:", 
                 paste(names(data_df), collapse = ", ")))
         }

        # CHECK FOR TIMING ISSUES
        cat("\n=== Timing Check for", disease, "===\n")
    
        timing_issues <- data_df %>%
                summarise(
                total = n(),
                start_gt_stop_gp = sum(start > stop_gp, na.rm = TRUE),
                start_gt_stop_sc = sum(start > stop_sc, na.rm = TRUE),
                pct_gp = round(sum(start > stop_gp, na.rm = TRUE) / n() * 100, 2),
                pct_sc = round(sum(start > stop_sc, na.rm = TRUE) / n() * 100, 2)
                )
        
        print(timing_issues)
        
    
         # Fit KM models
        # Ethnicity
        km_gp <- survfit(Surv(start, stop_gp, gp_binary) ~ ethnicity6_combined, data = data_df)
#         # Generate GP plot 
        gp_plot <- ggsurvplot(km_gp, data = data_df, risk.table = TRUE,
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", ethnicity (GP)"),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_ethnicity_gp.svg")
        ggsave(file.path(outdir, graph_file), gp_plot$plot)

        km_sc <- survfit(Surv(start, stop_sc, sc_binary) ~ ethnicity6_combined, data = data_df)
#         # Generate SC plot 
        sc_plot <- ggsurvplot(km_sc, data = data_df, risk.table = TRUE,
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", ethnicity (sc)"),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_ethnicity_sc.svg")
        ggsave(file.path(outdir, graph_file), sc_plot$plot)

        # IMD
        km_gp <- survfit(Surv(start, stop_gp, gp_binary) ~ imd_quintile, data = data_df)
#         # Generate GP plot 
        gp_plot <- ggsurvplot(km_gp, data = data_df, risk.table = TRUE,
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", IMD (GP)"),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_imd_gp.svg")
        ggsave(file.path(outdir, graph_file), gp_plot$plot)

        km_sc <- survfit(Surv(start, stop_sc, sc_binary) ~ imd_quintile, data = data_df)
#         # Generate SC plot 
        sc_plot <- ggsurvplot(km_sc, data = data_df, risk.table = TRUE,
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", IMD (SC)"),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_imd_sc.svg")
        ggsave(file.path(outdir, graph_file), sc_plot$plot)


 }

param_grid <- expand.grid(disease = diseases, difference = by_vars, stringsAsFactors = FALSE)

print(str(param_grid))

all_plots <- pmap(param_grid,
  function(disease, difference) {
    run_km(
      disease = as.character(disease),
      difference = as.character(difference)
    )
  })
#

