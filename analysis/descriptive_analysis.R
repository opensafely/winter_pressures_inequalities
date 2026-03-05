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

outdir <- here("output", "tables")
fs::dir_create(outdir)

#define function for midpoint 6 rounding
roundmid_any <- function(x, to = 6) {
  
  # like round_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x != 0))
  
}

## Redaction
redact <- function(vars) {
  case_when(vars > 7 ~ vars)
}

# Creating baseline tables - first import data
baseline_covid <- read_feather(here("output/an_bl_data_covid.arrow")) %>% 
        mutate(imd_quintile <- as.factor(imd_quintile),
        min_yr_start <- as.factor(min_start_yr))
baseline_flu_rsv <- read_feather(here("output/an_bl_data_flu_rsv.arrow"))%>% 
        mutate(imd_quintile <- as.factor(imd_quintile),
        min_yr_start <- as.factor(min_start_yr))

#baseline_covid$ethnicity6_combined <- forcats::fct_explicit_na(baseline_covid$ethnicity6_combined, na_level = "Missing")
#baseline_flu_rsv$ethnicity6_combined <- forcats::fct_explicit_na(baseline_flu_rsv$ethnicity6_combined, na_level = "Missing")

# COVID cohort baseline table by ethnicity
desc_covid <- baseline_covid %>% 
        select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
        urban_rural_5, imd_quintile, migrant, english_language, 
        flu_atrisk_grp, covid_atrisk_grp) %>% 
        mutate(ethnicity6_combined = fct_explicit_na(
                ethnicity6_combined, 
                na_level = "Missing")) %>%
        tbl_summary(
                missing = "no",
                by = ethnicity6_combined,
                label = list(
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
        ) %>% 
        add_overall(last = FALSE)
desc_covid$inputs$data <- NULL
gt::gtsave(as_gt(desc_covid), file.path(outdir, "baseline_table_covid.html"))

# Flu/RSV cohort baseline table by ethnicity
desc_flu_rsv <- baseline_flu_rsv %>% 
        select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
                urban_rural_5, imd_quintile, migrant, english_language, 
                flu_atrisk_grp, covid_atrisk_grp) %>% 
        mutate(ethnicity6_combined = fct_explicit_na(
                ethnicity6_combined, 
                na_level = "Missing")) %>%
        tbl_summary(
                missing = "no",
                by = ethnicity6_combined,
                label = list(
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
        ) %>% 
        add_overall(last = FALSE)
desc_flu_rsv$inputs$data <- NULL
gt::gtsave(as_gt(desc_flu_rsv), file.path(outdir, "baseline_table_flu_rsv.html"))

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
                facet_wrap(~measure, scale = "free_y") +
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

# Function to create table with number of outcomes, person-time, rate and flag for outcome type
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
run_cox_models <- function(cox_table = NULL, disease, difference) {
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
by_vars <- c("ethnicity6_combined", "imd_quintile")

param_grid <- expand.grid(disease = diseases, difference = by_vars, stringsAsFactors = FALSE)

# Use pmap to get full list of results
all_results <- pmap(
        param_grid,
        function(disease, difference) run_cox_models(NULL, disease, difference)
)

# Extract cox table by row-binding just the cox_results element
cox_table <- map_dfr(all_results, "cox_results")

# cox_table <- pmap_dfr(
#   param_grid,
#   function(disease, difference) run_cox_models(NULL, disease, difference)
# )

write_csv(cox_table, file.path(outdir, "cox_models.csv"))

# Kaplan Meier plots
run_km <- function(disease, difference) {
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
        
        # KM formulas (simple — no start/stop, no strata())
        f_km_gp <- as.formula(paste0("Surv(stop_gp, gp_binary) ~ ", difference))
        f_km_sc <- as.formula(paste0("Surv(stop_sc, sc_binary) ~ ", difference))
        
        # Fit KM models
        km_gp <- eval(bquote(survfit(.(f_km_gp), data = data_df)))
        km_sc <- eval(bquote(survfit(.(f_km_sc), data = data_df)))
        # Generate GP plot 
        gp_plot <- ggsurvplot(km_gp, data = data_df, risk.table = TRUE,
                conf.int = TRUE, title = paste("Kaplan-Meier:", disease, "(GP)"),
                ggtheme = theme_bw())

        # Generate SC plot 
        sc_plot <- ggsurvplot(km_sc, data = data_df, risk.table = TRUE,
                conf.int = TRUE, title = paste("Kaplan-Meier:", disease, "(SC)"),
                ggtheme = theme_bw())

        return(list(
                gp_plot = gp_plot,
                sc_plot = sc_plot,
                disease = disease,
                difference = difference
        )) 
}

param_grid <- expand.grid(disease = diseases, difference = by_vars, stringsAsFactors = FALSE)

all_plots <- pmap(param_grid, function(disease, difference) run_km(disease, difference))

# Save all plots
walk(all_plots, function(x) {
        svg(here("output", paste0("plot_km_", x$disease, "_", x$difference, "_gp.svg")), width = 10, height = 8)
        print(x$gp_plot)
        dev.off()

        svg(here("output", paste0("plot_km_", x$disease, "_", x$difference, "_sc.svg")), width = 10, height = 8)
        print(x$sc_plot)
        dev.off()
})

