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

outdir <- here("output", "tables")
fs::dir_create(outdir)

# Creating baseline tables - first import data
baseline_covid <- read_feather(here("output/an_bl_data_covid.arrow")) %>% 
        mutate(imd_quintile <- as.factor(imd_quintile),
        min_yr_start <- as.factor(min_start_yr))
baseline_flu_rsv <- read_feather(here("output/an_bl_data_flu_rsv.arrow"))%>% 
        mutate(imd_quintile <- as.factor(imd_quintile),
        min_yr_start <- as.factor(min_start_yr))

# COVID cohort baseline table by ethnicity
count_covid <- baseline_covid %>% 
  select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
         urban_rural_5, imd_quintile, migrant, english_language, 
         flu_atrisk_grp, covid_atrisk_grp) %>% 
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
count_covid$inputs$data <- NULL
gt::gtsave(as_gt(count_covid), file.path(outdir, "baseline_table_covid.html"))

# Flu/RSV cohort baseline table by ethnicity
count_flu_rsv <- baseline_flu_rsv %>% 
  select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
         urban_rural_5, imd_quintile, migrant, english_language, 
         flu_atrisk_grp, covid_atrisk_grp) %>% 
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
count_flu_rsv$inputs$data <- NULL
gt::gtsave(as_gt(count_flu_rsv), file.path(outdir, "baseline_table_flu_rsv.html"))
# Variables to go into table
# explanatory <- c("min_start_yr", "age_group", "sex", "ethnicity6_combined", "ethnicity16_combined", 
#         "urban_rural_5", "imd_quintile", "migrant", "english_language", 
#         "flu_atrisk_grp", "covid_atrisk_grp")

# print(baseline_covid %>% 
#         ff_glimpse(explanatory))

# table_covid <- summary_factorlist(baseline_covid, explanatory = explanatory, na_include = TRUE)
# write_csv(table_covid, file.path(outdir, "baseline_table_covid.csv"))

# table_flu_rsv <- summary_factorlist(baseline_flu_rsv, explanatory = explanatory, na_include = TRUE)
# write_csv(table_flu_rsv, file.path(outdir, "baseline_table_flu_rsv.csv"))

# # Tables stratified by year identified to check if characteristics change
# explanatory_yr <- c("age_group", "sex", "ethnicity6_combined", "ethnicity16_combined", 
#         "urban_rural_5", "imd_quintile", "migrant", "english_language", 
#         "flu_atrisk_grp", "covid_atrisk_grp")

# table_covid_yr <- summary_factorlist(baseline_covid, explanatory = explanatory_yr, dependent = "min_start_yr", na_include = TRUE)
# write_csv(table_covid_yr, file.path(outdir, "baseline_table_covid_yr.csv"))

# table_flu_rsv_yr <- summary_factorlist(baseline_flu_rsv, explanatory = explanatory_yr, dependent = "min_start_yr", na_include = TRUE)
# write_csv(table_flu_rsv_yr, file.path(outdir, "baseline_table_flu_rsv_yr.csv"))

# # Tables stratified by ethnicity
# explanatory_strata <- c("min_start_yr", "age_group", "sex", "ethnicity16_combined", 
#         "urban_rural_5", "imd_quintile", "migrant", "english_language", 
#         "flu_atrisk_grp", "covid_atrisk_grp")

# table_strata <- summary_factorlist(baseline_covid, explanatory = explanatory_strata, dependent = "ethnicity6_combined", na_include = TRUE)
# write_csv(table_strata, file.path(outdir, "baseline_table_covid_strata.csv"))

# table_flu_rsv_strata <- summary_factorlist(baseline_flu_rsv, explanatory = explanatory_strata, dependent = "ethnicity6_combined", na_include = TRUE)
# write_csv(table_flu_rsv_strata, file.path(outdir, "baseline_table_flu_rsv_strata.csv"))

# Making not very good line graphs of rates 
rates_covid <- read_csv(here("output/measures/measures_covid.csv")) %>% 
        mutate(ratio_100 = ratio*100)

rates_flu_18 <- read_csv(here("output/measures/measures_flu_rsv_2018.csv")) %>% 
        filter(measure=="flu_gp" | measure=="flu_admission") %>% 
        mutate(ratio_100 = ratio*100)

rates_flu_21 <- read_csv(here("output/measures/measures_flu_rsv_2021.csv")) %>% 
        filter(measure=="flu_gp" | measure=="flu_admission") %>% 
        mutate(ratio_100 = ratio*100)

rates_rsv_18 <- read_csv(here("output/measures/measures_flu_rsv_2018.csv")) %>% 
        filter(measure=="rsv_gp" | measure=="rsv_admission") %>% 
        mutate(ratio_100 = ratio*100)

rates_rsv_21 <- read_csv(here("output/measures/measures_flu_rsv_2021.csv")) %>% 
        filter(measure=="rsv_gp" | measure=="rsv_admission") %>% 
        mutate(ratio_100 = ratio*100)



plot_line <- function(file, disease) {

        p <- ggplot(
                data = file,
                mapping = aes(
                x = interval_start,
                y = ratio_100,
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
        data_df[[difference]] <- relevel(data_df[[difference]], ref = "1")  # or whichever you want
        }
        
        # define formulas
        f_gp <- as.formula(paste0("Surv(start, stop_gp, gp_binary) ~ ", difference, " + strata(year_end_gp)"))
        f_sc <- as.formula(paste0("Surv(start, stop_sc, sc_binary) ~ ", difference, " + strata(year_end_sc)"))
        # Fit Cox models
        mod_gp <- coxph(f_gp, data = data_df
        )

        mod_sc <- coxph(f_sc, data = data_df
        )
        
        new_rows <- bind_rows(
                tidy(mod_gp, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% mutate(outcome = "gp_binary", by_var = difference, disease = disease),
                tidy(mod_sc, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% mutate(outcome = "sc_binary", by_var = difference, disease = disease)
        )

        return(new_rows)
}

diseases <- c("covid", "flu", "rsv")
by_vars <- c("ethnicity6_combined", "imd_quintile")

param_grid <- expand.grid(disease = diseases, difference = by_vars, stringsAsFactors = FALSE)

cox_table <- pmap_dfr(
  param_grid,
  function(disease, difference) run_cox_models(NULL, disease, difference)
)

write_csv(cox_table, file.path(outdir, "cox_models.csv"))