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

outdir <- here("output", "tables")
fs::dir_create(outdir)

# Creating baseline tables
baseline_covid <- read_feather(here("output/an_bl_data_covid.arrow"))
baseline_flu_rsv <- read_feather(here("output/an_bl_data_flu_rsv.arrow"))

# Checking year of entry to uk variable
print(summary(baseline_covid$year_entry_uk))
print(summary(baseline_flu_rsv$year_entry_uk))

# Variables to go into table
explanatory <- c("age_group", "sex", "ethnicity6_combined", "ethnicity16_combined", 
        "urban_rural_5", "imd_quintile", "migrant", "english_language", 
        "flu_atrisk_grp", "covid_atrisk_grp")

table <- summary_factorlist(baseline_covid, explanatory = explanatory, cont_cut = "imd_quintile")
write_csv(table, file.path(outdir, "baseline_table_covid.csv"))

table_flu_rsv <- summary_factorlist(baseline_flu_rsv, explanatory = explanatory, cont_cut = "imd_quintile")
write_csv(table_flu_rsv, file.path(outdir, "baseline_table_flu_rsv.csv"))

explanatory_strata <- c("age_group", "sex", "ethnicity16_combined", 
        "urban_rural_5", "imd_quintile", "migrant", "english_language", 
        "flu_atrisk_grp", "covid_atrisk_grp")

table_strata <- summary_factorlist(baseline_covid, explanatory = explanatory_strata, dependent = "ethnicity6_combined", cont_cut = "imd_quintile")
write_csv(table_strata, file.path(outdir, "baseline_table_covid_strata.csv"))

table_flu_rsv_strata <- summary_factorlist(baseline_flu_rsv, explanatory = explanatory_strata, dependent = "ethnicity6_combined", cont_cut = "imd_quintile")
write_csv(table_flu_rsv_strata, file.path(outdir, "baseline_table_flu_rsv_strata.csv"))

# Making line graphs of rates 
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
        file_path <- paste0("output/an_outcome_first_",disease,".arrow")
        data_df <- read_feather(here(file_path))

        # Build variable names
        gp_var <- paste0(disease,"_gp_binary")
        sc_var <- paste0(disease,"_sc_binary")
        
        # define formulas
        f_gp <- as.formula(paste0("Surv(start, stop_gp, ", gp_var, ") ~ ", difference, " + strata(year_end_gp)"))
        f_sc <- as.formula(paste0("Surv(start, stop_sc, ", sc_var, ") ~ ", difference, " + strata(year_end_sc)"))
        # Fit Cox models
        mod_gp <- coxph(f_gp, data = data_df
        )

        mod_sc <- coxph(f_sc, data = data_df
        )
        
        new_rows <- bind_rows(
                tidy(mod_gp, exponentiate = TRUE) %>% mutate(outcome = paste0(disease, "_gp"), by_var = difference),
                tidy(mod_sc, exponentiate = TRUE) %>% mutate(outcome = paste0(disease, "_sc"), by_var = difference)
        )

        if (is.null(cox_table)) {
                return(new_rows)
        } else {
                return(bind_rows(cox_table, new_rows))
        }
}

diseases <- c("covid", "flu", "rsv")
by_vars <- c("ethnicity6_combined", "imd_quintile")

param_grid <- expand.grid(disease = diseases, difference = by_vars, stringsAsFactors = FALSE)

cox_table <- pmap_dfr(
  param_grid,
  function(disease, difference) run_cox_models(NULL, disease, difference)
)

write_csv(cox_table, file.path(outdir, "cox_models.csv"))