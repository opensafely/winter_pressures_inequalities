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
library(forcats)
library(gtsummary)
library(survminer)
library(glue)

## Import custom user functions
source(here::here("analysis", "redaction.R"))

outdir <- here("output", "tables")
fs::dir_create(outdir)

# Define BMI categories
bmi_levels <- c("ideal", "underweight", "overweight", "obese", "unknown or outlier", "Missing")

#Creating baseline tables - first import data
# using tables with outcomes as people removed with ONS death date prior to start of follow-up in that file
# # Table of outcomes - first event for Cox models
outcomes_covid_2021 <- read_feather(here("output/an_outcome_first_covid_2021.arrow"))%>% 
        mutate(imd_quintile = as.factor(imd_quintile),
        min_yr_start = as.factor(min_start_yr),
        bmi_category = factor(bmi_category, levels = bmi_levels)) 
outcomes_flu_2018 <- read_feather(here("output/an_outcome_first_flu_2018.arrow")) %>% 
        mutate(imd_quintile = as.factor(imd_quintile),
        min_yr_start = as.factor(min_start_yr),
        bmi_category = factor(bmi_category, levels = bmi_levels))
outcomes_rsv_2018 <- read_feather(here("output/an_outcome_first_rsv_2018.arrow")) %>% 
        mutate(imd_quintile = as.factor(imd_quintile),
        min_yr_start = as.factor(min_start_yr),
        bmi_category = factor(bmi_category, levels = bmi_levels))
outcomes_flu_2021 <- read_feather(here("output/an_outcome_first_flu_2021.arrow")) %>% 
        mutate(imd_quintile = as.factor(imd_quintile),
        min_yr_start = as.factor(min_start_yr),
        bmi_category = factor(bmi_category, levels = bmi_levels))
outcomes_rsv_2021 <- read_feather(here("output/an_outcome_first_rsv_2021.arrow")) %>% 
        mutate(imd_quintile = as.factor(imd_quintile),
        min_yr_start = as.factor(min_start_yr),
        bmi_category = factor(bmi_category, levels = bmi_levels))

var_labels_list <- list(
                min_start_yr ~ "Baseline year",
                age_group ~ "Age group",
                imd_quintile ~ "IMD quintile",
                ethnicity16_combined ~ "Ethnicity (16 groups)",
                sex ~ "Sex",
                urban_rural_5 ~ "Urban-rural classification",
                migrant ~ "Migrant",
                english_language ~ "English not main language spoken",
                flu_atrisk_grp ~ "Flu CRG",
                covid_atrisk_grp ~ "COVID-19 CRG",
                bmi_category ~ "BMI category"
                )

# COVID cohort baseline table by ethnicity
desc_covid_by <- outcomes_covid_2021 %>% 
        select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
        urban_rural_5, imd_quintile, migrant, english_language, 
        flu_atrisk_grp, covid_atrisk_grp, bmi_category) %>% 
        mutate(ethnicity6_combined = fct_na_value_to_level(
                ethnicity6_combined, 
                level = "Missing")) %>%
        tbl_summary(
                missing = "no",
                by = ethnicity6_combined,
                label = var_labels_list,
        ) %>% 
        add_overall(last = FALSE) %>% 
        modify_header(all_stat_cols() ~ "{level}") 
        
# # Apply redactions to baseline table
desc_covid_redact_round <- fn_redact_tbl_summary(
        tbl_summary = desc_covid_by, 
        threshold = 7,
        col_names  = c( "stat_0", "stat_1", "stat_2", "stat_3", "stat_4", "stat_5", "stat_6"),
        rename_cols = c("Overall", "Asian", "Black", "Chinese", "Missing", "Mixed", "White")
        ) 


# # saving both rounded/redacted and not rpound/redacted versions
write_csv(desc_covid_redact_round, file.path(outdir, "baseline_table_covid_r.csv"))
write_csv(desc_covid_by$table_body, file.path(outdir, "baseline_table_covid.csv"))

# # Flu/RSV cohort baseline table by ethnicity - same cohort so using flu outcome files
# # Grouped table

# desc_flu_rsv_2021_by <- outcomes_flu_2021 %>% 
#         select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
#                urban_rural_5, imd_quintile, migrant, english_language, 
#                vacc_elig_flu, vacc_elig_covid, bmi_category) %>% 
#         mutate(ethnicity6_combined = fct_na_value_to_level(ethnicity6_combined, level = "Missing")) %>%
#         tbl_summary(
#                 missing = "no",
#                 by = ethnicity6_combined,
#                 label = var_labels_list
#         )%>% 
#         add_overall(last = FALSE) %>% 
#         modify_header(all_stat_cols() ~ "{level}") 
        
# # # Apply redactions to baseline table
# desc_flu_rsv_2021_redact_round <- fn_redact_tbl_summary(
#         tbl_summary = desc_flu_rsv_2021_by, 
#         threshold = 7,
#         col_names  = c( "stat_0", "stat_1", "stat_2", "stat_3", "stat_4", "stat_5", "stat_6"),
#         rename_cols = c("Overall", "Asian", "Black", "Chinese", "Missing", "Mixed", "White")
#         ) 

# desc_flu_rsv_2018_by <- outcomes_flu_2018 %>% 
#         select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
#                urban_rural_5, imd_quintile, migrant, english_language, 
#                vacc_elig_flu, vacc_elig_covid, bmi_category) %>% 
#         mutate(ethnicity6_combined = fct_na_value_to_level(ethnicity6_combined, level = "Missing")) %>%
#         tbl_summary(
#                 missing = "no",
#                 by = ethnicity6_combined,
#                 label = var_labels_list
#         )%>% 
#         add_overall(last = FALSE) %>% 
#         modify_header(all_stat_cols() ~ "{level}") 
        
# # # Apply redactions to baseline table
# desc_flu_rsv_2018_redact_round <- fn_redact_tbl_summary(
#         tbl_summary = desc_flu_rsv_2018_by, 
#         threshold = 7,
#         col_names  = c( "stat_0", "stat_1", "stat_2", "stat_3", "stat_4", "stat_5", "stat_6"),
#         rename_cols = c("Overall", "Asian", "Black", "Chinese", "Missing", "Mixed", "White")
#         ) 

# # # Overall table (no by)
# # desc_flu_rsv_overall <- baseline_flu_rsv %>% 
# #         select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
# #                urban_rural_5, imd_quintile, migrant, english_language, 
# #                flu_atrisk_grp, covid_atrisk_grp) %>% 
# #         mutate(ethnicity6_combined = fct_na_value_to_level(ethnicity6_combined, level = "Missing")) %>%
# #         tbl_summary(
# #                 missing = "no",
# #                 label = var_labels_list
# #         )

# # raw_stats_by      <- extract_round_stats(desc_flu_rsv_by)
# # raw_stats_overall <- extract_round_stats(desc_flu_rsv_overall) %>%
# #         mutate(by = "Overall")  # add by column so pivot_wider works

# # # Combine and pivot
# # desc_flu_rsv_redact_round <- bind_rows(raw_stats_overall, raw_stats_by) %>%
# #         rowwise() %>%
# #         transmute(
# #                 var_label,
# #                 variable_levels,
# #                 by,
# #                 value = glue::glue(stat_display, .envir = cur_data())
# #         ) %>%
# #         pivot_wider(
# #                 names_from = by,
# #                 values_from = value
# #         )
# # # saving both rounded/redacted and not rpound/redacted versions
# write_csv(desc_flu_rsv_2018_redact_round, file.path(outdir, "baseline_table_flu_rsv_2018_r.csv"))
# write_csv(desc_flu_rsv_2018_by$table_body, file.path(outdir, "baseline_table_flu_rsv_2018.csv"))
# write_csv(desc_flu_rsv_2021_redact_round, file.path(outdir, "baseline_table_flu_rsv_2021_r.csv"))
# write_csv(desc_flu_rsv_2021_by$table_body, file.path(outdir, "baseline_table_flu_rsv_2021.csv"))

# #Function to create table with number of outcomes, person-time, rate and flag for outcome type - no rounding yet
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

# # Run function for each outcome with GP, hospitalisations and both and create table

count_outcomes <- function(disease,
                           year,
                           strata,
                           rate_multiplier = 1000) {

  # retrieve dataset
  data_name <- paste0("outcomes_", disease, "_", year)
  data <- get(data_name)

  counts <- bind_rows(
    create_event_table(
      data,
      time = "stop_gp",
      status = "gp_binary",
      outcome = paste(disease, "- GP"),
      group = strata
    ),
    create_event_table(
      data,
      time = "stop_gp_sens",
      status = "gp_sens_binary",
      outcome = paste(disease, "- GP sens"),
      group = strata
    ),
    if (disease == "covid") {
      create_event_table(
        data,
        time = "stop_gp_appt",
        status = "gp_appt_binary",
        outcome = paste(disease, "- GP appt"),
        group = strata
      )
    },
    create_event_table(
      data,
      time = "stop_sc",
      status = "sc_binary",
      outcome = paste(disease, "- SC"),
      group = strata
    ),
    create_event_table(
      data,
      time = "stop_emerg",
      status = "emerg_binary",
      outcome = paste(disease, "- A&E"),
      group = strata
    ),
    create_event_table(
      data,
      time = "stop",
      status = "binary_all",
      outcome = paste(disease, "- all"),
      group = strata
    )
  )

  counts_round <- counts %>%
    mutate(
      N_mid6 = if_else(N <= 7, NA_real_, roundmid_any(N)),
      Events_mid6 = if_else(Events <= 7, NA_real_, roundmid_any(Events)),
      Person_time_mid6 = if_else(Person_time <= 7,
                                 NA_real_,
                                 roundmid_any(Person_time)),
      Rate_mid6 = round(
        (Events_mid6 / Person_time_mid6) * rate_multiplier,
        5
      )
    ) %>% 
    select(-N, -Events, -Person_time)

  write_csv(
    counts,
    file.path(
      outdir,
      paste0("count_", disease, "_", year, "_", strata, ".csv")
    )
  )

  write_csv(
    counts_round,
    file.path(
      outdir,
      paste0("count_", disease, "_", year, "_", strata, "_round.csv")
    )
  )

  invisible(
    list(
      counts = counts,
      counts_round = counts_round
    )
  )
}

count_outcomes(
  disease = "covid",
  year = "2021",
  strata = "ethnicity6_combined",
)

count_outcomes(
  disease = "covid",
  year = "2021",
  strata = "imd_quintile"
)

count_outcomes(
  disease = "flu",
  year = "2021",
  strata = "ethnicity6_combined"
)

count_outcomes(
  disease = "flu",
  year = "2021",
  strata = "imd_quintile"
)

count_outcomes(
  disease = "flu",
  year = "2018",
  strata = "ethnicity6_combined"
)

count_outcomes(
  disease = "flu",
  year = "2018",
  strata = "imd_quintile"
)

count_outcomes(
  disease = "rsv",
  year = "2021",
  strata = "ethnicity6_combined"
)

count_outcomes(
  disease = "rsv",
  year = "2021",
  strata = "imd_quintile"
)

count_outcomes(
  disease = "rsv",
  year = "2018",
  strata = "ethnicity6_combined"
)

count_outcomes(
  disease = "rsv",
  year = "2018",
  strata = "imd_quintile"
)

# 16 ethnic groups 
count_outcomes(
  disease = "covid",
  year = "2021",
  strata = "ethnicity16_combined",
)

count_outcomes(
  disease = "flu",
  year = "2021",
  strata = "ethnicity16_combined"
)

count_outcomes(
  disease = "flu",
  year = "2018",
  strata = "ethnicity16_combined"
)

count_outcomes(
  disease = "rsv",
  year = "2021",
  strata = "ethnicity16_combined"
)

count_outcomes(
  disease = "rsv",
  year = "2018",
  strata = "ethnicity16_combined"
)


#test KM plot

km_gp <- survfit(Surv(stop_gp, gp_binary) ~ ethnicity6_combined, data = outcomes_covid_2021)
# Generate GP plot 
names(km_gp$strata) <- gsub("ethnicity6_combined=", "", names(km_gp$strata))
gp_plot <- ggsurvplot_facet(km_gp, data = outcomes_covid_2021, facet.by = "year_end_gp", fun = "event", risk.table = FALSE,
        conf.int = TRUE, pval = FALSE, legend = "right", #title = ("Kaplan-Meier: COVID-19 (GP)"),
        legend.labs = c("Asian", "Black", "Chinese", "Missing", "Mixed", "White"),
        ggtheme = theme_bw())

ggsave(file.path(outdir, "plot_km_covid_2021_gp.svg"), gp_plot)



