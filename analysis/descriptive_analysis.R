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
                sex ~ "N Sex",
                urban_rural_5 ~ "Urban-rural classification",
                migrant ~ "Migrant N",
                english_language ~ "English not main language spoken",
                flu_atrisk_grp ~ "Flu clinical risk group",
                covid_atrisk_grp ~ "COVID-19 clinical risk group"
                )

# COVID cohort baseline table by ethnicity
desc_covid_by <- baseline_covid %>% 
        select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
        urban_rural_5, imd_quintile, migrant, english_language, 
        flu_atrisk_grp, covid_atrisk_grp) %>% 
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

# # Flu/RSV cohort baseline table by ethnicity
# # Grouped table

desc_flu_rsv_by <- baseline_flu_rsv %>% 
        select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
               urban_rural_5, imd_quintile, migrant, english_language, 
               flu_atrisk_grp, covid_atrisk_grp) %>% 
        mutate(ethnicity6_combined = fct_na_value_to_level(ethnicity6_combined, level = "Missing")) %>%
        tbl_summary(
                missing = "no",
                by = ethnicity6_combined,
                label = var_labels_list
        )%>% 
        add_overall(last = FALSE) %>% 
        modify_header(all_stat_cols() ~ "{level}") 
        
# # Apply redactions to baseline table
desc_flu_rsv_redact_round <- fn_redact_tbl_summary(
        tbl_summary = desc_flu_rsv_by, 
        threshold = 7,
        col_names  = c( "stat_0", "stat_1", "stat_2", "stat_3", "stat_4", "stat_5", "stat_6"),
        rename_cols = c("Overall", "Asian", "Black", "Chinese", "Missing", "Mixed", "White")
        ) 

# # Overall table (no by)
# desc_flu_rsv_overall <- baseline_flu_rsv %>% 
#         select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
#                urban_rural_5, imd_quintile, migrant, english_language, 
#                flu_atrisk_grp, covid_atrisk_grp) %>% 
#         mutate(ethnicity6_combined = fct_na_value_to_level(ethnicity6_combined, level = "Missing")) %>%
#         tbl_summary(
#                 missing = "no",
#                 label = var_labels_list
#         )

# raw_stats_by      <- extract_round_stats(desc_flu_rsv_by)
# raw_stats_overall <- extract_round_stats(desc_flu_rsv_overall) %>%
#         mutate(by = "Overall")  # add by column so pivot_wider works

# # Combine and pivot
# desc_flu_rsv_redact_round <- bind_rows(raw_stats_overall, raw_stats_by) %>%
#         rowwise() %>%
#         transmute(
#                 var_label,
#                 variable_levels,
#                 by,
#                 value = glue::glue(stat_display, .envir = cur_data())
#         ) %>%
#         pivot_wider(
#                 names_from = by,
#                 values_from = value
#         )
# # saving both rounded/redacted and not rpound/redacted versions
write_csv(desc_flu_rsv_redact_round, file.path(outdir, "baseline_table_flu_rsv_r.csv"))
write_csv(desc_flu_rsv_by$table_body, file.path(outdir, "baseline_table_flu_rsv.csv"))

# # generating tables with rounded weekly events, denominators and rates from measures
# # ethnicity
rates_covid <- read_csv(here("output/measures/measures_covid.csv")) %>% 
        #filter(measure=="covid_gp" | measure=="covid_admission" | measure=="covid_emerg") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_covid, file.path(outdir, "rounded_rates_covid.csv"))

rates_flu_18 <- read_csv(here("output/measures/measures_flu_2018.csv")) %>% 
        filter(measure=="flu_gp" | measure=="flu_admission" | measure=="flu_emerg") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_flu_18, file.path(outdir, "rounded_rates_flu_18.csv"))

rates_flu_21 <- read_csv(here("output/measures/measures_flu_2021.csv")) %>% 
        filter(measure=="flu_gp" | measure=="flu_admission" | measure=="flu_emerg") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_flu_21, file.path(outdir, "rounded_rates_flu_21.csv"))

rates_rsv_18 <- read_csv(here("output/measures/measures_rsv_2018.csv")) %>% 
        filter(measure=="rsv_gp" | measure=="rsv_admission" | measure=="rsv_emerg") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_rsv_18, file.path(outdir, "rounded_rates_rsv_18.csv"))

rates_rsv_21 <- read_csv(here("output/measures/measures_rsv_2021.csv")) %>% 
        filter(measure=="rsv_gp" | measure=="rsv_admission" | measure=="rsv_emerg") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_rsv_21, file.path(outdir, "rounded_rates_rsv_21.csv"))

rates_rsv_23 <- read_csv(here("output/measures/measures_rsv_2023.csv")) %>% 
        filter(measure=="rsv_gp" | measure=="rsv_admission" | measure=="rsv_emerg") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_rsv_23, file.path(outdir, "rounded_rates_rsv_23.csv"))

# #IMD
rates_imd_covid <- read_csv(here("output/measures/measures_covid.csv")) %>% 
        #filter(measure=="covid_gp_imd" | measure=="covid_admission_imd" | measure=="covid_emerg_imd") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_imd_covid, file.path(outdir, "rounded_rates_imd_covid.csv"))

rates_imd_flu_18 <- read_csv(here("output/measures/measures_flu_2018.csv")) %>% 
        filter(measure=="flu_gp_imd" | measure=="flu_admission_imd" | measure=="flu_emerg_imd") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_imd_flu_18, file.path(outdir, "rounded_rates_imd_flu_18.csv"))

rates_imd_flu_21 <- read_csv(here("output/measures/measures_flu_2021.csv")) %>% 
        filter(measure=="flu_gp_imd" | measure=="flu_admission_imd" | measure=="flu_emerg_imd") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_imd_flu_21, file.path(outdir, "rounded_rates_imd_flu_21.csv"))

rates_imd_rsv_18 <- read_csv(here("output/measures/measures_rsv_2018.csv")) %>% 
        filter(measure=="rsv_gp_imd" | measure=="rsv_admission_imd" | measure=="rsv_emerg_imd") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_imd_rsv_18, file.path(outdir, "rounded_rates_imd_rsv_18.csv"))

rates_imd_rsv_21 <- read_csv(here("output/measures/measures_rsv_2021.csv")) %>% 
        filter(measure=="rsv_gp_imd" | measure=="rsv_admission_imd" | measure=="rsv_emerg_imd") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_imd_rsv_21, file.path(outdir, "rounded_rates_imd_rsv_21.csv"))

rates_imd_rsv_23 <- read_csv(here("output/measures/measures_rsv_2023.csv")) %>% 
        filter(measure=="rsv_gp_imd" | measure=="rsv_admission_imd" | measure=="rsv_emerg_imd") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_imd_rsv_23, file.path(outdir, "rounded_rates_imd_rsv_23.csv"))

# # Making (not very good) line graphs of rates 
# plot_line <- function(file, disease) {

#         p <- ggplot(
#                 data = file,
#                 mapping = aes(
#                 x = interval_start,
#                 y = rate_mid6,
#                 group = interaction(ethnicity, measure),
#                 color = ethnicity,
#                 )
#         ) +
#                 geom_line() +
#                 geom_point(size = 1) +
#                 facet_wrap(~measure, scales = "free_y") +
#                 labs(title = paste("Disease rates for", disease))

#         ggsave(
#                 filename = paste0("plot_", disease,".svg"),
#                 plot = p,
#                 path = outdir
#         )

#         return(p)
# }

# disease_list <- list(
#     covid = rates_covid,
#     flu_18 = rates_flu_18,
#     flu_21 = rates_flu_21,
#     rsv_18 = rates_rsv_18,
#     rsv_21 = rates_rsv_21
# )

# plots <- lapply(names(disease_list), function(d_name) {
#     plot_line(disease_list[[d_name]], d_name)
# })

# # Table of outcomes - first event for Cox models
outcomes_covid_2021 <- read_feather(here("output/an_outcome_first_covid_2021.arrow")) 
outcomes_flu_2018 <- read_feather(here("output/an_outcome_first_flu_2018.arrow")) 
outcomes_rsv_2018 <- read_feather(here("output/an_outcome_first_rsv_2018.arrow")) 
outcomes_flu_2021 <- read_feather(here("output/an_outcome_first_flu_2021.arrow")) 
outcomes_rsv_2021 <- read_feather(here("output/an_outcome_first_rsv_2021.arrow")) 

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
    )

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
# # # ethnicity
# count_covid_gp <- create_event_table(outcomes_covid, time = "stop_gp", status = "gp_binary", outcome = "COVID - GP", group = "ethnicity6_combined")
# count_covid_sc <- create_event_table(outcomes_covid, time = "stop_sc", status = "sc_binary", outcome = "COVID - SC", group = "ethnicity6_combined")
# count_covid_emerg <- create_event_table(outcomes_covid, time = "stop_emerg", status = "emerg_binary", outcome = "COVID - A & E", group = "ethnicity6_combined")
# count_covid_all <- create_event_table(outcomes_covid, time = "stop", status = "binary_all", outcome = "COVID - all", group = "ethnicity6_combined")
# count_covid <- bind_rows(count_covid_gp, count_covid_sc, count_covid_emerg, count_covid_all)
# # round counts to midpoint 6
# count_covid_round <- count_covid %>% 
#         mutate(
#                 N_mid6 = if_else(N <= 7, NA_real_, roundmid_any(N)),
#                 events_mid6 = if_else(Events <= 7, NA_real_, roundmid_any(Events)),
#                 person_time_mid6 = if_else(Person_time <= 7, NA_real_, roundmid_any(Person_time)),
#                 rate_mid6 = round((events_mid6 / person_time_mid6) * 1000, 5)) %>% 
#         select(ethnicity6_combined, ends_with("_mid6"), Outcome)
        
# write_csv(count_covid, file.path(outdir, "count_covid.csv"))
# write_csv(count_covid_round, file.path(outdir, "count_covid_round.csv"))

# count_flu_gp <- create_event_table(outcomes_flu, time = "stop_gp", status = "gp_binary", outcome = "flu - GP", group = "ethnicity6_combined")
# count_flu_sc <- create_event_table(outcomes_flu, time = "stop_sc", status = "sc_binary", outcome = "flu - SC", group = "ethnicity6_combined")
# count_flu_emerg <- create_event_table(outcomes_flu, time = "stop_emerg", status = "emerg_binary", outcome = "flu - A & E", group = "ethnicity6_combined")
# count_flu_all <- create_event_table(outcomes_flu, time = "stop", status = "binary_all", outcome = "flu - all", group = "ethnicity6_combined")
# count_flu <- bind_rows(count_flu_gp, count_flu_sc, count_flu_emerg, count_flu_all)
# # round counts to midpoint 6
# count_flu_round <- count_flu %>% 
#         mutate(
#                 N_mid6 = if_else(N <= 7, NA_real_, roundmid_any(N)),
#                 events_mid6 = if_else(Events <= 7, NA_real_, roundmid_any(Events)),
#                 person_time_mid6 = if_else(Person_time <= 7, NA_real_, roundmid_any(Person_time)),
#                 rate_mid6 = round((events_mid6 / person_time_mid6) * 1000, 5)) %>% 
#         select(ethnicity6_combined, ends_with("_mid6"), Outcome)
        
# write_csv(count_flu, file.path(outdir, "count_flu.csv"))
# write_csv(count_flu_round, file.path(outdir, "count_flu_round.csv"))

# count_rsv_gp <- create_event_table(outcomes_rsv, time = "stop_gp", status = "gp_binary", outcome = "rsv - GP", group = "ethnicity6_combined")
# count_rsv_sc <- create_event_table(outcomes_rsv, time = "stop_sc", status = "sc_binary", outcome = "rsv - SC", group = "ethnicity6_combined")
# count_rsv_emerg <- create_event_table(outcomes_rsv, time = "stop_emerg", status = "emerg_binary", outcome = "rsv - A & E", group = "ethnicity6_combined")
# count_rsv_all <- create_event_table(outcomes_rsv, time = "stop", status = "binary_all", outcome = "rsv - all", group = "ethnicity6_combined")
# count_rsv <- bind_rows(count_rsv_gp, count_rsv_sc, count_rsv_emerg, count_rsv_all)
# # round counts to midpoint 6
# count_rsv_round <- count_rsv %>% 
#         mutate(
#                 N_mid6 = if_else(N <= 7, NA_real_, roundmid_any(N)),
#                 events_mid6 = if_else(Events <= 7, NA_real_, roundmid_any(Events)),
#                 person_time_mid6 = if_else(Person_time <= 7, NA_real_, roundmid_any(Person_time)),
#                 rate_mid6 = round((events_mid6 / person_time_mid6) * 1000, 5)) %>% 
#         select(ethnicity6_combined, ends_with("_mid6"), Outcome)
        
# write_csv(count_rsv, file.path(outdir, "count_rsv.csv"))
# write_csv(count_rsv_round, file.path(outdir, "count_rsv_round.csv"))

# # #IMD
# count_covid_imd_gp <- create_event_table(outcomes_covid, time = "stop_gp", status = "gp_binary", outcome = "COVID - GP", group = "imd_quintile")
# count_covid_imd_sc <- create_event_table(outcomes_covid, time = "stop_sc", status = "sc_binary", outcome = "COVID - SC", group = "imd_quintile")
# count_covid_imd_emerg <- create_event_table(outcomes_covid, time = "stop_emerg", status = "emerg_binary", outcome = "COVID - A & E", group = "imd_quintile")
# count_covid_imd_all <- create_event_table(outcomes_covid, time = "stop", status = "binary_all", outcome = "COVID - all", group = "imd_quintile")
# count_covid_imd <- bind_rows(count_covid_imd_gp, count_covid_imd_sc, count_covid_imd_all)
# # round counts to midpoint 6
# count_covid_imd_round <- count_covid_imd %>% 
#         mutate(
#                 N_mid6 = if_else(N <= 7, NA_real_, roundmid_any(N)),
#                 events_mid6 = if_else(Events <= 7, NA_real_, roundmid_any(Events)),
#                 person_time_mid6 = if_else(Person_time <= 7, NA_real_, roundmid_any(Person_time)),
#                 rate_mid6 = round((events_mid6 / person_time_mid6) * 1000, 5)) %>% 
#         select(imd_quintile, ends_with("_mid6"), Outcome)
        
# write_csv(count_covid_imd, file.path(outdir, "count_covid_imd.csv"))
# write_csv(count_covid_imd_round, file.path(outdir, "count_covid_imd_round.csv"))

# count_flu_imd_gp <- create_event_table(outcomes_flu, time = "stop_gp", status = "gp_binary", outcome = "flu - GP", group = "imd_quintile")
# count_flu_imd_sc <- create_event_table(outcomes_flu, time = "stop_sc", status = "sc_binary", outcome = "flu - SC", group = "imd_quintile")
# count_flu_imd_emerg <- create_event_table(outcomes_flu, time = "stop_emerg", status = "emerg_binary", outcome = "flu - A & E", group = "imd_quintile")
# count_flu_imd_all <- create_event_table(outcomes_flu, time = "stop", status = "binary_all", outcome = "flu - all", group = "imd_quintile")
# count_flu_imd <- bind_rows(count_flu_imd_gp, count_flu_imd_sc, count_flu_imd_all)
# # round counts to midpoint 6
# count_flu_imd_round <- count_flu_imd %>% 
#         mutate(
#                 N_mid6 = if_else(N <= 7, NA_real_, roundmid_any(N)),
#                 events_mid6 = if_else(Events <= 7, NA_real_, roundmid_any(Events)),
#                 person_time_mid6 = if_else(Person_time <= 7, NA_real_, roundmid_any(Person_time)),
#                 rate_mid6 = round((events_mid6 / person_time_mid6) * 1000, 5)) %>% 
#         select(imd_quintile, ends_with("_mid6"), Outcome)
        
# write_csv(count_flu_imd, file.path(outdir, "count_flu_imd.csv"))
# write_csv(count_flu_imd_round, file.path(outdir, "count_flu_imd_round.csv"))

# count_rsv_imd_gp <- create_event_table(outcomes_rsv, time = "stop_gp", status = "gp_binary", outcome = "rsv - GP", group = "imd_quintile")
# count_rsv_imd_sc <- create_event_table(outcomes_rsv, time = "stop_sc", status = "sc_binary", outcome = "rsv - SC", group = "imd_quintile")
# count_rsv_imd_emerg <- create_event_table(outcomes_rsv, time = "stop_emerg", status = "emerg_binary", outcome = "rsv - A & E", group = "imd_quintile")
# count_rsv_imd_all <- create_event_table(outcomes_rsv, time = "stop", status = "binary_all", outcome = "rsv - all", group = "imd_quintile")
# count_rsv_imd <- bind_rows(count_rsv_imd_gp, count_rsv_imd_sc, count_rsv_imd_all)
# # round counts to midpoint 6
# count_rsv_imd_round <- count_rsv_imd %>% 
#         mutate(
#                 N_mid6 = if_else(N <= 7, NA_real_, roundmid_any(N)),
#                 events_mid6 = if_else(Events <= 7, NA_real_, roundmid_any(Events)),
#                 person_time_mid6 = if_else(Person_time <= 7, NA_real_, roundmid_any(Person_time)),
#                 rate_mid6 = round((events_mid6 / person_time_mid6) * 1000, 5)) %>% 
#         select(imd_quintile, ends_with("_mid6"), Outcome)
        
# write_csv(count_rsv_imd, file.path(outdir, "count_rsv_imd.csv"))
# write_csv(count_rsv_imd_round, file.path(outdir, "count_rsv_imd_round.csv"))

# # Cox PH models to measure differences in utilisation by ethnicity and IMD
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
                data_df[[difference]] <- relevel(data_df[[difference]], ref = "5") 
        }
        
        # define Cox model formulas
        f_gp <- as.formula(paste0("Surv(start, stop_gp, gp_binary) ~ ", difference, " + strata(year_end_gp)"))
        f_sc <- as.formula(paste0("Surv(start, stop_sc, sc_binary) ~ ", difference, " + strata(year_end_sc)"))
        f_emerg <- as.formula(paste0("Surv(start, stop_emerg, emerg_binary) ~ ", difference, " + strata(year_end_emerg)"))
        # Fit Cox models
        mod_gp <- coxph(f_gp, data = data_df)
        mod_sc <- coxph(f_sc, data = data_df)
        mod_emerg <- coxph(f_emerg, data = data_df)

        new_rows <- bind_rows(
                tidy(mod_gp, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% mutate(outcome = "gp_binary", by_var = difference, disease = disease),
                tidy(mod_sc, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% mutate(outcome = "sc_binary", by_var = difference, disease = disease),
                tidy(mod_emerg, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% mutate(outcome = "emerg_binary", by_var = difference, disease = disease),
        )

        return(new_rows)
}

diseases <- c("covid_2021", "flu_2018", "rsv_2018", "flu_2021", "rsv_2021")
by_vars <- c("imd_quintile", "ethnicity6_combined")

param_grid <- expand.grid(disease = diseases, difference = by_vars, stringsAsFactors = FALSE)

cox_table <- pmap_dfr(
  param_grid,
  function(disease, difference) run_cox_models(disease, difference)
)
print(head(cox_table))

cox_table <- cox_table %>% 
                mutate(across(c("estimate", "conf.low", "conf.high"), ~round(.x, 2))) %>%
                # don't need std.error or statistic
                select("term", "estimate", "conf.low", "conf.high", "outcome", "by_var", "disease")

write_csv(cox_table, file.path(outdir, "cox_models.csv"))

#test KM plot

km_gp <- survfit(Surv(stop_gp, gp_binary) ~ ethnicity6_combined, data = outcomes_covid_2021)
# Generate GP plot 
names(km_gp$strata) <- gsub("ethnicity6_combined=", "", names(km_gp$strata))
gp_plot <- ggsurvplot_facet(km_gp, data = outcomes_covid_2021, facet.by = "year_end_gp", fun = "event", risk.table = FALSE,
        conf.int = TRUE, pval = FALSE, legend = "right", #title = ("Kaplan-Meier: COVID-19 (GP)"),
        legend.labs = c("Asian", "Black", "Chinese", "Missing", "Mixed", "White"),
        ggtheme = theme_bw())

ggsave(file.path(outdir, "plot_km_covid_2021_gp.svg"), gp_plot)

# Kaplan Meier plots - COVID
 run_km <- function(disease) {
         disease <- as.character(disease)
         # Build filename and read dataset
        file_path <- paste0("outcomes_",disease)
        data_df <- get(file_path)
        print(head(data_df))

        # CHECK FOR TIMING ISSUES
        cat("\n=== Timing Check for", disease, "===\n")
    
        timing_issues <- data_df %>%
                summarise(
                total = n(),
                start_gt_stop_gp = sum(start > stop_gp, na.rm = TRUE),
                start_gt_stop_sc = sum(start > stop_sc, na.rm = TRUE),
                start_gt_stop_emerg = sum(start > stop_emerg, na.rm = TRUE),
                pct_gp = round(sum(start > stop_gp, na.rm = TRUE) / n() * 100, 2),
                pct_sc = round(sum(start > stop_sc, na.rm = TRUE) / n() * 100, 2),
                pct_emerg = round(sum(start > stop_emerg, na.rm = TRUE) / n() * 100, 2)
                )
        
        print(timing_issues)
        
    
         # Fit KM models
        # Ethnicity
        km_gp <- survfit(Surv(start, stop_gp, gp_binary) ~ ethnicity6_combined, data = data_df)
#         # Generate GP plot 
        gp_plot <- ggsurvplot(km_gp, data = data_df, fun = "event", risk.table = TRUE, 
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", ethnicity (GP)"),
                #ylim=c(0.75, 1),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_ethnicity_gp.svg")
        ggsave(file.path(outdir, graph_file), gp_plot$plot)

        km_sc <- survfit(Surv(start, stop_sc, sc_binary) ~ ethnicity6_combined, data = data_df)
#         # Generate SC plot 
        sc_plot <- ggsurvplot(km_sc, data = data_df, fun = "event", risk.table = TRUE,
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", ethnicity (sc)"),
                #ylim=c(0.95, 1),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_ethnicity_sc.svg")
        ggsave(file.path(outdir, graph_file), sc_plot$plot)

        # Emergency care plot
        km_emerg <- survfit(Surv(start, stop_emerg, emerg_binary) ~ ethnicity6_combined, data = data_df)
#         # Generate GP plot 
        emerg_plot <- ggsurvplot(km_emerg, data = data_df, fun = "event", risk.table = TRUE, 
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", ethnicity (GP)"),
                #ylim=c(0.75, 1),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_ethnicity_emerg.svg")
        ggsave(file.path(outdir, graph_file), emerg_plot$plot)

        # IMD
        km_gp <- survfit(Surv(start, stop_gp, gp_binary) ~ imd_quintile, data = data_df)
#         # Generate GP plot 
        gp_plot <- ggsurvplot(km_gp, data = data_df, fun = "event", risk.table = TRUE,
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", IMD (GP)"),
                #ylim=c(0.75, 1),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_imd_gp.svg")
        ggsave(file.path(outdir, graph_file), gp_plot$plot)

        km_sc <- survfit(Surv(start, stop_sc, sc_binary) ~ imd_quintile, data = data_df)
#         # Generate SC plot 
        sc_plot <- ggsurvplot(km_sc, data = data_df, fun = "event", risk.table = TRUE,
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", IMD (SC)"),
                #ylim=c(0.95, 1),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_imd_sc.svg")
        ggsave(file.path(outdir, graph_file), sc_plot$plot)

        # Emergency care plot
        km_emerg <- survfit(Surv(start, stop_emerg, emerg_binary) ~ imd_quintile, data = data_df)
#         # Generate GP plot 
        emerg_plot <- ggsurvplot(km_emerg, data = data_df, fun = "event", risk.table = TRUE, 
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", ethnicity (GP)"),
                #ylim=c(0.75, 1),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_imd_emerg.svg")
        ggsave(file.path(outdir, graph_file), emerg_plot$plot)

 }

param_grid <- expand.grid(disease = diseases, stringsAsFactors = FALSE)

print(str(param_grid))

all_plots <- pmap(param_grid,
  function(disease) {
    run_km(
      disease = as.character(disease)
    )
  })


