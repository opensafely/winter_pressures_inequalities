library(here)
library(tidyverse)
library(arrow)
library(lubridate)
library(readr)

# Import data
# prepare_data <- function(file_patterns, output_file) {
#     cr_data <- bind_rows(lapply(seq_along(file_patterns), function(i) {
#     read_feather(file_patterns[i]) %>%
#     mutate(year = as.numeric(gsub("\\D", "", basename(file_patterns[i]))))
#     }))

# cr_data <- cr_data %>%
#     group_by(patient_id) %>%
#     mutate(min_start_yr = min(year)) %>%
#     ungroup() %>%
#     mutate(
#         study_end = as.Date("2025-08-31"),
#         fu_end = pmin(dereg_date, ons_death_date, study_end, na.rm = TRUE),
#         bmi_category = case_when(
#             bmi == 0 ~ "unknown or outlier",
#             bmi < 18.5 ~ "underweight",
#             bmi < 25 ~ "ideal",
#             bmi < 30 ~ "overweight",
#             bmi >= 30 ~ "obese",
#             TRUE ~ NA_character_
#             ),
#         latest_ethnicity6_from_codes = case_when(
#             ethnicity6_gp == "1" ~ "White",
#             ethnicity6_gp == "2" ~ "Mixed",
#             ethnicity6_gp == "3" ~ "Asian or Asian British",
#             ethnicity6_gp == "4" ~ "Black or Black British",
#             ethnicity6_gp == "5" ~ "Chinese or Other Ethnic Groups",
#             TRUE ~ NA_character_
#         ),
#         ethnicity6_from_sus = case_when(
#             ethnicity_sus %in% c("A", "B", "C") ~ "White",
#             ethnicity_sus %in% c("D", "E", "F", "G") ~ "Mixed",
#             ethnicity_sus %in% c("H", "J", "K", "L") ~ "Asian or Asian British",
#             ethnicity_sus %in% c("M", "N", "P") ~ "Black or Black British",
#             ethnicity_sus %in% c("R", "S") ~ "Chinese or Other Ethnic Groups",
#             TRUE ~ NA_character_
#         ),
#         latest_ethnicity16_from_codes = case_when(
#             ethnicity16_gp == "1" ~ "White British",
#             ethnicity16_gp == "2" ~ "White Irish",
#             ethnicity16_gp == "3" ~ "Other White",
#             ethnicity16_gp == "4" ~ "White and Caribbean",
#             ethnicity16_gp == "5" ~ "White and African",
#             ethnicity16_gp == "6" ~ "White and Asian",
#             ethnicity16_gp == "7" ~ "Other Mixed",
#             ethnicity16_gp == "8" ~ "Indian",
#             ethnicity16_gp == "9" ~ "Pakistani",
#             ethnicity16_gp == "10" ~ "Bangladeshi",
#             ethnicity16_gp == "11" ~ "Other Asian",
#             ethnicity16_gp == "12" ~ "Caribbean",
#             ethnicity16_gp == "13" ~ "African",
#             ethnicity16_gp == "14" ~ "Other Black",
#             ethnicity16_gp == "15" ~ "Chinese",
#             ethnicity16_gp == "16" ~ "All other ethnic groups",
#             TRUE ~ NA_character_
#         ),

#         ethnicity16_from_sus = case_when(
#             ethnicity_sus == "A" ~ "White British",
#             ethnicity_sus == "B" ~ "White Irish",
#             ethnicity_sus == "C" ~ "Other White",
#             ethnicity_sus == "D" ~ "White and Caribbean",
#             ethnicity_sus == "E" ~ "White and African",
#             ethnicity_sus == "F" ~ "White and Asian",
#             ethnicity_sus == "G" ~ "Other Mixed",
#             ethnicity_sus == "H" ~ "Indian",
#             ethnicity_sus == "J" ~ "Pakistani",
#             ethnicity_sus == "K" ~ "Bangladeshi",
#             ethnicity_sus == "L" ~ "Other Asian",
#             ethnicity_sus == "M" ~ "Caribbean",
#             ethnicity_sus == "N" ~ "African",
#             ethnicity_sus == "P" ~ "Other Black",
#             ethnicity_sus == "R" ~ "Chinese",
#             ethnicity_sus == "S" ~ "All other ethnic groups",
#             TRUE ~ NA_character_
#         ),

#         ethnicity6_combined = if_else(
#             !is.na(latest_ethnicity6_from_codes), latest_ethnicity6_from_codes, ethnicity6_from_sus
#         ),
#         ethnicity16_combined = if_else(
#             !is.na(latest_ethnicity16_from_codes), latest_ethnicity16_from_codes, ethnicity16_from_sus
#         ),
#     )

# cr_data %>%
#    filter(min_start_yr == year) %>%
#    select(
#    patient_id, min_start_yr, fu_end, ethnicity6_combined, ethnicity16_combined, sex,
#    age_group, urban_rural_5, imd_quintile,
#    ends_with("atrisk_grp"), starts_with("vacc_"),
#    starts_with("prior_"), migrant, english_language
#    ) %>%
#     write_feather(here("output", output_file))
# }

# prepare_data(list.files("output", pattern = "dataset_covid_.*\\.arrow", full.names = TRUE),
# "an_bl_data_covid.arrow")

# prepare_data(list.files("output", pattern = "dataset_flu_rsv_.*\\.arrow", full.names = TRUE),
# "an_bl_data_flu_rsv.arrow")

# # save some memory
# rm(cr_data)
# Generate outcomes dataset for cohort analyses
prepare_outcomes_data <- function(file_patterns, disease, output_file) {

     # build readable dynamic column names
    primary_col <- paste0(disease, "_primary_date")
    sc_col      <- paste0(disease, "_sc_date")
    first_gp_col <- paste0("first_gp_", disease)
    first_sc_col <- paste0("first_sc_", disease)
    first_col <- paste0("first_", disease)

    cr_data <- bind_rows(lapply(seq_along(file_patterns), function(i) {
    read_feather(file_patterns[i]) %>%
    mutate(year = as.numeric(gsub("\\D", "", basename(file_patterns[i]))))
    }))

# find first event (GP and secondary care utilisation individually and together) then create dataset with only these variables
cr_data <- cr_data %>%
    mutate(
        !!primary_col := as.Date(.data[[primary_col]]), 
        !!sc_col      := as.Date(.data[[sc_col]])
    ) %>%
    group_by(patient_id) %>%
    mutate(min_start_yr = min(year),
     # dynamic column name on left, evaluated column on right
            !!first_gp_col := na_if(min(.data[[primary_col]], na.rm = TRUE),Inf),
            !!first_sc_col := na_if(min(.data[[sc_col]], na.rm = TRUE),Inf),
      # overall first date
            !!first_col := na_if(min(.data[[first_gp_col]], .data[[first_sc_col]], na.rm = TRUE), Inf) 
    ) %>%
    ungroup() %>%
    mutate(
        !!first_gp_col := as.Date(.data[[first_gp_col]]),
        !!first_sc_col := as.Date(.data[[first_sc_col]]),
        !!first_col    := as.Date(.data[[first_col]]),
        dereg_date = as.Date(dereg_date),
        ons_death_date = as.Date(ons_death_date),
        study_end = as.Date("2025-08-31"),
        fu_start_mth = case_when(!!disease=="covid" ~ 3, !!disease=="rsv" ~ 9, !!disease=="flu" ~ 9),
        fu_start = make_date(year = min_start_yr, month = fu_start_mth, day = 1),
        fu_end = pmin(dereg_date, ons_death_date, study_end, na.rm = TRUE),
        fu_end = as.Date(fu_end),
        # Creating binary flags for outcomes
        !!paste0(disease, "_gp_binary") := case_when(
            !is.na(.data[[first_gp_col]]) & .data[[first_gp_col]] < fu_end ~ 1,
            TRUE ~ 0),
        !!paste0(disease, "_sc_binary") := case_when(
            !is.na(.data[[first_sc_col]]) & .data[[first_sc_col]] < fu_end ~ 1,
            TRUE ~ 0),
        !!paste0(disease, "_binary") := case_when(
            !is.na(.data[[first_col]]) & .data[[first_col]] < fu_end ~ 1,
            TRUE ~ 0),
        # Generating stop dates for outcomes as earliest of either event or end of follow-up
        date_stop_gp = pmin(.data[[first_gp_col]], fu_end, na.rm = TRUE),
        date_stop_sc = pmin(.data[[first_sc_col]], fu_end, na.rm = TRUE),
        date_stop = pmin(.data[[first_col]], fu_end, na.rm = TRUE),
        stop_gp = as.numeric(date_stop_gp - fu_start),
        stop_sc = as.numeric(date_stop_sc - fu_start),
        stop = as.numeric(date_stop - fu_start),
        start = 0,
        # generating year of stop for stratification
        year_end_gp = year(date_stop_gp),
        year_end_sc = year(date_stop_sc),
        year_end = year(date_stop)
        ) %>%
    filter(min_start_yr == year) %>%
    select(patient_id, starts_with("first_"), ends_with("binary"), starts_with("sto"), starts_with("year_end"), start, fu_start) 

    write_rds(cr_data, here("output", output_file))
}

prepare_outcomes_data(list.files("output", pattern = "dataset_covid_.*\\.arrow", full.names = TRUE),
"an_outcome_data_covid.rds", disease = "covid")
prepare_outcomes_data(list.files("output", pattern = "dataset_flu_rsv_.*\\.arrow", full.names = TRUE),
"an_outcome_data_flu.rds", disease = "flu")
prepare_outcomes_data(list.files("output", pattern = "dataset_flu_rsv_.*\\.arrow", full.names = TRUE),
"an_outcome_data_rsv.rds", disease = "rsv")

# # Create file with both baseline information and outcomes.

# outcome_covid <- read_rds(here("output", "an_outcome_data_covid.rds")) %>%
#     full_join(read_feather(here("output", "an_bl_data_covid.arrow"), as_data_frame = TRUE), by="patient_id") %>%
#     write_feather(here("output", "an_outcome_first_covid.arrow"))

# outcome_flu <- read_rds(here("output", "an_outcome_data_flu.rds")) %>%
#     full_join(read_feather(here("output", "an_bl_data_flu_rsv.arrow"), as_data_frame = TRUE), by="patient_id") %>%
#     write_feather(here("output", "an_outcome_first_flu.arrow"))

# outcome_rsv <- read_rds(here("output", "an_outcome_data_rsv.rds")) %>%
#     full_join(read_feather(here("output", "an_bl_data_flu_rsv.arrow"), as_data_frame = TRUE), by="patient_id") %>%
#     write_feather(here("output", "an_outcome_first_rsv.arrow"))