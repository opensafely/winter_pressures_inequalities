library(here)
library(tidyverse)
library(arrow)
library(lubridate)
library(readr)

#Import data
prepare_data <- function(file_patterns, output_file) {
    cr_data <- bind_rows(lapply(seq_along(file_patterns), function(i) {
        read_feather(file_patterns[i]) %>%
        mutate(year = as.numeric(gsub("\\D", "", basename(file_patterns[i]))))
    }))

    cr_data <- cr_data %>%
        group_by(patient_id) %>%
        mutate(min_start_yr = min(year)) %>%
        ungroup() %>%
        mutate(
            #study_end = as.Date("2025-08-31"),
            #fu_end = pmin(dereg_date, ons_death_date, study_end, na.rm = TRUE),
            bmi_category = case_when(
                bmi == 0 ~ "unknown or outlier",
                bmi < 18.5 ~ "underweight",
                bmi < 25 ~ "ideal",
                bmi < 30 ~ "overweight",
                bmi >= 30 ~ "obese",
                TRUE ~ NA_character_
            ),
            latest_ethnicity6_from_codes = case_when(
                ethnicity6_gp == "1" ~ "White",
                ethnicity6_gp == "2" ~ "Mixed",
                ethnicity6_gp == "3" ~ "Asian or Asian British",
                ethnicity6_gp == "4" ~ "Black or Black British",
                ethnicity6_gp == "5" ~ "Chinese or Other Ethnic Groups",
                TRUE ~ NA_character_
            ),
            ethnicity6_from_sus = case_when(
                ethnicity_sus %in% c("A", "B", "C") ~ "White",
                ethnicity_sus %in% c("D", "E", "F", "G") ~ "Mixed",
                ethnicity_sus %in% c("H", "J", "K", "L") ~ "Asian or Asian British",
                ethnicity_sus %in% c("M", "N", "P") ~ "Black or Black British",
                ethnicity_sus %in% c("R", "S") ~ "Chinese or Other Ethnic Groups",
                TRUE ~ NA_character_
            ),
            latest_ethnicity16_from_codes = case_when(
                ethnicity16_gp == "1" ~ "White British",
                ethnicity16_gp == "2" ~ "White Irish",
                ethnicity16_gp == "3" ~ "Other White",
                ethnicity16_gp == "4" ~ "White and Caribbean",
                ethnicity16_gp == "5" ~ "White and African",
                ethnicity16_gp == "6" ~ "White and Asian",
                ethnicity16_gp == "7" ~ "Other Mixed",
                ethnicity16_gp == "8" ~ "Indian",
                ethnicity16_gp == "9" ~ "Pakistani",
                ethnicity16_gp == "10" ~ "Bangladeshi",
                ethnicity16_gp == "11" ~ "Other Asian",
                ethnicity16_gp == "12" ~ "Caribbean",
                ethnicity16_gp == "13" ~ "African",
                ethnicity16_gp == "14" ~ "Other Black",
                ethnicity16_gp == "15" ~ "Chinese",
                ethnicity16_gp == "16" ~ "All other ethnic groups",
                TRUE ~ NA_character_
            ),

            ethnicity16_from_sus = case_when(
                ethnicity_sus == "A" ~ "White British",
                ethnicity_sus == "B" ~ "White Irish",
                ethnicity_sus == "C" ~ "Other White",
                ethnicity_sus == "D" ~ "White and Caribbean",
                ethnicity_sus == "E" ~ "White and African",
                ethnicity_sus == "F" ~ "White and Asian",
                ethnicity_sus == "G" ~ "Other Mixed",
                ethnicity_sus == "H" ~ "Indian",
                ethnicity_sus == "J" ~ "Pakistani",
                ethnicity_sus == "K" ~ "Bangladeshi",
                ethnicity_sus == "L" ~ "Other Asian",
                ethnicity_sus == "M" ~ "Caribbean",
                ethnicity_sus == "N" ~ "African",
                ethnicity_sus == "P" ~ "Other Black",
                ethnicity_sus == "R" ~ "Chinese",
                ethnicity_sus == "S" ~ "All other ethnic groups",
                TRUE ~ NA_character_
            ),

            ethnicity6_combined = if_else(
                !is.na(latest_ethnicity6_from_codes), latest_ethnicity6_from_codes, ethnicity6_from_sus
            ),
            ethnicity16_combined = if_else(
                !is.na(latest_ethnicity16_from_codes), latest_ethnicity16_from_codes, ethnicity16_from_sus
            ),
        )

    cr_data %>%
        filter(min_start_yr == year) %>%
        select(
            patient_id, min_start_yr, ethnicity6_combined, ethnicity16_combined, sex,
            age_group, urban_rural_5, imd_quintile,
            ends_with("atrisk_grp"), starts_with("vacc_"),
            starts_with("prior_"), bmi_category, migrant, english_language
        ) %>%
        write_feather(here("output", output_file)
    )
    # save some memory
    rm(cr_data)
}

prepare_data(list.files("output", pattern = "dataset_covid_.*\\.arrow", full.names = TRUE),
"an_bl_data_covid.arrow")

prepare_data(list.files("output", pattern = "dataset_flu_rsv_.*\\.arrow", full.names = TRUE),
"an_bl_data_flu_rsv.arrow")


