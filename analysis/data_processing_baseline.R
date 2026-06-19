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
    print(output_file)
    cr_data <- cr_data %>%
        group_by(patient_id) %>%
        mutate(min_start_yr = min(year)) %>%
        ungroup() %>%
        filter(min_start_yr == year) %>%
        mutate(
            #study_end = as.Date("2025-08-31"),
            #fu_end = pmin(dereg_date, ons_death_date, study_end, na.rm = TRUE),
            bmi_category = case_when(
                bmi == 0 ~ "unknown or outlier",
                bmi < 18.5 ~ "underweight",
                bmi < 25 ~ "ideal",
                bmi < 30 ~ "overweight",
                bmi >= 30 ~ "obese",
                is.na(bmi) ~ "Missing",
                TRUE ~ "Missing",
            ),
            ethnicity6_from_codes = relevel(factor(ethnicity6_gp,
                                     levels = c("1", "2", "3", "4", "5"),
                                     labels = c("White", "Mixed",
                                                "Asian or Asian British",
                                                "Black or Black British",
                                                "Other Ethnic Groups"),
                                     ordered = FALSE), ref = "White"),
            ethnicity6_from_sus = relevel(factor(recode(
                    ethnicity_sus, "A" = "White", "B" = "White", "C" = "White",
                    "D" = "Mixed", "E" = "Mixed", "F" = "Mixed", "G" = "Mixed",
                    "H" = "Asian or Asian British", "J" = "Asian or Asian British",
                    "K" = "Asian or Asian British", "L" = "Asian or Asian British",
                    "M" = "Black or Black British", "N" = "Black or Black British",
                    "P" = "Black or Black British", "R" = "Other Ethnic Groups",
                    "S" = "Other Ethnic Groups"), ordered = F), ref = "White"),

            ethnicity16_from_codes = relevel(factor(ethnicity16_gp,
                                     levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", 
                                     "10", "11", "12", "13", "14", "15", "16"),
                                     labels = c("White British", "White Irish", "Other White",
                                                "White and Caribbean",
                                                "White and African",
                                                "White and Asian",
                                                "Other Mixed",
                                                "Indian",
                                                "Pakistani",
                                                "Bangladeshi",
                                                "Other Asian",
                                                "Caribbean",
                                                "African",
                                                "Other Black",
                                                "Chinese",
                                                "All other ethnic groups"),
                                     ordered = FALSE), ref = "White British"),

            ethnicity16_from_sus = relevel(factor(recode(
                    ethnicity_sus, "A" = "White British",
                "B" = "White Irish",
                "C" = "Other White",
                "D" = "White and Caribbean",
                "E" = "White and African",
                "F" = "White and Asian",
                "G" = "Other Mixed",
                "H" = "Indian",
                "J" = "Pakistani",
                "K" = "Bangladeshi",
                "L" = "Other Asian",
                "M" = "Caribbean",
                "N" = "African",
                "P" = "Other Black",
                "R" = "Chinese",
                "S" = "All other ethnic groups"), ordered = F), ref = "White British"),

            ethnicity6_combined = if_else(
                is.na(ethnicity6_from_codes), if_else(is.na(ethnicity6_from_sus), "Unknown", ethnicity6_from_sus), 
                ethnicity6_from_codes
            ),
            ethnicity16_combined = if_else(
                is.na(ethnicity16_from_codes), if_else(is.na(ethnicity16_from_sus), "Unknown", ethnicity16_from_sus), 
                ethnicity16_from_codes
            ),
            imd_quintile = na_if(imd_quintile, 99),
            imd_quintile = factor(imd_quintile),
            imd_quintile = forcats::fct_na_value_to_level(imd_quintile, level = "Missing"),
            migrant = factor(migrant),
            #migrant = forcats::fct_na_value_to_level(migrant, level = "Missing"),
            english_language = factor(english_language),
            #english_language = forcats::fct_na_value_to_level(english_language, level = "Missing"),
        )
    print("Check ethnicity GP variable after recoding")
    print(table(cr_data$ethnicity6_gp, cr_data$ethnicity6_from_codes, useNA = "always"))
    print("Check ethnicity SUS variable after recoding")
    print(table(cr_data$ethnicity_sus, cr_data$ethnicity6_from_sus, useNA = "always"))
    cr_data %>%
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


