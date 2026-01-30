library(here)
library(tidyverse)
library(arrow)
library(lubridate)
library(readr)

args <- commandArgs(trailingOnly = TRUE)

disease <- args[1]
file <- args[2]

# Generate outcomes dataset for cohort analyses - identifying first event for each infection: COVID-19, RSV and flu
#prepare_outcomes_data <- function(file_patterns, output_file) {

    # build readable dynamic column names
primary_col <- paste0(disease, "_primary_date")
sc_col      <- paste0(disease, "_sc_date")
first_gp_col <- paste0("first_gp_", disease)
first_sc_col <- paste0("first_sc_", disease)
first_col <- paste0("first_", disease)

file_pattern <- paste0("dataset_",file,"_.*\\.arrow")

# Find matching files
file_patterns <- list.files(
path = "output",
pattern = file_pattern,
full.names = TRUE
)

# Check files were found
if (length(file_patterns) == 0) {
stop(paste("No files found matching pattern:", file_pattern))
}

print(paste("Found", length(file_patterns), "files:"))
print(file_patterns)

# Read and bind the files
cr_data <- bind_rows(lapply(seq_along(file_patterns), function(i) {
read_feather(file_patterns[i]) %>%
    mutate(year = as.numeric(gsub("\\D", "", basename(file_patterns[i]))))
}))

print(dim(cr_data))

if (any(is.na(cr_data$year))) {
    stop("Year is missing")
}

#find first event (GP and secondary care utilisation individually and together) then create dataset with only these variables
cr_data <- cr_data %>%
    # Take out unneccessary columns
    select(-ethnicity6_gp, -ethnicity16_gp, -ethnicity_sus, -sex, -age, -urban_rural_5, -imd_quintile, 
    -ends_with("atrisk_grp"), -starts_with("vacc_"), -starts_with("prior_"), -severe_obesity, -bmi, 
    -bmi_date, -migrant, -english_language) %>%
    # Format variables as date
    mutate(
        !!primary_col := as.Date(.data[[primary_col]]), 
        !!sc_col      := as.Date(.data[[sc_col]]),
    ) %>%
    # Find minimum year for each person as eligibility assessed yearly, and follow-up could start at any of these
    group_by(patient_id) %>%
    mutate(min_start_yr_out = min(year),
    # dynamic column name on left, evaluated column on right
            !!first_gp_col := na_if(min(.data[[primary_col]], na.rm = TRUE),Inf),
            !!first_sc_col := na_if(min(.data[[sc_col]], na.rm = TRUE),Inf),
    # overall first date
            !!first_col := na_if(min(.data[[first_gp_col]], .data[[first_sc_col]], na.rm = TRUE), Inf) 
    ) %>%
    ungroup() %>%
    mutate(
        # format variables as dates
        !!first_gp_col := as.Date(.data[[first_gp_col]]),
        !!first_sc_col := as.Date(.data[[first_sc_col]]),
        !!first_col    := as.Date(.data[[first_col]]),
        dereg_date = as.Date(dereg_date),
        ons_death_date = as.Date(ons_death_date),
        study_end = as.Date("2025-08-31"),
        fu_start_mth = case_when(
            disease=="covid" ~ 3, 
            disease %in% c("rsv", "flu") ~ 9
            ),
        fu_start = make_date(year = min_start_yr_out, month = fu_start_mth, day = 1),
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
        year_end = year(date_stop),
        )

output_file <- paste0("an_outcome_data_", disease,".rds")
# File with outcome data only
cr_data %>%
    filter(min_start_yr_out == year) %>%
    select(patient_id, starts_with("first_"), ends_with("binary"), starts_with("sto"), starts_with("fu_"),  starts_with("date_"), starts_with("year_end"), start,) %>%
    write_rds(here("output", output_file))


# # Create file with both baseline information and outcomes.
bl_file <- paste0("an_bl_data_",file,".arrow")
final_file <- paste0("an_outcome_first_",disease,".arrow")
outcome_bl <- read_rds(here("output", output_file)) %>%
    full_join(read_feather(here("output", bl_file), as_data_frame = TRUE), by="patient_id") %>%
    write_feather(here("output", final_file))

