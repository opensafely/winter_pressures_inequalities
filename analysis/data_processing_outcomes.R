library(here)
library(tidyverse)
library(arrow)
library(lubridate)
library(readr)
library(data.table)

args <- commandArgs(trailingOnly = TRUE)

disease <- args[1]
file <- args[2]

# Generate outcomes dataset for cohort analyses - identifying first event for each infection: COVID-19, RSV and flu
#prepare_outcomes_data <- function(file_patterns, output_file) {

    # build readable dynamic column names
#primary_col <- paste0(disease, "_primary_date")
#sc_col      <- paste0(disease, "_sc_date")
#first_gp_col <- paste0("first_gp_", disease)
#first_sc_col <- paste0("first_sc_", disease)
#first_col <- paste0("first_", disease)

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
    # Take out unneccessary columns
    select(-ethnicity6_gp, -ethnicity16_gp, -ethnicity_sus, -sex, -age, -urban_rural_5, -imd_quintile, 
    -ends_with("atrisk_grp"), -starts_with("vacc_"), -starts_with("prior_"), , -ends_with("vaccination_date"), -severe_obesity, -bmi, 
    -bmi_date, -migrant, -english_language) %>%
    mutate(year = as.numeric(gsub("\\D", "", basename(file_patterns[i]))))
}))

print(dim(cr_data))

if (any(is.na(cr_data$year))) {
    stop("Year is missing")
}
# making standard variable names
if (disease == "covid") {
    cr_data <- cr_data %>% 
        mutate(primary_col = covid_primary_date,
        sc_col = covid_sc_date) %>% 
        select(-covid_primary_date, -covid_sc_date)
}

if (disease == "flu") {
    cr_data <- cr_data %>% 
        mutate(primary_col = flu_primary_date,
        sc_col = flu_sc_date) %>% 
        select(-flu_primary_date, -flu_sc_date)
}

if (disease == "rsv") {
    cr_data <- cr_data %>% 
        mutate(primary_col = rsv_primary_date,
        sc_col = rsv_sc_date) %>% 
        select(-rsv_primary_date, -rsv_sc_date)
}

#find first event (GP and secondary care utilisation individually and together) then create dataset with only these variables
setDT(cr_data)
 # Format variables as date
date_cols <- grep("_date$", names(cr_data), value = TRUE)
cr_data[, (date_cols) := lapply(.SD, as.Date), .SDcols = date_cols]
# Find first year in follow-up and first GP and secondary care healthcare utilisation
cr_data[, `:=`(
  min_start_yr_out = min(year),

  first_gp_col = {
    x <- min(primary_col, na.rm = TRUE)
    if(is.infinite(x)) as.Date(NA) else as.Date(x)
  },

  first_sc_col = {
    x <- min(sc_col, na.rm = TRUE)
    if(is.infinite(x)) as.Date(NA) else as.Date(x)
  }
), by = patient_id]

# cr_data <- cr_data %>%
#     # Format variables as date
#     mutate(across(ends_with("_date"), as.Date)
#     ) %>%
#     # Find minimum year for each person as eligibility assessed yearly, and follow-up could start at any of these
#     group_by(patient_id) %>%
#     mutate(min_start_yr_out = min(year),
#             first_gp_col = na_if(min(primary_col, na.rm = TRUE), Inf),
#             first_sc_col = na_if(min(sc_col, na.rm = TRUE), Inf),
#     ) %>%
#     ungroup() %>%
    cr_data[, `:=`(
        # format variables as dates
        first_gp_col = as.Date(first_gp_col),
        first_sc_col = as.Date(first_sc_col),
        #!!first_col    := as.Date(.data[[first_col]]),
        # overall first date
        first_col = pmin(first_gp_col, first_sc_col, na.rm = TRUE), 
        dereg_date = as.Date(dereg_date),
        ons_death_date = as.Date(ons_death_date),
        study_end = as.Date("2025-08-31"),
        #fu_start_mth = fifelse(disease=="covid", 3L, 9L),
        fu_start = as.Date(sprintf("%d-%02d-01", min_start_yr_out, fifelse(disease=="covid", 3L, 9L))),
        fu_end = as.Date(pmin(dereg_date, ons_death_date, as.Date("2025-08-31"), na.rm = TRUE))
    )]
    cr_data[, `:=`(
        # Creating binary flags for outcomes
        gp_binary = fifelse(!is.na(first_gp_col) & first_gp_col < fu_end, 1L, 0L),
        sc_binary = fifelse(!is.na(first_sc_col) & first_sc_col < fu_end, 1L, 0L),
        binary_all = fifelse(!is.na(first_col) & first_col < fu_end, 1L, 0L),
        # Generating stop dates for outcomes as earliest of either event or end of follow-up
        date_stop_gp = pmin(first_gp_col, fu_end, na.rm = TRUE),
        date_stop_sc = pmin(first_sc_col, fu_end, na.rm = TRUE),
        date_stop = pmin(first_col, fu_end, na.rm = TRUE)
    )]
    cr_data[, `:=`(
        stop_gp = as.numeric(date_stop_gp - fu_start),
        stop_sc = as.numeric(date_stop_sc - fu_start),
        stop = as.numeric(date_stop - fu_start),
        start = 0L,
        # generating year of stop for stratification
        year_end_gp = year(date_stop_gp),
        year_end_sc = year(date_stop_sc),
        year_end = year(date_stop)
        )]

# making disease specific variable names
# decided against this so that I can have consistent variable names across files, making it easier in analysis files
# if (disease == "covid") {
#     cr_data <- cr_data %>% 
#         mutate(covid_primary_date = primary_col, 
#         covid_sc_date = sc_col,
#         first_gp_covid = first_gp_col,
#         first_sc_covid = first_sc_col,
#         first_covid = first_col,
#         covid_gp_binary = gp_binary,
#         covid_sc_binary = sc_binary,
#         covid_binary = binary_all) %>% 
#         select(-primary_col, -sc_col, -first_gp_col, -first_sc_col, -first_col, -gp_binary, -sc_binary, -binary_all)
# }

# if (disease == "flu") {
#     cr_data <- cr_data %>% 
#         mutate(flu_primary_date = primary_col, 
#         flu_sc_date = sc_col,
#         first_gp_flu = first_gp_col,
#         first_sc_flu = first_sc_col,
#         first_flu = first_col,
#         flu_gp_binary = gp_binary,
#         flu_sc_binary = sc_binary,
#         flu_binary = binary_all) %>% 
#         select(-primary_col, -sc_col, -first_gp_col, -first_sc_col, -first_col, -gp_binary, -sc_binary, -binary_all)
# }

# if (disease == "rsv") {
#     cr_data <- cr_data %>% 
#         mutate(rsv_primary_date = primary_col, 
#         rsv_sc_date = sc_col,
#         first_gp_rsv = first_gp_col,
#         first_sc_rsv = first_sc_col,
#         first_rsv = first_col,
#         rsv_gp_binary = gp_binary,
#         rsv_sc_binary = sc_binary,
#         rsv_binary = binary_all) %>% 
#         select(-primary_col, -sc_col, -first_gp_col, -first_sc_col, -first_col, -gp_binary, -sc_binary, -binary_all)
# }


output_file <- paste0("an_outcome_data_", disease,".rds")
# File with outcome data only
cr_data %>%
    filter(min_start_yr_out == year) %>%
    select(patient_id, starts_with("first_"), ends_with("binary"), starts_with("sto"), starts_with("fu_"),  starts_with("date_"), starts_with("year_end"), start, binary_all, min_start_yr_out) %>%
    write_rds(here("output", output_file))


# # Create file with both baseline information and outcomes.
bl_file <- paste0("an_bl_data_",file,".arrow")
final_file <- paste0("an_outcome_first_",disease,".arrow")
outcome_bl <- read_rds(here("output", output_file)) %>%
    full_join(read_feather(here("output", bl_file), as_data_frame = TRUE), by="patient_id") %>%
    write_feather(here("output", final_file))

