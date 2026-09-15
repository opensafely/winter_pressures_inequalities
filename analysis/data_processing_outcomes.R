library(here)
library(tidyverse)
library(arrow)
library(lubridate)
library(readr)
library(data.table)

args <- commandArgs(trailingOnly = TRUE)

disease <- args[1]
file <- args[2]
min_year <- args[3]
max_year <- args[4]

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

years <- as.numeric(gsub("\\D", "", basename(file_patterns)))

file_patterns <- file_patterns[years <= max_year & years >= min_year]

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
    -ends_with("atrisk_grp"), -starts_with("vacc_"), -starts_with("prior_"), , -ends_with("vaccination_date"), -severe_obesity, -recent_bmi, 
    -recent_bmi_date, -migrant, -english_language) %>%
    mutate(year = as.numeric(gsub("\\D", "", basename(file_patterns[i]))))
}))
print(paste("size of outcomes cr_data file"))
print(dim(cr_data))

if (any(is.na(cr_data$year))) {
    stop("Year is missing")
}
# making standard variable names
if (disease == "covid") {
    cr_data <- cr_data %>% 
        mutate(primary_col = covid_primary_date,
        sens_col = covid_primary_sens_date,
        sc_col = covid_sc_date, 
        emerg_col = covid_emerg_date) %>%
        select(-covid_primary_date, -covid_sc_date, -covid_emerg_date)
}

if (disease == "flu") {
    cr_data <- cr_data %>% 
        mutate(primary_col = flu_primary_date,
        sens_col = flu_primary_sens_date,
        sc_col = flu_sc_date,
        emerg_col = flu_emerg_date) %>% 
        select(-flu_primary_date, -flu_sc_date, -flu_emerg_date)
}

if (disease == "rsv") {
    cr_data <- cr_data %>% 
        mutate(primary_col = rsv_primary_date,
        sens_col = rsv_primary_sens_date,
        sc_col = rsv_sc_date,
        emerg_col = rsv_emerg_date) %>% 
        select(-rsv_primary_date, -rsv_sc_date, -rsv_emerg_date)
}

#find first event (GP and secondary care utilisation individually and together) then create dataset with only these variables
setDT(cr_data)

# check whether dereg_Date and ons_death_date are unique across rows per patient
print(paste("Number of people with multiple deregistration dates"))
multiple_dereg <- cr_data[, .(
  n_dereg_date = uniqueN(dereg_date, na.rm = TRUE)
), by = patient_id][n_dereg_date > 1]
print(dim(multiple_dereg))
print(paste("Number of people with multiple ONS death dates"))
multiple_ons <- cr_data[, .(
  n_ons_death_date = uniqueN(ons_death_date, na.rm = TRUE)
), by = patient_id][n_ons_death_date > 1]
print(dim(multiple_ons))

# Find first year in follow-up and first GP and secondary care healthcare utilisation
# Find first deregistration date as people could deregister one year and then re-register one of the years after, so taking first registration period

safe_min_date <- function(x, fu_start = NULL) {
  x <- x[!is.na(x)]
  if (!is.null(fu_start)) {
        x <- x[x >= fu_start]
    }
  if (!length(x)) as.Date(NA) else min(x)
}

cr_data[, `:=`(
  min_start_yr_out = min(year),
  first_gp_col = safe_min_date(primary_col),
  first_gp_sens_col = safe_min_date(sens_col),
  first_sc_col = safe_min_date(sc_col),
  first_emerg_col = safe_min_date(emerg_col),
  first_dereg_date = safe_min_date(dereg_date)

), by = patient_id]

# Checking event dates are after start date and not in the future
print("Number of events prior to follow-up start")
event_yr_prior <- cr_data[year(sc_col) < min_start_yr_out, .(
    n_prior_events = .N
), by = patient_id]
print(dim(event_yr_prior))

# filter to only rows 
cr_data <- cr_data[min_start_yr_out == year]

study_end <- as.Date(paste0(max_year,"-08-31"))
cat("study_end:", as.character(study_end), "\n")
fu_start_month <- if (disease == "covid") 3L else 9L

cr_data[, `:=`(
    first_col = fifelse(
        is.na(first_gp_col) & is.na(first_gp_sens_col) & is.na(first_sc_col) & is.na(first_emerg_col),
        as.Date(NA),
        pmin(first_gp_col, first_gp_sens_col, first_sc_col, first_emerg_col, na.rm = TRUE)
    ),
    
    ons_death_date = as.Date(ons_death_date),
    fu_start = as.Date(sprintf("%d-%02d-01", min_start_yr_out, fu_start_month))
)]
print("Check fu_start")
print(summary(cr_data$fu_start))
print(summary(cr_data$min_start_yr_out))
# Checking event dates are after start date and not in the future
print("Number of events prior to first follow-up start")
events_prior <- cr_data[first_sc_col < fu_start, .(
    n_prior_events = .N
), by = patient_id]
print(dim(events_prior))
print("First dereg prior to follow-up start")
dereg_prior <- cr_data[first_dereg_date < fu_start, .(
    n_prior_events = .N
), by = patient_id]
print(dim(dereg_prior))
print("Death prior to follow-up start")
death_prior <- cr_data[ons_death_date < fu_start, .(
    n_prior_events = .N
), by = patient_id]
print(dim(death_prior))
# number of people in dataset prior to filtering on ONS death prior to start of follow-up
print(dim(cr_data))
# take out people who have an ONS death date prior to the start of follow-up
cr_data <- cr_data[is.na(ons_death_date<fu_start) | ons_death_date >= fu_start]
# number of people in dataset after filtering on ONS death prior to start of follow-up 
print(dim(cr_data))


# safer fu_end with all-NA handling

cr_data[, fu_end := {
    x <- pmin(
        as.numeric(first_dereg_date),
        as.numeric(ons_death_date),
        as.numeric(study_end),
        na.rm = TRUE
    )
    as.Date(x, origin = "1970-01-01")
}]

# CHECK FOR TIMING ISSUES
cat("\n=== Timing Check for", disease, "===\n")

timing_issues <- cr_data %>%
        summarise(
        total = n(),
        fu_end_study_end = sum(fu_end > study_end, na.rm = TRUE),
        )

print(timing_issues)

cr_data[, `:=`(
  gp_binary = fifelse(!is.na(first_gp_col) & first_gp_col < fu_end, 1L, 0L),
  gp_sens_binary = fifelse(!is.na(first_gp_sens_col) & first_gp_sens_col < fu_end, 1L, 0L),
  sc_binary = fifelse(!is.na(first_sc_col) & first_sc_col < fu_end, 1L, 0L),
  emerg_binary = fifelse(!is.na(first_emerg_col) & first_emerg_col < fu_end, 1L, 0L),
  binary_all = fifelse(!is.na(first_col) & first_col < fu_end, 1L, 0L),
  date_stop_gp = fifelse(is.na(first_gp_col), fu_end, pmin(first_gp_col, fu_end)),
  date_stop_gp_sens = fifelse(is.na(first_gp_sens_col), fu_end, pmin(first_gp_sens_col, fu_end)),
  date_stop_sc = fifelse(is.na(first_sc_col), fu_end, pmin(first_sc_col, fu_end)),
  date_stop_emerg = fifelse(is.na(first_emerg_col), fu_end, pmin(first_emerg_col, fu_end)),
  date_stop = fifelse(is.na(first_col), fu_end, pmin(first_col, fu_end))
)]
cr_data[, `:=`(
    stop_gp = as.numeric(date_stop_gp - fu_start),
    stop_gp_sens = as.numeric(date_stop_gp_sens - fu_start),
    stop_sc = as.numeric(date_stop_sc - fu_start),
    stop_emerg = as.numeric(date_stop_emerg - fu_start),
    stop = as.numeric(date_stop - fu_start),
    #Setting start as half a day prior to start date so that we can still have same day events
    start = -0.5,
    # generating year of stop for stratification
    year_end_gp = year(date_stop_gp),
    year_end_gp_sens = year(date_stop_gp_sens),
    year_end_sc = year(date_stop_sc),
    year_end_emerg = year(date_stop_emerg),
    year_end = year(date_stop)
    )]

print("Check stop times")
print(summary(cr_data$stop_gp))
print(summary(cr_data$stop_gp_sens))
print(summary(cr_data$stop_sc))
print(summary(cr_data$stop_emerg))
print("GP")
neg_gp <- cr_data[stop_gp < -0.5, .(
    n_prior_events = .N
), by = patient_id]
print(dim(neg_gp))
print("GP sens")
neg_gp_sens <- cr_data[stop_gp_sens < -0.5, .(
    n_prior_events = .N
), by = patient_id]
print(dim(neg_gp_sens))
print("SC")
neg_sc <- cr_data[stop_sc < -0.5, .(
    n_prior_events = .N
), by = patient_id]
print(dim(neg_sc))
print("AE")
neg_ae <- cr_data[stop_emerg < -0.5, .(
    n_prior_events = .N
), by = patient_id]
print(dim(neg_ae))
print("Check fu_end")
neg_end <- cr_data[fu_end < fu_start, .(
    n_prior_events = .N
), by = patient_id]
print(dim(neg_end))

output_file <- paste0("an_outcome_data_", disease,"_", min_year,".rds")
# File with outcome data only
cr_data %>%
    filter(min_start_yr_out == year) %>%
    select(patient_id, starts_with("first_"), ends_with("binary"), starts_with("sto"), starts_with("fu_"),  starts_with("date_"), starts_with("year_end"), start, binary_all, min_start_yr_out) %>%
    write_rds(here("output", output_file))


# safety checks before join
bl_file <- paste0("an_bl_data_",file,"_",min_year,".arrow")
print(bl_file)
outcomes <- read_rds(here("output", output_file))
baseline <- read_feather(here("output", bl_file), as_data_frame = TRUE)

stopifnot(!anyDuplicated(outcomes$patient_id))
stopifnot(!anyDuplicated(baseline$patient_id))

final_file <- paste0("an_outcome_first_",disease,"_",min_year,".arrow")

# choose left_join if outcomes are the analysis denominator
outcome_bl <- baseline %>%
  inner_join(outcomes, by = "patient_id")

write_feather(outcome_bl, here("output", final_file))
