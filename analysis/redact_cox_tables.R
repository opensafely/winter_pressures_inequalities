library(here)
library(tidyverse)
library(readr)

## Import custom user functions
source(here::here("analysis", "redaction.R"))

outdir <- here("output", "tables")
fs::dir_create(outdir)

# Redacts a single Cox model output table: midpoint 6 rounds the population
# size and number of events (overall and within each exposure level),
# blanking counts of 7 or fewer.
# Argument is the table name without extension, e.g. cox_models_time_covid_2021
# Reads output/tables/<table>.csv and writes output/tables/<table>_r.csv
args <- commandArgs(trailingOnly = TRUE)
table_name <- args[1]

cox_output <- read_csv(file.path(outdir, paste0(table_name, ".csv")))

cox_output <- cox_output %>%
    mutate(
        N_mid6 = if_else(n <= 7, NA_real_, roundmid_any(n)),
        Events_mid6 = if_else(n_events <= 7, NA_real_, roundmid_any(n_events)),
        N_level_mid6 = if_else(n_level <= 7, NA_real_, roundmid_any(n_level)),
        Events_level_mid6 = if_else(n_events_level <= 7, NA_real_, roundmid_any(n_events_level))
    ) %>%
    select(-n, -n_events, -n_level, -n_events_level)

write_csv(cox_output, file.path(outdir, paste0(table_name, "_r.csv")))
