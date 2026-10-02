library(here)
library(tidyverse)
library(arrow)
library(lubridate)
library(readr)

## Import custom user functions
source(here::here("analysis", "redaction.R"))

outdir <- here("output", "tables")
fs::dir_create(outdir)

# Imports outputs produced from Cox model scripts and midpoint 6 rounds the population size and number of events
cox_output <- readr:: read_csv(here(outdir, "cox_models_covid_2021.csv"))
cox_int_sex_output <- readr:: read_csv(here(outdir, "cox_models_int_covid_2021_sex.csv"))
cox_int_imd_quintile_output <- readr:: read_csv(here(outdir, "cox_models_int_covid_2021_imd_quintile.csv"))
cox_time_output <- readr:: read_csv(here(outdir, "cox_models_time_covid_2021.csv"))

cox_output <- cox_output %>% 
    mutate(
        N_mid6 = if_else(n <= 7, NA_real_, roundmid_any(n)),
        Events_mid6 = if_else(n_events <= 7, NA_real_, roundmid_any(n_events))
    ) %>% 
    select(-n, -n_events)

write_csv(
    cox_output,
    file.path(
      outdir,
      paste0("cox_models_covid_2021_r.csv")
    )
  )

cox_int_sex_output <- cox_int_sex_output %>% 
    mutate(
        N_mid6 = if_else(n <= 7, NA_real_, roundmid_any(n)),
        Events_mid6 = if_else(n_events <= 7, NA_real_, roundmid_any(n_events))
    ) %>% 
    select(-n, -n_events)

write_csv(
    cox_output,
    file.path(
      outdir,
      paste0("cox_models_int_covid_2021_sex_r.csv")
    )
  )

cox_int_imd_quintile_output <- cox_int_imd_quintile_output %>% 
    mutate(
        N_mid6 = if_else(n <= 7, NA_real_, roundmid_any(n)),
        Events_mid6 = if_else(n_events <= 7, NA_real_, roundmid_any(n_events))
    ) %>% 
    select(-n, -n_events)

write_csv(
    cox_output,
    file.path(
      outdir,
      paste0("cox_models_int_covid_2021_imd_quintile_r.csv")
    )
  )

cox_time_output <- cox_time_output %>% 
    mutate(
        N_mid6 = if_else(n <= 7, NA_real_, roundmid_any(n)),
        Events_mid6 = if_else(n_events <= 7, NA_real_, roundmid_any(n_events))
    ) %>% 
    select(-n, -n_events)

write_csv(
    cox_output,
    file.path(
      outdir,
      paste0("cox_models_time_covid_2021_r.csv")
    )
  )