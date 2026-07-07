library(tidyverse)
library(here)
library(arrow)

outdir <- here("output", "tables")
fs::dir_create(outdir)

data_covid_2021 <- read_feather(here("output/an_outcome_first_covid_2021.arrow")) 
capture.output(
  skimr::skim_without_charts(data_covid_2021),
  file = here(outdir, paste0("skim_covid_2021.txt")),
  split=FALSE
)

data_flu_2018 <- read_feather(here("output/an_outcome_first_flu_2018.arrow")) 
capture.output(
  skimr::skim_without_charts(data_flu_2018),
  file = here(outdir, paste0("skim_flu_2018.txt")),
  split=FALSE
)

data_flu_2021 <- read_feather(here("output/an_outcome_first_flu_2021.arrow")) 
capture.output(
  skimr::skim_without_charts(data_flu_2021),
  file = here(outdir, paste0("skim_flu_2021.txt")),
  split=FALSE
)

data_rsv_2018 <- read_feather(here("output/an_outcome_first_rsv_2018.arrow")) 
capture.output(
  skimr::skim_without_charts(data_rsv_2018),
  file = here(outdir, paste0("skim_rsv_2018.txt")),
  split=FALSE
)

data_rsv_2021 <- read_feather(here("output/an_outcome_first_rsv_2021.arrow")) 
capture.output(
  skimr::skim_without_charts(data_rsv_2021),
  file = here(outdir, paste0("skim_rsv_2021.txt")),
  split=FALSE
)