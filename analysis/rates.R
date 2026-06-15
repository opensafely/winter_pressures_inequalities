library(here)
library(broom)
library(dplyr)
library(tidyverse)
library(arrow)

## Import custom user functions
source(here::here("analysis", "redaction.R"))

outdir <- here("output", "tables")
fs::dir_create(outdir)

#define function for midpoint 6 rounding
roundmid_any <- function(x, to = 6) {
  
  # like round_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x != 0))
  
}

# # generating tables with rounded weekly events, denominators and rates from measures

rates_covid <- read_csv(here("output/measures/measures_covid.csv")) %>% 
        #filter(measure=="covid_gp" | measure=="covid_admission" | measure=="covid_emerg") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, imd, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_covid, file.path(outdir, "rounded_rates_covid.csv"))

rates_flu_18 <- read_csv(here("output/measures/measures_flu_2018.csv")) %>% 
        #filter(measure=="flu_gp" | measure=="flu_admission" | measure=="flu_emerg") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, imd, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_flu_18, file.path(outdir, "rounded_rates_flu_18.csv"))

rates_flu_21 <- read_csv(here("output/measures/measures_flu_2021.csv")) %>% 
        #filter(measure=="flu_gp" | measure=="flu_admission" | measure=="flu_emerg") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, imd, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_flu_21, file.path(outdir, "rounded_rates_flu_21.csv"))

rates_rsv_18 <- read_csv(here("output/measures/measures_rsv_2018.csv")) %>% 
        #filter(measure=="rsv_gp" | measure=="rsv_admission" | measure=="rsv_emerg") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, imd, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_rsv_18, file.path(outdir, "rounded_rates_rsv_18.csv"))

rates_rsv_21 <- read_csv(here("output/measures/measures_rsv_2021.csv")) %>% 
        #filter(measure=="rsv_gp" | measure=="rsv_admission" | measure=="rsv_emerg") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, imd, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_rsv_21, file.path(outdir, "rounded_rates_rsv_21.csv"))

rates_rsv_23 <- read_csv(here("output/measures/measures_rsv_2023.csv")) %>% 
        #filter(measure=="rsv_gp" | measure=="rsv_admission" | measure=="rsv_emerg") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, imd, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_rsv_23, file.path(outdir, "rounded_rates_rsv_23.csv"))

rates_rsv_age_18 <- read_csv(here("output/measures/measures_rsv_age_2018.csv")) %>% 
        #filter(measure=="rsv_gp" | measure=="rsv_admission" | measure=="rsv_emerg") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, imd, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_rsv_age_18, file.path(outdir, "rounded_rates_rsv_age_18.csv"))

rates_rsv_age_21 <- read_csv(here("output/measures/measures_rsv_age_2021.csv")) %>% 
        #filter(measure=="rsv_gp" | measure=="rsv_admission" | measure=="rsv_emerg") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, imd, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_rsv_age_21, file.path(outdir, "rounded_rates_rsv_age_21.csv"))

rates_rsv_age_23 <- read_csv(here("output/measures/measures_rsv_age_2023.csv")) %>% 
        #filter(measure=="rsv_gp" | measure=="rsv_admission" | measure=="rsv_emerg") %>% 
        mutate(outcome_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(numerator)),
                n_mid6 = if_else(numerator <= 7, NA_real_, roundmid_any(denominator)),
                rate_mid6 = round((outcome_mid6 / n_mid6) * 100000, 2)) %>% 
        select(measure, interval_start, interval_end, ethnicity, imd, outcome_mid6, n_mid6, rate_mid6)
write_csv(rates_rsv_age_23, file.path(outdir, "rounded_rates_rsv_age_23.csv"))

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