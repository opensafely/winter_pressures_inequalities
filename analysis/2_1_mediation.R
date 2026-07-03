library(here)
library(readr)
library(dplyr)
library(arrow)
library(survival)
library(mediation)

# Need to generate code that may not run on dummy data but can try on real data
# test with single subgroup
data_df <- read_feather(here("output/an_outcome_first_covid_2021.arrow"), as_data_frame = TRUE) 
print(summary(data_df$stop_sc))

data_df_asian <- data_df %>% 
  mutate(stop_sc = if_else(stop_sc == 0, 0.5, stop_sc)) %>% 
  filter(ethnicity6_combined %in% c("Asian or Asian British", "White")) %>%
  droplevels()

med_fit <- glm(migrant ~ ethnicity6_combined + age_group, 
               data = data_df_asian,
               family = binomial(link = "logit"))
print(summary(med_fit))
out_fit <- out_model <- survreg(Surv(stop_sc,sc_binary) ~ ethnicity6_combined + 
                                  age_group + migrant,
                                data = data_df_asian,
                                dist = "weibull",
                                robust = TRUE)  
print(summary(out_fit))
med_out <- mediate(med_fit, out_fit, treat = "ethnicity6_combined", mediator = "migrant",
                   treat.value = "Asian or Asian British", control.value = "White")