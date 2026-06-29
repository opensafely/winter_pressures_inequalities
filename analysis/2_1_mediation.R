library(here)
library(readr)
library(dplyr)
library(arrow)
library(survival)
library(mediation)

# Need to generate code that may not run on dummy data but can try on real data
# test with single subgroup
data_df <- read_feather(here("output/an_outcome_first_covid_2021.arrow"), as_data_frame = TRUE)


#data_outcome_covid <- droplevels(data_outcome_covid$migrant)
med_fit <- glm(migrant ~ ethnicity6_combined + age_group, 
               data = data_outcome_covid,
               family = binomial(link = "logit"))
out_fit <- out_model <- survreg(Surv(stop_gp, gp_binary) ~ ethnicity6_combined + 
                                  age_group + migrant,
                                data = data_outcome_covid,
                                dist = "weibull")  
med_out <- mediate(med_fit, out_fit, treat = "ethnicity6_combined", mediator = "migrant",
                   treat.value = "Asian or Asian British", control.value = "White", robustSE = TRUE)