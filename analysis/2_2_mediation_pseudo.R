library(arrow)
library(survival)
library(mediation)
library(tidyverse)
library(broom)
library(here)

outdir <- here("output", "tables")
fs::dir_create(outdir)

# use mediation package to do AFT model for COVID outcomes

data_covid <- read_feather(here("output/an_outcome_first_covid_2021.arrow"), as_data_frame = TRUE)

# Run e-value method in COVID cohort (https://arxiv.org/pdf/2411.17533)
data_df <- data_covid %>% 
    # setting events on same day as follow up at half day
    mutate(stop_sc = if_else(stop_sc == 0, 0.5, stop_sc)) %>% 
    filter(ethnicity6_combined %in% c("Mixed", "White")) %>%
    filter(migrant != 'Unknown') %>% 
    mutate(migrant = as.numeric(migrant == "Non-migrant")) %>%
    droplevels()


med_fit <- glm(migrant ~ ethnicity6_combined + age_group + sex, data = data_df, family = binomial(link = "logit"))
print(summary(med_fit))

km_fit <- survfit(Surv(stop_sc,sc_binary) ~ 1, data = data_df) # Kaplan-Meier Estimator
# Fit pseudo values. Can fit targeting survival probability or RMST - chose RMST as gives absolute tome gained/lost
# set times as 730 = two years
data_df$pseudo <- pseudo(fit = km_fit,times = 730 ,type = "rmst")

# Now fit linear mediation model
out_fit_pseudo <- lm(pseudo ~ ethnicity6_combined + sex + age_group + migrant, data = data_df)
print(summary(out_fit_pseudo))

med_out_pseudo <- mediate(med_fit, out_fit_pseudo, treat = "ethnicity6_combined", mediator = "migrant",
                           treat.value = "Mixed", control.value = "White")
print(summary(med_out_pseudo))

med_tidy <- data.frame(
  term      = c("ACME", "ADE", "Total Effect", "Prop. Mediated"),
  estimate  = c(med_out_pseudo$d.avg,     med_out_pseudo$z.avg,     med_out_pseudo$tau.coef, med_out_pseudo$n.avg),
  conf.low  = c(med_out_pseudo$d.avg.ci[1], med_out_pseudo$z.avg.ci[1], med_out_pseudo$tau.ci[1], med_out_pseudo$n.avg.ci[1]),
  conf.high = c(med_out_pseudo$d.avg.ci[2], med_out_pseudo$z.avg.ci[2], med_out_pseudo$tau.ci[2], med_out_pseudo$n.avg.ci[2]),
  p.value   = c(med_out_pseudo$d.avg.p,   med_out_pseudo$z.avg.p,   med_out_pseudo$tau.p,    med_out_pseudo$n.avg.p),
  group     = "Mixed",
  mediator  = "migrant"
)

write.csv(med_tidy, file.path(outdir, "mediation_covid_pseudo.csv"), row.names = FALSE)