library(arrow)
library(survival)
library(mediation)
library(tidyverse)
library(broom)
library(here)

outdir <- here("output", "tables")
fs::dir_create(outdir)

# Which mediator to run - split out per-run via the yaml to keep memory
# down, since mediate()'s default 1000 simulations are computed over the
# whole (group vs White) subset for every one of the 36 group x mediator
# combinations otherwise, in a single R session.
args <- commandArgs(trailingOnly = TRUE)
mediator_arg <- args[1]

# use mediation package to do AFT model for COVID outcomes

data_covid <- read_feather(here("output/an_outcome_first_covid_2021.arrow"), as_data_frame = TRUE) 

#print(levels(data_covid$ethnicity6_combined))

# Mediators with a natural order are modelled with an ordered logistic
# mediator model (MASS::polr) rather than binomial glm, since glm(family =
# binomial) silently collapses any factor with >2 levels into a "reference
# vs everything else" binary, which mediate() then can't reconcile with the
# real multi-level mediator used in the outcome model.
ordinal_levels <- list(
  imd_quintile  = c("1", "2", "3", "4", "5"),
  urban_rural_5 = c("Urban major conurbation", "Urban minor conurbation", "Urban city and town",
                     "Rural town and fringe", "Rural village and dispersed"),
  bmi_category  = c("underweight", "ideal", "overweight", "obese")
)

# Function to run mediation models
run_mediation <- function(group, mediator) {
  data_df <- data_covid %>%
    # setting events on same day as follow up at half day
    mutate(stop_sc = if_else(stop_sc == 0, 0.5, stop_sc)) %>%
    filter(ethnicity6_combined %in% c(group, "White")) %>%
    droplevels()

  is_ordinal <- mediator %in% names(ordinal_levels)

  if (is_ordinal) {
    # Restrict to the well-defined ordered categories and set their order
    # (drops e.g. "Unknown"/"Missing"/"unknown or outlier")
    data_df <- data_df %>% filter(.data[[mediator]] %in% ordinal_levels[[mediator]])
    data_df[[mediator]] <- factor(data_df[[mediator]], levels = ordinal_levels[[mediator]], ordered = TRUE)
  } else if ("Unknown" %in% levels(factor(data_df[[mediator]]))) {
    # Binary mediator: drop the "Unknown" category so both the mediator and
    # outcome model treat it as a genuine 2-level factor
    data_df <- data_df %>% filter(.data[[mediator]] != "Unknown")
  }
  data_df <- droplevels(data_df)

  # Mediator model
  med_formula <- as.formula(paste0(mediator, " ~ ethnicity6_combined + age_group + sex"))
  med_fit <- if (is_ordinal) {
    MASS::polr(med_formula, data = data_df, Hess = TRUE)
  } else {
    glm(med_formula, data = data_df, family = binomial(link = "logit"))
  }
  print(summary(med_fit))
  # Outcome model
  out_formula <- as.formula(paste0("Surv(stop_sc, sc_binary) ~ ethnicity6_combined + sex + age_group + ", mediator))
  out_fit <- survreg(out_formula, data = data_df, dist = "weibull", robust = TRUE)
  print(summary(out_fit))
  # Mediation
  med_out <- mediate(med_fit, out_fit, treat = "ethnicity6_combined", mediator = mediator,
                     treat.value = group, control.value = "White")
  print(summary(med_out))
  
  med_tidy <- data.frame(
    term    = c("ACME", "ADE", "Total Effect", "Prop. Mediated"),
    estimate = c(med_out$d.avg,  med_out$z.avg,  med_out$tau.coef, med_out$n.avg),
    conf.low = c(med_out$d.avg.ci[1], med_out$z.avg.ci[1], med_out$tau.ci[1], med_out$n.avg.ci[1]),
    conf.high = c(med_out$d.avg.ci[2], med_out$z.avg.ci[2], med_out$tau.ci[2], med_out$n.avg.ci[2]),
    p.value  = c(med_out$d.avg.p, med_out$z.avg.p, med_out$tau.p, med_out$n.avg.p),
    group    = group,
    mediator = mediator
  )
  
  return(med_tidy)
  
}

group_vars <- c("Mixed", "Asian or Asian British", "Black or Black British", "Other Ethnic Groups")
mediation_vars <- c("migrant", "english_language", "imd_quintile", "urban_rural_5", "vacc_elig_covid", "vacc_elig_flu", "prior_flu_vacc_bin", "prior_covid_vacc_bin", "bmi_category")

param_grid <- expand.grid(
  group = group_vars, 
  mediator = mediation_vars,
  stringsAsFactors = FALSE 
)
print(param_grid)

mediation_outputs <- pmap(
  param_grid,
  function(group, mediator) run_mediation(group, mediator)
)

all_results <- bind_rows(mediation_outputs)
write.csv(all_results, file.path(outdir, paste0("mediation_covid_aft_", mediator_arg,".csv")), row.names = FALSE)

# Run e-value method in COVID cohort (https://arxiv.org/pdf/2411.17533)
# data_df <- data_covid %>% 
#     # setting events on same day as follow up at half day
#     mutate(stop_sc = if_else(stop_sc == 0, 0.5, stop_sc)) %>% 
#     filter(ethnicity6_combined %in% c("Mixed", "White")) %>%
#     droplevels()

# med_fit <- glm(migrant ~ ethnicity6_combined + age_group + sex, data = data_df, family = binomial(link = "logit"))
# km_fit <- survfit(Surv(stop_sc,sc_binary) ~ 1, data = data_covid) # Kaplan-Meier Estimator 
# # Fit pseudo values. Can fit targeting survival probability or RMST - chose RMST as gives absolute tome gained/lost
# # set times as 730 = two years 
# data_df$pseudo <- pseudo(fit = km_fit,times = 730 ,type = "rmst")
# # Now fit linear mediation model
# out_fit_pseudo <- lm(pseudo ~ ethnicity6_combined + sex + age_group + migrant, data = data_df)

# med_out_pseudo <- mediate(med_fit, out_fit_pseudo, treat = "ethnicity6_combined", mediator = "migrant",
#                     treat.value = "Mixed", control.value = "White")