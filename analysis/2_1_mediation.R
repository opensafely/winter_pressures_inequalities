library(here)
library(readr)
library(dplyr)
library(arrow)
library(mets)
library(survival)
library(numDeriv)  

# Mets package multinomial regression code works: https://cran.r-project.org/web/packages/mets/vignettes/mediation-survival.html
# Need to generate code that may not run on dummy data but can try on real data
# test with single subgroup
data_df <- read_feather(here("output/an_outcome_first_covid.arrow"), as_data_frame = TRUE)

data_df <- data_df %>% 
    mutate(
        ethnicity6_combined = as.factor(ethnicity6_combined),
        ethnicity16_combined = as.factor(ethnicity16_combined),
        imd_quintile = as.factor(imd_quintile),
        bmi_category = as.factor(bmi_category)
    )
# Keep only non missing values for mediation
data_df <- data_df %>% 
    filter(ethnicity6_combined != "Unknown" | imd_quintile != "Missing") %>%
    droplevels()

# Step 1 - fit weight model for mediator

weightmodel <- fit <- mets::mlogit(imd_quintile.5 ~ ethnicity6_combined.5 + age_group.6,
                      data = data_df, family = binomial)

# Step 2 - create weighted expanded data
wdata <- medweight(weightmodel, data = data_df, var = "ethnicity6_combined")

# Step 3 - fit binomial regression outcome model on weighted data
# note: ethnicity6_combined0 and ethnicity6_combined1 are created by medweight
out_model <- binreg(
        Event(stop_gp, gp_binary) ~ ethnicity6_combined0 + ethnicity6_combined1 + 
                                     age_group + cluster(patient_id),
        data = wdata,
        time = max(data_df$stop_gp, na.rm = TRUE),  # time point of interest
        weights = wdata$weights,
        cause = 1
)

# Step 4 - estimate direct and indirect effects
med_result <- mediatorSurv(out_model, fit, 
                            data = data_df, 
                            wdata = wdata)

print(summary(med_result))
write_csv(mediation, file.path(outdir,"mediation.csv"))