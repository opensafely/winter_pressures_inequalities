from pathlib import Path 

from datetime import date, datetime
from ehrql import Dataset, create_dataset, case, when, maximum_of, minimum_of, years, days, show, get_parameter
from ehrql.tables.tpp import (
  patients,
  medications,
  ons_deaths,
  addresses,
  clinical_events,
  practice_registrations,
  vaccinations,
  apcs,
  emergency_care_attendances,
  ethnicity_from_sus
)

import codelists

from variable_lib import (
  emergency_care_diagnosis_matches,
  hospitalisation_primary_secondary_diagnosis_matches,
  hospital_events,
  get_latest_ethnicity,
  gp_events,
  first_gp_event,
  rural_urban_5,
  has_prior_event,
  has_prior_meds
)

from clinical_risk_grps import *
from vaccine_history import *

dataset = create_dataset()
dataset.configure_dummy_data(population_size=100)
start_date = date.fromisoformat(get_parameter("start_date"))

#  ---------------------- Inclusion criteria --------------------------------
# assessed each year
# Age 0 - 110 (as per WP2)
age_at_start = patients.age_on(start_date)
age_filter = (age_at_start >= 0) & (
    age_at_start <= 110)

# Registered at start date
# Create dataset that can be applied for all three infections
# if age <1 change inclusion
was_registered = case(
    when((age_at_start < 1)).then(
        practice_registrations.where(practice_registrations.start_date.is_on_or_before(start_date)).exists_for_patient()
    ),
    when((age_at_start >= 1)).then(
        practice_registrations.spanning_with_systmone(start_date - days(90), start_date).exists_for_patient()
    )
)

# Alive at start date
was_alive = ((patients.date_of_death.is_after(start_date) | patients.date_of_death.is_null())
)

# No missing data: known sex, IMD, practice region (as per WP 2) 
has_known_sex = patients.sex.is_in(["female", "male", "intersex"])

dataset.define_population(
    was_alive
    & was_registered
    & has_known_sex
    & age_filter
    )

# Information for follow-up 
dataset.dereg_date = (
  (practice_registrations.for_patient_on(start_date)).end_date
)
dataset.ons_death_date = ons_deaths.date

# EXPOSURE
dataset.ethnicity16 = get_latest_ethnicity(
    start_date,
    ethnicity_from_sus,
    ethnicity_codelist=codelists.ethnicity16_codes,
    grouping=16,
)
dataset.ethnicity6 = get_latest_ethnicity(
    start_date,
    ethnicity_from_sus,
    ethnicity_codelist=codelists.ethnicity6_codes,
    grouping=6,
)
# COVARIATES
# demographics
dataset.sex = patients.sex
dataset.age = age_at_start
dataset.age_group = case(
    when((dataset.age >= 0) & (dataset.age < 5)).then("preschool"),
    when((dataset.age >= 5) & (dataset.age < 17)).then("school"),
    when((dataset.age >= 18) & (dataset.age < 65)).then("adult_under_65"),
    when((dataset.age >= 65) & (dataset.age < 75)).then("adult_under_75"),
    when((dataset.age >= 75) & (dataset.age < 80)).then("adult_under_80"),
    when((dataset.age >= 80) & (dataset.age < 111)).then("adult_80+")
)
dataset.urban_rural_5 = rural_urban_5(start_date)


imd_rounded = addresses.for_patient_on(start_date).imd_rounded
max_imd = 32844
 
dataset.imd_quintile = case(
    when((imd_rounded >= 0) & (imd_rounded <= int(max_imd * 1 / 5))).then(1),
    when(imd_rounded <= int(max_imd * 2 / 5)).then(2),
    when(imd_rounded <= int(max_imd * 3 / 5)).then(3),
    when(imd_rounded <= int(max_imd * 4 / 5)).then(4),
    when(imd_rounded <= max_imd).then(5),
    otherwise = 99
    )

# Comorbidity - add cms once finalised by Zoe and Shrinkhala

#******** High risk vaccination group for COVID at start date - code from comparative spring booster 2023 
# Clinical risk groups stay the same but age restrictions change through follow-up
# Will be assessed at start_date and determine if eligible at that point
dataset.covid_atrisk_grp = covid_atrisk(start_date) # in clinical risk group
dataset.flu_atrisk_grp = flu_atrisk(start_date) 
# define eligibility based on age as well
if start_date.year==2021 :
   dataset.vacc_elig_covid = False,

if start_date.year==2022 :
   dataset.vacc_elig_covid = case(
       when((dataset.age>= 16) | ((dataset.age>12) & (dataset.age< 16) & (dataset.covid_atrisk_grp==True))).then(True),
       otherwise=False,
   )
if start_date.year==2023 :
   dataset.vacc_elig_covid = case(
       when((dataset.age>=50) | ((dataset.age >=5) & (dataset.age <50) & (dataset.covid_atrisk_grp==True))).then(True),
       otherwise = False,
   )
if start_date.year==2024 :
   dataset.vacc_elig_covid = case(
       when((dataset.age>=65) | ((dataset.age >=0) & (dataset.age <65) & (dataset.covid_atrisk_grp==True))).then(True),
       otherwise = False,
   )

# Flu eligibility
dataset.vacc_elig_flu = case(
    when((dataset.age>=65) | ((dataset.age >= 2) & (dataset.age < 17)) | ((dataset.age > 0) & (dataset.flu_atrisk_grp==True))).then(True),
    otherwise = False
)

# RSV eligibility - age 75 to 79 only. Not including pregnanacy currently.
dataset.vacc_elig_rsv = case(
    when((dataset.age >= 75) & (dataset.age < 80)).then(True),
    otherwise = False
)

# vaccination status for flu, COVID (season prior and current season) & RSV (ever)

add_vaccine_history(
    dataset = dataset, start_date = (start_date + days(364)), 
    target_disease = "SARS-2 Coronavirus", target_disease_short ="covid", 
    number_of_vaccines = 6
)

add_vaccine_history(
    dataset = dataset, start_date = (start_date + days(364)), 
    target_disease = "INFLUENZA", target_disease_short ="flu", 
    number_of_vaccines = 5
)

add_vaccine_history(
    dataset = dataset, start_date = (start_date + days(364)), 
    target_disease = "HUMAN RESPIRATORY SYNCYTIAL VIRUS", target_disease_short ="rsv", 
    number_of_vaccines = 3
)

# BMI - part of risk groups so defining severe obesity
dataset.severe_obesity = has_severe_obesity(start_date) 
# date of latest severe obesity measure
date_bmi_stage = last_prior_event(
        codelists.bmi_stage, 
        start_date
    ).date
    # Last severe obesity event
date_sev_obesity = last_prior_event(
        codelists.sev_obesity, 
        start_date
    ).date
event_bmi = last_prior_event(
        codelists.bmi, 
        start_date,
        where=(
            clinical_events.numeric_value.is_not_null() & 
            # Ignore out-of-range values
            (clinical_events.numeric_value > 4) & 
            (clinical_events.numeric_value < 200)
        )
    )
dataset.severe_obesity_date = case(
    when(dataset.severe_obesity == True).then(maximum_of(date_bmi_stage, date_sev_obesity, event_bmi.date)),
    otherwise = None
)

# Migrant status
dataset.migrant = has_prior_event(codelists.migrant_codelist, start_date)

# English language proficiency
dataset.english_language = has_prior_event(codelists.english_language_codelist, start_date)

# Define outcome - primary care or secondary care COVID
# extract date of first episode 
dataset.covid_primary_date = (
      first_gp_event(codelists.covid_primary_codelist).date
    )
dataset.covid_sc_date = (
    hospitalisation_primary_secondary_diagnosis_matches(codelists.covid_secondary_codelist)
    .where(hospital_events.admission_date.is_on_or_between(start_date, start_date + years(1)))
    .admission_date.minimum_for_patient()
  )

dataset.covid_emergency_date = emergency_care_diagnosis_matches(codelists.covid_attendance).where(
       emergency_care_attendances.arrival_date
        .is_on_or_between(start_date, start_date + years(1))).arrival_date.minimum_for_patient()

dataset.has_covid_admission = dataset.covid_sc_date.is_not_null() | dataset.covid_emergency_date.is_not_null()
dataset.covid_admission_date = minimum_of(dataset.covid_sc_date, dataset.covid_emergency_date)

# Flu outcome
dataset.flu_primary_date = (
      first_gp_event(codelists.flu_primary_codelist).date
    )
dataset.flu_sc_date = (
    hospitalisation_primary_secondary_diagnosis_matches(codelists.flu_secondary_codelist)
    .where(hospital_events.admission_date.is_on_or_between(start_date, start_date + years(1)))
    .admission_date.minimum_for_patient()
  )

dataset.flu_emergency_date = emergency_care_diagnosis_matches(codelists.flu_attendance).where(
      emergency_care_attendances.arrival_date
      .is_on_or_between(start_date, start_date + years(1))).arrival_date.minimum_for_patient()

dataset.has_flu_admission = dataset.flu_sc_date.is_not_null() | dataset.flu_emergency_date.is_not_null()
dataset.flu_admission_date = minimum_of(dataset.flu_sc_date, dataset.flu_emergency_date)

# RSV outcome - check these with Em's code
 #infant and infant subgroup cohorts
dataset.rsv_primary_date = case(
    when(dataset.age <= 2).then(
        minimum_of((first_gp_event(codelists.rsv_primary_codelist).date),(emergency_care_diagnosis_matches(codelists.bronchiolitis_attendance))
        .arrival_date.minimum_for_patient())
    ),
    when(dataset.age > 2).then(
        first_gp_event(codelists.rsv_primary_codelist).date
    )
)

#extract rsv secondary care dates for 'specific' phenotype
  
#extract date of first episode - looking at the first date for which there is
#a code in the RSV secondary codelist as the primary or secondary diagnosis
dataset.rsv_secondary_date = (
hospitalisation_primary_secondary_diagnosis_matches(codelists.rsv_secondary_codelist)
.admission_date.minimum_for_patient()
)