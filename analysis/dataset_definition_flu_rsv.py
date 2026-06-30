from pathlib import Path 

from datetime import date, datetime
from ehrql import Dataset, create_dataset, case, when, minimum_of, years, days, months, get_parameter, claim_permissions
from ehrql.tables.tpp import (
  patients,
  medications,
  ons_deaths,
  addresses,
  clinical_events,
  practice_registrations,
  vaccinations,
  appointments,
  emergency_care_attendances,
  apcs,
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
  has_prior_meds,
  is_gp_event,
  emergency_events,
  first_gp_event_with_washout
)

from clinical_risk_grps import *
from vaccine_history import *
claim_permissions("appointments")

start_date = date.fromisoformat(get_parameter("start_date"))
dataset = create_dataset()
dataset.configure_dummy_data(population_size=10000, additional_population_constraint = (((practice_registrations.for_patient_on(start_date).end_date)>(start_date)) | (practice_registrations.for_patient_on(start_date).end_date.is_null())) & (((ons_deaths.date)>(start_date)) | (ons_deaths.date.is_null())))

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

# known sex
has_known_sex = patients.sex.is_in(["female", "male"])

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
dataset.ethnicity6_gp = (
   clinical_events.where(clinical_events.snomedct_code.is_in(codelists.ethnicity6_codes))
    .where(clinical_events.date.is_on_or_before(start_date))
    .sort_by(clinical_events.date)
    .last_for_patient()
    .snomedct_code.to_category(codelists.ethnicity6_codes)
)

dataset.ethnicity16_gp = (clinical_events.where(clinical_events.snomedct_code.is_in(codelists.ethnicity16_codes))
    .where(clinical_events.date.is_on_or_before(start_date))
    .sort_by(clinical_events.date)
    .last_for_patient()
    .snomedct_code.to_category(codelists.ethnicity16_codes)
)

dataset.ethnicity_sus = ethnicity_from_sus.code

# COVARIATES
# demographics
dataset.sex = patients.sex
dataset.age = age_at_start
dataset.age_group = case(
    when((dataset.age >= 0) & (dataset.age < 5)).then("preschool"),
    when((dataset.age >= 5) & (dataset.age < 18)).then("school"),
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
# in 2021 no column created as all FALSE

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
# Need vaccinations for year of follow-up so start at end of period then look back for vaccinations both during follow-up and year prior.
#extract flu vaccination in previous season
dataset.prior_flu_vaccination = (
vaccinations.where(vaccinations.target_disease.is_in(["INFLUENZA"]))
.sort_by(vaccinations.date)
.where(vaccinations.date.is_on_or_between(start_date - years(1), start_date - days(1)))
.last_for_patient().date
)

#extract flu vaccination in current season
dataset.flu_vaccination_date = (
vaccinations.where(vaccinations.target_disease.is_in(["INFLUENZA"]))
.sort_by(vaccinations.date)
.where(vaccinations.date.is_on_or_between(start_date, start_date + years(1)))
.first_for_patient().date
)

#extract covid vaccination in previous season if applicable
dataset.prior_covid_vaccination = (
    vaccinations.where(vaccinations.target_disease.is_in(["SARS-2 Coronavirus"]))
    .sort_by(vaccinations.date)
    .where(vaccinations.date.is_on_or_between(start_date - years(1), start_date - days(1)))
    .last_for_patient().date
)

#extract covid vaccination in current season if applicable
dataset.covid_vaccination_date = (
    vaccinations.where(vaccinations.target_disease.is_in(["SARS-2 Coronavirus"]))
    .sort_by(vaccinations.date)
    .where(vaccinations.date.is_on_or_between(start_date, start_date + years(1)))
    .first_for_patient().date
)

#extract RSV vaccination in 2024
if start_date.year==2024 :
    dataset.rsv_vaccination = (
        vaccinations.where(vaccinations.target_disease.is_in(["HUMAN RESPIRATORY SYNCYTIAL VIRUS"]))
        .sort_by(vaccinations.date)
        .where(vaccinations.date.is_on_or_between(start_date, start_date + years(1)))
        .last_for_patient().date
    )

# BMI - part of risk groups so defining severe obesity - over 18's only
dataset.severe_obesity = has_severe_obesity(start_date) 
## Latest BMI
bmi_record = clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.bmi)
    ).where(
        clinical_events.date >= (patients.date_of_birth + years(16))
    ).where(
        (clinical_events.date >= (start_date - years(2))) & (clinical_events.date <= start_date)
    ).sort_by(
        clinical_events.date
    ).last_for_patient()

dataset.bmi = bmi_record.numeric_value
dataset.bmi_date = bmi_record.date

# Migrant status
migrant_status = has_prior_event(codelists.migrant_codelist, start_date)
entry_uk = has_prior_event(codelists.entry_uk_codelist, start_date)
born_in_uk = has_prior_event(codelists.born_in_uk_codelist, start_date)
british_ethnicities = has_prior_event(codelists.british_ethnicities_codelist, start_date)

dataset.migrant = case(
   when(migrant_status).then("Migrant"),
   when(entry_uk).then("Non-migrant"),
   when(british_ethnicities).then("Non-migrant"),
   otherwise="Unknown"
)

# English language proficiency
english_spoken = has_prior_event(codelists.english_language_codelist, start_date)
interpreter = has_prior_event(codelists.interpreter_codelist, start_date)

dataset.english_language = case(
   when(english_spoken).then(True),
   when(interpreter).then(True),
   otherwise=False
)

# Define outcomes 
# Flu outcome
# first_gp_event function takes first event in the year after start date
dataset.flu_primary_date = (
      first_gp_event_with_washout(codelists.flu_primary_codelist)
    )
#extract date of second episode
# is_gp_event function identifies any clinical events in year after start date
dataset.flu_primary_second_date = (
    is_gp_event(codelists.flu_primary_codelist)
    .where(gp_events.date.is_on_or_after(dataset.flu_primary_date + months(1)))
    .sort_by(gp_events.date).first_for_patient().date
)

#extract date of third episode
dataset.flu_primary_third_date = (
    is_gp_event(codelists.flu_primary_codelist)
    .where(gp_events.date.is_on_or_after(dataset.flu_primary_second_date + months(1)))
    .sort_by(gp_events.date).first_for_patient().date
)

# Flu sensitive phenotype
gp_flu_sens_date = first_gp_event_with_washout(codelists.flu_primary_sens_codelist)
gp_flu_sens_excl_prior = (clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists.flu_primary_sens_excl_codelist)
    ).where(
    clinical_events.date.is_on_or_between((gp_flu_sens_date - days(28)),gp_flu_sens_date - days(1))
    ).exists_for_patient()
)

dataset.flu_primary_sens_date = case(
        when(dataset.flu_primary_date.is_not_null()).then(dataset.flu_primary_date),
        when((gp_flu_sens_date.is_not_null()) & (gp_flu_sens_excl_prior==False)).then(gp_flu_sens_date),
    otherwise = None,
    )

# hospital events function takes hospital admissions in the year after start date
# hospitalisation_primary_secondary_diagnosis_matches function takes events which are primary or secondarty diagnoses
flu_sc = (hospitalisation_primary_secondary_diagnosis_matches(codelists.flu_secondary_codelist)
        ).admission_date.minimum_for_patient()
prior_sc = apcs.where(
   (apcs.primary_diagnosis.is_in(codelists.flu_secondary_codelist)) | (apcs.secondary_diagnosis.is_in(codelists.flu_secondary_codelist))).where(
      apcs.admission_date.is_on_or_between((flu_sc - days(28)), (flu_sc - days(1)))).admission_date.minimum_for_patient()


# Date of first hospitalisation based on 28 days after prior hospitalisation
prior_sc_first = apcs.where(
   (apcs.primary_diagnosis.is_in(codelists.flu_secondary_codelist)) | (apcs.secondary_diagnosis.is_in(codelists.flu_secondary_codelist))).where(
      apcs.admission_date.is_on_or_between((prior_sc + days(28)), (start_date + years(1)))).admission_date.minimum_for_patient()

# Date of first hospitalisation based on start date
dataset.flu_sc_date = case(
        when((flu_sc.is_not_null()) & (prior_sc.is_null())).then(flu_sc),
        when(flu_sc.is_not_null() & (prior_sc.is_not_null()) & (prior_sc_first.is_not_null())).then(prior_sc_first),
        otherwise = None,
)
#extract date of second episode - using the same criteria as the first episode
dataset.flu_sc_second_date = (
    hospitalisation_primary_secondary_diagnosis_matches(codelists.flu_secondary_codelist)
    .where(hospital_events.admission_date.is_on_or_after(dataset.flu_sc_date + months(1)))
    .admission_date.minimum_for_patient()
)
# emergency_events function takes A & E attendances in year after start date
# emergency_care_diagnosis_matches function identifies A & E attendance diagnoses in the first 3 positions
flu_emerg = emergency_care_diagnosis_matches(codelists.flu_attendance).where(
       emergency_care_attendances.arrival_date
        .is_on_or_between(start_date, start_date + years(1))).arrival_date.minimum_for_patient()
 # Any attendance in 28 days prior?
flu_emerg_prior =(emergency_care_diagnosis_matches(codelists.flu_attendance).where(
        emergency_care_attendances.arrival_date
    .is_on_or_between((flu_emerg - days(28)), flu_emerg - days(1))).arrival_date.minimum_for_patient()
    ) 
# Any attendance after prior admission within interval?
flu_emerg_first_prior =(emergency_care_diagnosis_matches(codelists.flu_attendance).where(
        emergency_care_attendances.arrival_date.is_on_or_between((flu_emerg_prior + days(28)), start_date + years(1))).arrival_date.minimum_for_patient()
    ) 
dataset.flu_emerg_date = case(
    when((flu_emerg.is_not_null()) & (flu_emerg_prior.is_null())).then(flu_emerg),
    when((flu_emerg.is_not_null()) & (flu_emerg_prior.is_not_null()) & (flu_emerg_first_prior.is_not_null())).then(flu_emerg_first_prior),
    otherwise = None,
)
# extract second emergency care date
dataset.flu_emerg_second_date = emergency_care_diagnosis_matches(codelists.flu_attendance).where(
       emergency_events.arrival_date
        .is_on_or_between(dataset.flu_emerg_date + months(1), start_date + years(1))).arrival_date.minimum_for_patient()

dataset.has_flu_admission = dataset.flu_sc_date.is_not_null() | dataset.flu_emerg_date.is_not_null()
dataset.flu_admission_date = minimum_of(dataset.flu_sc_date, dataset.flu_emerg_date)
dataset.has_flu_second_admission = dataset.flu_sc_second_date.is_not_null() | dataset.flu_emerg_second_date.is_not_null()
dataset.flu_second_admission_date = minimum_of(dataset.flu_sc_second_date, dataset.flu_emerg_second_date)

# RSV outcome 
bronchiolitis_emerg = emergency_care_diagnosis_matches(codelists.bronchiolitis_attendance).where(
       emergency_care_attendances.arrival_date
        .is_on_or_between(start_date, start_date + years(1))).arrival_date.minimum_for_patient()
 # Any attendance in 28 days prior?
bronchiolitis_emerg_prior =(emergency_care_diagnosis_matches(codelists.bronchiolitis_attendance).where(
        emergency_care_attendances.arrival_date
    .is_on_or_between((bronchiolitis_emerg - days(28)), bronchiolitis_emerg - days(1))).arrival_date.minimum_for_patient()
    ) 
# Any attendance after prior admission within interval?
bronchiolitis_emerg_first_prior =(emergency_care_diagnosis_matches(codelists.bronchiolitis_attendance).where(
        emergency_care_attendances.arrival_date.is_on_or_between((bronchiolitis_emerg_prior + days(28)), start_date + years(1))).arrival_date.minimum_for_patient()
    ) 
bronchiolitis_emerg_date = case(
    when((bronchiolitis_emerg.is_not_null()) & (bronchiolitis_emerg_prior.is_null())).then(bronchiolitis_emerg),
    when((bronchiolitis_emerg.is_not_null()) & (bronchiolitis_emerg_prior.is_not_null()) & (bronchiolitis_emerg_first_prior.is_not_null())).then(bronchiolitis_emerg_first_prior),
    otherwise = None,
)

dataset.rsv_primary_date = case(
    when(dataset.age <= 2).then(
        minimum_of((first_gp_event_with_washout(codelists.rsv_primary_codelist)),(bronchiolitis_emerg_date))),
    when(dataset.age > 2).then(
        first_gp_event_with_washout(codelists.rsv_primary_codelist)
    )
)
#extract date of second episode - using the same criteria
# Find second emergency admission
dataset.rsv_primary_second_date = case(
    when(dataset.age <= 2).then(
        minimum_of(
            (is_gp_event(codelists.rsv_primary_codelist)
                .where(gp_events.date.is_on_or_after(dataset.rsv_primary_date + months(1)))
                .sort_by(gp_events.date).first_for_patient().date),
            (emergency_care_diagnosis_matches(codelists.bronchiolitis_attendance)
                .where(emergency_events.arrival_date.is_on_or_after(dataset.rsv_primary_date + months(1)))
                .arrival_date.minimum_for_patient())
        )
    ),
    when(dataset.age > 2).then(
        is_gp_event(codelists.rsv_primary_codelist)
            .where(gp_events.date.is_on_or_after(dataset.rsv_primary_date + months(1)))
            .sort_by(gp_events.date).first_for_patient().date
        ),
)

# sensitivie phenotype
gp_rsv_sens_date = first_gp_event_with_washout(codelists.rsv_primary_sens_codelist)
gp_rsv_sens_excl_prior = (clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists.rsv_primary_sens_excl_codelist)
    ).where(
    clinical_events.date.is_on_or_between((gp_rsv_sens_date - days(28)),gp_rsv_sens_date - days(1))
    ).exists_for_patient()
)

dataset.rsv_primary_sens_date = case(
        when(dataset.rsv_primary_date.is_not_null()).then(dataset.rsv_primary_date),
        when((gp_rsv_sens_date.is_not_null()) & (gp_rsv_sens_excl_prior==False)).then(gp_rsv_sens_date),
    otherwise = None,
    )

#extract rsv secondary care dates for 'specific' phenotype
  
#extract date of first episode - looking at the first date for which there is
#a code in the RSV secondary codelist as the primary or secondary diagnosis
rsv_sc = (hospitalisation_primary_secondary_diagnosis_matches(codelists.rsv_secondary_codelist)
        ).admission_date.minimum_for_patient()
prior_sc = apcs.where(
   (apcs.primary_diagnosis.is_in(codelists.rsv_secondary_codelist)) | (apcs.secondary_diagnosis.is_in(codelists.rsv_secondary_codelist))).where(
      apcs.admission_date.is_on_or_between((rsv_sc - days(28)), (rsv_sc - days(1)))).admission_date.minimum_for_patient()


# Date of first hospitalisation based on 28 days after prior hospitalisation
prior_sc_first = apcs.where(
   (apcs.primary_diagnosis.is_in(codelists.rsv_secondary_codelist)) | (apcs.secondary_diagnosis.is_in(codelists.rsv_secondary_codelist))).where(
      apcs.admission_date.is_on_or_between((prior_sc + days(28)), (start_date + years(1)))).admission_date.minimum_for_patient()

# Date of first hospitalisation based on start date
dataset.rsv_sc_date = case(
        when((rsv_sc.is_not_null()) & (prior_sc.is_null())).then(rsv_sc),
        when(rsv_sc.is_not_null() & (prior_sc.is_not_null()) & (prior_sc_first.is_not_null())).then(prior_sc_first),
        otherwise = None,
)

#extract date of second episode - using the same criteria as the first episode
dataset.rsv_sc_second_date = (
hospitalisation_primary_secondary_diagnosis_matches(codelists.rsv_secondary_codelist)
.where(hospital_events.admission_date.is_on_or_after(dataset.rsv_sc_date + months(1)))
.admission_date.minimum_for_patient()
)

# emergency_events function takes A & E attendances in year after start date
# emergency_care_diagnosis_matches function identifies A & E attendance diagnoses in the first 3 positions
rsv_emerg = emergency_care_diagnosis_matches(codelists.rsv_attendance).where(
       emergency_care_attendances.arrival_date
        .is_on_or_between(start_date, start_date + years(1))).arrival_date.minimum_for_patient()
 # Any attendance in 28 days prior?
rsv_emerg_prior =(emergency_care_diagnosis_matches(codelists.rsv_attendance).where(
        emergency_care_attendances.arrival_date
    .is_on_or_between((rsv_emerg - days(28)), (rsv_emerg - days(1)))).arrival_date.minimum_for_patient()
    ) 
# Any attendance after prior admission within interval?
rsv_emerg_first_prior =(emergency_care_diagnosis_matches(codelists.rsv_attendance).where(
        emergency_care_attendances.arrival_date.is_on_or_between((rsv_emerg_prior + days(28)), start_date + years(1))).arrival_date.minimum_for_patient()
    ) 
dataset.rsv_emerg_date = case(
    when((rsv_emerg.is_not_null()) & (rsv_emerg_prior.is_null())).then(rsv_emerg),
    when((rsv_emerg.is_not_null()) & (rsv_emerg_prior.is_not_null()) & (rsv_emerg_first_prior.is_not_null())).then(rsv_emerg_first_prior),
    otherwise = None,
)

# extract second emergency care date
dataset.rsv_emerg_second_date = emergency_care_diagnosis_matches(codelists.rsv_attendance).where(
       emergency_events.arrival_date
        .is_on_or_between(dataset.rsv_emerg_date + months(1), start_date + years(1))).arrival_date.minimum_for_patient()
