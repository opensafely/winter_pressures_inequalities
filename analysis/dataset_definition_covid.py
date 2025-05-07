from pathlib import Path 

from datetime import date, datetime
from ehrql import Dataset, create_dataset, case, when, maximum_of, minimum_of, years, days, show
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
  parents,
  ethnicity_from_sus
)

import codelists

from variable_lib import (
  emergency_care_diagnosis_matches
)

dataset = create_dataset()

study_start = "2020-03-01"
end_date = "2025-04-01"

# Study population is people with an infection event so identify whether these in either APC admission or emergency admission for each of COVID, RSV and flu

covid_secondary_date = (
      apcs.sort_by(apcs.admission_date)
      .where(apcs.primary_diagnosis
      .is_in(codelists.covid_secondary_codelist) 
      |apcs.secondary_diagnosis
      .is_in(codelists.covid_secondary_codelist))
      .where(apcs.admission_date.is_on_or_between(
      study_start, end_date)).first_for_patient()
      .admission_date
    )

covid_emergency_date = emergency_care_diagnosis_matches(codelists.covid_attendance).where(
       emergency_care_attendances.arrival_date
        .is_on_or_between(study_start, end_date)).arrival_date.minimum_for_patient()

has_covid_admission = covid_secondary_date.is_not_null() | covid_emergency_date.is_not_null()
index_date = minimum_of(covid_secondary_date, covid_emergency_date)

#  ---------------------- Inclusion criteria --------------------------------
# no admission for covid in year prior
prior_secondary_date = (
      apcs.sort_by(apcs.admission_date)
      .where(apcs.primary_diagnosis
      .is_in(codelists.covid_secondary_codelist) 
      |apcs.secondary_diagnosis
      .is_in(codelists.covid_secondary_codelist))
      .where(apcs.admission_date.is_on_or_between(
      (index_date - days(365)), (index_date - days(1)))).last_for_patient()
      .admission_date
    )
prior_emergency_date = emergency_care_diagnosis_matches(codelists.covid_attendance).where(
       emergency_care_attendances.arrival_date
        .is_on_or_between((index_date - days(365)), (index_date - days(1)))).arrival_date.maximum_for_patient()
has_prior_admission = prior_secondary_date.is_not_null() | prior_emergency_date.is_not_null()

# Age 0 - 110 (as per WP2)
age_at_start = patients.age_on(index_date)
age_filter = (age_at_start >= 0) & (
    age_at_start <= 110)

# Alive throughout the interval period (vs. at the beginning)
was_alive = (
    patients.date_of_death.is_after(index_date) | 
    patients.date_of_death.is_null()
)

# Registered throughout the interval period and 90 days before
was_registered = practice_registrations.spanning((index_date - days(90)), index_date).exists_for_patient()

# No missing data: known sex, IMD, practice region (as per WP 2) 
is_female_or_male = patients.sex.is_in(["female", "male"])

dataset.define_population(
    has_covid_admission
    & was_alive
    & was_registered
    & is_female_or_male
    & age_filter
    & (~has_prior_admission))

dataset.sex = patients.sex
dataset.age = patients.age_on(index_date)
