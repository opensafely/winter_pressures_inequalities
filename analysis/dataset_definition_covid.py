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
  parents,
  ethnicity_from_sus
)

import codelists

from variable_lib import (
  emergency_care_diagnosis_matches,
  hospitalisation_primary_secondary_diagnosis_matches,
  hospital_events
)

dataset = create_dataset()

start_date = get_parameter("start_date")

# Index date is latest of study start, 3 months registration, no admission for COVID for one year
#  ---------------------- Inclusion criteria --------------------------------

# Registered throughout the interval period (vs at the begining)
was_registered = practice_registrations.spanning((start_date - days(90)), start_date).exists_for_patient()

# no admission for covid in year prior
prior_secondary_date = (
      apcs.sort_by(apcs.admission_date)
      .where(apcs.primary_diagnosis
      .is_in(codelists.covid_secondary_codelist) 
      |apcs.secondary_diagnosis
      .is_in(codelists.covid_secondary_codelist))
      .where(apcs.admission_date.is_on_or_between(
      (start_date - days(365)), (start_date))).last_for_patient()
      .admission_date
    )
prior_emergency_date = emergency_care_diagnosis_matches(codelists.covid_attendance).where(
       emergency_care_attendances.arrival_date
        .is_on_or_between((start_date - days(365)), (start_date))).arrival_date.maximum_for_patient()
has_prior_admission = prior_secondary_date.is_not_null() | prior_emergency_date.is_not_null()


# Age 0 - 110 (as per WP2)
age_at_start = patients.age_on(start_date)
age_filter = (age_at_start >= 0) & (
    age_at_start <= 110)

# Alive throughout the interval period (vs. at the beginning)
was_alive = (
    patients.date_of_death.is_after(start_date) | 
    patients.date_of_death.is_null()
)

# No missing data: known sex, IMD, practice region (as per WP 2) 
is_female_or_male = patients.sex.is_in(["female", "male"])

dataset.define_population(
    was_alive
    & was_registered
    & is_female_or_male
    & age_filter
    & (~has_prior_admission))

dataset.sex = patients.sex
dataset.age = age_at_start

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