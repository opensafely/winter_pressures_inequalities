from pathlib import Path 

from datetime import date, datetime
from ehrql import Dataset, create_dataset, case, when, maximum_of, minimum_of, years, days
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

covid_secondary =  dataset.covid_secondary_date = (
      apcs.sort_by(apcs.admission_date)
      .where(apcs.primary_diagnosis
      .is_in(codelists.covid_secondary_codelist) 
      |apcs.secondary_diagnosis
      .is_in(codelists.covid_secondary_codelist))
      .where(apcs.admission_date.is_on_or_between(
      study_start, end_date)).first_for_patient()
      .admission_date
    )

covid_emergency = emergency_care_diagnosis_matches(codelists.covid_primary_codelist).where(
        emergency_care_diagnosis_matches.date.is_on_or_after(study_start)
).sort_by(
        emergency_care_diagnosis_matches.date
).first_for_patient().date

covid_hosp_date = minimum_of(covid_secondary, covid_emergency),
    
index_date = covid_hospitalisation.date

#  ---------------------- Inclusion criteria --------------------------------

# Age 0 - 110 (as per WP2)
age_at_start = patients.age_on(index_date)
age_filter = (age_at_interval_start >= 0) & (
    age_at_interval_start <= 110)

# Alive throughout the interval period (vs. at the beginning)
was_alive = (
    patients.date_of_death.is_after(index_date) | 
    patients.date_of_death.is_null()
)

# Registered throughout the interval period and 90 days before
was_registered = practice_registrations.spanning((index_date - days(90)), index_date).exists_for_patient()

# No missing data: known sex, IMD, practice region (as per WP 2) 
was_female_or_male = patients.sex.is_in(["female", "male"])
has_deprivation_index = addresses.for_patient_on(index_date).imd_rounded.is_not_null()
has_region = practice_registrations.for_patient_on(index_date).practice_nuts1_region_name.is_not_null()



dataset.define_population(has_registration)

dataset.sex = patients.sex
dataset.age = patients.age_on(index_date)
