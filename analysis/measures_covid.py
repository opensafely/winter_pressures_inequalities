#################################################################
# This script creates monthly counts/rates of primary, secndary and emergency
# care healthcare utilisation for covid
#
# 
#####################################################################

from ehrql import months, INTERVAL, create_measures
from ehrql.tables.tpp import (
    patients, 
    practice_registrations)

import codelists

##########

from argparse import ArgumentParser

parser = ArgumentParser()
parser.add_argument("--start-date", type=str)
parser.add_argument("--intervals", type=int)

args = parser.parse_args()

start_date = args.start_date
intervals = args.intervals

##########

index_date = INTERVAL.start_date

from dataset_definition_measures import make_dataset_covid_utilisation

dataset = make_dataset_covid_utilisation(index_date=INTERVAL.start_date, end_date=INTERVAL.end_date)

##########

measures = create_measures()
measures.configure_disclosure_control(enabled=False)

measures.define_defaults(intervals=months(intervals).starting_on(start_date))

measures.configure_dummy_data(population_size=5000)

## In full population
denominator = (
        (patients.age_on(index_date) >= 0) 
        & (patients.age_on(index_date) < 110)
        & ((patients.sex == "male") | (patients.sex == "female"))
        & (patients.date_of_death.is_after(index_date) | patients.date_of_death.is_null())
        & (practice_registrations.for_patient_on(index_date).exists_for_patient())
    )

# By ethnicity
# GP consultation - specific
measures.define_measure(
    name="covid_gp",
    numerator=dataset.covid_gp,
    denominator=denominator,
    group_by={"ethnicity": dataset.ethnicity6},
)

# COVID code and GP appointment in same interval
measures.define_measure(
    name="covid_gp_appt",
    numerator=dataset.covid_gp_appt,
    denominator=denominator,
    group_by={"ethnicity": dataset.ethnicity6}, 
)

# COVID code that is not a test
measures.define_measure(
    name="covid_gp_notest",
    numerator=dataset.covid_gp_notest,
    denominator=denominator,
    group_by={"ethnicity": dataset.ethnicity6},
)

# COVID - sensitive
measures.define_measure(
    name="covid_gp_sens",
    numerator=dataset.covid_gp_sens,
    denominator=denominator,
    group_by={"ethnicity": dataset.ethnicity6},
)

# Hospital admission
measures.define_measure(
    name="covid_admission",
    numerator=dataset.covid_sc,
    denominator=denominator,
    group_by= {"ethnicity": dataset.ethnicity6},
)
# Emergency care attendance
measures.define_measure(
    name="covid_emerg",
    numerator=dataset.covid_emerg,
    denominator=denominator,
    group_by= {"ethnicity": dataset.ethnicity6},
)

# By IMD
# GP consultation
measures.define_measure(
    name="covid_gp_imd",
    numerator=dataset.covid_gp,
    denominator=denominator,
    group_by={"imd": dataset.imd_quintile},
)
# COVID code and GP appointment in same interval
measures.define_measure(
    name="covid_gp_appt_imd",
    numerator=dataset.covid_gp_appt,
    denominator=denominator,
    group_by={ "imd": dataset.imd_quintile},
)

# COVID code that is not a test
measures.define_measure(
    name="covid_gp_notest_imd",
    numerator=dataset.covid_gp_notest,
    denominator=denominator,
    group_by={ "imd": dataset.imd_quintile}, 
)

# COVID -sensitive
measures.define_measure(
    name="covid_gp_sens_imd",
    numerator=dataset.covid_gp_sens,
    denominator=denominator,
    group_by={ "imd": dataset.imd_quintile}, 
)

# Hospital admission
measures.define_measure(
    name="covid_admission_imd",
    numerator=dataset.covid_sc,
    denominator=denominator,
    group_by= {"imd": dataset.imd_quintile},
)
# Emergency care attendance
measures.define_measure(
    name="covid_emerg_imd",
    numerator=dataset.covid_emerg,
    denominator=denominator,
    group_by= {"imd": dataset.imd_quintile}, 
)