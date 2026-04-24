#################################################################
# This script creates monthly counts/rates of primary and secndary 
# care healthcare utilisation for Flu
#
# 
#####################################################################

from ehrql import months, INTERVAL, create_measures
from ehrql.tables.tpp import (
    patients, 
    practice_registrations)

import codelists

from dataset_definition_measures import make_dataset_flu_rsv_utilisation


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

dataset = make_dataset_flu_rsv_utilisation(index_date=INTERVAL.start_date, end_date=INTERVAL.end_date)

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
# RSV - ethnicity also stratified by age
measures.define_measure(
    name="rsv_gp_age",
    numerator=dataset.rsv_gp,
    denominator=denominator,
    group_by={ "ethnicity": dataset.ethnicity6,
              "age": dataset.age_infants},
)

measures.define_measure(
    name="rsv_gp_sens_age",
    numerator=dataset.rsv_gp_sens,
    denominator=denominator,
    group_by={ "ethnicity": dataset.ethnicity6,
              "age": dataset.age_infants},
)

measures.define_measure(
    name="rsv_admission_age",
    numerator=dataset.rsv_sc,
    denominator=denominator,
    group_by= {"ethnicity": dataset.ethnicity6,
              "age": dataset.age_infants},
)

measures.define_measure(
    name="rsv_emerg_age",
    numerator=dataset.rsv_emerg,
    denominator=denominator,
    group_by= {"ethnicity": dataset.ethnicity6,
              "age": dataset.age_infants},
)

# RSV - IMD also stratified by age
measures.define_measure(
    name="rsv_gp_imd_age",
    numerator=dataset.rsv_gp,
    denominator=denominator,
    group_by={"imd": dataset.imd_quintile,
              "age": dataset.age_infants},
)

measures.define_measure(
    name="rsv_gp_sens_imd_age",
    numerator=dataset.rsv_gp_sens,
    denominator=denominator,
    group_by={"imd": dataset.imd_quintile,
              "age": dataset.age_infants},
)

measures.define_measure(
    name="rsv_admission_imd_age",
    numerator=dataset.rsv_sc,
    denominator=denominator,
    group_by= {"imd": dataset.imd_quintile,
              "age": dataset.age_infants},
)

measures.define_measure(
    name="rsv_emerg_imd_age",
    numerator=dataset.rsv_emerg,
    denominator=denominator,
    group_by= {"imd": dataset.imd_quintile,
              "age": dataset.age_infants},
)