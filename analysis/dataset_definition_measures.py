###################################################
# This script defines functions that 
#   creates healthcare utilisation variables
#   for use in downstream measures code
#
# Author: Ruth Costello, LSHTM
#####################################################################


from ehrql import Dataset, case, when, claim_permissions, minimum_of
from ehrql.tables.tpp import (
    patients,
    apcs,
    clinical_events,
    emergency_care_attendances,
    ethnicity_from_sus,
    appointments, 
    addresses)
from functools import reduce
import operator
import codelists

claim_permissions("appointments")

# ethnicity function
def get_latest_ethnicity(
        index_date, ethnicity_from_sus, ethnicity_codelist, grouping=6
    ):
        latest_ethnicity_from_codes_category_num = (
            clinical_events.where(clinical_events.snomedct_code.is_in(ethnicity_codelist))
            .where(clinical_events.date.is_on_or_before(index_date))
            .sort_by(clinical_events.date)
            .last_for_patient()
            .snomedct_code.to_category(ethnicity_codelist)
        )

        if grouping == 6:
            latest_ethnicity_from_codes = case(
                when(latest_ethnicity_from_codes_category_num == "1").then("White"),
                when(latest_ethnicity_from_codes_category_num == "2").then("Mixed"),
                when(latest_ethnicity_from_codes_category_num == "3").then(
                    "Asian or Asian British"
                ),
                when(latest_ethnicity_from_codes_category_num == "4").then(
                    "Black or Black British"
                ),
                when(latest_ethnicity_from_codes_category_num == "5").then(
                    "Chinese or Other Ethnic Groups"
                ),
            )

            ethnicity_from_sus = case(
                when(ethnicity_from_sus.code.is_in(["A", "B", "C"])).then("White"),
                when(ethnicity_from_sus.code.is_in(["D", "E", "F", "G"])).then("Mixed"),
                when(ethnicity_from_sus.code.is_in(["H", "J", "K", "L"])).then(
                    "Asian or Asian British"
                ),
                when(ethnicity_from_sus.code.is_in(["M", "N", "P"])).then(
                    "Black or Black British"
                ),
                when(ethnicity_from_sus.code.is_in(["R", "S"])).then(
                    "Chinese or Other Ethnic Groups"
                ),
            )
        elif grouping == 16:
            latest_ethnicity_from_codes = case(
                when(latest_ethnicity_from_codes_category_num == "1").then("White British"),
                when(latest_ethnicity_from_codes_category_num == "2").then("White Irish"),
                when(latest_ethnicity_from_codes_category_num == "3").then("Other White"),
                when(latest_ethnicity_from_codes_category_num == "4").then(
                    "White and Caribbean"
                ),
                when(latest_ethnicity_from_codes_category_num == "5").then(
                    "White and African"
                ),
                when(latest_ethnicity_from_codes_category_num == "6").then(
                    "White and Asian"
                ),
                when(latest_ethnicity_from_codes_category_num == "7").then("Other Mixed"),
                when(latest_ethnicity_from_codes_category_num == "8").then("Indian"),
                when(latest_ethnicity_from_codes_category_num == "9").then("Pakistani"),
                when(latest_ethnicity_from_codes_category_num == "10").then("Bangladeshi"),
                when(latest_ethnicity_from_codes_category_num == "11").then(
                    "Other Asian"
                ),
                when(latest_ethnicity_from_codes_category_num == "12").then("Caribbean"),
                when(latest_ethnicity_from_codes_category_num == "13").then("African"),
                when(latest_ethnicity_from_codes_category_num == "14").then("Other Black"),
                when(latest_ethnicity_from_codes_category_num == "15").then("Chinese"),
                when(latest_ethnicity_from_codes_category_num == "16").then(
                    "All other ethnic groups"
                ),
            )

            ethnicity_from_sus = case(
                when(ethnicity_from_sus.code == "A").then("White British"),
                when(ethnicity_from_sus.code == "B").then("White Irish"),
                when(ethnicity_from_sus.code == "C").then("Other White"),
                when(ethnicity_from_sus.code == "D").then("White and Caribbean"),
                when(ethnicity_from_sus.code == "E").then("White and African"),
                when(ethnicity_from_sus.code == "F").then("White and Asian"),
                when(ethnicity_from_sus.code == "G").then("Other Mixed"),
                when(ethnicity_from_sus.code == "H").then("Indian"),
                when(ethnicity_from_sus.code == "J").then("Pakistani"),
                when(ethnicity_from_sus.code == "K").then("Bangladeshi"),
                when(ethnicity_from_sus.code == "L").then("Other Asian"),
                when(ethnicity_from_sus.code == "M").then("Caribbean"),
                when(ethnicity_from_sus.code == "N").then("African"),
                when(ethnicity_from_sus.code == "P").then("Other Black"),
                when(ethnicity_from_sus.code == "R").then("Chinese"),
                when(ethnicity_from_sus.code == "S").then("All other ethnic groups"),
            )

        ethnicity_combined = case(
            when(latest_ethnicity_from_codes.is_not_null()).then(
                latest_ethnicity_from_codes
            ),
            when(
                latest_ethnicity_from_codes.is_null() & ethnicity_from_sus.is_not_null()
            ).then(ethnicity_from_sus),
            otherwise="Missing",
        )

        return ethnicity_combined

# Functions for emergency events

def any_of(conditions):
    return reduce(operator.or_, conditions)

def emergency_care_diagnosis_matches(codelist):
    conditions = [
        getattr(emergency_care_attendances, column_name).is_in(codelist)
        for column_name in [f"diagnosis_{i:02d}" for i in range(1, 3)]
    ]
    return emergency_care_attendances.where(any_of(conditions))

# Function to define dataset for covid with ethnicity and utilisation variables
def make_dataset_covid_utilisation(index_date, end_date):
    
    dataset = Dataset()
    
    #ethnicity
    dataset.ethnicity6 = get_latest_ethnicity(index_date, ethnicity_from_sus, codelists.ethnicity6_codes)
    
    # IMD
    imd_rounded = addresses.for_patient_on(index_date).imd_rounded
    max_imd = 32844
 
    dataset.imd_quintile = case(
        when((imd_rounded >= 0) & (imd_rounded <= int(max_imd * 1 / 5))).then(1),
        when(imd_rounded <= int(max_imd * 2 / 5)).then(2),
        when(imd_rounded <= int(max_imd * 3 / 5)).then(3),
        when(imd_rounded <= int(max_imd * 4 / 5)).then(4),
        when(imd_rounded <= max_imd).then(5),
        otherwise = 99
    )
    
    # GP code for COVID
    dataset.covid_gp = clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.covid_primary_codelist)
        ).where(
        clinical_events.date.is_on_or_between(index_date,end_date)
        ).exists_for_patient()
    
    # GP code for COVID where there is an appointment on the same day (aim to take out testing)
    covid_gp_date = (clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.covid_primary_codelist))
        .where(clinical_events.date.is_on_or_between(index_date,end_date))
        .sort_by(clinical_events.date)
        .first_for_patient()
        .date
    )
    
    dataset.covid_gp_appt = appointments.where(appointments.seen_date.is_on_or_between(index_date, end_date)).where(
        covid_gp_date.is_on_or_between(index_date, end_date)).exists_for_patient()
    
    # Code for COVID - not including testing codes
    dataset.covid_gp_notest = clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.covid_notest_codelist)
        ).where(
        clinical_events.date.is_on_or_between(index_date,end_date)
        ).exists_for_patient()

    # GP consultation - Sensitive definition
    

    # Hospital admission for COVID
    dataset.covid_sc = (apcs.where(
        apcs.primary_diagnosis.is_in(codelists.covid_secondary_codelist)
        ).where(
        apcs.admission_date.is_on_or_between(index_date,end_date)
        ).exists_for_patient()) 

    # Emergency care attendance for COVID
    dataset.covid_emerg = (emergency_care_diagnosis_matches(codelists.covid_attendance).where(
         emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, end_date)).exists_for_patient()
        )
        
    return dataset

# Function to define dataset for flu and RSV with ethnicity and utilisation variables
def make_dataset_flu_rsv_utilisation(index_date, end_date):
    
    dataset = Dataset()

    ## Define relevant variables: GP and hospital COVID, flu and RSV event, ethnicity - keep each condition separate?

    # ethnicity 
    dataset.ethnicity6 = get_latest_ethnicity(index_date, ethnicity_from_sus, codelists.ethnicity6_codes)
    
    # IMD
    imd_rounded = addresses.for_patient_on(index_date).imd_rounded
    max_imd = 32844
 
    dataset.imd_quintile = case(
        when((imd_rounded >= 0) & (imd_rounded <= int(max_imd * 1 / 5))).then(1),
        when(imd_rounded <= int(max_imd * 2 / 5)).then(2),
        when(imd_rounded <= int(max_imd * 3 / 5)).then(3),
        when(imd_rounded <= int(max_imd * 4 / 5)).then(4),
        when(imd_rounded <= max_imd).then(5),
        otherwise = 99
    )
    
    #flu
    # GP consultation - specific
    dataset.flu_gp = clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.flu_primary_codelist)
        ).where(
            clinical_events.date.is_on_or_between(index_date,end_date)
        ).exists_for_patient()
    #Hospital admission
    dataset.flu_sc = (apcs.where(
        apcs.primary_diagnosis.is_in(codelists.flu_secondary_codelist)
        ).where(
        apcs.admission_date.is_on_or_between(index_date,end_date)
        ).exists_for_patient()) 
    # Emergency attendance
    dataset.flu_emerg = (emergency_care_diagnosis_matches(codelists.flu_attendance).where(
         emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, end_date)).exists_for_patient()
    )
    
    #RSV
    # define age as RSV definition age dependent
    dataset.age = patients.age_on(index_date)

    dataset.age_infants = (dataset.age <= 2)

    # GP consultation - definition takes age into account and for infants diagnosis also basedo n emergency care attendance
    infant_gp_rsv = (clinical_events.where(
              clinical_events.snomedct_code.is_in(codelists.rsv_primary_codelist)
                 ).where(
                      clinical_events.date.is_on_or_between(index_date,end_date)
                      ).exists_for_patient()
    )

    infant_ae_rsv =(emergency_care_diagnosis_matches(codelists.bronchiolitis_attendance).where(
         emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, end_date)).exists_for_patient())
    
    dataset.rsv_gp = case(when((dataset.age <= 2) & ((infant_gp_rsv == True) | (infant_ae_rsv == True))).then(True),
                          when((dataset.age > 2) & ((clinical_events.where(
                               clinical_events.snomedct_code.is_in(codelists.rsv_primary_codelist)
                                                                           ).where(
                               clinical_events.date.is_on_or_between(index_date,end_date)
                               ).exists_for_patient()))).then(True),
                        otherwise=False,
    )
   
    # Hospital admission
    dataset.rsv_sc = (apcs.where(
        apcs.primary_diagnosis.is_in(codelists.rsv_secondary_codelist)
        ).where(
        apcs.admission_date.is_on_or_between(index_date,end_date)
        ).exists_for_patient()) 
    
    # Emergency care attendance - uses extra code to pick up attendances (infants uses bronchiolitis only)
    dataset.rsv_emerg = (emergency_care_diagnosis_matches(codelists.rsv_attendance).where(
         emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, end_date)).exists_for_patient()
    )      
    
    return dataset


##############################################