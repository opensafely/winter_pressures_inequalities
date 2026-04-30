###################################################
# This script defines functions that 
#   creates healthcare utilisation variables
#   for use in downstream measures code
#
# Author: Ruth Costello, LSHTM
#####################################################################


from ehrql import Dataset, case, when, days, claim_permissions, minimum_of
from ehrql.tables.tpp import (
    patients,
    apcs,
    clinical_events,
    emergency_care_attendances,
    ethnicity_from_sus,
    appointments, 
    addresses,
    medications)
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
    # Date of GP code for COVID
    covid_gp_date = (clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.covid_primary_codelist))
        .where(clinical_events.date.is_on_or_between(index_date,end_date))
        .sort_by(clinical_events.date)
        .first_for_patient()
        .date
    )

    # Code for COVID in 28 days prior
    covid_prior_date = (clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.covid_primary_codelist))
        .where(clinical_events.date.is_on_or_between((covid_gp_date - days(28)), covid_gp_date))
        .sort_by(clinical_events.date)
        .first_for_patient()
        .date
    )

    # GP code for COVID
    dataset.covid_gp = case(
         when((covid_gp_date.is_not_null()) & (covid_prior_date.is_null())).then(True),
         otherwise = False,
         )
    
    # GP code for COVID where there is an appointment within the same month (aim to take out testing)  
    covid_appt_prior = appointments.where(appointments.seen_date.is_on_or_between((index_date - days(28)), index_date)).where(
        covid_prior_date.is_not_null()).exists_for_patient()
    
    dataset.covid_gp_appt = case(
         when((appointments.where(appointments.seen_date.is_on_or_between(index_date, end_date)).where(
        dataset.covid_gp == True).exists_for_patient()) & (covid_appt_prior==False)).then(True),
        otherwise = False,
    )
    
    # Code for COVID - not including testing codes
    covid_notest_date = (clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.covid_notest_codelist))
        .where(clinical_events.date.is_on_or_between(index_date,end_date))
        .sort_by(clinical_events.date)
        .first_for_patient()
        .date
    )
    # Code for COVID in 28 days prior
    covid_notest_prior_date = (clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.covid_notest_codelist))
        .where(clinical_events.date.is_on_or_between((covid_notest_date - days(28)), covid_notest_date))
        .sort_by(clinical_events.date)
        .first_for_patient()
        .date
    )
    dataset.covid_gp_notest = case(
         when((covid_notest_date.is_not_null()) & (covid_notest_prior_date.is_null())).then(True),
         otherwise = False,
         )

    # GP consultation - Sensitive definition
    # Find code and exclusions during interval
    gp_covid_sens_excl = (clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.covid_primary_sens_excl_codelist)
        ).where(
        clinical_events.date.is_on_or_between(index_date,end_date)
        ).exists_for_patient()
    )
    gp_covid_sens_date = (clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.covid_primary_sens_codelist)
        ).where(
        clinical_events.date.is_on_or_between(index_date,end_date)
        ).sort_by(clinical_events.date)
        .first_for_patient()
        .date
    )
    covid_gp_sens_flag = case(
         when(dataset.covid_gp==True).then(True),
         when((gp_covid_sens_date.is_not_null()) & (gp_covid_sens_excl==False)).then(True),
        otherwise = False,
        )
    
    #Find sensitive events in prior 28 days if 
    gp_covid_sens_excl_prior = case(when((clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.covid_primary_sens_excl_codelist)
        ).where(
        clinical_events.date.is_on_or_between((gp_covid_sens_date - days(28)),gp_covid_sens_date)
        ).exists_for_patient()) & (covid_gp_sens_flag == True)).then(True),
        otherwise = False,
    )
    gp_covid_sens_prior = case(when((clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.covid_primary_sens_codelist)
        ).where(
        clinical_events.date.is_on_or_between((gp_covid_sens_date - days(28)),gp_covid_sens_date)
        ).exists_for_patient()) & (covid_gp_sens_flag == True)).then(True),
        otherwise = False,
    )
    covid_sens_flag_prior = case(
         when((gp_covid_sens_prior==True) & (gp_covid_sens_excl_prior==False)).then(True),
         otherwise = False,
         )
    dataset.covid_gp_sens = case(when(
         (covid_gp_sens_flag == True) & (covid_sens_flag_prior == False)).then(True),
         otherwise = False,
    )

    # Hospital admission for COVID
    # Date of admission during interval
    covid_sc_date = (apcs.where(
        apcs.primary_diagnosis.is_in(codelists.covid_secondary_codelist)
        ).where(
        apcs.admission_date.is_on_or_between(index_date,end_date)
        ) .admission_date.minimum_for_patient()) 
    # Any admission in prior 28 days?
    covid_sc_prior = (apcs.where(
        apcs.primary_diagnosis.is_in(codelists.covid_secondary_codelist)
        ).where(
        apcs.admission_date.is_on_or_between((covid_sc_date - days(28)),covid_sc_date)
        ) .exists_for_patient()) 
    dataset.covid_sc = case(
         when((covid_sc_date.is_not_null()) & (covid_sc_prior==False)).then(True),
         otherwise = False,
    )

    # Emergency care attendance for COVID
    # Date of emergency attendance
    covid_emerg_date = (emergency_care_diagnosis_matches(codelists.covid_attendance).where(
         emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, end_date)).arrival_date.minimum_for_patient()
        )
    # Any attendance in 28 days prior?
    covid_emerg_prior =(emergency_care_diagnosis_matches(codelists.covid_attendance).where(
         emergency_care_attendances.arrival_date
        .is_on_or_between((covid_emerg_date - days(28)), covid_emerg_date)).exists_for_patient()
        ) 
    dataset.covid_emerg = case(
         when((covid_emerg_date.is_not_null()) & (covid_emerg_prior==False)).then(True),
         otherwise = False
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
    # Date of GP code for Flu
    flu_gp_date = (clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.flu_primary_codelist))
        .where(clinical_events.date.is_on_or_between(index_date,end_date))
        .sort_by(clinical_events.date)
        .first_for_patient()
        .date
    )

    # Code for Flu in 28 days prior
    flu_prior_date = (clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.flu_primary_codelist))
        .where(clinical_events.date.is_on_or_between((flu_gp_date - days(28)), flu_gp_date))
        .sort_by(clinical_events.date)
        .first_for_patient()
        .date
    )
    # GP code for Flu
    dataset.flu_gp = case(
         when((flu_gp_date.is_not_null()) & (flu_prior_date.is_null())).then(True),
         otherwise = False,
         )
    
    # GP consultation  - sensitive
    gp_flu_sens_excl = clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.flu_primary_sens_excl_codelist)
        ).where(
        clinical_events.date.is_on_or_between(index_date,end_date)
        ).exists_for_patient()
    gp_flu_sens_date = (clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.flu_primary_sens_codelist)
        ).where(
        clinical_events.date.is_on_or_between(index_date,end_date)
        ).sort_by(clinical_events.date)
        .first_for_patient()
        .date
    )
    flu_gp_sens_flag = case(
         when(dataset.flu_gp==True).then(True),
         when((gp_flu_sens_date.is_not_null()) & (gp_flu_sens_excl==False)).then(True),
        otherwise = False,
        )
    
    #Find sensitive events in prior 28 days if 
    gp_flu_sens_excl_prior = case(when((clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.flu_primary_sens_excl_codelist)
        ).where(
        clinical_events.date.is_on_or_between((gp_flu_sens_date - days(28)),gp_flu_sens_date)
        ).exists_for_patient()) & (flu_gp_sens_flag == True)).then(True),
        otherwise = False,
    )
    gp_flu_sens_prior = case(when((clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.flu_primary_sens_codelist)
        ).where(
        clinical_events.date.is_on_or_between((gp_flu_sens_date - days(28)),gp_flu_sens_date)
        ).exists_for_patient()) & (flu_gp_sens_flag == True)).then(True),
        otherwise = False,
    )
    flu_sens_flag_prior = case(
         when((gp_flu_sens_prior==True) & (gp_flu_sens_excl_prior==False)).then(True),
         otherwise = False,
         )
    dataset.flu_gp_sens = case(when(
         (flu_gp_sens_flag == True) & (flu_sens_flag_prior == False)).then(True),
         otherwise = False,
    )


    #Hospital admission
    # Admission date
    flu_sc_date = (apcs.where(
        apcs.primary_diagnosis.is_in(codelists.flu_secondary_codelist)
        ).where(
        apcs.admission_date.is_on_or_between(index_date,end_date)
        ) .admission_date.minimum_for_patient()) 
    # Any admission in prior 28 days?
    flu_sc_prior = (apcs.where(
        apcs.primary_diagnosis.is_in(codelists.flu_secondary_codelist)
        ).where(
        apcs.admission_date.is_on_or_between((flu_sc_date - days(28)),flu_sc_date)
        ) .exists_for_patient()) 
    dataset.flu_sc = case(
         when((flu_sc_date.is_not_null()) & (flu_sc_prior==False)).then(True),
         otherwise = False,
    )
    
    # Emergency attendance
    # Date of emergency attendance
    flu_emerg_date = (emergency_care_diagnosis_matches(codelists.flu_attendance).where(
         emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, end_date)).arrival_date.minimum_for_patient()
        )
    # Any attendance in 28 days prior?
    flu_emerg_prior =(emergency_care_diagnosis_matches(codelists.flu_attendance).where(
         emergency_care_attendances.arrival_date
        .is_on_or_between((flu_emerg_date - days(28)), flu_emerg_date)).exists_for_patient()
        ) 
    dataset.flu_emerg = case(
         when((flu_emerg_date.is_not_null()) & (flu_emerg_prior==False)).then(True),
         otherwise = False
    )

    
    #RSV
    # define age as RSV definition age dependent
    dataset.age = patients.age_on(index_date)

    dataset.age_infants = (dataset.age <= 2)

    # GP consultation - definition takes age into account and for infants diagnosis also basedo n emergency care attendance
    gp_rsv_date = (clinical_events.where(
                clinical_events.snomedct_code.is_in(codelists.rsv_primary_codelist)
                ).where(clinical_events.date.is_on_or_between(index_date,end_date)
                ).sort_by(clinical_events.date)
                .first_for_patient()
                .date
    )

    infant_ae_rsv_date = (emergency_care_diagnosis_matches(codelists.bronchiolitis_attendance).where(
         emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, end_date))
        .sort_by(emergency_care_attendances.arrival_date)
        .first_for_patient()
        .arrival_date
        )
    
    infant_rsv_date = case(
         when(dataset.age <= 2).then(minimum_of(gp_rsv_date, infant_ae_rsv_date))
    )
         
    # Washout - identifying events in 28 days prior to event date
    # This differs depending on age, as infants date can be either of GP or emergency care attendance
    gp_rsv_prior_date = case(
         when(dataset.age <= 2).then(clinical_events.where(
                clinical_events.snomedct_code.is_in(codelists.rsv_primary_codelist)
                ).where(clinical_events.date.is_on_or_between((infant_rsv_date - days(28)),infant_rsv_date)
                ).sort_by(clinical_events.date)
                .first_for_patient()
                .date),
        otherwise = ((clinical_events.where(
                clinical_events.snomedct_code.is_in(codelists.rsv_primary_codelist)
                ).where(clinical_events.date.is_on_or_between((gp_rsv_date - days(28)),gp_rsv_date))
                ).sort_by(clinical_events.date)
                .first_for_patient()
                .date)
    )

    infant_rsv_prior_date = (emergency_care_diagnosis_matches(codelists.bronchiolitis_attendance).where(
         emergency_care_attendances.arrival_date
        .is_on_or_between((infant_rsv_date - days(28)),infant_rsv_date))
        .sort_by(emergency_care_attendances.arrival_date)
        .first_for_patient()
        .arrival_date
        )

    dataset.rsv_gp = case(when((dataset.age <= 2) & ((infant_rsv_date.is_not_null()) & (gp_rsv_prior_date.is_null()) & (infant_rsv_prior_date.is_null()))).then(True),
                          when((dataset.age > 2) & (gp_rsv_date.is_not_null()) & (gp_rsv_prior_date.is_null())).then(True),
                        otherwise=False,
    )
    
    # GP consultation  - sensitive
    # GP code from sensitive list with no exclusion, if infant also include bronciolitis or wheeze emergency admission
    gp_rsv_sens_excl = clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.rsv_primary_sens_excl_codelist)
        ).where(
        clinical_events.date.is_on_or_between(index_date,end_date)
        ).exists_for_patient()
    gp_rsv_sens_date = (clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.rsv_primary_sens_codelist)
        ).where(
        clinical_events.date.is_on_or_between(index_date,end_date)
        ).sort_by(clinical_events.date)
                .first_for_patient()
                .date)
    infant_ae_wheeze_date =(emergency_care_diagnosis_matches(codelists.wheeze_attendance).where(
         emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, end_date))
        .sort_by(emergency_care_attendances.arrival_date)
        .first_for_patient()
        .arrival_date)
    
    rsv_sens_date = case(
         when((dataset.age <= 2) & (gp_rsv_sens_excl == False) & (gp_rsv_sens_date.is_not_null())).then(minimum_of(gp_rsv_sens_date, infant_ae_rsv_date, infant_ae_wheeze_date)),
         when((dataset.age <= 2) & (gp_rsv_sens_excl == True) & (gp_rsv_sens_date.is_not_null())).then(minimum_of(infant_ae_rsv_date, infant_ae_wheeze_date)),
         when((dataset.age > 2) & (gp_rsv_sens_excl == False) & (gp_rsv_sens_date.is_not_null())).then(gp_rsv_sens_date),
    )

    # Washout period
    # GP code without exclusion code
    gp_rsv_sens_excl_prior = clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.rsv_primary_sens_excl_codelist)
        ).where(
        clinical_events.date.is_on_or_between((rsv_sens_date - days(28)),rsv_sens_date)
        ).exists_for_patient()
    gp_rsv_sens_date_prior = (clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.rsv_primary_sens_codelist)
        ).where(
        clinical_events.date.is_on_or_between((rsv_sens_date - days(28)),rsv_sens_date)
        ).sort_by(clinical_events.date)
                .first_for_patient()
                .date)
    # Emergency attendance for wheeze or bronchiolitis
    infant_ae_wheeze_date_prior =(emergency_care_diagnosis_matches(codelists.wheeze_attendance).where(
         emergency_care_attendances.arrival_date
        .is_on_or_between((rsv_sens_date - days(28)),rsv_sens_date))
        .sort_by(emergency_care_attendances.arrival_date)
        .first_for_patient()
        .arrival_date)
    infant_ae_date_prior =(emergency_care_diagnosis_matches(codelists.bronchiolitis_attendance).where(
         emergency_care_attendances.arrival_date
        .is_on_or_between((rsv_sens_date - days(28)),rsv_sens_date))
        .sort_by(emergency_care_attendances.arrival_date)
        .first_for_patient()
        .arrival_date)

    prior_rsv = case(
         when((dataset.age <= 2) & ((gp_rsv_sens_excl_prior == False) & (gp_rsv_sens_date_prior.is_not_null()) | (infant_ae_date_prior.is_not_null()) | (infant_ae_wheeze_date_prior.is_not_null()))).then(True),
         when((dataset.age > 2) & ((gp_rsv_sens_excl_prior == False) & gp_rsv_sens_date_prior.is_not_null())).then(True),
         otherwise = False,
    )

    dataset.rsv_gp_sens = case(when((rsv_sens_date.is_not_null()) & (prior_rsv == False)).then(True),
                        otherwise=False)
    

    # Hospital admission
    # Admission date
    rsv_sc_date = (apcs.where(
        apcs.primary_diagnosis.is_in(codelists.rsv_secondary_codelist)
        ).where(
        apcs.admission_date.is_on_or_between(index_date,end_date)
        ) .admission_date.minimum_for_patient()) 
    # Any admission in prior 28 days?
    rsv_sc_prior = (apcs.where(
        apcs.primary_diagnosis.is_in(codelists.rsv_secondary_codelist)
        ).where(
        apcs.admission_date.is_on_or_between((rsv_sc_date - days(28)),rsv_sc_date)
        ) .exists_for_patient()) 
    dataset.rsv_sc = case(
         when((rsv_sc_date.is_not_null()) & (rsv_sc_prior==False)).then(True),
         otherwise = False,
    )
    
    # Emergency attendance
    # Date of emergency attendance
    rsv_emerg_date = (emergency_care_diagnosis_matches(codelists.rsv_attendance).where(
         emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, end_date)).arrival_date.minimum_for_patient()
        )
    # Any attendance in 28 days prior?
    rsv_emerg_prior =(emergency_care_diagnosis_matches(codelists.rsv_attendance).where(
         emergency_care_attendances.arrival_date
        .is_on_or_between((rsv_emerg_date - days(28)), rsv_emerg_date)).exists_for_patient()
        ) 
    dataset.rsv_emerg = case(
         when((rsv_emerg_date.is_not_null()) & (rsv_emerg_prior==False)).then(True),
         otherwise = False
    )
    
    return dataset


##############################################