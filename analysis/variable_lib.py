import operator
from functools import reduce

from ehrql.codes import SNOMEDCTCode, CTV3Code, ICD10Code
from ehrql import case, years, when, get_parameter
from ehrql.tables.tpp import (emergency_care_attendances, 
apcs, clinical_events, ethnicity_from_sus, addresses, medications)

start_date = get_parameter("start_date")

def any_of(conditions):
    return reduce(operator.or_, conditions)

#gp events occuring after index date but before end of follow up
gp_events = (
  clinical_events.where(clinical_events.date
  .is_on_or_between(start_date, start_date + years(1)))
)

#query gp_events for date of earliest event-in-codelist
def first_gp_event(codelist, where = True):
    return (
        gp_events.where(where)
        .where(gp_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .first_for_patient()
    )

def has_prior_event(codelist, start_date, where=True):
    prior_events = clinical_events.where(clinical_events.date.is_on_or_before(start_date))
    return (
        prior_events
        .where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .exists_for_patient()
    )

# query prior_meds for existence of event-in-codelist, returns a patientSeries
def has_prior_meds(codelist, start_date, where=True):
    prior_meds = medications.where(medications.date.is_on_or_before(start_date))
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .exists_for_patient()
    )

#emergency attendances occuring after index date but before end of follow up
emergency_events = (
  emergency_care_attendances.where(emergency_care_attendances
  .arrival_date.is_on_or_between(start_date, start_date + years(1)))
)
def any_of(conditions):
    return reduce(operator.or_, conditions)

def emergency_care_diagnosis_matches(codelist):
    conditions = [
        getattr(emergency_events, column_name).is_in(codelist)
        for column_name in [f"diagnosis_{i:02d}" for i in range(1, 3)]
    ]
    return emergency_events.where(any_of(conditions))

#query gp_events for existence of event-in-codelist 
def is_gp_event(codelist, where = True):
    return (
        gp_events.where(where)
        .where(gp_events.snomedct_code.is_in(codelist))
    )

#emergency attendances occuring after index date but before end of follow up
emergency_events = (
  emergency_care_attendances.where(emergency_care_attendances
  .arrival_date.is_on_or_between(start_date, start_date + years(1)))
)

#hospital events occuring after index date but before end of follow up
hospital_events = (
  apcs.where(apcs.admission_date
  .is_on_or_between(start_date, start_date + years(1)))
)

def hospitalisation_diagnosis_matches(codelist):
    code_strings = set()
    for code in codelist:
        # Pass the string through the ICD10Code to constructor to validate that it has
        # the expected format
        code_string = ICD10Code(code)._to_primitive_type()
        code_strings.add(code_string)
    conditions = [
        # The reason a plain substring search like this works is twofold:
        #
        # * ICD-10 codes all start with the sequence [A-Z][0-9] and do not contain
        #   such a sequence in any other part of the code. In this sense they are
        #   suffix-free and two codes will only match at they start if they match at
        #   all.
        #
        # * Although the codes are not prefix-free they are organised hierarchically
        #   such that code A0123 represents a child concept of code A01. So although
        #   the naive substring matching below will find code A01 if code A0123 is
        #   present, this happens to be the behaviour we actually want.
        #
        # Obviously this is all far from ideal though, and later we hope to be able
        # to pull these codes out in a separate table and handle the matching
        # properly.
        apcs.all_diagnoses.contains(code_string)
        for code_string in code_strings
    ]
    return apcs.where(any_of(conditions))

def hospitalisation_primary_secondary_diagnosis_matches(codelist):

    conditions = [
        (hospital_events.primary_diagnosis.is_in(codelist))|
        (hospital_events.secondary_diagnosis.is_in(codelist))
    ]
    return hospital_events.where(any_of(conditions))

##define function for outcome identification
def get_codes_dates(codelist_name, num_events, start_date, num_codes):

    # Dynamically get the codelist object
    pathogen_codelist = getattr(codelists, codelist_name)

    # Get all relevant events sorted by date
    all_events = (
        clinical_events.where(
            clinical_events.date.is_on_or_between(start_date, followup_end_date)
        )
        .where(clinical_events.snomedct_code.is_in(pathogen_codelist))
        .sort_by(clinical_events.date)
    )

    # Get the first event
    event = all_events.first_for_patient()

    # # Use this as the default if we don't match any others
    # default_event = event

    # Start with an empty list of possible cases for the date and code
    date_cases = []
    code_cases = []

    # For the next three events ...
    for n in range(num_events):
        # Check if there are multiple distinct codes within 14 days
        events_in_date_window = all_events.where(
            all_events.date.is_on_or_between(event.date, event.date + days(14))
        )
        has_multiple_codes = (
            events_in_date_window.snomedct_code.count_distinct_for_patient() >= num_codes
        )
        # Append this event to the lists of cases
        if num_codes == 1:
          date_cases.append(event.date)
        else:
          date_cases.append(
            when(has_multiple_codes).then(event.date)
          )
        code_cases.append(
            when(has_multiple_codes).then(event.snomedct_code)
        )
        # Get the next event after this one and repeat
        event = all_events.where(
            all_events.date.is_after(event.date)
        ).first_for_patient()

    if num_codes != 1:
      codes_date = case(*date_cases, otherwise = None)
    code = case(*code_cases, otherwise = None)

    if num_codes == 1: 
      return(date_cases) 
    else:
      return(codes_date, code)

def get_latest_ethnicity(
    start_date, ethnicity_from_sus, ethnicity_codelist, grouping
):
    latest_ethnicity_from_codes_category_num = (
        clinical_events.where(clinical_events.snomedct_code.is_in(ethnicity_codelist))
        .where(clinical_events.date.is_on_or_before(start_date))
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

def rural_urban_5 (
    start_date
):
    urban_rural_all = addresses.for_patient_on(start_date).rural_urban_classification
    rural_urban_5 = case(
        when(urban_rural_all == 1).then("Urban major conurbation"),
        when(urban_rural_all == 2).then("Urban minor conurbation"),
        when(urban_rural_all == 3).then("Urban city and town"),
        when(urban_rural_all == 4).then("Urban city and town"),
        when(urban_rural_all == 5).then("Rural town and fringe"),
        when(urban_rural_all == 6).then("Rural town and fringe"),
        when(urban_rural_all == 7).then("Rural village and dispersed"),
        when(urban_rural_all == 8).then("Rural village and dispersed"),
        when(urban_rural_all.is_null()).then("")
    )

    return rural_urban_5
