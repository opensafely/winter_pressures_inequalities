import operator
from functools import reduce

from ehrql.codes import SNOMEDCTCode, CTV3Code, ICD10Code
from ehrql import case, days, when, any_of
from ehrql.tables.tpp import (emergency_care_attendances, 
apcs, clinical_events, patients, ons_deaths, practice_registrations)

def emergency_care_info(codelist):
    conditions = [
        getattr(emergency_care_attendances, column_name).is_in(codelist)
        for column_name in [f"diagnosis_{i:02d}" for i in range(1, 25)]
    ]

    return {
        "covid_emergency_date": minimum_of(conditions.date),
        "covid_emergency": emergency_care_attendances.where(any_of(conditions))
    }

def emergency_care_diagnosis_matches(codelist):
    conditions = [
        getattr(emergency_care_attendances, column_name).is_in(codelist)
        for column_name in [f"diagnosis_{i:02d}" for i in range(1, 25)]
    ]
    return emergency_care_attendances.where(any_of(conditions))

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