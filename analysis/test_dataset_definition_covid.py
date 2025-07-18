from datetime import date
from dataset_definition_covid import dataset

# Need to edit for my study
test_data = {
    # Expected in population with 3 months registration and no hospitalisation
    1: {
        "patients": {"date_of_birth": date(1970, 1, 1), "sex": "female"},
        "addresses": {"rural_urban_classification": "1"},
        "clinical_events": [
            {
                "snomedct_code": "185984009",
                "date": date(2015, 1, 1),
            }
        ],
        "apcs": [
            {
                "admission_date": date(2020, 9, 8),
                "primary_diagnosis": "U071",
            }
        ],
        "ethnicity_from_sus": [],
        "emergency_care_attendances": [],
        "practice_registrations": [
            {
                "start_date": date(2015, 1, 1),
                "practice_pseudo_id": 12345,
            }
        ],
        "expected_in_population": True,
        "expected_columns": {
            "sex": "female",
            "age": 50,
            "urban_rural_5": "Urban major conurbation",
            "ethnicity16": 'Other White',
            "ethnicity6": 'White',
            "covid_primary_date": None,
            "covid_sc_date": date(2020, 9, 8),
            "covid_emergency_date": None,
            "has_covid_admission": True,
            "covid_admission_date": date(2020, 9, 8),

        },
    },
    # Not expected in population, due to hospitalisation for covid in year prior
    2: {
        "patients": {"date_of_birth": date(1990, 1, 1), "sex": "female"},
        "addresses": {"rural_urban_classification": "1"},
        "clinical_events": [
            {
            "snomedct_code": "17095009",
            "date": date(2013, 1, 1),
            }
        ],
        "apcs": [
            {
                "admission_date": date(2020, 2, 3),
                "primary_diagnosis": "U071",
            },
           
        ],
        "ethnicity_from_sus": [],
        "emergency_care_attendances": [],
        "practice_registrations": [
            {
                "start_date": date(2013, 1, 1),
                "end_date": date(2014, 3, 3),
                "practice_pseudo_id": 12345,
            },
            {
                "start_date": date(2014, 5, 1),
                "practice_pseudo_id": 54321,
            },
        ],
        "expected_in_population": False,
    },
    # Expected in population with emergency attendance for covid after index date
    3: {
        "patients": [{"date_of_birth": date(1950, 1, 1), "sex": "male"}],
        "addresses": {"rural_urban_classification": "7"},
        "clinical_events": [
            {
            "snomedct_code": "186001005",
            "date": date(2014, 5, 1),
            },
            {
                "snomedct_code": "1157045004",
                "date": date(2020, 7, 1),
            }
        ],
        "apcs": [],
        "ethnicity_from_sus": [],
        "emergency_care_attendances": [
            {
            "diagnosis_01": "1240751000000100",
            "arrival_date": date(2020, 5, 2),
            }
            ],
        "practice_registrations": [
            {
                "start_date": date(2014, 5, 1),
                "practice_pseudo_id": 54322,
            },
            ],
        "expected_in_population": True,
        "expected_columns": {
            "sex": "male",
            "age": 70,
            "urban_rural_5": "Rural village and dispersed",
            "ethnicity16": 'Indian',
            "ethnicity6": 'Asian or Asian British',
            "covid_primary_date": date(2020, 7, 1),
            "covid_sc_date": None,
            "covid_emergency_date": date(2020, 5, 2),
            "has_covid_admission": True,
            "covid_admission_date": date(2020, 5, 2),
        },
    },
    # Not expected in population - registration ends prior to index date
    4: {
        "patients": [{"date_of_birth": date(2001, 1, 1), "sex": "male"}],
        "addresses": {"rural_urban_classification": "7"},
        "clinical_events": [
            {
            "snomedct_code": "186001005",
            "date": date(2013, 5, 1),
            }
        ],
        "apcs": [],
        "ethnicity_from_sus": [],
        "emergency_care_attendances": [],
        "practice_registrations": [
            {
                "start_date": date(2013, 1, 1),
                "end_date": date(2014, 3, 3),
                "practice_pseudo_id": 21345,
            },
        ],
        "expected_in_population": False,
    },
}