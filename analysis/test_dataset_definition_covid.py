from datetime import date
from dataset_definition_covid import dataset

# Need to edit for my study
test_data = {
    # Expected in population with 3 months registration and no hospitalisation
    1: {
        "patients": {"date_of_birth": date(1970, 1, 1), "sex": "female"},
        "apcs": [
            {
                "admission_date": date(2020, 9, 8),
                "primary_diagnosis": "U071",
            }
        ],
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
            "covid_sc_date": date(2020, 9, 8),
            "covid_emergency_date": None,
            "has_covid_admission": True,
            "covid_admission_date": date(2020, 9, 8),

        },
    },
    # Not expected in population, due to hospitalisation for covid in year prior
    2: {
        "patients": {"date_of_birth": date(1990, 1, 1), "sex": "female"},
        "apcs": [
            {
                "admission_date": date(2020, 2, 3),
                "primary_diagnosis": "U071",
            },
           
        ],
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
        "apcs": [],
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
            "covid_sc_date": None,
            "covid_emergency_date": date(2020, 5, 2),
            "has_covid_admission": True,
            "covid_admission_date": date(2020, 5, 2),
        },
    },
    # Not expected in population - registration ends prior to index date
    4: {
        "patients": [{"date_of_birth": date(2001, 1, 1), "sex": "male"}],
        "apcs": [],
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