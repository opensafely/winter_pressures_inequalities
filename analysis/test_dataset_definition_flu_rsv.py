from datetime import date
from dataset_definition_flu_rsv import dataset

# Testing RSV second date using 2018 definition
test_data = {
    # Expected in population - age under 2 - has 2 RSV infections during follow-up check finds correctly
    1: {
        "patients": {"date_of_birth": date(2019, 10, 1), "sex": "female"},
        "addresses": {
            "rural_urban_classification": 1,
            "start_date": date(2020, 1, 1),
            "imd_rounded": 100,
            },
        "ons_deaths": [],
        "clinical_events": [
            {
                "snomedct_code": "185984009", # White British ethnic group
                "date": date(2015, 1, 1),
            },
            {
                "snomedct_code": "155513009", # rsv 1
                "date": date(2021,10,10),
            },
            {
                "snomedct_code": "155513009", # rsv 2
                "date": date(2022,1,10),
            },
            
        ],
        "apcs": [],
        "medications": [],
        "ethnicity_from_sus": [],
        "emergency_care_attendances": [
            {
                "diagnosis_01": "4120002", # rsv
                "arrival_date": date(2018, 10, 2),
            }
            
        ],
        "practice_registrations": [
            {
                "start_date": date(2017, 1, 1),
                "practice_pseudo_id": 12345,
                "practice_systmone_go_live_date": date(2001,5,1),
            },
        ],
        "vaccinations": [],
        "appointments": [
            {
                "seen_date": date(2021,10,10),
                "seen_date": date(2022,1,10),
            } 
        ],
        "expected_in_population": True,
        "expected_columns": {
            "sex": "female",
            "age": 1,
            "age_group": "preschool",
            "urban_rural_5": "Urban major conurbation",
            "ethnicity6_gp": "1",
            "ethnicity16_gp": "3",
            "ethnicity_sus": None,
            "imd_quintile": 1,
            "covid_atrisk_grp": False,
            "flu_atrisk_grp": False,
            "vacc_elig_flu" : False,
            "severe_obesity": False,
            "bmi": None,
            "bmi_date" : None,
            "migrant": False,
            "english_language": False,
            "flu_primary_date": None,
            "flu_sc_date": None,
            "flu_emerg_date": None,
            "has_flu_admission": False,
            "flu_admission_date": None,
            "rsv_primary_date": date(2021, 10, 10),
            "rsv_primary_second_date": date(2022, 1, 10),
            "rsv_sc_date": None,
        },
    },
    # Expected in population - age 80 - has RSV admission
    2: {
        "patients": {"date_of_birth": date(1937, 4, 1), "sex": "male"},
        "addresses": {
            "rural_urban_classification": 1,
            "start_date": date(2015, 1, 1),
            "imd_rounded": 100,
            },
        "ons_deaths": [],
        "clinical_events": [
            {
                "snomedct_code": "185984009", # White British ethnic group
                "date": date(2015, 1, 1),
            },        
        ],
        "apcs": [
             {
                "admission_date": date(2021, 11, 11),
                "primary_diagnosis": "J121", # rsv
            },
        ],
        "medications": [],
        "ethnicity_from_sus": [],
        "emergency_care_attendances": [
            {
                "diagnosis_01": "4120002", # rsv
                "arrival_date": date(2018, 10, 2),
            }
            
        ],
        "practice_registrations": [
            {
                "start_date": date(2017, 1, 1),
                "practice_pseudo_id": 12345,
                "practice_systmone_go_live_date": date(2001,5,1),
            },
        ],
        "vaccinations": [],
        "appointments": [],
        "expected_in_population": True,
        "expected_columns": {
            "sex": "male",
            "age": 84,
            "age_group": "adult_80+",
            "urban_rural_5": "Urban major conurbation",
            "ethnicity6_gp": "1",
            "ethnicity16_gp": "3",
            "ethnicity_sus": None,
            "imd_quintile": 1,
            "covid_atrisk_grp": False,
            "flu_atrisk_grp": False,
            "vacc_elig_flu" : True,
            "severe_obesity": False,
            "bmi": None,
            "bmi_date" : None,
            "migrant": False,
            "english_language": False,
            "flu_primary_date": None,
            "flu_sc_date": None,
            "flu_emerg_date": None,
            "has_flu_admission": False,
            "flu_admission_date": None,
            "rsv_primary_date": None,
            "rsv_primary_second_date": None,
            "rsv_sc_date":  date(2021,11,11),
        },
    },
}