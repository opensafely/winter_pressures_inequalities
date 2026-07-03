from datetime import date
from dataset_definition_covid import dataset

# Need to edit for my study
test_data = {
    # Expected in population - covid admission during fu, asthma so in clinical risk group
    1: {
        "patients": {"date_of_birth": date(1970, 1, 1), "sex": "female"},
        "addresses": {
            "rural_urban_classification": 1,
            "start_date": date(2015, 1, 1),
            "imd_rounded": 100,
            },
        "ons_deaths": [],
        "clinical_events": [
            {
                "snomedct_code": "154161000", # White British ethnic group
                "date": date(2015, 1, 1),
            },
            {
                "snomedct_code": "10675391000119101", # asthma
                "date": date(2016,8,8),
            },
            {
                "snomedct_code": "233602006", # rsv
                "date": date(2023,1,8),
            },
        ],
        "apcs": [
            {
                "admission_date": date(2022, 2, 15),
                "primary_diagnosis": "U071", # covid
            },
            {
                "admission_date": date(2022, 3, 1),
                "primary_diagnosis": "U071", # covid
            },
            {
                "admission_date": date(2020, 3, 1),
                "primary_diagnosis": "U071", # covid
            },
        ],
        "medications": [
            {
                "dmd_code": "21019411000001101", # asthma inhaled med
                "date": date(2022,2,1),
            },
            {
                "dmd_code": "21019411000001101", # asthma inhaled med
                "date": date(2022,1,6),
            },
            {
                "dmd_code": "32776311000001106", # oral steroid
                "date": date(2022,1,6),
            },
            {
                "dmd_code": "32776311000001106", # oral steroid
                "date": date(2022,2,6),
            },
        ],
        "ethnicity_from_sus": [],
        "emergency_care_attendances": [],
        "practice_registrations": [
            {
                "start_date": date(2015, 1, 1),
                "practice_pseudo_id": 12345,
                "practice_systmone_go_live_date": date(2001,5,1),
            },
        ],
        "vaccinations": [
            {
                "date": date(2019,10,3),
                "target_disease": "INFLUENZA",
            },
            {
                "date": date(2021,11,3),
                "target_disease": "INFLUENZA",
            },
            {
                "date": date(2022,10,14),
                "target_disease": "INFLUENZA",
            },
        ],
        "appointments": [
            {
                "seen_date": date(2023,1,8)
            }
        ],
        "expected_in_population": True,
        "expected_columns": {
            "sex": "female",
            "age": 52,
            "age_group": "adult_under_65",
            "urban_rural_5": "Urban major conurbation",
            "ethnicity6_gp": "1",
            "ethnicity16_gp": "1",
            "ethnicity_sus": None,
            "imd_quintile": 1,
            "covid_atrisk_grp": True,
            "flu_atrisk_grp": True,
            "vacc_elig_covid": True,
            "vacc_elig_flu" : True,
            "severe_obesity": False,
            "recent_bmi": None,
            "recent_bmi_date": None,
            "recent_bmi_age": None,
            "migrant": "Non-migrant",
            "english_language": False,
            "covid_primary_date": None,
            "covid_sc_date": None,
            "covid_emerg_date": None,
            "has_covid_admission": False,
            "covid_admission_date": None,
            "prior_flu_vaccination": date(2021,11,3),
        },
    },
    # Not expected in population, age >110
    2: {
        "patients": {"date_of_birth": date(1900, 1, 1), "sex": "female"},
        "addresses": {
            "rural_urban_classification": 1,
            "start_date": date(2014, 5, 1),
            "imd_rounded": 32500,
            },
        "ons_deaths": [],
        "clinical_events": [
            {
            "snomedct_code": "17095009", # Norwegian White ethnic grouo
            "date": date(2013, 1, 1),
            },
        ],
        "apcs": [
            {
                "admission_date": date(2020, 2, 3),
                "primary_diagnosis": "U071", # covid
            },
           
        ],
        "medications": [],
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
                "practice_systmone_go_live_date": date(2010,5,1),
            },
        ],
        "vaccinations": [],
        "appointments": [],
        "expected_in_population": False,
    },
    # Expected in population with emergency attendance for covid after index date, has smi so in covid but not flu clinical risk groups
    3: {
        "patients": [{"date_of_birth": date(1950, 1, 1), "sex": "male"}],
        "addresses": {
            "rural_urban_classification": 7,
            "start_date": date(2014, 5, 1),
            "imd_rounded": 32300,
            },
        "ons_deaths": [],
        "clinical_events": [
            {
                "snomedct_code": "186001005", # Indian ethnic group
                "date": date(2014, 5, 1),
            },
            {
                "snomedct_code": "1157045004", # covid
                "date": date(2022, 2, 15),
            },
            {
                "snomedct_code": "1157045004", # covid
                "date": date(2022, 3, 5),
            },
            {
                "snomedct_code": "195927002", # flu
                "date": date(2022,9,8),
            },
            {
                "snomedct_code": "111482003", # severe mental illness
                "date": date(2018,9,8),
            },
        ],
        "apcs": [
            {
                "admission_date": date(2022, 9, 11),
                "primary_diagnosis": "J11", # flu
            },
        ],
        "medications": [
            {
                "dmd_code": "31063111000001106", # asthma med
                "date": date(2014,3,1),
            },
        ],
        "ethnicity_from_sus": [],
        "emergency_care_attendances": [
            {
                "diagnosis_01": "1240751000000100", # covid
                "arrival_date": date(2022, 5, 2),
            },
            ],
        "practice_registrations": [
            {
                "start_date": date(2014, 5, 1),
                "practice_pseudo_id": 54322,
                "practice_systmone_go_live_date": date(2001,5,1),
            },
            ],
        "vaccinations": [
            {
                "date": date(2020,10,3),
                "target_disease": "SARS-2 Coronavirus",
            },
        ],
        "appointments": [],
        "expected_in_population": True,
        "expected_columns": {
            "sex": "male",
            "age": 72,
            "age_group": "adult_under_75",
            "urban_rural_5": "Rural village and dispersed",
            "ethnicity6_gp": "3",
            "ethnicity16_gp": "8",
            "ethnicity_sus": None,
            "imd_quintile": 5,
            "covid_atrisk_grp": True,
            "flu_atrisk_grp": False,
            "vacc_elig_covid": True,
            "vacc_elig_flu" : True,
            "severe_obesity": False,
            "recent_bmi": None,
            "recent_bmi_date": None,
            "recent_bmi_age": None,
            "migrant": "Unknown",
            "covid_primary_date": None,
            "covid_primary_appt": False,
            "covid_sc_date": None,
            "covid_emerg_date": date(2022, 5, 2),
            "has_covid_admission": True,
            "covid_admission_date": date(2022, 5, 2),
        },
    },
    # Not expected in population - registration ends prior to index date
    4: {
        "patients": [
            {
                "date_of_birth": date(2001, 1, 1), 
                "sex": "intersex",
            },
        ],
        "addresses": [{
            "rural_urban_classification": 7, 
            "start_date": date(2013, 1, 1),  
            "end_date": date(2014, 3, 3),
            "imd_rounded": 2000,
            },
        ],
        "ons_deaths": [],
        "clinical_events": [
            {
            "snomedct_code": "186001005", # Indian ethnicity
            "date": date(2013, 5, 1),
            },
        ],
        "apcs": [],
        "medications": [],
        "ethnicity_from_sus": [],
        "emergency_care_attendances": [],
        "practice_registrations": [
            {
                "start_date": date(2013, 1, 1),
                "end_date": date(2014, 3, 3),
                "practice_pseudo_id": 21345,
                 "practice_systmone_go_live_date": date(2011,5,1),
            },
        ],
        "vaccinations": [],
        "appointments": [],
        "expected_in_population": False,
    },
    # Not expected in population - died before study start
    5: {
        "patients": [
            {
                "date_of_birth": date(1949, 1, 1), 
                "sex": "female",
                "date_of_death": date(2014,3,3),
            },
        ],
        "addresses": {
            "rural_urban_classification": 6, 
            "start_date": date(2011, 1, 1),  
            "imd_rounded": 2000,
            },
        "ons_deaths": [],
        "clinical_events": [
            {
            "snomedct_code": "186001005", # Indian ethnicity
            "date": date(2013, 5, 1),
            },
        ],
        "apcs": [],
        "medications": [],
        "ethnicity_from_sus": [],
        "emergency_care_attendances": [],
        "practice_registrations": [
            {
                "start_date": date(2011, 1, 1),
                "practice_pseudo_id": 21345,
                "practice_systmone_go_live_date": date(2001,5,1),
            },
        ],
        "vaccinations": [],
        "appointments": [],
        "expected_in_population": False,
    },
    # Not expected in population - no recorded sex
    6: {
        "patients": [
            {
                "date_of_birth": date(1939, 1, 1), 
            },
        ],
        "addresses": {
            "rural_urban_classification": 2, 
            "start_date": date(2011, 1, 1),  
            "imd_rounded": 2000,
            },
        "ons_deaths": [],
        "clinical_events": [
            {
            "snomedct_code": "186001005", # Indian ethnicity
            "date": date(2013, 5, 1),
            },
        ],
        "apcs": [],
        "medications": [],
        "ethnicity_from_sus": [],
        "emergency_care_attendances": [],
        "practice_registrations": [
            {
                "start_date": date(2011, 1, 1),
                "practice_pseudo_id": 21345,
                "practice_systmone_go_live_date": date(2001,5,1),
            },
        ],
        "vaccinations": [],
        "appointments": [],
        "expected_in_population": False,
    },
    # Expected in population - testing ckd
    7: {
        "patients": [{"date_of_birth": date(1950, 1, 1), "sex": "male"}],
        "addresses": {
            "rural_urban_classification": 7,
            "start_date": date(2014, 5, 1),
            "imd_rounded": 32300,
            },
        "ons_deaths": [],
        "clinical_events": [
            {
                "snomedct_code": "186001005", # Indian ethnic group
                "date": date(2014, 5, 1),
            },
            {
                "snomedct_code": "1157045004", # covid
                "date": date(2022, 7, 1),
            },
            #{
            #    "snomedct_code": "108241001", # CKD - ckd_cov codelist
            #    "date": date(2016,9,8),
            #},
            #{
            #    "snomedct_code": "324181000000105", # CKD - ckd_15 codelist
            #    "date": date(2016,9,8),
            #},
            {
                "snomedct_code": "819948005", # Severe Obesity at age 15
                "date": date(1965,9,8),
            },
            {
                "snomedct_code": "243614001", # flu
                "date": date(2020,9,1),
            },
            {
                "snomedct_code": "1021141000000101", # flu exclusion
                "date": date(2020,9,1),
            },
        ],
        "apcs": [],
        "medications": [],
        "ethnicity_from_sus": [],
        "emergency_care_attendances": [
            {
                "diagnosis_01": "6142004", # flu
                "arrival_date": date(2023, 1, 2),
            },
        ],
        "practice_registrations": [
            {
                "start_date": date(2014, 5, 1),
                "practice_pseudo_id": 54322,
                "practice_systmone_go_live_date": date(2015,5,1),
            },
            ],
        "vaccinations": [],
        "appointments": [
            {
                "seen_date": date(2022, 7, 1),
            }
        ],
        "expected_in_population": True,
        "expected_columns": {
            "sex": "male",
            "age": 72,
            "age_group": "adult_under_75",
            "urban_rural_5": "Rural village and dispersed",
            "ethnicity6_gp": "3",
            "ethnicity16_gp": "8",
            "ethnicity_sus": None,
            "imd_quintile": 5,
            "covid_atrisk_grp": False,
            "flu_atrisk_grp": False,
            "vacc_elig_covid": True,
            "vacc_elig_flu" : True,
            "severe_obesity": False,
            "recent_bmi": None,
            "recent_bmi_date": None,
            "recent_bmi_age": None,
            "migrant": "Unknown",
            "covid_primary_date": date(2022, 7, 1),
            "covid_primary_appt": True,
            "covid_sc_date": None,
            "covid_emerg_date": None,
            "has_covid_admission": False,
            "covid_admission_date": None,
        },
    },
    # Expected in population - testing bmi
    8: {
        "patients": [{"date_of_birth": date(2010, 1, 1), "sex": "female"}],
        "addresses": {
            "rural_urban_classification": 5,
            "start_date": date(2010, 5, 1),
            "imd_rounded": 13136,
            },
        "ons_deaths": [],
        "clinical_events": [
            {
                "snomedct_code": "33897005", # Chinese ethnic group - other
                "date": date(2000, 5, 1),
            },
            {
                "snomedct_code": "137906009", # Migrant
                "date": date(2000, 5, 1),
            },
            {
                "snomedct_code": "60621009", # BMI
                "numeric_value": 42,
                "date": date(2021, 7, 1),
            },
            {
                "snomedct_code": "121006005", # flu 
                "date": date(2023,2,1),
            },
            #{
            #    "snomedct_code": "324181000000105", # CKD - ckd_15 codelist
            #    "date": date(2016,9,8),
            #},
        ],
        "apcs": [],
        "medications": [],
        "ethnicity_from_sus": [],
        "emergency_care_attendances": [],
        "practice_registrations": [
            {
                "start_date": date(2014, 5, 1),
                "practice_pseudo_id": 54322,
                 "practice_systmone_go_live_date": date(2001,5,1),
            },
            ],
        "vaccinations": [],
        "appointments": [],
        "expected_in_population": True,
        "expected_columns": {
            "sex": "female",
            "age": 12,
            "age_group": "school",
            "urban_rural_5": "Rural town and fringe",
            "ethnicity6_gp": "5",
            "ethnicity16_gp": "15",
            "ethnicity_sus": None,
            "imd_quintile": 2,
            "covid_atrisk_grp": False,
            "flu_atrisk_grp": False,
            "vacc_elig_covid": False,
            "vacc_elig_flu" : True,
            "severe_obesity": False,
            "recent_bmi": 42,
            "recent_bmi_date": date(2021, 7, 1),
            "recent_bmi_age": 11,
            "migrant": "Migrant",
            "covid_primary_date": None,
            "covid_sc_date": None,
            "covid_emerg_date": None,
            "has_covid_admission": False,
            "covid_admission_date": None,
        },
    },
    # Expected in population - no eligible for flu vaccine
    9: {
        "patients": [{"date_of_birth": date(1980, 1, 1), "sex": "female"}],
        "addresses": {
            "rural_urban_classification": 3,
            "start_date": date(2010, 5, 1),
            "imd_rounded": 32700,
            },
        "ons_deaths": [],
        "clinical_events": [
            {
                "snomedct_code": "76574004", # White ethnic group - Swiss=Any other White background
                "date": date(2000, 5, 1),
            },
            {
                "snomedct_code": "137906009", # Migrant
                "date": date(2000, 5, 1),
            },
        ],
        "apcs": [],
        "medications": [],
        "ethnicity_from_sus": [],
        "emergency_care_attendances": [],
        "practice_registrations": [
            {
                "start_date": date(2014, 5, 1),
                "practice_pseudo_id": 54322,
                 "practice_systmone_go_live_date": date(2001,5,1),
            },
            ],
        "vaccinations": [],
        "appointments": [],
        "expected_in_population": True,
        "expected_columns": {
            "sex": "female",
            "age": 42,
            "age_group": "adult_under_65",
            "urban_rural_5": "Urban city and town",
            "ethnicity6_gp": "1",
            "ethnicity16_gp": "3",
            "ethnicity_sus": None,
            "imd_quintile": 5,
            "covid_atrisk_grp": False,
            "flu_atrisk_grp": False,
            "vacc_elig_covid": True,
            "vacc_elig_flu" : False,
            "severe_obesity": False,
            "recent_bmi": None,
            "recent_bmi_date": None,
            "recent_bmi_age": None,
            "migrant": "Migrant",
            "covid_primary_date": None,
            "covid_sc_date": None,
            "covid_emerg_date": None,
            "has_covid_admission": False,
            "covid_admission_date": None, 
        },
    },
}