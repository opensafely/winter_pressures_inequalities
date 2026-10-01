# import ehrql function for importing codelists
from ehrql import (
  codelist_from_csv
)

#expand codes in SUS which are 3 characters to have X at the end
def expand_three_char_icd10_codes(codelist):
    return codelist + [f"{code}X" for code in codelist if len(code) == 3]

## exposures 

# ethnicity codes
ethnicity6_codes = codelist_from_csv(
  "codelists/opensafely-ethnicity-snomed-0removed.csv",
  column = "code",
  category_column = "Grouping_6",
)
ethnicity16_codes = codelist_from_csv(
  "codelists/opensafely-ethnicity-snomed-0removed.csv",
  column = "code",
  category_column = "Grouping_16",
)

##outcomes

# rsv primary - specific 
rsv_primary_codelist = codelist_from_csv(
  "codelists/opensafely-rsv-identification-primary-care.csv",
  column = "code",
)

# rsv emergency attendances - specific
bronchiolitis_attendance = ["4120002"]

# rsv emergency attendance - sensitive
wheeze_attendance = ["4120002", "276191000000107"]

# RSV emergency attendance (RSV and bronchiolitis SNOMED codes)
rsv_attendance = ["79479005", "195727009", "4120002", "57089007", "195739001"]  # RSV bronchitis, Bronchiolitis

# rsv secondary - specific
rsv_secondary_codelist = codelist_from_csv(
  "codelists/opensafely-rsv-identification-secondary-care.csv",
  column = "code",
)
rsv_secondary_codelist = expand_three_char_icd10_codes(rsv_secondary_codelist)

# RSV primary care sensitive
rsv_primary_sens_codelist = codelist_from_csv(
    "codelists/opensafely-rsv-identification-primary-care-maximal-sensitivity.csv", 
    column = "code",
)

rsv_primary_sens_excl_codelist = codelist_from_csv(
    "codelists/opensafely-rsv-exclusion-primary-care-maximal-sensitivity.csv",
    column = "code",
)

# flu primary - specific
flu_primary_codelist = codelist_from_csv(
  "codelists/opensafely-influenza-identification-primary-care.csv",
  column = "code",
)

# flu primary care - sensitive
flu_primary_sens_codelist = codelist_from_csv(
  "codelists/opensafely-influenza-identification-primary-care-maximal-sensitivity.csv",
  column = "code",
)

flu_primary_sens_excl_codelist = codelist_from_csv(
  "codelists/opensafely-influenza-exclusion-primary-care-maximal-sensitivity.csv",
  column = "code",
)

# flu secondary - specific
flu_secondary_codelist = codelist_from_csv(
  "codelists/opensafely-influenza-identification-secondary-care.csv",
  column = "code",
)
flu_secondary_codelist = expand_three_char_icd10_codes(flu_secondary_codelist)

# flu - emergency
flu_attendance = ["6142004", "442696006", "195878008", "81524006", "442438000", "24662006"]

# acute respiratory illness primary
ari_primary_codelist = codelist_from_csv(
  "codelists/opensafely-acute-respiratory-illness-primary-care.csv",
  column = "code",
)

# Fever
fever_codelist = codelist_from_csv(
    "codelists/opensafely-symptoms-fever.csv",
    column = "code",
)

# covid primary - specific
covid_primary_codelist = codelist_from_csv(
  "codelists/opensafely-covid-19-identification-primary-care.csv",
  column = "code",
)

# covid primary - sensitive
covid_primary_sens_codelist = codelist_from_csv(
  "codelists/opensafely-covid-19-identification-primary-care-maximal-sensitivity.csv",
  column = "code",
)

covid_primary_sens_excl_codelist = codelist_from_csv(
  "codelists/opensafely-covid-19-exclusion-primary-care-maximal-sensitivity.csv",
  column = "code",
)

# covid secondary - specific
covid_secondary_codelist = codelist_from_csv(
  "codelists/opensafely-covid-identification.csv",
  column = "icd10_code",
)

covid_attendance = codelist_from_csv(
  "codelists/opensafely-covid-19-ae-diagnosis-codes.csv",
  column = "code",
)

covid_notest_codelist = codelist_from_csv(
    "codelists/user-ruthcostello-covid-19-notest-identification-primary-care.csv",
    column = "code",
)
# covariates 

# migrant
migrant_codelist = codelist_from_csv(
    "codelists/opensafely-migration-status.csv",
    column = "code",
)

# Years in UK
entry_uk_codelist = ["860021000000109"]

born_in_uk_codelist = codelist_from_csv(
    "codelists/opensafely-born-in-the-uk.csv",
    column = "code",
)
british_ethnicities_codelist = codelist_from_csv(
    "codelists/opensafely-british-ethnicities.csv",
    column = "code",
)
# English language proficiency
english_language_codelist = codelist_from_csv(
    "codelists/opensafely-english-not-main-language.csv",
    column = "code",
)

interpreter_codelist = codelist_from_csv(
    "codelists/opensafely-interpreter-required.csv",
    column = "code",
)

# Clinical risk group codes
# Asthma Diagnosis code
ast = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-ast.csv",
  column="code",
)

# Asthma Admission codes
astadm = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-astadm.csv",
  column="code",
)

# Asthma inhaler or nebuliser medication codes
astrxm1 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-astrxm1.csv",
  column="code",
)

# Asthma systemic steroid medication codes
astrx = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-astrx.csv",
  column="code",
)

# Chronic Respiratory Disease
resp_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-resp_cov.csv",
  column="code",
)

# Chronic heart disease codes
chd_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-chd_cov.csv",
  column="code",
)

# CKD

## Chronic kidney disease diagnostic codes
ckd_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-ckd_cov.csv",
  column="code",
)

## Chronic kidney disease codes - all stages
ckd15 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-ckd15.csv",
  column="code",
)

## Chronic kidney disease codes-stages 3 - 5
ckd35 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-ckd35.csv",
  column="code",
)

# Chronic Liver disease codes
cld = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-cld.csv",
  column="code",
)

# Diabetes

## Diabetes diagnosis codes
diab = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-diab.csv",
  column="code",
)

## Diabetes resolved codes
dmres = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-dmres.csv",
  column="code",
)

## Gestational diabetes diagnosis codes
gdiab = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-gdiab_cod.csv",
  column = "code",
)

# Addisons disease and hypoadrenalism diagnosis codes
addis = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-addis_cod.csv",
  column = "code",
)

# Pregnancy delivery codes
pregdel = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-pregdel.csv",
  column = "code",
)

# Pregnancy codes
preg = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-preg.csv",
  column = "code",
)

# Severe Mental Illness codes
sev_mental = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-sev_mental.csv",
  column="code",
)

# Remission codes relating to Severe Mental Illness
smhres = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-smhres.csv",
  column="code",
)

# Chronic Neurological Disease including Significant Learning Disorder
cns_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-cns_cov.csv",
  column="code",
)

# Immunosuppression diagnosis codes
immdx_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-immdx_cov.csv",
  column="code",
)

# Immunosuppression medication codes
immrx = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-immrx.csv",
  column="code",
)

# Immunosuppression admin codes
immadm = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-immunosuppression-admin-codes.csv",
  column="code",
)

# Chemotherapy or radiation (Primis)
dxt_chemo = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-dxt_chemo_cod.csv",
  column="code",
)

# Chronic Neurological Disease including Significant Learning Disorder
cns_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-cns_cov.csv",
  column="code",
)

# Asplenia or Dysfunction of the Spleen codes
spln_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-spln_cov.csv",
  column="code",
)

# Severe Obesity

## BMI
bmi = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-bmi.csv",
  column="code",
)

## All BMI coded terms
bmi_stage = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-bmi_stage.csv",
  column="code",
)

## Severe Obesity code recorded
sev_obesity = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-sev_obesity.csv",
  column="code",
)

# Wider Learning Disability
learndis = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-learndis.csv",
  column="code",
)

multi_copd = codelist_from_csv(
    "codelists/bristol-multimorbidity_copd.csv",
    column="code",
)

cons_drug = codelist_from_csv(
    "codelists/bristol-multimorbidity_prescription_constipation_bnf.csv",
    column = "code",
)

def create_codelist_dict(dic: dict) -> dict:
    '''
    Create a dictionary of codelists, so that queries can be run iteratively on
    groups of codelists that are subject to the same ehrQL query.
    Args:
        dic: dictionary where key = name, value = codelist csv path
    Returns:
        Dictionary where key = name, value = codelist
    '''
    for name in dic:
        dic[name] = codelist_from_csv(dic[name], 
                                                column = "code")
    return dic

multimorbidity_dict = {
    ## Alcohol Problems
    "MS_AlcoholProblem_snomed": "codelists/bristol-multimorbidity_alcoholproblems.csv",

    ## Anxiety/Depression (medication to be added)
    "MS_AnxietyDepression_snomed": "codelists/bristol-multimorbidity_anxietydepression.csv",
    "MS_AnxietyDepression_dmd": "codelists/user-ZoeMZou-multimorbidity_prescription_anxiolytics_anti_depressants.csv",

    ## Asthma (medication to be added)
    "MS_Asthma_snomed": "codelists/nhsd-primary-care-domain-refsets-ast_cod.csv",
    "MS_Asthma_dmd": "codelists/opensafely-asthma-inhaler-salbutamol-medication.csv",

    ## Atrial Fibrillation
    "MS_AF_snomed": "codelists/bristol-multimorbidity_atrial-fibrillation.csv",

    ## Cancer
    "MS_Cancer_snomed": "codelists/bristol-multimorbidity_cancer.csv",

    ## Chronic Kidney Disease
    "MS_CKD_snomed": "codelists/bristol-multimorbidity_chronic-kidney-disease.csv",

    ## Constipation (medication only)
    "MS_Constipation_dmd": "codelists/bristol-multimorbidity_prescription_constipation_bnf.csv",

    ## Connective Tissue Disorder
    "MS_CTD_snomed": "codelists/bristol-multimorbidity_connective-tissue-disorder.csv",

    ## COPD
    "MS_COPD_snomed": "codelists/bristol-multimorbidity_copd.csv",

    ## Coronary Heart Disease
    "MS_CHD_snomed": "codelists/bristol-multimorbidity_coronary-heart-disease.csv",

    ## Dementia
    "MS_Dementia_snomed": "codelists/bristol-multimorbidity_dementia.csv",

    ## Diabetes Mellitus
    "MS_Diabetes_snomed": "codelists/bristol-multimorbidity_diabetes.csv",

    ## Epilepsy (medication to be added)
    "MS_Epilepsy_snomed": "codelists/bristol-multimorbidity_epilepsy.csv",
    "MS_Epilepsy_dmd": "codelists/bristol-multimorbidity_prescription_epilepsy_bnf.csv",

    ## Hearing Loss
    "MS_HL_snomed": "codelists/bristol-multimorbidity_hearing-loss.csv",

    ## Heart Failure
    "MS_HF_snomed": "codelists/bristol-multimorbidity_heart-failure.csv",

    ## Hypertension
    "MS_Hypertension_snomed": "codelists/nhsd-primary-care-domain-refsets-hyp_cod.csv",

    ## Irritable Bowel Syndrome (medication to be added)
    "MS_IBS_snomed": "codelists/bristol-multimorbidity_irritable-bowel-syndrome.csv",
    "MS_IBS_dmd": "codelists/user-ZoeMZou-multimorbidity_prescription_anti_spasmodic.csv",
    
    ## Painful Condition (Osteoarthritis)
    "MS_PC_Analgesics_dmd": "codelists/bristol-multimorbidity_prescription_analgesics_opiods_exc_migraine_bnf.csv",
    "MS_PC_Antiepileptic_dmd": "codelists/bristol-multimorbidity_prescription_antiepileptics_for_pain_bnf.csv",

    ## Psychosis/Bipolar Disorder (medication to be added)
    "MS_Psychosis_snomed": "codelists/bristol-multimorbidity_psychosisbipolar-disorder.csv",
    "MS_Psychosis_dmd": "codelists/user-ZoeMZou-multimorbidity_prescription_schizophrenia_bipolar_disorder.csv",

    ## Stroke/Transient Ischemic Attack (TIA)
    "MS_StrokeTIA_snomed": "codelists/bristol-multimorbidity_stroketransient-ischemic-attack.csv",
}

# Apply helper to load
multimorbidity_dict = create_codelist_dict(multimorbidity_dict)