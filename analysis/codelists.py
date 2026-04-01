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

# RSV emergency attendance (RSV and bronchiolitis SNOMED codes)
rsv_attendance = ["55735004", "4120002"]  # RSV infection, Bronchiolitis

# rsv secondary - specific
rsv_secondary_codelist = codelist_from_csv(
  "codelists/opensafely-rsv-identification-secondary-care.csv",
  column = "code",
)
rsv_secondary_codelist = expand_three_char_icd10_codes(rsv_secondary_codelist)

# flu primary - specific
flu_primary_codelist = codelist_from_csv(
  "codelists/opensafely-influenza-identification-primary-care.csv",
  column = "code",
)

# flu secondary - specific
flu_secondary_codelist = codelist_from_csv(
  "codelists/opensafely-influenza-identification-secondary-care.csv",
  column = "code",
)
flu_secondary_codelist = expand_three_char_icd10_codes(flu_secondary_codelist)

# flu - emergency
flu_attendance = ["6142004"]

# acute respiratory illness primary
ari_primary_codelist = codelist_from_csv(
  "codelists/opensafely-acute-respiratory-illness-primary-care.csv",
  column = "code",
)

# covid primary - specific
covid_primary_codelist = codelist_from_csv(
  "codelists/opensafely-covid-19-identification-primary-care.csv",
  column = "code",
)

# covid secondary - specific
covid_secondary_codelist = codelist_from_csv(
  "codelists/opensafely-covid-identification.csv",
  column = "icd10_code",
)

covid_attendance = ["1240751000000100"]

# covariates 

# migrant
migrant_codelist = codelist_from_csv(
    "codelists/user-YaminaB-migration-status.csv",
    column = "code",
)

# Years in UK
entry_uk_codelist = ["860021000000109"]

# English language proficiency
english_language_codelist = codelist_from_csv(
    "codelists/user-ruthcostello-english-language-proficiency.csv",
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