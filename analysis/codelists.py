# import ehrql function for importing codelists
from ehrql import (
  codelist_from_csv
)

## exposures 

# ethnicity codes
ethnicity_codes = codelist_from_csv(
  "codelists/opensafely-ethnicity-snomed-0removed.csv",
  column = "code",
  category_column = "Grouping_6",
)

##outcomes

# rsv primary - specific 
rsv_primary_codelist = codelist_from_csv(
  "codelists/opensafely-rsv-identification-primary-care.csv",
  column = "code",
)

# rsv emergency attendances - specific
bronchiolitis_attendance = ["4120002"]

# rsv secondary - specific
rsv_secondary_codelist = codelist_from_csv(
  "codelists/opensafely-rsv-identification-secondary-care.csv",
  column = "code",
)

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

# English language proficiency
english_language_codelist = codelist_from_csv(
    "codelists/user-ruthcostello-english-language-proficiency.csv",
    column = "code",
)

