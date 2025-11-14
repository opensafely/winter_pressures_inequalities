###################################################
# This script defines generates baseline tables 
# for both cohorts
#
# Author: Ruth Costello, LSHTM
#####################################################################
library(here)
library(dplyr)
library(tidyverse)
library(arrow)
library(finalfit)

outdir <- here("output", "tables")
fs::dir_create(outdir)

baseline_covid <- read_feather(here("output/an_bl_data_covid.arrow"))
baseline_flu_rsv <- read_feather(here("output/an_bl_data_flu_rsv.arrow"))

# Variables to go into table
explanatory <- c("age_group", "sex", "ethnicity6_combined", "ethnicity16_combined", 
        "urban_rural_5", "imd_quintile", "migrant", "english_language", 
        "flu_atrisk_grp", "covid_atrisk_grp")

table <- summary_factorlist(baseline_covid, explanatory = explanatory, cont_cut = "imd_quintile")
write_csv(table, file.path(outdir, "baseline_table_covid.csv"))

table_flu_rsv <- summary_factorlist(baseline_flu_rsv, explanatory = explanatory, cont_cut = "imd_quintile")
write_csv(table_flu_rsv, file.path(outdir, "baseline_table_flu_rsv.csv"))

explanatory_strata <- c("age_group", "sex", "ethnicity16_combined", 
        "urban_rural_5", "imd_quintile", "migrant", "english_language", 
        "flu_atrisk_grp", "covid_atrisk_grp")

table_strata <- summary_factorlist(baseline_covid, explanatory = explanatory_strata, dependent = "ethnicity6_combined", cont_cut = "imd_quintile")
write_csv(table_strata, file.path(outdir, "baseline_table_covid_strata.csv"))

table_flu_rsv_strata <- summary_factorlist(baseline_flu_rsv, explanatory = explanatory_strata, dependent = "ethnicity6_combined", cont_cut = "imd_quintile")
write_csv(table_flu_rsv_strata, file.path(outdir, "baseline_table_flu_rsv_strata.csv"))