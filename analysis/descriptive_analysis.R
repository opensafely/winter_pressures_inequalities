###################################################
# This script defines generates baseline tables 
# for both cohorts
#
# Author: Ruth Costello, LSHTM
#####################################################################
library(here)
library(broom)
library(dplyr)
library(tidyverse)
library(arrow)
library(finalfit)
library(survival)
library(rlang)
library(purrr)
library(finalfit)
library(forcats)
library(gtsummary)
library(survminer)
library(glue)

## Import custom user functions
source(here::here("analysis", "redaction.R"))

outdir <- here("output", "tables")
fs::dir_create(outdir)

#define function for midpoint 6 rounding
roundmid_any <- function(x, to = 6) {
  
  # like round_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x != 0))
  
}
# Function to add a row where level is redacted in stratified analysis
redacted_row <- function(disease, difference, strat_var, strat_level, reason) {
        tibble(
                term        = difference,
                estimate    = NA_real_,
                std.error   = NA_real_,
                statistic   = NA_real_,
                p.value     = NA_real_,
                conf.low    = NA_real_,
                conf.high   = NA_real_,
                outcome     = NA_character_,
                by_var      = difference,
                disease     = disease,
                strat_var   = strat_var,
                strat_level = strat_level,
                redacted    = TRUE,
                redact_reason = reason
        )
}

#Creating baseline tables - first import data
baseline_covid <- read_feather(here("output/an_bl_data_covid.arrow")) %>% 
        mutate(imd_quintile = as.factor(imd_quintile),
        min_yr_start = as.factor(min_start_yr))
baseline_flu_rsv <- read_feather(here("output/an_bl_data_flu_rsv.arrow"))%>% 
        mutate(imd_quintile = as.factor(imd_quintile),
        min_yr_start = as.factor(min_start_yr))

var_labels_list <- list(
                min_start_yr ~ "Baseline year",
                age_group ~ "Age group",
                imd_quintile ~ "IMD quintile",
                ethnicity16_combined ~ "Ethnicity (16 groups)",
                sex ~ "N Sex",
                urban_rural_5 ~ "Urban-rural classification",
                migrant ~ "Migrant N",
                english_language ~ "English not main language spoken",
                flu_atrisk_grp ~ "Flu clinical risk group",
                covid_atrisk_grp ~ "COVID-19 clinical risk group"
                )

# COVID cohort baseline table by ethnicity
desc_covid_by <- baseline_covid %>% 
        select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
        urban_rural_5, imd_quintile, migrant, english_language, 
        flu_atrisk_grp, covid_atrisk_grp) %>% 
        mutate(ethnicity6_combined = fct_na_value_to_level(
                ethnicity6_combined, 
                level = "Missing")) %>%
        tbl_summary(
                missing = "no",
                by = ethnicity6_combined,
                label = var_labels_list,
        ) %>% 
        add_overall(last = FALSE) %>% 
        modify_header(all_stat_cols() ~ "{level}") 
        
# # Apply redactions to baseline table
desc_covid_redact_round <- fn_redact_tbl_summary(
        tbl_summary = desc_covid_by, 
        threshold = 7,
        col_names  = c( "stat_0", "stat_1", "stat_2", "stat_3", "stat_4", "stat_5", "stat_6"),
        rename_cols = c("Overall", "Asian", "Black", "Chinese", "Missing", "Mixed", "White")
        ) 


# # saving both rounded/redacted and not rpound/redacted versions
write_csv(desc_covid_redact_round, file.path(outdir, "baseline_table_covid_r.csv"))
write_csv(desc_covid_by$table_body, file.path(outdir, "baseline_table_covid.csv"))

# # Flu/RSV cohort baseline table by ethnicity
# # Grouped table

desc_flu_rsv_by <- baseline_flu_rsv %>% 
        select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
               urban_rural_5, imd_quintile, migrant, english_language, 
               flu_atrisk_grp, covid_atrisk_grp) %>% 
        mutate(ethnicity6_combined = fct_na_value_to_level(ethnicity6_combined, level = "Missing")) %>%
        tbl_summary(
                missing = "no",
                by = ethnicity6_combined,
                label = var_labels_list
        )%>% 
        add_overall(last = FALSE) %>% 
        modify_header(all_stat_cols() ~ "{level}") 
        
# # Apply redactions to baseline table
desc_flu_rsv_redact_round <- fn_redact_tbl_summary(
        tbl_summary = desc_flu_rsv_by, 
        threshold = 7,
        col_names  = c( "stat_0", "stat_1", "stat_2", "stat_3", "stat_4", "stat_5", "stat_6"),
        rename_cols = c("Overall", "Asian", "Black", "Chinese", "Missing", "Mixed", "White")
        ) 

# # Overall table (no by)
# desc_flu_rsv_overall <- baseline_flu_rsv %>% 
#         select(min_start_yr, age_group, sex, ethnicity6_combined, ethnicity16_combined, 
#                urban_rural_5, imd_quintile, migrant, english_language, 
#                flu_atrisk_grp, covid_atrisk_grp) %>% 
#         mutate(ethnicity6_combined = fct_na_value_to_level(ethnicity6_combined, level = "Missing")) %>%
#         tbl_summary(
#                 missing = "no",
#                 label = var_labels_list
#         )

# raw_stats_by      <- extract_round_stats(desc_flu_rsv_by)
# raw_stats_overall <- extract_round_stats(desc_flu_rsv_overall) %>%
#         mutate(by = "Overall")  # add by column so pivot_wider works

# # Combine and pivot
# desc_flu_rsv_redact_round <- bind_rows(raw_stats_overall, raw_stats_by) %>%
#         rowwise() %>%
#         transmute(
#                 var_label,
#                 variable_levels,
#                 by,
#                 value = glue::glue(stat_display, .envir = cur_data())
#         ) %>%
#         pivot_wider(
#                 names_from = by,
#                 values_from = value
#         )
# # saving both rounded/redacted and not rpound/redacted versions
write_csv(desc_flu_rsv_redact_round, file.path(outdir, "baseline_table_flu_rsv_r.csv"))
write_csv(desc_flu_rsv_by$table_body, file.path(outdir, "baseline_table_flu_rsv.csv"))

# # Table of outcomes - first event for Cox models
outcomes_covid_2021 <- read_feather(here("output/an_outcome_first_covid_2021.arrow")) 
outcomes_flu_2018 <- read_feather(here("output/an_outcome_first_flu_2018.arrow")) 
outcomes_rsv_2018 <- read_feather(here("output/an_outcome_first_rsv_2018.arrow")) 
outcomes_flu_2021 <- read_feather(here("output/an_outcome_first_flu_2021.arrow")) 
outcomes_rsv_2021 <- read_feather(here("output/an_outcome_first_rsv_2021.arrow")) 

# #Function to create table with number of outcomes, person-time, rate and flag for outcome type - no rounding yet
create_event_table <- function(data, time, status, outcome, group = NULL, rate_multiplier = 1000) {
  
  if (is.null(group)) {
    
    data %>%
      summarise(
        N = n(),
        Events = sum(.data[[status]], na.rm = TRUE),
        Person_time = sum(.data[[time]], na.rm = TRUE),
        Rate = (Events / Person_time) * rate_multiplier,
        Outcome = outcome
      )
    
  } else {
    
    data %>%
      group_by(.data[[group]]) %>%
      summarise(
        N = n(),
        Events = sum(.data[[status]], na.rm = TRUE),
        Person_time = sum(.data[[time]], na.rm = TRUE),
        Rate = (Events / Person_time) * rate_multiplier,
        Outcome = outcome,
        .groups = "drop"
      )
  }
}

# # Run function for each outcome with GP, hospitalisations and both and create table

count_outcomes <- function(disease,
                           year,
                           strata,
                           rate_multiplier = 1000) {

  # retrieve dataset
  data_name <- paste0("outcomes_", disease, "_", year)
  data <- get(data_name)

  counts <- bind_rows(
    create_event_table(
      data,
      time = "stop_gp",
      status = "gp_binary",
      outcome = paste(disease, "- GP"),
      group = strata
    ),
    create_event_table(
      data,
      time = "stop_gp_sens",
      status = "gp_sens_binary",
      outcome = paste(disease, "- GP sens"),
      group = strata
    ),
    create_event_table(
      data,
      time = "stop_sc",
      status = "sc_binary",
      outcome = paste(disease, "- SC"),
      group = strata
    ),
    create_event_table(
      data,
      time = "stop_emerg",
      status = "emerg_binary",
      outcome = paste(disease, "- A&E"),
      group = strata
    ),
    create_event_table(
      data,
      time = "stop",
      status = "binary_all",
      outcome = paste(disease, "- all"),
      group = strata
    )
  )

  counts_round <- counts %>%
    mutate(
      N_mid6 = if_else(N <= 7, NA_real_, roundmid_any(N)),
      Events_mid6 = if_else(Events <= 7, NA_real_, roundmid_any(Events)),
      Person_time_mid6 = if_else(Person_time <= 7,
                                 NA_real_,
                                 roundmid_any(Person_time)),
      Rate_mid6 = round(
        (Events_mid6 / Person_time_mid6) * rate_multiplier,
        5
      )
    )

  write_csv(
    counts,
    file.path(
      outdir,
      paste0("count_", disease, "_", year, "_", strata, ".csv")
    )
  )

  write_csv(
    counts_round,
    file.path(
      outdir,
      paste0("count_", disease, "_", year, "_", strata, "_round.csv")
    )
  )

  invisible(
    list(
      counts = counts,
      counts_round = counts_round
    )
  )
}

count_outcomes(
  disease = "covid",
  year = "2021",
  strata = "ethnicity6_combined",
)

count_outcomes(
  disease = "covid",
  year = "2021",
  strata = "imd_quintile"
)

count_outcomes(
  disease = "flu",
  year = "2021",
  strata = "ethnicity6_combined"
)

count_outcomes(
  disease = "flu",
  year = "2021",
  strata = "imd_quintile"
)

count_outcomes(
  disease = "flu",
  year = "2018",
  strata = "ethnicity6_combined"
)

count_outcomes(
  disease = "flu",
  year = "2018",
  strata = "imd_quintile"
)

count_outcomes(
  disease = "rsv",
  year = "2021",
  strata = "ethnicity6_combined"
)

count_outcomes(
  disease = "rsv",
  year = "2021",
  strata = "imd_quintile"
)

count_outcomes(
  disease = "rsv",
  year = "2018",
  strata = "ethnicity6_combined"
)

count_outcomes(
  disease = "rsv",
  year = "2018",
  strata = "imd_quintile"
)

# 16 ethnic groups 
count_outcomes(
  disease = "covid",
  year = "2021",
  strata = "ethnicity16_combined",
)

count_outcomes(
  disease = "flu",
  year = "2021",
  strata = "ethnicity16_combined"
)

count_outcomes(
  disease = "flu",
  year = "2018",
  strata = "ethnicity16_combined"
)

count_outcomes(
  disease = "rsv",
  year = "2021",
  strata = "ethnicity16_combined"
)

count_outcomes(
  disease = "rsv",
  year = "2018",
  strata = "ethnicity16_combined"
)

# # Cox PH models to measure differences in utilisation by ethnicity and IMD
run_cox_models <- function(disease, difference, adjustment) {
        # Build filename and read dataset
        file_path <- paste0("an_outcome_first_",disease,".arrow")
        data_df <- read_feather(here("output", file_path), as_data_frame = TRUE)

        # Ensure factor + set reference
        if (difference == "ethnicity6_combined") {
                data_df[[difference]] <- factor(data_df[[difference]])
                data_df[[difference]] <- relevel(data_df[[difference]], ref = "White")
        }

        if (difference == "ethnicity16_combined") {
                data_df[[difference]] <- factor(data_df[[difference]])
                data_df[[difference]] <- relevel(data_df[[difference]], ref = "White British")
        }

        if (difference == "imd_quintile") {
                data_df[[difference]] <- factor(data_df[[difference]])
                data_df[[difference]] <- relevel(data_df[[difference]], ref = "5") 
        }
        
        # Define adjustment terms
        adj_vars <- switch(adjustment,
                "unadjusted"  = "",
                "age_sex"     = " + age_group + sex",
                "migration"        = " + age_group + sex + migrant + english_language",
                "area"  = " + age_group + sex + imd_quintile + urban_rural_5",
                "health" = " + age_group + sex + vacc_elig_covid + vacc_elig_flu + vacc_elig_rsv + prior_flu_vaccination + prior_covid_vaccination + prior_rsv_vaccination + bmi",
                stop("Unknown adjustment: ", adjustment)
        )

        # Define Cox model formulas
        make_formula <- function(start, stop, event, strata_var) {
                as.formula(paste0("Surv(", start, ", ", stop, ", ", event, ") ~ ", 
                                  difference, adj_vars, " + strata(", strata_var, ")"))
        }

        f_gp     <- make_formula("start", "stop_gp",      "gp_binary",      "year_end_gp")
        f_gp_sens <- make_formula("start", "stop_gp_sens", "gp_sens_binary", "year_end_gp")
        f_sc     <- make_formula("start", "stop_sc",      "sc_binary",      "year_end_sc")
        f_emerg  <- make_formula("start", "stop_emerg",   "emerg_binary",   "year_end_emerg")


        # define Cox model formulas
        f_gp <- as.formula(paste0("Surv(start, stop_gp, gp_binary) ~ ", difference, " + strata(year_end_gp)"))
        f_gp_sens <- as.formula(paste0("Surv(start, stop_gp_sens, gp_sens_binary) ~ ", difference, " + strata(year_end_gp)"))
        f_sc <- as.formula(paste0("Surv(start, stop_sc, sc_binary) ~ ", difference, " + strata(year_end_sc)"))
        f_emerg <- as.formula(paste0("Surv(start, stop_emerg, emerg_binary) ~ ", difference, " + strata(year_end_emerg)"))
        # Helper to either fit model or return redacted row
        fit_or_redact <- function(formula, binary_var, outcome_name) {
                n_events <- sum(data_df[[binary_var]] == 1, na.rm = TRUE)
                if (n_events > 7) {
                        coxph(formula, data = data_df) %>%
                                tidy(exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>%
                                mutate(outcome = outcome_name, by_var = difference, disease = disease, adjustment = adjustment)
                } else {
                        message("Redacted: 7 or fewer ", disease, " ", outcome_name, " events")
                        tibble(
                                term        = difference,
                                estimate    = NA_real_,
                                std.error   = NA_real_,
                                statistic   = NA_real_,
                                p.value     = NA_real_,
                                conf.low    = NA_real_,
                                conf.high   = NA_real_,
                                outcome     = outcome_name,
                                by_var      = difference,
                                disease     = disease,
                                adjustment = adjustment,
                                redacted    = TRUE
                        )
                }
        }

        new_rows <- bind_rows(
                fit_or_redact(f_gp,    "gp_binary",    "gp_binary"),
                fit_or_redact(f_gp_sens,    "gp_sens_binary",    "gp_sens_binary"),
                fit_or_redact(f_sc,    "sc_binary",    "sc_binary"),
                fit_or_redact(f_emerg, "emerg_binary", "emerg_binary")
        )

        return(new_rows)
}

diseases <- c("covid_2021", "flu_2018", "rsv_2018", "flu_2021", "rsv_2021")
by_vars <- c("imd_quintile", "ethnicity6_combined", "ethnicity16_combined")
adjustments <- c("unadjusted", "age_sex", "migration", "area", "health")

param_grid <- expand.grid(
  disease = diseases, 
  difference = by_vars, 
  adjustment = adjustments,
  stringsAsFactors = FALSE
  )

cox_table <- pmap_dfr(
  param_grid,
  function(disease, difference, adjustment) run_cox_models(disease, difference, adjustment)
)
print(head(cox_table))

cox_table <- cox_table %>% 
                mutate(across(c("estimate", "conf.low", "conf.high"), ~round(.x, 2))) %>%
                # don't need std.error or statistic
                select("term", "estimate", "conf.low", "conf.high", "outcome", "by_var", "disease", "adjustment")

write_csv(cox_table, file.path(outdir, "cox_models.csv"))

# Stratified models - unadjusted
run_cox_models_stratified <- function(disease, difference, 
                                       strat_var, strat_level) {
        # Build filename and read dataset
        file_path <- paste0("an_outcome_first_", disease, ".arrow")
        data_df <- read_feather(here("output", file_path), as_data_frame = TRUE)

        # Subset to stratum
        data_df <- data_df[data_df[[strat_var]] == strat_level, ]
        data_df <- droplevels(data_df)

        # Check data exists after subsetting
        if (nrow(data_df) == 0) {
                message("No data for ", strat_var, " = ", strat_level, " in ", disease)
                return(redacted_row(disease, difference, strat_var, strat_level, "no data in stratum"))
        }

        # Ensure factor + set reference
        if (difference == "ethnicity6_combined") {
                data_df[[difference]] <- factor(data_df[[difference]])
                if ("White" %in% levels(data_df[[difference]])) {
                data_df[[difference]] <- relevel(data_df[[difference]], ref = "White")
        } else {
                message("Skipping: 'White' reference level not present in ", 
                        strat_var, " = ", strat_level)
                return(redacted_row(disease, difference, strat_var, strat_level, "reference level not present"))
        }
}
        if (difference == "ethnicity16_combined") {
                data_df[[difference]] <- factor(data_df[[difference]])
                if ("White British" %in% levels(data_df[[difference]])) {
                data_df[[difference]] <- relevel(data_df[[difference]], ref = "White British")
        } else {
                message("Skipping: 'White British' reference level not present in ", 
                        strat_var, " = ", strat_level)
                return(redacted_row(disease, difference, strat_var, strat_level, "reference level not present"))
        }
}
#         if (difference == "imd_quintile") {
#                 data_df[[difference]] <- factor(data_df[[difference]])
#                 if ("5" %in% levels(data_df[[difference]])) {
#                 data_df[[difference]] <- relevel(data_df[[difference]], ref = "5")
#         } else {
#                 message("Skipping: IMD quintile '5' not present in ", 
#                         strat_var, " = ", strat_level)
#                 return(redacted_row(disease, difference, strat_var, strat_level, "reference level not present"))
#         }
# }
        # Check difference variable has at least 2 levels
        if (nlevels(data_df[[difference]]) < 2) {
                message("Skipping: ", difference, " has fewer than 2 levels in ",
                        strat_var, " = ", strat_level, " for ", disease)
                return(redacted_row(disease, difference, strat_var, strat_level, "fewer than 2 levels"))
        }
        print(class(data_df$age_group))
        print(table(data_df$age_group, data_df$imd_quintile))
        # Define Cox model formulas
        make_formula <- function(start, stop, event, strata_var) {
                as.formula(paste0("Surv(", start, ", ", stop, ", ", event, ") ~ ",
                                  difference, " + strata(", strata_var, ")"))
        }

        f_gp      <- make_formula("start", "stop_gp",      "gp_binary",      "year_end_gp")
        f_gp_sens <- make_formula("start", "stop_gp_sens", "gp_sens_binary", "year_end_gp")
        f_sc      <- make_formula("start", "stop_sc",      "sc_binary",      "year_end_sc")
        f_emerg   <- make_formula("start", "stop_emerg",   "emerg_binary",   "year_end_emerg")

        # Helper to either fit model or return redacted row
        fit_or_redact <- function(formula, binary_var, outcome_name) {
                n_events <- sum(data_df[[binary_var]] == 1, na.rm = TRUE)
                if (n_events > 7) {
                        coxph(formula, data = data_df) %>%
                                tidy(exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>%
                                mutate(outcome = outcome_name, by_var = difference,
                                       disease = disease, 
                                       strat_var = strat_var, strat_level = strat_level)
                } else {
                        message("Redacted: 7 or fewer ", disease, " ", outcome_name, 
                                " events in ", strat_var, " = ", strat_level)
                        tibble(
                                term        = difference,
                                estimate    = NA_real_,
                                std.error   = NA_real_,
                                statistic   = NA_real_,
                                p.value     = NA_real_,
                                conf.low    = NA_real_,
                                conf.high   = NA_real_,
                                outcome     = outcome_name,
                                by_var      = difference,
                                disease     = disease,
                                strat_var   = strat_var,
                                strat_level = strat_level,
                                redacted    = TRUE,
                                redact_reason = "fewer than 7 events"
                        )
                }
        }

        new_rows <- bind_rows(
                fit_or_redact(f_gp,      "gp_binary",      "gp_binary"),
                fit_or_redact(f_gp_sens, "gp_sens_binary", "gp_sens_binary"),
                fit_or_redact(f_sc,      "sc_binary",      "sc_binary"),
                fit_or_redact(f_emerg,   "emerg_binary",   "emerg_binary")
        )

        return(new_rows)
}
eth_vars <- c("ethnicity6_combined", "ethnicity16_combined")
strat_params <- bind_rows(
        expand.grid(strat_var = "sex",         strat_level = c("male", "female"),   stringsAsFactors = FALSE),
        expand.grid(strat_var = "imd_quintile", strat_level = c("1","2","3","4","5"), stringsAsFactors = FALSE)
)

param_grid_strat <- expand.grid(
        disease    = diseases,
        difference = eth_vars,
        stringsAsFactors = FALSE
) %>% cross_join(strat_params)

cox_table_stratified <- pmap_dfr(param_grid_strat, function(disease, difference, strat_var, strat_level) {
        run_cox_models_stratified(disease, difference, strat_var, strat_level)
})

cox_table_stratified <- cox_table_stratified %>% 
                mutate(across(c("estimate", "conf.low", "conf.high"), ~round(.x, 2))) %>%
                # don't need std.error or statistic
                select("term", "estimate", "conf.low", "conf.high", "outcome", "by_var", "disease", "strat_var", "strat_level", "redacted", "redact_reason")

write_csv(cox_table_stratified, file.path(outdir, "cox_models_stratified.csv"))

#test KM plot

km_gp <- survfit(Surv(stop_gp, gp_binary) ~ ethnicity6_combined, data = outcomes_covid_2021)
# Generate GP plot 
names(km_gp$strata) <- gsub("ethnicity6_combined=", "", names(km_gp$strata))
gp_plot <- ggsurvplot_facet(km_gp, data = outcomes_covid_2021, facet.by = "year_end_gp", fun = "event", risk.table = FALSE,
        conf.int = TRUE, pval = FALSE, legend = "right", #title = ("Kaplan-Meier: COVID-19 (GP)"),
        legend.labs = c("Asian", "Black", "Chinese", "Missing", "Mixed", "White"),
        ggtheme = theme_bw())

ggsave(file.path(outdir, "plot_km_covid_2021_gp.svg"), gp_plot)

# Kaplan Meier plots
 run_km <- function(disease) {
         disease <- as.character(disease)
         # Build filename and read dataset
        file_path <- paste0("an_outcome_first_",disease,".arrow")
        data_df <- read_feather(here("output", file_path), as_data_frame = TRUE)
        print(head(data_df))
        # Derive x-axis limit from actual follow-up in the data
        max_follow_up <- max(c(data_df$stop_gp, data_df$stop_gp_sens, data_df$stop_sc, data_df$stop_emerg), na.rm = TRUE)
        # CHECK FOR TIMING ISSUES
        cat("\n=== Timing Check for", disease, "===\n")
    
        timing_issues <- data_df %>%
                summarise(
                total = n(),
                start_gt_stop_gp = sum(start > stop_gp, na.rm = TRUE),
                start_gt_stop_sc = sum(start > stop_sc, na.rm = TRUE),
                start_gt_stop_emerg = sum(start > stop_emerg, na.rm = TRUE),
                pct_gp = round(sum(start > stop_gp, na.rm = TRUE) / n() * 100, 2),
                pct_sc = round(sum(start > stop_sc, na.rm = TRUE) / n() * 100, 2),
                pct_emerg = round(sum(start > stop_emerg, na.rm = TRUE) / n() * 100, 2)
                )
        
        print(timing_issues)
        
    
         # Fit KM models
        # Ethnicity
        km_gp <- survfit(Surv(start, stop_gp, gp_binary) ~ ethnicity6_combined, data = data_df)
#         # Generate GP plot 
        gp_plot <- ggsurvplot(km_gp, data = data_df, fun = "event", risk.table = TRUE, 
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", ethnicity (GP)"),
                xlim = c(0, max_follow_up),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_ethnicity_gp.svg")
        ggsave(file.path(outdir, graph_file), gp_plot$plot)

        km_sc <- survfit(Surv(start, stop_sc, sc_binary) ~ ethnicity6_combined, data = data_df)
#         # Generate SC plot 
        sc_plot <- ggsurvplot(km_sc, data = data_df, fun = "event", risk.table = TRUE,
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", ethnicity (sc)"),
                xlim = c(0, max_follow_up),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_ethnicity_sc.svg")
        ggsave(file.path(outdir, graph_file), sc_plot$plot)

        # Emergency care plot
        km_emerg <- survfit(Surv(start, stop_emerg, emerg_binary) ~ ethnicity6_combined, data = data_df)
#         # Generate GP plot 
        emerg_plot <- ggsurvplot(km_emerg, data = data_df, fun = "event", risk.table = TRUE, 
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", ethnicity (GP)"),
                xlim = c(0, max_follow_up),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_ethnicity_emerg.svg")
        ggsave(file.path(outdir, graph_file), emerg_plot$plot)

        # IMD
        km_gp <- survfit(Surv(start, stop_gp, gp_binary) ~ imd_quintile, data = data_df)
#         # Generate GP plot 
        gp_plot <- ggsurvplot(km_gp, data = data_df, fun = "event", risk.table = TRUE,
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", IMD (GP)"),
                xlim = c(0, max_follow_up),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_imd_gp.svg")
        ggsave(file.path(outdir, graph_file), gp_plot$plot)

        km_sc <- survfit(Surv(start, stop_sc, sc_binary) ~ imd_quintile, data = data_df)
#         # Generate SC plot 
        sc_plot <- ggsurvplot(km_sc, data = data_df, fun = "event", risk.table = TRUE,
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", IMD (SC)"),
                xlim = c(0, max_follow_up),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_imd_sc.svg")
        ggsave(file.path(outdir, graph_file), sc_plot$plot)

        # Emergency care plot
        km_emerg <- survfit(Surv(start, stop_emerg, emerg_binary) ~ imd_quintile, data = data_df)
#         # Generate GP plot 
        emerg_plot <- ggsurvplot(km_emerg, data = data_df, fun = "event", risk.table = TRUE, 
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", ethnicity (GP)"),
                xlim = c(0, max_follow_up),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_imd_emerg.svg")
        ggsave(file.path(outdir, graph_file), emerg_plot$plot)

 }

param_grid <- expand.grid(disease = diseases, stringsAsFactors = FALSE)

print(str(param_grid))

all_plots <- pmap(param_grid,
  function(disease) {
    run_km(
      disease = as.character(disease)
    )
  })


