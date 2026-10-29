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
        km_gp_imd <- survfit(Surv(start, stop_gp, gp_binary) ~ imd_quintile, data = data_df)
         # Generate GP plot 
        gp_plot_imd <- ggsurvplot(km_gp_imd, data = data_df, fun = "event", risk.table = TRUE,
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", IMD (GP)"),
                xlim = c(0, max_follow_up),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_imd_gp.svg")
        ggsave(file.path(outdir, graph_file), gp_plot_imd$plot)

        km_sc_imd <- survfit(Surv(start, stop_sc, sc_binary) ~ imd_quintile, data = data_df)
         # Generate SC plot 
        sc_plot_imd <- ggsurvplot(km_sc_imd, data = data_df, fun = "event", risk.table = TRUE,
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", IMD (SC)"),
                xlim = c(0, max_follow_up),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_imd_sc.svg")
        ggsave(file.path(outdir, graph_file), sc_plot_imd$plot)

        # Emergency care plot
        km_emerg_imd <- survfit(Surv(start, stop_emerg, emerg_binary) ~ imd_quintile, data = data_df)
         # Generate GP plot 
        emerg_plot_imd <- ggsurvplot(km_emerg_imd, data = data_df, fun = "event", risk.table = TRUE, 
                conf.int = TRUE, pval = FALSE, legend = "right", 
                title = paste("Kaplan-Meier:", disease, ", IMD (GP)"),
                xlim = c(0, max_follow_up),
                ggtheme = theme_bw())
        graph_file <- paste0("plot_km_", disease, "_imd_emerg.svg")
        ggsave(file.path(outdir, graph_file), emerg_plot_imd$plot)

 }

diseases <- c("covid_2022", "flu_2018", "rsv_2018", "flu_2021", "rsv_2021")

param_grid <- expand.grid(disease = diseases, stringsAsFactors = FALSE)

print(str(param_grid))

all_plots <- pmap(param_grid,
  function(disease) {
    run_km(
      disease = as.character(disease)
    )
  })
