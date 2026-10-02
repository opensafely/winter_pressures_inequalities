# Plots - run on released summary data

library(here)
library(tidyverse)
library(readxl)
library(forestplot)
library(stringr)
library(cowplot)

outdir <- here("output", "covid_plots")
fs::dir_create(outdir)

## Forest plot of Cox model results
# Results have very tight confidence intervals so to review doing this
# Import data
cox_models_covid_2021 <- read_csv(here("output/tables/cox_models_covid_2021.csv")) 

# New plots for Cox model outputs - probably take out one above
# Labels and terms to use
ethnic_terms <- c("ethnicity6_combinedAsian or Asian British", "ethnicity6_combinedBlack or Black British", "ethnicity6_combinedOther Ethnic Groups", "ethnicity6_combinedMixed")
ethnic_labs <- c("Asian or Asian British", "Black or Black British", "Other Ethnic Groups", "Mixed")
ethnic16_terms <- c("ethnicity16_combinedIndian", "ethnicity16_combinedPakistani", "ethnicity16_combinedBangladeshi", "ethnicity16_combinedOther Asian", "ethnicity16_combinedCaribbean", "ethnicity16_combinedAfrican", "ethnicity16_combinedOther Black", "ethnicity16_combinedChinese", "ethnicity16_combinedAll other ethnic groups", "ethnicity16_combinedWhite and Caribbean", "ethnicity16_combinedWhite and African", "ethnicity16_combinedWhite and Asian", "ethnicity16_combinedOther Mixed", "ethnicity16_combinedWhite British", "ethnicity16_combinedWhite Irish", "ethnicity16_combinedOther White")
ethnic16_labs <- c("Asian: Indian", "Asian: Pakistani", "Asian: Bangladeshi", "Asian: Other Asian", "Black: Caribbean", "Black: African", "Black: Other Black", "Other: Chinese", "Other: All other ethnic groups", "Mixed: White and Caribbean", "Mixed: White and African", "Mixed: White and Asian", "Mixed: Other Mixed", "White: White British", "White: White Irish", "White: Other White")
imd_terms <- c("imd_quintile1", "imd_quintile2", "imd_quintile3", "imd_quintile4")
imd_labs <- c("1","2","3","4")

outcomes <- c("gp_binary", "gp_sens_binary", "sc_binary", "emerg_binary")
outcome_labels <- c(
  gp_binary = "GP consultation (specific)",
  gp_sens_binary = "GP consultation (sensitivity)",
  sc_binary = "Hospital admission",
  emerg_binary = "A & E attendance"
)

plot_hr <- function(by_var_val, outcome_val, terms, labs, title_prefix = "Outcome") {
  
  raw_levels <- str_remove(terms, by_var_val) 
  
  p <- cox_models_covid_2021 %>%
    filter(by_var == by_var_val & term %in% terms & outcome == outcome_val) %>%
    mutate(term = factor(str_remove(term, by_var_val), 
                         levels = raw_levels,
                         labels = labs)) %>%
    ggplot(aes(x = estimate, y = term, colour = adjustment)) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                    position = position_dodge(width = 0.6)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
    labs(
      #title = paste(title_prefix, ":", outcome_val),
      x = "Hazard ratio (95% CI)",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text.y = element_text(angle = 0),
      legend.position = "right"
    )
  
  return(p)
}

plot_hr_combined <- function(by_var_val, outcomes_val, terms, labs, title_prefix = "Exposure") {
  
  p <- cox_models_covid_2021 %>%
    filter(by_var == by_var_val & term %in% terms & outcome %in% outcomes_val) %>%
    mutate(term = factor(str_remove(term, by_var_val), levels = labs), 
           outcome = factor(outcome, levels = outcomes_val)) %>%
    ggplot(aes(x = estimate, y = term, colour = adjustment)) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                    position = position_dodge(width = 0.6)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
    facet_grid(outcome ~ adjustment, scales = "free_x", drop = FALSE, labeller = labeller(outcome = outcome_labels)) +
    labs(
      #title = paste(title_prefix, ":", by_var_val),
      x = "Hazard ratio (95% CI)",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "right",
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
    )
  
  return(p)
}

combined_plot2 <- plot_hr_combined(
  by_var_val = "ethnicity6_combined",
  outcomes_val = outcomes,
  terms = ethnic_terms,
  labs = ethnic_labs
)

ggsave(
  filename = file.path(outdir, paste0("hr_plot_ethnicity6_combined.svg")),
  plot = combined_plot2,
  width = 8, height = 6, dpi = 300
)

combined_plot_imd <- plot_hr_combined(
  by_var_val = "imd_quintile",
  outcomes_val = outcomes,
  terms = imd_terms,
  labs = imd_labs
)

ggsave(
  filename = file.path(outdir, paste0("hr_plot_imd_quintile.svg")),
  plot = combined_plot_imd,
  width = 8, height = 6, dpi = 300
)

plots <- list()

for (out in outcomes) {
  plots[[out]] <- plot_hr(
    by_var_val = "ethnicity6_combined",
    outcome_val = out,
    terms = ethnic_terms,
    labs = ethnic_labs
  )
}

# Save each plot using its list name as the filename
for (out in names(plots)) {
  ggsave(
    filename = file.path(outdir, paste0("hr_plot_ethnicity6_combined_", out, ".svg")),
    plot = plots[[out]],
    width = 8, height = 6, dpi = 300
  )
}

for (out in outcomes) {
  plots[[out]] <- plot_hr(
    by_var_val = "ethnicity16_combined",
    outcome_val = out,
    terms = ethnic16_terms,
    labs = ethnic16_labs
  )
}

# Save each plot using its list name as the filename
for (out in names(plots)) {
  ggsave(
    filename = file.path(outdir, paste0("hr_plot_ethnicity16_combined_", out, ".svg")),
    plot = plots[[out]],
    width = 8, height = 6, dpi = 300
  )
}

for (out in outcomes) {
  plots[[out]] <- plot_hr(
    by_var_val = "imd_quintile",
    outcome_val = out,
    terms = imd_terms,
    labs = imd_labs
  )
}

# Save each plot using its list name as the filename
for (out in names(plots)) {
  ggsave(
    filename = file.path(outdir, paste0("hr_plot_imd_quintile_", out, ".svg")),
    plot = plots[[out]],
    width = 8, height = 6, dpi = 300
  )
}

# Plot stratified Cox models
#cox_models_stratified_covid_2021 <- read_csv(here("output/tables/cox_models_stratified_covid_2021.csv")) 
cox_models_time_covid_2021 <- read_csv(here("output/tables/cox_models_time_covid_2021.csv"))

# Same code but different legend
# want each exposure grouped by strata - code for individual plots

plot_hr_stratified <- function(file, by_var_val, outcome_val, terms, strata, labs, title_prefix = "Outcome") {
  
  raw_levels <- str_remove(terms, by_var_val) 
  
  p <- file %>%
    filter(by_var == by_var_val & term %in% terms & outcome == outcome_val & strat_var == strata) %>%
    mutate(term = factor(str_remove(term, by_var_val), 
                         levels = raw_levels,
                         labels = labs)) %>%
    ggplot(aes(x = estimate, y = term, colour = adjustment)) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                    position = position_dodge(width = 0.6)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
    labs(
      #title = paste(title_prefix, ":", outcome_val),
      x = "Hazard ratio (95% CI)",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text.y = element_text(angle = 0),
      legend.position = "right"
    )
  
  return(p)
}

# Combined plot
plot_hr_combined_stratified <- function(file, by_var_val, outcomes_val, terms, strata, labs, title_prefix = "Exposure",
    adjustment_order = c("unadjusted", "age_sex", "area", "migration", "health", "all")) {
  
  p <- file %>%
    filter(by_var == by_var_val & term %in% terms & outcome %in% outcomes_val &  strat_var == strata) %>%
    mutate(term = factor(str_remove(term, by_var_val), levels = labs),
           outcome = factor(outcome, levels = outcomes_val),
           adjustment = factor(adjustment, levels = adjustment_order),) %>%
    ggplot(aes(x = estimate, y = term, colour = strat_level)) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                    position = position_dodge(width = 0.6)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
    facet_wrap(~ outcome, scales = "free_x",
               , labeller = labeller(outcome = outcome_labels)) +
    labs(
      #title = paste(title_prefix, ":", by_var_val),
      x = "Hazard ratio (95% CI)",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "right",
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
    )
  
  return(p)
}

# combined_plot3 <- plot_hr_combined_stratified(
#   file = cox_models_stratified_covid_2021,
#   by_var_val = "ethnicity6_combined",
#   outcomes_val = outcomes,
#   terms = ethnic_terms,
#   strata = "sex",
#   labs = ethnic_labs
# )

# ggsave(
#   filename = file.path(outdir, paste0("hr_s_sex_plot_ethnicity6_combined.svg")),
#   plot = combined_plot3,
#   width = 8, height = 6, dpi = 300
# )

# combined_plot4 <- plot_hr_combined_stratified(
#   file = cox_models_stratified_covid_2021,
#   by_var_val = "ethnicity6_combined",
#   outcomes_val = outcomes,
#   terms = ethnic_terms,
#   strata = "imd_quintile",
#   labs = ethnic_labs
# )

# ggsave(
#   filename = file.path(outdir, paste0("hr_s_imd_plot_ethnicity6_combined.svg")),
#   plot = combined_plot4,
#   width = 8, height = 6, dpi = 300
# )

# Split by time period
# cox_models_time_covid_2021 has `adjustment` and `period` columns (not
# strat_var/strat_level like the stratified output). One plot per outcome;
# within each, all 6 adjustments shown as coloured/dodged dots, faceted by
# period (so both periods appear side by side rather than overplotted).
plot_hr_time <- function(file, by_var_val, outcome_val, terms, labs, title_prefix = "Outcome",
                          adjustment_order = c("unadjusted", "age_sex", "area", "migration", "health", "all")) {

  raw_levels <- str_remove(terms, by_var_val)

  plot_data <- file %>%
    filter(by_var == by_var_val & term %in% terms & outcome == outcome_val)

  if (nrow(plot_data) == 0) {
    message("Skipping ", outcome_val, " - no non-redacted ", by_var_val, " estimates in any adjustment/period")
    return(NULL)
  }

  p <- plot_data %>%
    mutate(term = factor(str_remove(term, by_var_val),
                         levels = raw_levels,
                         labels = labs),
           adjustment = factor(adjustment, levels = adjustment_order),
           period = factor(period)) %>%
    ggplot(aes(x = estimate, y = term, colour = adjustment)) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                    position = position_dodge(width = 0.6)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
    facet_wrap(~ period, drop = FALSE) +
    scale_colour_discrete(drop = FALSE) +
    labs(
      #title = paste(title_prefix, ":", outcome_val),
      x = "Hazard ratio (95% CI)",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold"),
      legend.position = "right"
    )

  return(p)
}

time_plots <- list()

for (out in outcomes) {
  time_plots[[out]] <- plot_hr_time(
    file = cox_models_time_covid_2021,
    by_var_val = "ethnicity6_combined",
    outcome_val = out,
    terms = ethnic_terms,
    labs = ethnic_labs
  )
}

for (out in names(time_plots)) {
  if (is.null(time_plots[[out]])) next
  ggsave(
    filename = file.path(outdir, paste0("hr_plot_time_ethnicity6_combined_", out, ".svg")),
    plot = time_plots[[out]],
    width = 10, height = 6, dpi = 300
  )
}

for (out in outcomes) {
  time_plots[[out]] <- plot_hr_time(
    file = cox_models_time_covid_2021,
    by_var_val = "ethnicity16_combined",
    outcome_val = out,
    terms = ethnic16_terms,
    labs = ethnic16_labs
  )
}

for (out in names(time_plots)) {
  if (is.null(time_plots[[out]])) next
  ggsave(
    filename = file.path(outdir, paste0("hr_plot_time_ethnicity16_combined_", out, ".svg")),
    plot = time_plots[[out]],
    width = 10, height = 6, dpi = 300
  )
}

# Ethnicity in 16 groups needs separate plots

# for (out in outcomes) {
#   plots[[out]] <- plot_hr_stratified(
#     by_var_val = "ethnicity16_combined",
#     outcome_val = out,
#     terms = ethnic16_terms,
#     strata = "sex",
#     labs = ethnic16_labs
#   )
# }

# # Save each plot using its list name as the filename
# for (out in names(plots)) {
#   ggsave(
#     filename = file.path(outdir, paste0("hr_s_sex_plot_ethnic16_", out, ".svg")),
#     plot = plots[[out]],
#     width = 8, height = 6, dpi = 300
#   )
# }

# for (out in outcomes) {
#   plots[[out]] <- plot_hr_stratified(
#     by_var_val = "ethnicity16_combined",
#     outcome_val = out,
#     terms = ethnic16_terms,
#     strata = "imd_quintile",
#     labs = ethnic16_labs
#   )
# }

# # Save each plot using its list name as the filename
# for (out in names(plots)) {
#   ggsave(
#     filename = file.path(outdir, paste0("hr_s_imd_plot_ethnic16_", out, ".svg")),
#     plot = plots[[out]],
#     width = 8, height = 6, dpi = 300
#   )
# }