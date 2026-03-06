# Import libraries ----
library('tidyverse')
library('lubridate')

# taken from ckd-coverage-ve
redactor <- function(n, threshold){

  # given a vector of frequencies, this returns a boolean vector that is TRUE if
  # a) the frequency is <= the redaction threshold and
  # b) if the sum of redacted frequencies in a) is still <= the threshold, then the
  # next largest frequency is also redacted

  n <- as.integer(n)
  leq_threshold <- dplyr::between(n, 1, threshold)
  n_sum <- sum(n)

  # redact if n is less than or equal to redaction threshold
  redact <- leq_threshold

  # also redact next smallest n if sum of redacted n is still less than or equal to threshold
  if((sum(n*leq_threshold) <= threshold) & any(leq_threshold)){
    redact[which.min(dplyr::if_else(leq_threshold, n_sum+1L, n))] = TRUE
  }

  redact
}


redactor2 <- function(n, threshold=5, x=NULL){

  # given a vector of frequencies (n), this returns a redacted vector (if x is NULL) or
  # reaction of a secondary vector based on frequencies in the first (if x is not nULL).
  # using the following rules:
  # a) the frequency is <= the redaction threshold and
  # b) if the sum of redacted frequencies in a) is still <= the threshold, then the
  # next largest frequency is also redacted


  stopifnot("n must be non-missing" = any(!is.na(n)))
  stopifnot("n must non-negative" = any(n>=0))
  stopifnot("n must non-negative" = any(n>=0))

  if(is.null(x)){
    x <- n
  }

  if(!is.null(x)){
    stopifnot("x must be same length as n" = length(n) == length(x))
  }



  n <- as.integer(n)
  leq_threshold <- dplyr::between(n, 1, threshold)
  n_sum <- sum(n)

  # redact if n is less than or equal to redaction threshold
  redact <- leq_threshold

  # also redact next smallest n if sum of redacted n is still less than or equal to threshold
  if((sum(n*leq_threshold) <= threshold) & any(leq_threshold)){
    redact[which.min(dplyr::if_else(leq_threshold, n_sum+1L, n))] = TRUE
  }


  typedNA <- NA
  mode(typedNA) <- typeof(x)

  redacted <- dplyr::if_else(redact, typedNA, x)

  redacted
}

redact_tblsummary <- function(x, threshold, redact_chr=NA_character_){

  ## function to redact a tbl_summary object
  ## x is a tbl_summary object
  ## threshold is the redaction threshold, passed to the redactor2 function
  ## redact_chr is the character string used to replace redacted statistics. default is NA

  ## the function redacts all statistics based on counts less than 5 (including means, medians, etc)
  ## it also removes potentially disclosive items from the object, namely:
  ##  - `x$inputs$data` which contains the input data
  ##  - `x$inputs$meta_data` which contains the raw summary table for the table

  stopifnot("x must be a tbl_summary object" = all(class(x) %in% c("tbl_summary", "gtsummary")))

  raw_stats <- x$meta_data %>%
    select(var_label, df_stats) %>%
    unnest(df_stats) %>%
    mutate(
      redact_n = if_else(is.na(n), N_obs, n),
      variable_levels = if_else(!is.na(variable_levels), as.character(variable_levels), var_label)
    )

  if(x$inputs$missing %in% c("ifany", "always")){
    missing_stats <- raw_stats %>%
      select(by, var_label, variable, N_miss, N_obs, N) %>%
      distinct() %>%
      mutate(
        stat_display = "{N_miss}",
        row_type = "missing",
        redact_n = N_miss,
        variable_levels = x$inputs$missing_text
      ) %>%
      filter(N_miss>0 & x$inputs$missing!="always")

    raw_stats <- bind_rows(raw_stats, missing_stats)
  }

  name_by_stat <- set_names(x$df_by$by_col, as.character(x$df_by$by))
  name_stat_by <- set_names(as.character(x$df_by$by), x$df_by$by_col)

  table_body_long <- x$table_body %>%
    pivot_longer(cols=starts_with("stat_"), names_to="by", values_to="display") %>%
    mutate(
      by = fct_recode(by, .fun=!!!name_by_stat)
    )

  redacted_stats <-
    left_join(
      raw_stats %>%
        select(by, var_label, variable, variable_levels, redact_n),
      table_body_long %>%
        filter(!is.na(display)),
      by=c("by", "var_label", "variable", "variable_levels"="label")
    ) %>%
    group_by(by, var_label) %>%
    mutate(
      display=redactor2(redact_n, threshold, display),
      display = if_else(is.na(display), redact_chr, display)
    )

  redacted_body <-
    left_join(
      table_body_long %>% select(-display),
      redacted_stats %>% select(by, variable, var_label, variable_levels, display),
      by=c("by", "variable", "var_label", "label"="variable_levels")
    ) %>%
    mutate(
      by = fct_recode(by, .fun=!!!name_stat_by)
    ) %>%
    pivot_wider(
      id_cols = c(variable, var_type, var_class, var_label, row_type, label), # removed var_class
      names_from = by,
      values_from = display
    )

  x$table_body <- redacted_body

  x$inputs$data <- NULL
  x$inputs$meta_data <- NULL
  x$inputs$label <- NULL

  x

}