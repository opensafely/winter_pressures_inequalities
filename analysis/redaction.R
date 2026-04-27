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


  stopifnot("n must be non-missing" = all(!is.na(n)))
  stopifnot("n must non-negative" = all(n>=0))

  if(is.null(x)){
    x <- n
  } else {
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

# redact_tblsummary <- function(x, threshold, redact_chr=NA_character_){

#   ## function to redact a tbl_summary object
#   ## x is a tbl_summary object
#   ## threshold is the redaction threshold, passed to the redactor2 function
#   ## redact_chr is the character string used to replace redacted statistics. default is NA

#   ## the function redacts all statistics based on counts less than 5 (including means, medians, etc)
#   ## it also removes potentially disclosive items from the object, namely:
#   ##  - `x$inputs$data` which contains the input data
#   ##  - `x$inputs$meta_data` which contains the raw summary table for the table

#   stopifnot(inherits(x, "gtsummary"))

#   raw_stats <- x$cards %>%
#     bind_rows() %>%
#     mutate(
#       redact_n = if_else(is.na(n), N, n),
#       variable_levels = if_else(
#         !is.na(variable_levels), 
#         as.character(variable_levels), 
#         var_label)
#     )

#   # if(x$inputs$missing %in% c("ifany", "always")){
#   #   missing_stats <- raw_stats %>%
#   #     select(by, var_label, variable, N_miss, N_obs, N) %>%
#   #     distinct() %>%
#   #     mutate(
#   #       stat_display = "{N_miss}",
#   #       row_type = "missing",
#   #       redact_n = N_miss,
#   #       variable_levels = x$inputs$missing_text
#   #     ) 
    
#   #   if(x$inputs$missing == "ifany") {
#   #     missing_stats <- missing_stats %>% filter(N_miss > 0)
#   #   }  

#   #   raw_stats <- bind_rows(raw_stats, missing_stats)
#   # }

#   name_by_stat <- set_names(x$df_by$by_col, as.character(x$df_by$by))
#   name_stat_by <- set_names(as.character(x$df_by$by), x$df_by$by_col)

#   table_body_long <- x$table_body %>%
#     pivot_longer(cols=starts_with("stat_"), names_to="by", values_to="display") %>%
#     mutate(
#       by = fct_recode(by, !!!name_by_stat)
#     )

#   redacted_stats <-
#     left_join(
#       raw_stats %>%
#         select(by, var_label, variable, variable_levels, redact_n),
#       table_body_long %>%
#         filter(!is.na(display)),
#       by=c("by", "var_label", "variable", "variable_levels"="label")
#     ) %>%
#     group_by(by, var_label) %>%
#     mutate(
#       display=redactor2(redact_n, threshold, display),
#       display = if_else(is.na(display), redact_chr, display)
#     )

#   redacted_body <-
#     left_join(
#       table_body_long %>% select(-display),
#       redacted_stats %>% select(by, variable, var_label, variable_levels, display),
#       by=c("by", "variable", "var_label", "label"="variable_levels")
#     ) %>%
#     mutate(
#       by = fct_recode(by, !!!name_stat_by)
#     ) %>%
#     pivot_wider(
#       id_cols = c(variable, var_type, var_label, row_type, label), # removed var_class
#       names_from = by,
#       values_from = display
#     )

#   x$table_body <- redacted_body

#   x$inputs$data <- NULL
#   #x$inputs$table_body <- NULL
#   #x$inputs$label <- NULL

#   x

# }
# Copied from metformin_covid repo
################################################################################
# Custom made function to round a tbl_summary object, it does the following:
# Take a tbl_summary() object as input and a threshold for redaction
# Redact categorical/dichotomous counts, including the total column total (second row N Total)
# Recalculate percentages for subgroup rows, based on the redacted column total
# Leave continuous variables untouched
# Return a clean tibble with renamed columns
## CAVE: inside uses the general midpoint rounding function: fn_roundmid_any
################################################################################
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

fn_redact_tbl_summary <- function(tbl_summary_obj, 
                                  threshold = 6,
                                  col_names = c("stat_1", "stat_2"),
                                  rename_cols = c("group1", "group2")) {
  
  # Step 0: extract table body as tibble
  tbl_df <- as_tibble(tbl_summary_obj$table_body)
  
  # Step 1: round counts for categorical/dichotomous rows
  # takes off percentage in brackets and rounds all numbers (regardless of whether will be redacted or not)
  tbl_df <- tbl_df %>%
    mutate(
      across(
        all_of(col_names),
        ~ ifelse(
          var_type %in% c("categorical", "dichotomous"),
          map_chr(.x, function(cell) {
            if (is.na(cell) || cell == "") return(cell)
            txt <- str_replace_all(cell, ",", "")
            txt <- str_trim(txt)
            count <- suppressWarnings(as.numeric(str_extract(txt, "^[0-9]+")))
            if (is.na(count)) return(cell)
            ifelse(cell <=threshold, NA_real_, roundmid_any(count)) %>% as.character()
          }),
          .x
        )
      )
    )
  
  # Step 2: extract numeric counts dynamically
  count_col_names <- paste0(col_names, "_count")
  
  for (i in seq_along(col_names)) {
    tbl_df[[count_col_names[i]]] <- tbl_df[[col_names[i]]] %>%
      str_remove_all(",") %>%
      str_extract("^[0-9]+") %>%
      as.numeric()
  }
  
  # Step 3: get redacted totals dynamically
  # totals <- sapply(count_col_names, function(count_col) {
  #   tbl_df %>%
  #     filter(var_label == "Overall" & var_type == "categorical" & row_type == "level") %>%
  #     pull(count_col) %>%
  #     as.numeric()
  # })
  
  # Step 4: recalc percentages dynamically
  print("stat_0_count" %in% names(tbl_df))
  overall_counts <- tbl_df[["stat_0_count"]]  # extract once outside the loop

  for (i in seq_along(col_names)) {
    count_col <- count_col_names[i]
    #total     <- totals[i]
    
    tbl_df <- tbl_df %>%
      mutate(
        !!col_names[i] := ifelse(
          var_type %in% c("categorical", "dichotomous") &
            !var_label %in% c("Total N", "N") &
            !is.na(.data[[count_col]]),
          paste0(
            formatC(.data[[count_col]], format = "f", digits = 0, big.mark = ","),
            " (", round(100 * .data[[count_col]] / overall_counts, 1), "%)"
          ),
          !!sym(col_names[i])
        )
      )
  }
  
  # Drop count columns
  tbl_df <- tbl_df %>%
    select(-all_of(count_col_names))
  
  # Step 5: rename dynamically
  rename_mapping <- setNames(col_names, rename_cols[seq_along(col_names)])
  tbl_df <- tbl_df %>%
    rename(!!!rename_mapping)
  
  return(tbl_df)
}

# fn_redact_tbl_summary <- function(tbl_summary_obj, threshold = 7, # default threshold
#                                col_names = c("stat_0", "stat_1", "stat_2", "stat_3", "stat_4", "stat_5", "stat_6"),
#                                rename_cols = c("Overall", "Asian", "Black", "Other", "Missing", "Mixed", "White")) {
  
#   # Step 0: extract table body as tibble
#   tbl_df <- as_tibble(tbl_summary_obj$table_body)
  
#   # Step 1: redact counts for categorical/dichotomous rows
#   tbl_df <- tbl_df %>%
#     mutate(
#       across(
#         all_of(col_names),
#         ~ ifelse(
#           var_type %in% c("categorical", "dichotomous"),
#           map_chr(.x, function(cell) {
#             if (is.na(cell) || cell == "") return(cell)
#             txt <- str_replace_all(cell, ",", "")
#             txt <- str_trim(txt)
#             count <- suppressWarnings(as.numeric(str_extract(txt, "^[0-9]+")))
#             if (is.na(count)) return(cell)
#             roundmid_any(count, threshold) %>% as.character()
#           }),
#           .x
#         )
#       )
#     )
#     # Step 2: extract numeric counts
#     # Generate names for count columns
#     count_col_names <- paste0("stat_", seq_along(col_names), "_count")

#     for (i in seq_along(col_names)) {
#       tbl_df <- tbl_df %>% 
#         mutate(
#         !!count_col_names[i] <- suppressWarnings(as.numeric(!!sym(col_names[i])))
#         # stat_1_count = suppressWarnings(as.numeric(!!sym(col_names[1]))),
#         # stat_2_count = suppressWarnings(as.numeric(!!sym(col_names[2]))),
#         # stat_3_count = suppressWarnings(as.numeric(!!sym(col_names[3]))),
#         # stat_4_count = suppressWarnings(as.numeric(!!sym(col_names[4]))),
#         # stat_5_count = suppressWarnings(as.numeric(!!sym(col_names[5]))),
#         # stat_6_count = suppressWarnings(as.numeric(!!sym(col_names[6]))),
#       )
#     }
  
  # # Step 3: get redacted totals as scalars
  # total_stat_1 <- tbl_df %>%
  #   filter(var_label == "Total N" & var_type == "categorical" & row_type == "level") %>%
  #   pull(stat_1_count) %>% as.numeric()
  
  # total_stat_2 <- tbl_df %>%
  #   filter(var_label == "Total N" & var_type == "categorical" & row_type == "level") %>%
  #   pull(stat_2_count) %>% as.numeric()
  
  # total_stat_3 <- tbl_df %>%
  #   filter(var_label == "Total N" & var_type == "categorical" & row_type == "level") %>%
  #   pull(stat_3_count) %>% as.numeric()
  
  # total_stat_4 <- tbl_df %>%
  #   filter(var_label == "Total N" & var_type == "categorical" & row_type == "level") %>%
  #   pull(stat_4_count) %>% as.numeric()

  # total_stat_5 <- tbl_df %>%
  #   filter(var_label == "Total N" & var_type == "categorical" & row_type == "level") %>%
  #   pull(stat_5_count) %>% as.numeric()
  
  # total_stat_6 <- tbl_df %>%
  #   filter(var_label == "Total N" & var_type == "categorical" & row_type == "level") %>%
  #   pull(stat_6_count) %>% as.numeric()
  
  # # Step 4: recalc percentages for non-total categorical/dichotomous rows
  # tbl_df <- tbl_df %>%
  #   mutate(
  #     !!col_names[1] := ifelse(
  #       var_type %in% c("categorical", "dichotomous") & !var_label %in% c("Total N", "N") & !is.na(stat_1_count),
  #       paste0(formatC(stat_1_count, format = "f", digits = 0, big.mark = ","),
  #              " (", round(100 * stat_1_count / total_stat_1, 1), "%)"),
  #       !!sym(col_names[1])
  #     ),
  #     !!col_names[2] := ifelse(
  #       var_type %in% c("categorical", "dichotomous") & !var_label %in% c("Total N", "N") & !is.na(stat_2_count),
  #       paste0(formatC(stat_2_count, format = "f", digits = 0, big.mark = ","),
  #              " (", round(100 * stat_2_count / total_stat_2, 1), "%)"),
  #       !!sym(col_names[2])
  #     ),
  #     !!col_names[3] := ifelse(
  #       var_type %in% c("categorical", "dichotomous") & !var_label %in% c("Total N", "N") & !is.na(stat_3_count),
  #       paste0(formatC(stat_3_count, format = "f", digits = 0, big.mark = ","),
  #              " (", round(100 * stat_3_count / total_stat_3, 1), "%)"),
  #       !!sym(col_names[3])
  #     ),
  #     !!col_names[4] := ifelse(
  #       var_type %in% c("categorical", "dichotomous") & !var_label %in% c("Total N", "N") & !is.na(stat_4_count),
  #       paste0(formatC(stat_4_count, format = "f", digits = 0, big.mark = ","),
  #              " (", round(100 * stat_4_count / total_stat_4, 1), "%)"),
  #       !!sym(col_names[4])
  #     ),
  #     !!col_names[5] := ifelse(
  #       var_type %in% c("categorical", "dichotomous") & !var_label %in% c("Total N", "N") & !is.na(stat_5_count),
  #       paste0(formatC(stat_5_count, format = "f", digits = 0, big.mark = ","),
  #              " (", round(100 * stat_5_count / total_stat_5, 1), "%)"),
  #       !!sym(col_names[5])
  #     ),
  #     !!col_names[6] := ifelse(
  #       var_type %in% c("categorical", "dichotomous") & !var_label %in% c("Total N", "N") & !is.na(stat_6_count),
  #       paste0(formatC(stat_6_count, format = "f", digits = 0, big.mark = ","),
  #              " (", round(100 * stat_6_count / total_stat_6, 1), "%)"),
  #       !!sym(col_names[6])
  #     )
  #   ) %>%
  #   select(-stat_1_count, -stat_2_count, -stat_3_count, -stat_4_count, -stat_5_count, -stat_6_count) %>%
  #   # Step 5: optional rename for output clarity
  #   rename(!!rename_cols[1] := !!sym(col_names[1]),
  #          !!rename_cols[2] := !!sym(col_names[2]),
  #          !!rename_cols[3] := !!sym(col_names[3]),
  #          !!rename_cols[4] := !!sym(col_names[4]),
  #          !!rename_cols[5] := !!sym(col_names[5]),
  #          !!rename_cols[6] := !!sym(col_names[6]),
  #          !!rename_cols[7] := !!sym(col_names[7]))
  
  #return(tbl_df)
#}