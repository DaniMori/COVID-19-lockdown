# ==============================================================================
# 
# FILE NAME:   Stata_output.R
# DESCRIPTION: Functions for processing Stata output obtained with package
#              `RStata`
# 
# AUTHOR:      Mori (danivmorillo@gmail.com)
# 
# DATE:        15/10/2020
# 
# ==============================================================================


## ---- GLOBAL OPTIONS: --------------------------------------------------------


## ---- INCLUDES: --------------------------------------------------------------

library(stringr)
library(tibble)
library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(glue)

source("R/parse_arguments.R", encoding = 'UTF-8')


## ---- CONSTANTS: -------------------------------------------------------------

# Stata table reading constants:
TABLE_DELIM_INIT   <- "init"
TABLE_DELIM_END    <- "end"
TABLE_DELIM_TYPES  <- c(TABLE_DELIM_INIT, TABLE_DELIM_END)
TABLE_CI_HEADER    <- "[95% Conf. Interval]"

# Stata table reading regular expressions:
REGEXP_NUM_VALUE      <- "-?\\d*\\.\\d+"
REGEXP_TABLE_DELIM    <- "^-+$"      # A line with only hyphens
REGEXP_HEADER_DELIM   <- "^-+\\+-+$" # A line of hyphens with one `+` in between
REGEXP_HEADER_VAR_SEP <- "(?!^)\\s+(\\|)?\\s+"
REGEXP_EFFECT_LINE    <- "^\\s*\\S+.?\\S*\\s*\\|$"
REGEXP_LINE_NAME      <- "^\\s*(\\S+.?\\S*)\\s*\\|"
REGEXP_SEP_LINE       <- "^\\s*\\|$"
# A separator can follow the pattern of an empty line, or a "header" separator:
REGEXP_SEPARATION_ANY <- glue("({REGEXP_SEP_LINE}|{REGEXP_HEADER_DELIM})")


## ---- FUNCTIONS: -------------------------------------------------------------


get_table_delims <- function(lines) {
  
  ## Argument checking and formatting: ----
  lines <- parse_char(lines)
  
  ## Main: ----
  
  init_sym <- TABLE_DELIM_INIT %>% sym()
  end_sym  <- TABLE_DELIM_END  %>% sym()
  
  lines %>%
    str_which(REGEXP_TABLE_DELIM) %>%
    enframe("table") %>%
    mutate(
      table = (table / 2) %>% ceiling(),
      type  = rep(TABLE_DELIM_TYPES, n() / 2)
    ) %>%
    spread(type, value)
}

get_lines_by_table <- function(lines) {
  
  ## Argument checking and formatting: ----
  lines <- parse_char(lines)
  
  ## Main: ----
  
  lines %>% get_table_delims() %>% {
    map2(
      .[[TABLE_DELIM_INIT]], .[[TABLE_DELIM_END]],
      ~lines[.x:.y]
    )
  }
}

get_tables <- function(lines) {
  
  ## Argument checking and formatting: ----
  lines <- parse_char(lines)
  
  ## Main: ----
  
  lines %>% get_lines_by_table() %>% map(lines_to_table)
}

lines_to_table <- function(lines) {
  
  ## Constants: ----
  VAR_COEF_LEVEL <- "var(_cons)"
  Z_STAT_HEADER  <- "z"
  P_VALUE_HEADER <- "P>|z|"
  TEST_STAT_HDRS <- c(Z_STAT_HEADER, P_VALUE_HEADER)
  
  ## Argument checking and formatting: ----
  
  delimiters <- lines %>% { c(first(.), last(.)) }
  
  lines <- parse_char(lines)
  assert_all_are_matching_regex(delimiters, REGEXP_TABLE_DELIM)
  
  
  ## Main: ----               # In case there is more than 1 (which can happen):
  header_sep   <- str_which(lines, REGEXP_HEADER_DELIM) %>% extract(1)
  header_lines <- lines[2:(header_sep - 1)]
  
  
  ## TODO: There may be another header line (e.g. "Robust" before "Std. Err.")
  hdr_names <- header_lines %>%
    last() %>%
    trimws() %>%
    str_split(REGEXP_HEADER_VAR_SEP, simplify = TRUE) %>%
    trimws()
  
  # Change confidence interval name into two names:
  ci_hdr_pos <- TABLE_CI_HEADER %>% match(hdr_names) ## TODO: should be scalar!
  if (is_not_a_na(ci_hdr_pos)) {
    
    hdr_names <- hdr_names %>% append(c("ci_inf", "ci_sup"), after = ci_hdr_pos)
    hdr_names <- hdr_names[-ci_hdr_pos]
  }
  
  body_lines <- lines %>% { extract(., (header_sep + 1):(length(.) - 1)) }
  
  sep_index    <- body_lines %>% # Match 
    str_detect(REGEXP_SEPARATION_ANY)
  effect_index <- body_lines %>% str_detect(REGEXP_EFFECT_LINE)
  
  body_indexed <- tibble(
    sep_index    = sep_index,
    effect_num   = sep_index %>% cumsum(),
    effect_index = effect_index,
    line         = body_lines
  )
  
  effects <- body_indexed %>%
    filter(effect_index) %>%      # 2nd column corresponds to the capture group:
    mutate(
      name = line %>%
        str_match(REGEXP_LINE_NAME) %>%
        extract(, 2) %>%
        trimws()
    ) %>%
    group_by(effect_num) %>%
    summarise(name = paste(name, collapse = ""), .groups = "drop")
  
  result <- body_indexed %>%
    filter(!effect_index, !sep_index) %>%
    mutate(                       # 2nd column corresponds to the capture group:
      level  = line %>%
        str_match(REGEXP_LINE_NAME) %>%
        extract(, 2) %>%
        trimws(),
      values = line %>%
        str_match_all(REGEXP_NUM_VALUE) %>%
        map(drop) %>%
        map(as.numeric) %>%
        map(enframe)
    ) %>%
    select(-sep_index, -effect_index, -line) %>%
    unnest(values) %>%
    mutate(name = hdr_names[-1][name]) %>%
    pivot_wider() %>%
    full_join(effects, by = "effect_num") %>%
    select(name, everything(), -effect_num) %>%
    mutate(
      name  = name  %>% { if_else(is.na(.), level, name) },
      level = level %>% { if_else(. == name, NA_character_, .) }
    )
  
  ## FIXME: Stata output coefficients are not properly aligned
  ##   (see this quickfix to understand the issue)
  if (
    VAR_COEF_LEVEL %in% (result %>% pull(level)) &
    any(TEST_STAT_HDRS %in% (result %>% colnames()))
  ) {
    
    result_var <- result %>% filter(level == VAR_COEF_LEVEL)
    result_var_values <- result_var %>%
      select(where(is.numeric)) %>%
      pivot_longer(cols = everything()) %>%
      filter(!is.na(value))
    result_var <- result_var %>%
      mutate(
        ci_inf = result_var_values %>% slice(n() - 1) %>% pull(),
        ci_sup = result_var_values %>% slice_tail()   %>% pull()
      ) %>%
      mutate(across(any_of(TEST_STAT_HDRS), ~NA_real_))
    
    result <- result %>%
      filter(!level %in% VAR_COEF_LEVEL) %>%
      bind_rows(result_var)
  }
  
  result %>% rename(!!sym(hdr_names[1]) := name)
}
