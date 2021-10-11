# ==============================================================================
# 
# FILE NAME:   Output.R
# DESCRIPTION: Functions for output formatting
# 
# AUTHOR:      Mori (danivmorillo@gmail.com)
# 
# DATE:        21/04/2020
# 
# ==============================================================================


## ---- GLOBAL OPTIONS: --------------------------------------------------------

library(pacman)


## ---- INCLUDES: --------------------------------------------------------------

p_load(tidyverse, magrittr, janitor, glue, scales, rlang)

source("R/parse_arguments.R")


## ---- CONSTANTS: -------------------------------------------------------------

QUOTING_CHARS    <- c(
  '*', '**', '"', "'", '_', '~', '`', '(', '[', '{', '^', '__'
)
QT_CLOSING_CHARS <- c(
  '*', '**', '"', "'", '_', '~', '`', ')', ']', '}', '^', '__'
) %>% 
  set_names(QUOTING_CHARS)

INTERCEPT_TERM <- "(Intercept)"

FOOTNOTE_SYMBOL <- letters


## ---- FUNCTIONS: -------------------------------------------------------------

### Data variables and values outputs: ----

enumerate_levels <- function(.data, var, last = ", and ", italics = FALSE) {
  
  ## Argument checking and formatting: ----
  
  var <- enquo(var)
  
  assert_is_data.frame(.data)
  parse_varnames(as_label(var), .data)
  
  var <- .data %>% pull(!!var)
  
  assert_class_is_one_of(var, c("character", "factor"))
  if (is.character(var)) var <- factor(var)
  
  italics <- parse_bool(italics)
  
  
  ## Main: ----
  result <- var %>% levels()
  
  if (italics) result <- result %>% enclose("*")
  
  result %>% glue_collapse(sep = ", ", last = last)
}

### Statistical tables: ----

frequencies_table <- function(.data,
                              .segmentation,
                              missing   = TRUE,
                              pct_total = missing,
                              pct_valid = !missing,
                              miss_cat  = "(Missing)",
                              totals    = !missing) {
  ## Constants: ----
  VALID_PCT_HDR <- "valid_percent"
  
  ## Argument checking and formatting: ----
  
  assert_is_data.frame(.data)
  
  totals    <- totals    %>% parse_bool()
  missing   <- missing   %>% parse_bool()
  miss_cat  <- miss_cat  %>% parse_string()
  pct_valid <- pct_valid %>% parse_bool()
  pct_total <- pct_total %>% parse_bool()
  if (!missing) pct_total <- FALSE # If no missing, don't compute % over total
  
  if (is_not_missing(.segmentation)) {
    
    .segmentation <- enquo(.segmentation)
    
    parse_varnames(as_label(.segmentation), .data)
    
    if (missing) {
      
      .data <- .data %>% mutate_at(vars(!!.segmentation), fct_explicit_na)
    }
    
  } else {
    
    .segmentation <- NULL
  }
  
  
  ## Main: ----
  
  .data <- .data %>% select_if(is.factor)
  
  result <- .data %>% select(-!!.segmentation) %>% imap(
    ~{
      if (!missing) .data <- .data %>% filter_at(.y, is_not_na)
      
      result <- if (is_null(.segmentation)) {
        
        res <- tabyl(.data, !!sym(.y))
        
        if (missing) {
          
          res <- res %>% mutate_at(.y, fct_explicit_na, na_level = miss_cat)
        }
        
        if (!"valid_percent" %in% colnames(res)) {
          
          res <- res %>% mutate(valid_percent = percent)
        }
        
        if (!pct_total) res <- res %>% select(-percent)
        if (!pct_valid) res <- res %>% select(-valid_percent)
        
        res
        
      } else {
        
        tabyl(.data, !!sym(.y), !!.segmentation)
      }
      
      ## TODO: totals if (is_not_null(.segmentation)) ??
      if (totals) {
        
        result <- result %>%
          adorn_totals() %>%
          mutate_at(.y, fct_inorder) %>%
          mutate_at(.y, fct_relevel, "Total") %>%
          arrange(!!sym(.y))
      }
      
      result <- result %>%
        rename_at(1, ~"Level") %>%
        mutate(Level = Level %>% as.character())
      
      result <- result %>% {
        
        if (is_not_null(.segmentation)) {
          
          if (!pct_total & !pct_valid) {
            
            .
            
          } else {
            
            total_pcts <- if (pct_total) {
              
              adorn_percentages(.) %>%
                adorn_pct_formatting(2) %>%
                rename_with(paste0, -Level, "_pct")
            }
            
            valid_pcts <- if (pct_valid) {
              
              filter(., Level != "(Missing)") %>%
                adorn_percentages() %>%
                adorn_pct_formatting(2) %>%
                rename_with(paste0, -Level, "_valid_pct")
            }
            
            pcts <- if (pct_total & pct_valid) {
              
              total_pcts %>% full_join(valid_pcts, by = "Level")
              
            } else {
              
              if (pct_total) total_pcts else valid_pcts
            }
            
            full_join(
              adorn_totals(., where = "col"),
              # adorn_percentages(.) %>% adorn_pct_formatting(2),
              pcts,
              by     = "Level"
              # suffix = c("", "_pct")
            )
          }
          
        } else {
          
          if (any(pct_total, pct_valid)) {
            
            adorn_pct_formatting(., 2, ... = -(Level:n))
            
          } else {
            
            .
          }
        }
      }
      
      result
    }
  )
  
  suppressWarnings(result <- result %>% bind_rows(.id = "Variable"))
  
  result <- if (is_not_null(.segmentation)) {
    
    result %>%
      select(
        any_of(
          c(
            "Variable",
            "Level",
            .data %>%
              pull(!!.segmentation) %>%
              levels() %>%
              outer(c("", "_pct", "_valid_pct"), paste0) %>%
              t(),
            "Total"
          )
        )
      ) %>%
      mutate_at(vars(ends_with("_pct")), ~as.character(glue("({.})"))) %>%
      group_by(Variable) # %>%
    # mutate(Total_pct = paste0('(', Total / sum(Total)) %>% percent(1e-2), ')')
    
  } else {
    
    result <- result %>% rename(N = n)
    
    if (pct_total) result <- result %>% rename(Percent = percent)
    if (pct_valid) result <- result %>% rename(`Percent valid` = valid_percent)
    
    result
  }
  
  result# %>%
    # mutate(group = Variable)                                           %>%
    # group_by(group)                                                    %>%
    # ungroup()                                                          %>%
    # select(-group)
}

correlations_table <- function(.data, significance = FALSE) {
  
  ## Argument checking and formatting: ----
  assert_is_data.frame(.data)
  significance <- significance %>% parse_bool()
  
  
  ## Main: ----
  
  .data <- .data %>% select_if(is.numeric)
  
  result <- .data %>% cor(use = "pairwise.complete.obs")
  
  result[upper.tri(result, diag = TRUE)] <- NA
  
  result %>% data.frame(check.names = FALSE) %>%
    rownames_to_column("Variable")           %>%
    as_tibble()                              %>%
    select_at(-length(.))                    %>%
    slice(-1)                                %>%
    mutate_if(is.numeric, label_number(1e-3))
}


### Statistical values output formatting: ----

format_prop_like <- function(values, sig = 3, drop_0 = TRUE) {
  
  ## Constants: ----
  LEADING_ZERO_PATTERN <- "0(?=\\.)"
  
  ## Argument checking and formatting: ----
  
  assert_is_numeric(values)
  assert_all_are_in_closed_range(values, -1L, 1L)
  
  sig    <- parse_whole_number(sig)
  drop_0 <- parse_bool(drop_0)

  
  ## Main: ----
  
  result <- values %>% round(digits = sig) %>%
    format(digits = sig, nsmall = sig)
  
  if (drop_0) result <- result %>% str_remove(LEADING_ZERO_PATTERN)
  
  result
}

print_pvalue <- function(p_value,
                         sig = 3,
                         drop_0    = TRUE,
                         preffix   = TRUE,
                         italics   = TRUE,
                         underline = FALSE) {
  ## Constants: ----
  LEADING_ZERO_PATTERN <- "0(?=\\.)"
  P_SYMBOL             <- "p"
  OUTPUT_PATTERN       <- "{formatted_p}-value {eq}{result}"
  TRUNCATION_PATTERN   <- "^< "
  EQUAL_SYMBOL         <- "= "
  
  
  ## Argument checking and formatting: ----
  
  p_value <- parse_scalar(p_value)
  assert_is_in_range(
    p_value,
    0L, 1L,
    lower_is_strict = FALSE, upper_is_strict = FALSE
  )
  
  sig       <- parse_whole_number(sig)
  drop_0    <- parse_bool(drop_0)
  preffix   <- parse_bool(preffix)
  italics   <- parse_bool(italics)
  underline <- parse_bool(underline)
  
  
  ## Main: ----
  formatted_p <- P_SYMBOL
  if (italics)   formatted_p <- formatted_p %>% enclose('*')
  if (underline) formatted_p <- formatted_p %>% enclose('__')
  
  result <- p_value %>% format_pvalues(sig, drop_0)

  if (preffix) {
    
    eq <- if (result %>% str_detect(TRUNCATION_PATTERN)) "" else EQUAL_SYMBOL
    glue(OUTPUT_PATTERN)
    
  } else {
    
    result # %>% str_remove(TRUNCATION_PATTERN) ## TODO: Check this?
  }
}

format_pvalues <- function(p_values, sig = 3, drop_0 = TRUE) {
  
  ## Constants: ----
  LEADING_ZERO_PATTERN <- "[ ]?0(?=\\.)"

  ## Argument checking and formatting: ----
  
  assert_is_numeric(p_values)
  assert_all_are_in_closed_range(p_values, 0L, 1L, na_ignore = TRUE)
  
  sig     <- parse_whole_number(sig)
  drop_0  <- parse_bool(drop_0)

  
  ## Main: ----
  result <- p_values    %>%
    round(digits = sig) %>%
    format.pval(eps = 10^(-sig), nsmall = sig, na.form = '')
  
  result <- result %>% str_replace(
    LEADING_ZERO_PATTERN,
    if (drop_0) " " else " 0"
  )

  result
}

print_z_test <- function(z_test,
                         sig         = 2,
                         p_sig       = 3,
                         drop_0      = TRUE,
                         italics     = FALSE,
                         underline_p = FALSE) {
  ## Constants: ----
  Z_STAT         <- sym("statistic")
  P_VALUE        <- sym("p.value")
  OUTPUT_PATTERN <- "{stat_name} = {z_stat}, {p_value}"
  STAT_NAME      <- "z"
  ITALICS_MOD    <- glue("*{STAT_NAME}*")
  
  
  ## Argument checking and formatting: ----
  
  assert_is_data.frame(z_test)
  assert_are_identical(z_test %>% nrow(), 1L)
  parse_varnames(c(Z_STAT, P_VALUE), z_test)
  
  p_sig       <- parse_whole_number(p_sig)
  sig         <- parse_whole_number(sig)
  drop_0      <- parse_bool(drop_0)
  italics     <- parse_bool(italics)
  underline_p <- parse_bool(underline_p)
  
  
  ## Main: ----
  
  stat_name <- if (italics) ITALICS_MOD else STAT_NAME
  
  z_stat  <- z_test %>% pull(!!Z_STAT)  %>% number(10^(-sig))
  p_value <- z_test %>% pull(!!P_VALUE) %>%
    print_pvalue(
      sig       = p_sig,
      drop_0    = drop_0,
      italics   = italics,
      underline = underline_p
    )
  
  glue(OUTPUT_PATTERN)
}

print_t_test <- function(t_test,
                         sig         = 2,
                         p_sig       = 3,
                         drop_0      = TRUE,
                         stat_var    = "statistic",
                         pval_var    = "p.value",
                         df_var      = "parameter",
                         italics     = FALSE,
                         underline_p = FALSE) {
  ## Constants: ----
  OUTPUT_PATTERN <- "${stat_name}_{{{df}}}$ = {t_stat}, {p_value}"
  STAT_NAME      <- "t"
  UPRIGHT_MOD    <- glue("\\mathrm{{{STAT_NAME}}}")
  
  ## Argument checking and formatting: ----
  stat_var <- sym(stat_var)
  pval_var <- sym(pval_var)
  df_var   <- sym(df_var)
  
  assert_is_data.frame(t_test)
  assert_are_identical(t_test %>% nrow(), 1L)
  parse_varnames(c(stat_var, pval_var, df_var), t_test)
  
  p_sig       <- parse_whole_number(p_sig)
  sig         <- parse_whole_number(sig)
  drop_0      <- parse_bool(drop_0)
  italics     <- parse_bool(italics)
  underline_p <- parse_bool(underline_p)
  

  ## Main: ----
  
  stat_name <- if (italics) STAT_NAME else UPRIGHT_MOD
  
  t_stat  <- t_test %>% pull(!!stat_var)  %>% number(10^(-sig))
  p_value <- t_test %>% pull(!!pval_var) %>%
    print_pvalue(
      sig       = p_sig,
      drop_0    = drop_0,
      italics   = italics,
      underline = underline_p)
  
  df      <- t_test %>% pull(!!df_var)
  df_prec <- if (is_a_whole_number(df)) 0 else -2
  df      <- df %>% number(., 10^df_prec, big.mark = '')
  
  glue(OUTPUT_PATTERN)
}

print_chisq_test <- function(test,
                             sig         = 2,
                             p_sig       = 3,
                             drop_0      = TRUE,
                             stat_var    = "statistic",
                             pval_var    = "p.value",
                             df_var      = "parameter",
                             italics     = FALSE,
                             underline_p = FALSE) {
  ## Constants: ----
  OUTPUT_PATTERN <- "${stat_name}^2{df}$ = {chisq_stat}, {p_value}"
  STAT_NAME      <- "\\chi"
  UPRIGHT_MOD    <- glue("\\mathrm{{{STAT_NAME}}}")

  ## Argument checking and formatting: ----
  
  assert_is_data.frame(test)
  assert_are_identical(test %>% nrow(), 1L)
  
  stat_var <- parse_string(stat_var) %>% parse_varnames(test, sym = "yes")
  pval_var <- parse_string(pval_var) %>% parse_varnames(test, sym = "yes")
  df_var   <- parse_string(df_var)   %>% parse_varnames(test, sym = "yes")
  
  sig         <- parse_whole_number(sig)
  p_sig       <- parse_whole_number(p_sig)
  drop_0      <- parse_bool(drop_0)
  italics     <- parse_bool(italics)
  underline_p <- parse_bool(underline_p)
  
  
  ## Main: ----
  
  stat_name <- if (italics) STAT_NAME else UPRIGHT_MOD
  
  chisq_stat <- test %>% pull(!!stat_var) %>% number(10^(-sig))
  p_value    <- test %>% pull(!!pval_var) %>%
    print_pvalue(
      sig       = p_sig,
      drop_0    = drop_0,
      italics   = italics,
      underline = underline_p
    )
  df         <- test %>% pull(!!df_var) %>% unname()
  df <- if (df %>% is_identical_to_na()) "" else glue("_{{{df}}}")
  
  glue(OUTPUT_PATTERN)
}

print_F_test <- function(test,
                         sig         = 2,
                         p_sig       = 3,
                         drop_0      = TRUE,
                         stat_var    = "statistic",
                         pval_var    = "p.value",
                         df_num_var  = "df",
                         df_den_var  = "df_Residuals",
                         italics     = FALSE,
                         underline_p = FALSE) {
  ## Constants: ----
  OUTPUT_PATTERN <- "${stat_name}_{{{df_num}, {df_den}}}$ = {F_stat}, {p_value}"
  STAT_NAME      <- "F"
  UPRIGHT_MOD    <- glue("\\mathrm{{{STAT_NAME}}}")
  
  ## Argument checking and formatting: ----
  
  assert_is_data.frame(test)
  assert_are_identical(test %>% nrow(), 1L)
  
  stat_var   <- parse_string(stat_var)   %>% parse_varnames(test, sym = "yes")
  pval_var   <- parse_string(pval_var)   %>% parse_varnames(test, sym = "yes")
  df_num_var <- parse_string(df_num_var) %>% parse_varnames(test, sym = "yes")
  df_den_var <- parse_string(df_den_var) %>% parse_varnames(test, sym = "yes")
  
  sig         <- parse_whole_number(sig)
  p_sig       <- parse_whole_number(p_sig)
  drop_0      <- parse_bool(drop_0)
  italics     <- parse_bool(italics)
  underline_p <- parse_bool(underline_p)
  
  
  ## Main: ----
  
  stat_name <- if (italics) STAT_NAME else UPRIGHT_MOD
  
  F_stat  <- test %>% pull(!!stat_var) %>% number(10^(-sig))
  p_value <- test %>% pull(!!pval_var) %>%
    print_pvalue(
      sig       = p_sig,
      drop_0    = drop_0,
      italics   = italics,
      underline = underline_p)
  
  df_num  <- test %>% pull(!!df_num_var) %>% unname()
  df_den  <- test %>% pull(!!df_den_var) %>% unname()

  glue(OUTPUT_PATTERN)
}

print_cor_test <- function(test,
                           est_sig     = 3,
                           stat_sig    = 2,
                           p_sig       = 3,
                           drop_0      = TRUE,
                           estimate    = TRUE,
                           est_var     = "estimate",
                           stat_var    = "statistic",
                           pval_var    = "p.value",
                           df_var      = "parameter",
                           italics     = FALSE,
                           underline_p = FALSE) {
  ## Constants: ----
  OUTPUT_PATTERN_EST <- "${est_name}$ = {cor_est} "
  OUTPUT_PATTERN     <- "${stat_name}{df}$ = {cor_stat}, {p_value}"
  EST_NAME           <- "\\rho"
  UPRIGHT_EST        <- glue("\\mathrm{{{EST_NAME}}}")
  STAT_NAME          <- "t"
  UPRIGHT_STAT       <- glue("\\mathrm{{{STAT_NAME}}}")
  
  
  ## Argument checking and formatting: ----
  
  assert_is_data.frame(test)
  assert_are_identical(test %>% nrow(), 1L)
  
  est_var  <- parse_string(est_var)  %>% parse_varnames(test, sym = "yes")
  stat_var <- parse_string(stat_var) %>% parse_varnames(test, sym = "yes")
  pval_var <- parse_string(pval_var) %>% parse_varnames(test, sym = "yes")
  df_var   <- parse_string(df_var)   %>% parse_varnames(test, sym = "yes")
  
  est_sig     <- parse_whole_number(est_sig)
  stat_sig    <- parse_whole_number(stat_sig)
  p_sig       <- parse_whole_number(p_sig)
  drop_0      <- parse_bool(drop_0)
  estimate    <- parse_bool(estimate)
  italics     <- parse_bool(italics)
  underline_p <- parse_bool(underline_p)
  
  
  ## Main: ----
  
  es_name   <- if (italics) EST_NAME  else glue(UPRIGHT_EST)
  stat_name <- if (italics) STAT_NAME else glue(UPRIGHT_STAT)
  
  cor_est  <- test %>% pull(!!est_var)  %>% format_prop_like(est_sig)
  cor_stat <- test %>% pull(!!stat_var) %>% number(10^(-stat_sig))
  p_value  <- test %>% pull(!!pval_var) %>%
    print_pvalue(
      sig       = p_sig,
      drop_0    = drop_0,
      italics   = italics,
      underline = underline_p
    )
  
  df         <- test %>% pull(!!df_var) %>% unname()
  df <- if (df %>% is_identical_to_na()) "" else glue("_{{{df}}}")
  
  if (estimate) {
    
    OUTPUT_PATTERN <- paste0(OUTPUT_PATTERN_EST, "(", OUTPUT_PATTERN, ")")
  }
  
  glue(OUTPUT_PATTERN)
}

print_lr_test <- function(test,
                          sig         = 2,
                          p_sig       = 3,
                          drop_0      = TRUE,
                          italics     = FALSE,
                          underline_p = FALSE) {
  ## Constants: ----
  CHISQ_STAT     <- sym("Chisq")
  DF             <- sym("Df")
  P_VALUE        <- sym("P(>|Chi|)")
  OUTPUT_PATTERN <- "${stat_name}^2{df}$ = {chisq_stat}, {p_value}"
  STAT_NAME      <- "\\chi"
  UPRIGHT_MOD    <- glue("\\mathrm{{{STAT_NAME}}}")
  
  ## Argument checking and formatting: ----
  
  assert_is_data.frame(test)
  assert_are_identical(test %>% nrow(), 2L)
  parse_varnames(c(CHISQ_STAT, P_VALUE, DF), test)
  test <- test %>% slice(2)
  
  p_sig       <- parse_whole_number(p_sig)
  sig         <- parse_whole_number(sig)
  drop_0      <- parse_bool(drop_0)
  italics     <- parse_bool(italics)
  underline_p <- parse_bool(underline_p)
  
  
  ## Main: ----

  stat_name <- if (italics) STAT_NAME else UPRIGHT_MOD
  
  chisq_stat <- test %>% pull(!!CHISQ_STAT) %>% number(10^(-sig))
  df         <- test %>% pull(!!DF)
  p_value    <- test %>% pull(!!P_VALUE) %>%
    print_pvalue(
      sig       = p_sig,
      drop_0    = drop_0,
      italics   = italics,
      underline = underline_p
    )
  
  glue(OUTPUT_PATTERN)
}

print_lavaan_lr_test <- function(test,
                                 sig         = 2,
                                 p_sig       = 3,
                                 drop_0      = TRUE,
                                 italics     = FALSE,
                                 underline_p = FALSE) {
  ## Constants: ----
  CHISQ_STAT     <- sym("Chisq diff")
  DF             <- sym("Df diff")
  P_VALUE        <- sym("Pr(>Chisq)")
  OUTPUT_PATTERN <- "${stat_name}^2_{df}$ = {chisq_stat}, {p_value}"
  STAT_NAME      <- "\\chi"
  UPRIGHT_MOD    <- glue("\\mathrm{{{STAT_NAME}}}")

  ## Argument checking and formatting: ----
  
  assert_is_data.frame(test)
  assert_are_identical(test %>% nrow(), 2L)
  parse_varnames(c(CHISQ_STAT, P_VALUE, DF), test)
  test <- test %>% slice(2)
  
  p_sig       <- parse_whole_number(p_sig)
  sig         <- parse_whole_number(sig)
  drop_0      <- parse_bool(drop_0)
  italics     <- parse_bool(italics)
  underline_p <- parse_bool(underline_p)
  
  
  ## Main: ----

  stat_name <- if (italics) STAT_NAME else UPRIGHT_MOD
  
  chisq_stat <- test %>% pull(!!CHISQ_STAT) %>% number(10^(-sig))
  df         <- test %>% pull(!!DF)
  p_value    <- test %>% pull(!!P_VALUE)    %>%
    print_pvalue(
      sig       = p_sig,
      drop_0    = drop_0,
      italics   = italics,
      underline = underline_p
    )
  
  glue(OUTPUT_PATTERN)
}

print_ci <- function(lower, upper, sig = 3, quoting = '[') {
  
  ## Constants: ----
  SEPARATOR <- " - "
  
  ## Argument checking and formatting: ----
  
  assert_is_a_number(lower)
  assert_is_a_number(upper)
  assert_is_a_natural_number(sig)
  
  quoting <- quoting %>% match.arg(QUOTING_CHARS)
  
  
  ## Main: ----
  format_ci(lower, upper, sig, quoting)
}

format_ci <- function(lower, upper, sig = 3, quoting = '[') {
  
  ## Constants: ----
  SEPARATOR <- " - "
  
  ## Argument checking and formatting: ----
  
  assert_is_numeric(lower)
  assert_is_numeric(upper)
  # assert_is_a_natural_number(sig) ## TODO: assertive.extra not compiled in 3.6.3
  
  quoting <- quoting %>% match.arg(QUOTING_CHARS)
  
  
  ## Main: ----
  paste(
    lower %>% number(10^-sig, big.mark = ""),
    upper %>% number(10^-sig, big.mark = ""),
    sep = SEPARATOR
  ) %>%
    enclose(quoting)
}

enclose <- function(strings, quoting = QUOTING_CHARS, omit_na = TRUE) {
  
  ## Argument checking and formatting: ----
  strings <- strings %>% parse_char()
  quoting <- match.arg(quoting)
  omit_na <- omit_na %>% parse_bool()

  ## Main: ----
  result <- paste0(quoting, strings, QT_CLOSING_CHARS[quoting])
  
  if (omit_na) result[is.na(strings)] <- NA_character_
  
  result
}


### Model fitting output table formatting: ----

format_term_label <- function(coef_table, .data, .labels, add_ref = TRUE) {

  ## Constants: ----
  INTERCEPT_TERM <- "(Intercept)"
  
  ## Argument checking and formatting: ----
  
  assert_is_data.frame(coef_table)
  parse_varnames("term", coef_table)
  
  assert_is_data.frame(.data)
  
  if (missing(.labels)) {
    
    .labels <- .data %>% map(attr, "label") %>% flatten_chr()
  }
  assert_is_character(.labels)
  assert_has_names(.labels)
  
  add_ref <- parse_bool(add_ref)
  
  
  ## Main: ----
  
  vars_cats_terms <- .data %>%
    terms_from_data() %>%
    mutate(is_cat = !is.na(cat)) %>%
    group_by(var) %>%
    mutate(
      ref = first(cat),
      Term = if_else(
        condition = term == INTERCEPT_TERM,
        true      = term,
        false     = .labels[var]
      ),
      Term = if_else(
          condition = is_cat & n() > 2,
          true      = if_else(
            condition = ref == cat,
            true      = .labels[var] %>% paste0(" (Ref. ", first(ref), ")"),
            false     = cat
          ),
          false     = Term
        ),
      indent = is_cat & n() > 2 & ref != cat
    )
  
  result <- coef_table %>%
    left_join(vars_cats_terms, by = "term") %>%
  # FIXME: When `var` and `cat` already exist?
    mutate(
      indent = Term %>% is.na() %>% if_else(FALSE, indent),
      Term   = Term %>% if_else(is.na(.), term, .)
    )
  
  multiple_cat_titles <- vars_cats_terms %>%
    filter(!indent, n() > 2) %>%
    semi_join(result, by = "var")
  
  result <- result %>%
    bind_rows(multiple_cat_titles) %>%
    mutate(var = var %>% parse_factor()) %>%
    arrange(var, indent) %>%
    mutate(var = var %>% as.character())
  
  ## TODO: Add reference category "per term"?
  if (add_ref) {

    result <- result %>% mutate(
      Term = if_else(
        is.na(cat),
        cat,
        paste0(Term, " (Ref. ", ref_level[.], ")")
      )
    )
  }

  result %>% select(-(var:ref))
}

vars_cats_from_terms <- function(coef_table, .data) {
  
  ## Argument checking and formatting: ----
  
  assert_is_data.frame(coef_table)
  parse_varnames("term", coef_table)
  
  assert_is_data.frame(.data)
  

  ## Main: ----
  coef_table %>% left_join(.data %>% terms_from_data(), by = "term")
}

terms_from_data <- function(.data) {
  
  ## Argument checking and formatting: ----
  assert_is_data.frame(.data)
  
  ## Main: ----
  .data %>% ## TODO: remove blanks??
    map(levels) %>%
    map(`%||%`, NA_character_) %>%
    enframe("var", "cat") %>%
    unnest(cat) %>%
    unite(term, var, cat, sep = "", remove = FALSE, na.rm = TRUE)
}

order_terms_with_data <- function(coef_table, .data) {
  
  ## Argument checking and formatting: ----
  
  assert_is_data.frame(coef_table)
  parse_varnames("term", coef_table)
  
  assert_is_data.frame(.data)
  
  
  ## Main: ----
  terms <- .data %>% terms_from_data() %>% pull(term) %>%
    intersect(coef_table %>% pull(term))
  
  coef_table %>%
    mutate(term = term %>% factor(levels = c(INTERCEPT_TERM, terms))) %>%
    arrange(term)
}

### Time format functions: ----
min_format <- function(x) { # TODO: Round up??
  x %>% as_datetime() %>% label_time(glue("%M'"))()
}
minsec_format <- function(x, digits = 0) {
  x %>% as_datetime() %>% label_time(glue("%M' %OS{digits}\""))()
}
sec_format    <- function(x, digits = 1) {
  x %>% round(digits) %>% as_datetime() %>% label_time(glue("%OS{digits}\""))()
}

### Coefficient direction function: ----
coef_dir <- function(x,
                     form = c("adj", "noun", "verb", "indet", "sign", "comp")) {
  ## Argument checking and formatting: ----
  form <- match.arg(form)
  
  ## Main: ----
  
  if (sign(x) == -1) {
    
    result  <- "decrease"
    article <- "a"
    sign    <- "-"
    comp    <- "lower"
    
  } else {
    
    result  <- "increase"
    article <- "an"
    sign    <- ""
    comp    <- "higher"
  }
  
  switch(
    form,
    adj   = result %>% str_glue("d"),
    noun  = ,
    verb  = result,
    indet = str_glue(article, result, .sep = " "),
    sign  = sign,
    comp  = comp
  )
}
