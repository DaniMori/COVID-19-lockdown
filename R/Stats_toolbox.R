# ==============================================================================
# 
# FILE NAME:   Stats_toolbox.R
# DESCRIPTION: Ancillary functions for statistical inference
# 
# AUTHOR:      Mori (danivmorillo@gmail.com)
# 
# DATE:        17/04/2020
# 
# ==============================================================================


## ---- GLOBAL OPTIONS: --------------------------------------------------------


## ---- INCLUDES: --------------------------------------------------------------

library(broom)
library(dplyr)
library(assertive)

source("R/parse_arguments.R", encoding = 'UTF-8')


## ---- CONSTANTS: -------------------------------------------------------------


## ---- FUNCTIONS: -------------------------------------------------------------


#' Compute the Akaike information criterion (AIC) of a model when dropping
#' a single predictor
#'
#' @param predictor 
#' @param model 
#'
#' @return A tibble
#' @export
#'
#' @examples
get_predictor_AIC <- function(predictor, model) {
  
  drop_formula <- paste0(
    "`",
    as.character(formula(model))[2],
    "` ~ . - ",
    predictor
  )
  drop_formula <- formula(drop_formula)
  
  drop_term_model <- update(
    model,
    formula = drop_formula,
    data    = model.frame(model),
    weights = `(weights)`
  )
  
  tibble(term = predictor, AIC = AIC(drop_term_model))
}


compare_AICs <- function(model, arrange = TRUE) {
  
  result <- attributes(terms(model))[["term.labels"]] %>%
    purrr::map_dfr(get_predictor_AIC, model)
  
  if (arrange) return(result %>% arrange(AIC))
  
  result
}


get_interaction_AIC <- function(predictor_1, predictor_2, model) {
  
  term <- paste0(predictor_1, ":", predictor_2)
  
  add_model <- model %>%
    update(formula(paste(". ~ . +", term)))
  
  lr_test <- anova(add_model, model) %>% tidy() %>% slice(2)
  
  tibble(term = term, AIC = AIC(add_model)) %>% bind_cols(lr_test)
}


compare_interaction_AICs <- function(model, arrange = TRUE) {
  
  predictors <- attributes(terms(model))[["term.labels"]] %>% {
    expand_grid(
      predictor_1 = parse_factor(., ordered = TRUE),
      predictor_2 = parse_factor(., ordered = TRUE)
    )
  } %>% filter(predictor_2 > predictor_1) %>% mutate_all(as.character)
  
  result <- predictors %>% pmap_dfr(get_interaction_AIC, model = model)
  
  if (arrange) return(result %>% arrange(AIC))
  
  result
}


get_pspline_AIC <- function(df, term, model) {
  
  nl_AIC <- update(
    model,
    as.formula(paste(". ~ . -", term, "+ pspline(" , term, ", df =", df, ")"))
  ) %>% AIC()
  
  nl_AIC - AIC(model)
}


compare_pspline_AICs <- function(model, term, dfs = 2:8) {
  
  tibble(
    df  = dfs,
    AIC = map_dbl(dfs, get_pspline_AIC, term = term, model = model)
  )
}

get_coxph_R2 <- function(model) {
  
  ## Argument checking and formatting: ----
  assert_is_of_class(model, "coxph")
  
  summary(model)$rsq[["rsq"]]
}

get_tidy_estimates <- function(model, as_exp = FALSE, suffix = c("", "_exp")) {
  ## Constants: ----
  MODEL_CLASSES <- c("lm", "glm", "coxph") # TODO: other models
  
  ## Argument checking and formatting: ----
  
  assert_is_inherited_from(model, MODEL_CLASSES)
  
  as_exp <- parse_bool(as_exp)
  
  if (as_exp) {
    
    assert_is_character(suffix)
    assert_is_of_length(suffix, 2)
  }

    
  ## Main: ----
  
  result <- model %>% tidy()

  if (as_exp) {
    
    result <- result %>% full_join(
      .,
      select(., term, estimate, conf.low, conf.high) %>%
        mutate_if(assertive::is_numeric, exp), by = "term", suffix = suffix
    )
  }
  
  result
}

step_backwards_lmer <- function(init_fit,
                                min_model,
                                alpha_fixed  = .05, # Alpha for "fixed terms"
                                bonf_correct = FALSE) {
  
  keep <- min_model %>% formula() %>% terms() %>% attr("term.labels")
  
  history <- init_fit %>%
    drop1() %>%
    slice(0) %>%
    as_tibble() %>%
    add_column(term = character(0L), .before = 0L) %>%
    mutate(
      ngroups = integer(0L),
      nobs    = integer(0L)
    )
  
  current_fit <- init_fit
  
  repeat {
    
    candidates <- current_fit %>%
      drop1() %>%
      as.data.frame() %>%
      rownames_to_column("term") %>%
      as_tibble() %>%
      filter(!term %in% keep)
    
    if (!nrow(candidates)) break # If there are no candidate terms to drop
    
    candidate <- candidates %>%
      slice(`Pr(>F)` %>% which.max()) %>%
      mutate(
        ngroups = current_fit %>% summary() %>% extract2("ngrps"),
        nobs    = current_fit@frame %>% nrow()
      )
    
    alpha_test <- alpha_fixed / if (bonf_correct) nrow(candidates) else 1

    if (candidate %>% pull(`Pr(>F)`) <= alpha_test) break;
    
    history <- bind_rows(history, candidate)
    
    dropped <- candidate %>% pull(term)
    
    current_fit <- update(current_fit, paste(". ~ . -", dropped))
  }
  
  attr(current_fit, "history") <- history
  
  current_fit
}

correct_weights <- function(dataset, id_var, weights_var = weights) {
  
  id_var      <- enquo(id_var)
  weights_var <- enquo(weights_var)
  
  weights_tbl <- dataset %>%
    ungroup() %>%
    distinct(!!id_var, !!weights_var) %>%
    mutate(!!weights_var := !!weights_var / mean(!!weights_var))
  
  dataset %>%
    select(-!!weights_var) %>%
    left_join(weights_tbl, by = id_var %>% as_name())
}

## Adapted from: https://www.r-bloggers.com/2012/04/weighted-t-test-in-r/
# weighted variance, inspired by a function from Gavin Simpson on R-Help
# FIXME: This function may be wrong
var_weighted <- function(x, weights, na.rm = FALSE) {
  
  ## Argument checking and formatting: ----
  
  assert_is_numeric(x)
  assert_is_numeric(weights)
  assert_are_same_length(x, weights)
  assert_all_are_not_na(weights)
  
  na.rm <- parse_bool(na.rm)
  
  drop <- is.na(x) 
  
  if (na.rm) {
    
    weights <- weights[!drop]
    x       <- x[!drop]
    
  } else {
    
    if (any(drop)) return(NA)
  }
  
  # Normalize weights (just in case, although it shouldn't matter):
  weights     <- weights / mean(weights)
  sum_weights <- sum(weights) # Must equal the length of x after dropping NA's
  
  
  ## Main: ----
  (sum(weights * x^2) * sum_weights - sum(weights * x)^2) /
    (sum_weights^2 - sum(weights^2))
}

weighted.t.test <- function(x,
                            weights,
                            mu          = 0,
                            conf.level  = 0.95,
                            alternative = c("two.sided", "less", "greater"),
                            na.rm       = TRUE) {
  ## Argument checking and formatting: ----
  
  assert_is_numeric(x)
  assert_is_numeric(weights)
  assert_are_same_length(x, weights)
  assert_all_are_not_na(weights)
  
  na.rm <- parse_bool(na.rm)
  
  drop <- is.na(x) 
  
  if (na.rm) {
    
    weights <- weights[!drop]
    x       <- x[!drop]
    
  } else {
    
    if (any(drop)) return(NA)
  }
  
  # Normalize weights (just in case, although it shouldn't matter):
  weights     <- weights / mean(weights)
  sum_weights <- sum(weights) # Must equal the length of x after dropping NA's
  
  
  
  if(!missing(conf.level) &
     (length(conf.level) != 1 || !is.finite(conf.level) ||
      conf.level < 0 || conf.level > 1))
    stop("'conf.level' must be a single number between 0 and 1")
  
  if (na.rm) {
    weights <- weights[i <- !is.na(x)]
    x <- x[i]
  }
  
  # to achieve consistent behavior in loops, return NA-structure in case of complete missings
  if (sum(is.na(x)) == length(x)) return(list(estimate=NA, se=NA, conf.int=NA, statistic=NA, df=NA, p.value=NA))
  
  # if only one value is present: this is the best estimate, no significance test provided
  if (sum(!is.na(x)) == 1) {
    warning("Warning weighted.t.test: only one value provided; this value is returned without test of significance!", call.=FALSE)
    return(list(estimate=x[which(!is.na(x))], se=NA, conf.int=NA, statistic=NA, df=NA, p.value=NA))
  }
  
  x.w <- weighted.mean(x,weights, na.rm=na.rm)
  var.w <- var_weighted(x,weights, na.rm=na.rm)
  df <- length(x)-1
  t.value <- sqrt(length(x))*((x.w-mu)/sqrt(var.w))
  se <- sqrt(var.w)/sqrt(length(x))
  
  if (alternative == "less") {
    pval <- pt(t.value, df)
    cint <- c(-Inf, x.w + se*qt(conf.level, df) )
  }
  else if (alternative == "greater") {
    pval <- pt(t.value, df, lower.tail = FALSE)
    cint <- c(x.w - se * qt(conf.level, df), Inf)
  }
  else {
    pval <- 2 * pt(-abs(t.value), df)
    alpha <- 1 - conf.level
    cint <- x.w + se*qt(1 - alpha/2, df)*c(-1,1)
  }
  
  names(t.value) <- "t"
  return(list(estimate=x.w, se=se, conf.int=cint, statistic=t.value, df=df, p.value=pval))
}

paired_t_test_df_var <- function(.data,
                             criterion,
                             predictor,
                             id,
                             ...) {
  ## Argument checking and formatting: ----
  
  criterion <- enquo(criterion)
  predictor <- enquo(predictor)
  id        <- enquo(id)
  
  assert_is_data.frame(.data)
  .data %>% pull(!!criterion) %>% assert_is_numeric()
  
  .data %>% pull(!!predictor) %>%
    assert_class_is_one_of(c("character", "factor"))
  
  pred_levels <- .data %>%
    pull(!!predictor) %>%
    unique() %>%
    as.character() # In case it is a factor
  assert_is_of_length(pred_levels, 2)
  
  
  ## Main: ----
  
  .data <- .data %>% pivot_wider(
    id_cols = !!id, names_from = !!predictor, values_from = !!criterion
  )
  
  result <- tryCatch(
    t.test(
      .data[[pred_levels[1]]], .data[[pred_levels[2]]],
      paired = TRUE,
      ...
    ) %>%
      tidy(),
    error = function(e) NULL
  )
}

paired_t_tests_df <- function(.data,
                             predictor,
                             id,
                             ...) {
  ## Argument checking and formatting: ----
  
  predictor <- enquo(predictor)
  id        <- enquo(id)
  
  assert_is_data.frame(.data)

  .data %>% pull(!!predictor) %>%
    assert_class_is_one_of(c("character", "factor"))
  
  pred_levels <- .data %>%
    pull(!!predictor) %>%
    unique() %>%
    as.character() # In case it is a factor
  assert_is_of_length(pred_levels, 2)
  
  
  ## Main: ----
  .data %>%
    select(where(is.numeric), -!!id) %>% {
      imap_dfr(.,
        ~{
          result <- paired_t_test_df_var(
            .data, sym(.y), !!predictor, !!id
          )
          if (is_not_null(result)) result %>% add_column(var = .y, .before = 1)
        }
      )
    }
}



mcnemar_test_df_pre_post <- function(.data,
                                     pre,
                                     post,
                                     category,
                                     weight = NULL,
                                     correct = FALSE) {
  ## Argument checking and formatting: ----
  
  pre    <- enquo(pre)
  post   <- enquo(post)
  weight <- enquo(weight)
  
  assert_is_data.frame(.data)
  
  pre_vec  <- .data %>% pull(!!pre)
  post_vec <- .data %>% pull(!!post)
  
  # Check that both measures are categorical variables:
  pre_vec  %>% assert_class_is_one_of(c("character", "factor"))
  post_vec %>% assert_class_is_one_of(c("character", "factor"))
  
  # Levels in the Pre and Post measure:
  pre_levels  <- pre_vec  %>% unique() %>% sort() %>% as.character()
  assert_is_of_length(pre_levels, 2)
  post_levels <- post_vec %>% unique() %>% sort() %>% as.character()
  assert_is_of_length(post_levels, 2)
  
  # Check that the target category is in both measures:
  assert_is_a_string(category)
  assert_is_subset(category, pre_levels)

  correct <- parse_bool(correct)
  
  
  ## Main: ----
  
  .data <- .data %>% mutate(across(c(!!pre, !!post), `==`, category))
  
  contingency_matrix <- if (!is.null(eval_tidy(weight))) {
    
    .data %>%
      drop_na(!!pre, !!post, !!weight) %>%
      mutate(!!weight := !!weight / mean(!!weight)) %>%
      count(!!pre, !!post, wt = !!weight) %>%
      complete(!!pre, !!post, fill = list(n = 0))
    
  } else {
    
    .data %>% drop_na(!!pre, !!post) %>% count(!!pre, !!post)
    
  }
  
  contingency_matrix %>% group_modify(
    ~pivot_wider(., id_cols = !!pre, names_from = !!post, values_from = n) %>%
      select(-!!pre) %>%
      as.matrix() %>%
      mcnemar.test(correct = correct) %>%
      tidy()
  )
}


mcnemar_test_df_cat <- function(.data,
                                var,
                                measure,
                                id,
                                category,
                                correct = FALSE) {
  ## Argument checking and formatting: ----
  
  var      <- enquo(var)
  measure  <- enquo(measure)
  id       <- enquo(id)
  
  assert_is_data.frame(.data)
  
  var_vals     <- .data %>% pull(!!var)
  measure_vals <- .data %>% pull(!!measure)
  
  var_vals     %>% assert_class_is_one_of(c("character", "factor"))
  measure_vals %>% assert_class_is_one_of(c("character", "factor"))
  
  # To character, in case it is a factor
  measure_levels <- measure_vals %>% unique() %>% as.character()
  assert_is_of_length(measure_levels, 2)
  measure_levels <- measure_levels %>% syms()
  
  # To character, in case it is a factor
  var_levels <- var_vals %>% unique() %>% na.omit() %>% as.character()
  assert_is_subset(category, var_levels)
  
  correct <- parse_bool(correct)
  
  
  ## Main: ----
  .data %>%
    mutate(across(!!var, `==`, category)) %>%
    pivot_wider(
      id_cols = !!id, names_from = !!measure, values_from = !!var
    ) %>%
    tabyl(var1 = !!measure_levels[[1]], var2 = !!measure_levels[[2]]) %>%
    filter(!!measure_levels[[1]] != "NA") %>%
    select(-!!measure_levels[[1]], -any_of("NA_")) %>%
    as.matrix() %>%
    mcnemar.test(correct = correct) %>%
    tidy()
}

mcnemar_tests_df_var <- function(.data,
                                var,
                                measure,
                                id,
                                correct = FALSE) {
  ## Argument checking and formatting: ----
  
  var      <- enquo(var)
  measure  <- enquo(measure)
  id       <- enquo(id)
  
  assert_is_data.frame(.data)
  
  var_vals     <- .data %>% pull(!!var)
  measure_vals <- .data %>% pull(!!measure)
  
  var_vals     %>% assert_class_is_one_of(c("character", "factor"))
  measure_vals %>% assert_class_is_one_of(c("character", "factor"))
  
  # To character, in case it is a factor
  measure_levels <- measure_vals %>% unique() %>% as.character()
  assert_is_of_length(measure_levels, 2)
  measure_levels <- measure_levels %>% syms()
  
  # To character, in case it is a factor
  var_levels <- var_vals %>% unique() %>% na.omit() %>% as.character()

  correct <- parse_bool(correct)

    
  ## Main: ----
  var_levels %>%
    map_dfr(
      mcnemar_test_df_cat,
      .data = .data, var = !!var, measure = !!measure, id = !!id,
      correct = correct
    ) %>%
    add_column(cat = var_levels, .before = 1)
}

mcnemar_tests_df <- function(.data, measure, id, correct = FALSE) {
  
  ## Argument checking and formatting: ----
  
  measure  <- enquo(measure)
  id       <- enquo(id)
  
  assert_is_data.frame(.data)
  
  measure_vals <- .data %>% pull(!!measure)
  measure_vals %>% assert_class_is_one_of(c("character", "factor"))
  
  # To character, in case it is a factor
  measure_levels <- measure_vals %>% unique() %>% as.character()
  assert_is_of_length(measure_levels, 2)
  measure_levels <- measure_levels %>% syms()
  
  correct <- parse_bool(correct)
  
  
  ## Main: ----
  .data %>%
    select(where(is.factor), -!!measure, -!!id) %>% {
      imap_dfr(.,
        ~{
          result <- mcnemar_tests_df_var(
            .data, sym(.y), !!measure, !!id
          ) %>%
            add_column(var = .y, .before = 1)
        }
      )
    }
}


prevalence_mcnemar_test <- function(pre, post, n) {
  
  ## Argument checking and formatting: ----
  
  assert_is_numeric(pre)
  assert_all_are_proportions(pre)
  assert_is_numeric(post)
  assert_all_are_proportions(post)
  assert_all_are_whole_numbers(n)
  assert_all_are_non_negative(n)
  
  
  ## Main: ----
  
  test_matrix <- (c(pre, 1 - pre, post, 1 - post) * n) %>% matrix(nrow = 2)
  
  test_matrix %>% mcnemar.test(correct = FALSE) %>% tidy()
}

prevalences_mcnemar_test <- function(.data, pre, post, n) {
  
  ## Argument checking and formatting: ----
  
  pre  <- enquo(pre)
  post <- enquo(post)
  n    <- enquo(n)
  
  assert_is_data.frame(.data)
  
  pre  <- .data %>% pull(pre)
  post <- .data %>% pull(post)
  n    <- .data %>% pull(n)
  
  assert_is_numeric(pre)
  assert_all_are_proportions(pre)
  assert_is_numeric(post)
  assert_all_are_proportions(post)
  assert_all_are_non_negative(n)
  assert_all_are_whole_numbers(n)
  

  ## Main: ----
  
  .data %>% pmap(~prevalence_mcnemar_test(!!pre, !!post, !!n))
}

fisher_test_df_var <- function(.data, x, y, correct = TRUE) {
  ## Argument checking and formatting: ----
  
  x_quo <- enquo(x)
  y_quo <- enquo(y)
  
  assert_is_data.frame(.data)
  
  correct <- parse_bool(correct)
  
  x <- .data %>% pull(!!x_quo)
  y <- .data %>% pull(!!y_quo)
  
  ## Main: ----
  
  fisher.test(x, y) %>%
    tidy()          %>%
    bind_cols(
      rcompanion::cramerV(x, y, bias.correct = correct) %>% tibble(Cramer_V = .)
    )
}

chisq_test_df_var <- function(.data,
                              x, y,
                              correct   = FALSE,
                              correct_v = TRUE,
                              ...) {
  ## Argument checking and formatting: ----
  
  x_quo <- enquo(x)
  y_quo <- enquo(y)
  
  assert_is_data.frame(.data)
  
  correct_v <- parse_bool(correct_v)
  correct   <- parse_bool(correct)
  
  x <- .data %>% pull(!!x_quo)
  y <- .data %>% pull(!!y_quo)
  
  ## Main: ----
  
  chisq.test(x, y, correct = correct, ...) %>%
    tidy()              %>%
    bind_cols(
      rcompanion::cramerV(x, y, bias.correct = correct_v) %>%
        tibble(Cramer_V = .)
    )
}

estimate_prevalence <- function(.data, var, weights, cat = "Yes") {
  
  var     <- enquo(var)
  weights <- enquo(weights)
  
  .data <- .data %>% drop_na(!!var) %>% mutate(wts = !!weights / sum(!!weights))
  
  result <- .data %>%
    count(., Pos = (!!var == cat), wt = wts, name = "Prevalence") %>%
    filter(Pos) %>%
    select(-Pos)
  
  result2 <- .data %>% summarize(sum_sqwt = sum(wts^2), N = n())

  result <- if (.data %>% group_vars() %>% is_empty()) {
    
    result %>% bind_cols(result2)
    
  } else {
    
    result %>% full_join(result2, by = .data %>% group_vars())
  }
  
  result %>%
    mutate(Std_err = sqrt(Prevalence * (1 - Prevalence) * sum_sqwt)) %>%
    select(-sum_sqwt)
      ## FIXME: Normalizing transform
      # CI_inf  = Prevalence - CI_FACTOR * Std_err,
      # CI_sup  = Prevalence + CI_FACTOR * Std_err
}
