library(assertive.extra)
library(assertive)

parse_bool <- function(arg, default = "warning") {
  
  ## TODO: severities
  assert_is_a_bool(arg, severity = default)
  as.logical(arg[1])
}

parse_scalar <- function(arg, default = "warning") {
  
  ## TODO: severities
  assert_is_a_number(arg, severity = default)
  as.double(arg[1])
}

parse_whole_number <- function(arg, default = "warning") {
  
  ## TODO: severities
  assert_is_a_whole_number(arg, severity = default)
  as.integer(arg[1])
}

parse_natural_number <- function(arg, default = "warning") {
  
  ## TODO: severities
  assert_is_a_natural_number(arg, severity = default)
  arg <- as.integer(arg[1])
  
  ## TODO: error message
  if (is_negative(arg)) stop ("Must be a natural (positive integer) number")
  
  arg
}

parse_string <- function(arg, default = "warning", ...) {
  
  ## TODO: severities
  assert_is_a_string(arg, severity = default)
  as.character(arg[1])
}

parse_char <- function(arg, default = "warning", ...) {
  
  ## TODO: severities
  assert_is_character(arg, severity = default)
  as.character(arg)
}

parse_char_or_symbols <- function(arg, default = "stop", ...) {
  ## Constants: ----
  VALID_CLASSES <- c("character", "list", "name")
  SYMBOL_TYPE   <- "symbol"
  
  ## Main: ----
  
  assert_class_is_one_of(arg, VALID_CLASSES, severity = default)
  
  if (is_list(arg)) {
    
    assert_all_are_matching_fixed(
      arg %>% map_chr(typeof),
      SYMBOL_TYPE,
      severity = default
    )
  }
  
  if (!class_is_one_of(arg, VALID_CLASSES)) arg <- as.character(arg)
  
  arg
}

vars_as_symbols <- function(vars) {
  ## Argument checking and formatting: ----
  parse_char_or_symbols(vars)
  
  ## Main: ----
  if (length(vars) > 1) vars %>% syms() else vars %>% sym()
}


parse_varnames <- function(arg,
                           dataset,
                           default = "stop",
                           sym     = c("asis", "no", "yes")) {
  ## Argument checking and formatting: ----
  
  assert_is_data.frame(dataset)
  parse_char_or_symbols(arg)
  
  default <- parse_string(default)
  sym     <- match.arg(sym)
  
  
  ## Main: ----
  
  arg_check <- if (is.symbol(arg)) as_name(arg) else map_chr(arg, as_name)
  
  ## TODO: severities
  assert_is_subset(arg_check, colnames(dataset), severity = default)
  
  if (sym == "asis") return(arg)
  if (sym == "no")   return(arg_check)
  if (sym == "yes")  return(vars_as_symbols(arg))
}
