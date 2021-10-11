# ==============================================================================
# 
# FILE NAME:   Data.R
# DESCRIPTION: Data manipulation functions
# 
# AUTHOR:      Mori (danivmorillo@gmail.com)
# 
# DATE:        29/03/2021
# 
# ==============================================================================


## ---- GLOBAL OPTIONS: --------------------------------------------------------

library(pacman)


## ---- INCLUDES: --------------------------------------------------------------

p_load(broom, dplyr, assertive)

source("R/parse_arguments.R", encoding = 'UTF-8')


## ---- CONSTANTS: -------------------------------------------------------------

AGE_GROUPS <- c("18 - 29", "30 - 49", "50 +")


## ---- FUNCTIONS: -------------------------------------------------------------

group_ages <- function(age) {
  
  cut(
    age,
    breaks         = c(-Inf, 30, 50, Inf),
    labels         = AGE_GROUPS,
    include.lowest = TRUE,
    right          = FALSE
  )
}
