## ---- SCRIPT SETUP: ----------------------------------------------------------

## ----global-configuration----

# rm(list = ls())

# Packages:

## ----script-configuration----


## ---- MAIN: ------------------------------------------------------------------


## ---- Shared chunks: ---------------------------------------------------------

## ----includes----
library(tidyverse)
library(magrittr)
library(haven)
library(psych)

source("R/Output.R",        encoding = 'UTF-8')


## ----constants----

# File system:
BASE_DIR <- "~/../UAM"
DB_PATH_MAIN <- file.path(
  BASE_DIR,
  "marta.miret@uam.es - Bases de datos maestras Edad con Salud"
)
DB_PRE_DIR <- file.path(
  DB_PATH_MAIN,
  "Ola_3/Cohorte_2019/Submuestra_1_preconfinamiento"
)
DB_POST_DIR <- file.path(DB_PATH_MAIN, "Subestudio_COVID")

# Source datasets:
DB_PRE_PATH <- file.path(
  DB_PRE_DIR,
  "Edad con Salud ola 3_cohorte 2019_base completa_Stata14.dta"
)
DB_POST_PATH <- file.path(
  DB_POST_DIR,
  "Edad_con_salud_Fichero_Completo.dta"
)


# Variable definitions

RESILIENCE_POST_ITEMS <- c(
  "SM26_1", "SM26_2", "SM26_3", "SM26_4", "SM26_5", "SM26_6"
)

SOCIAL_SUPPORT_PRE_ITEMS  <- c(
  "q6310_help_neig", "q6320_close", "q6330_concern"
)
SOCIAL_SUPPORT_POST_ITEMS <- c("SOLO9A", "SOLO9B", "SOLO9C")

LONELINESS_PRE_ITEMS  <- c("q6351_companion", "q6352_leftout", "q6353_isolated")
LONELINESS_POST_ITEMS <- c("SOLO7_1", "SOLO7_2", "SOLO7_3")

DISABILITY_PRE_ITEMS  <- c(
  "q2028_stand",
  "q2032_hh_resp",
  "q2011_learn",
  "q2033_activ",
  "q2047_affect",
  "q2035_concent",
  "q2036_walk",
  "q2037_wash",
  "q2038_dress",
  "q2015_strang",
  "q2014_friend",
  "q2039_daily"
)
DISABILITY_POST_ITEMS <- "SF2_" %>% paste0(1:12)

DISABILITY_RECODE_PRE  <- c(
  "q2032_hh_resp",
  "q2033_activ",
  "q2037_wash",
  "q2015_strang",
  "q2014_friend",
  "q2039_daily"
)
DISABILITY_RECODE_POST <- "SF2_" %>% paste0(c(2, 4, 8, 10, 11, 12))


PHYSICAL_ACTIVITY_PRE_ITEMS  <- c(
  "q3017_days", "q3018_hours", "q3018_mins",
  "q3020_days", "q3021_hours", "q3021_mins",
  "q3023_days", "q3024_hours", "q3024_mins"
)
PHYSICAL_ACTIVITY_POST_ITEMS <- c(
  "AF1A", "AF1B_H_1", "AF1B_M_1",
  "AF2A", "AF2B_H_1", "AF2B_M_1",
  "AF3A", "AF3B_H_1", "AF3B_M_1"
)

ITEMS_WITH_MISSING_PRE  <- c(SOCIAL_SUPPORT_PRE_ITEMS,  DISABILITY_PRE_ITEMS)
ITEMS_WITH_MISSING_POST <- c(SOCIAL_SUPPORT_POST_ITEMS, DISABILITY_POST_ITEMS)


## ----load-data----

dataset_pre  <- DB_PRE_PATH  %>% read_dta()
dataset_post <- DB_POST_PATH %>% read_dta()


## ----preprocess-data----

dataset_pre <- dataset_pre %>% mutate(
  # Missing values:
  across(all_of(ITEMS_WITH_MISSING_PRE), na_if, 888),
  across(all_of(ITEMS_WITH_MISSING_PRE), na_if, 999),
  
  # Values missing by design in social support item:
  across(q6310_help_neig, ~if_else(q6010h_neighbours == 2, 5, as.numeric(.))),
  
  # Recode response categories in disability:
  across(all_of(DISABILITY_RECODE_PRE), as.numeric),
  across(all_of(DISABILITY_RECODE_PRE), recode, `2` = 3, `4` = 5),
  
  # Recode physical activity items:
  across(all_of(PHYSICAL_ACTIVITY_PRE_ITEMS), as.numeric),
  across(
    all_of(PHYSICAL_ACTIVITY_PRE_ITEMS),
    ~if_else(q0007a_result ==  1, recode(., .missing = 0), .)
  ),
  across(all_of(PHYSICAL_ACTIVITY_PRE_ITEMS) & matches("hours$"), `*`, 60),
  met1 = q3017_days * (q3018_hours + q3018_mins) * 8,
  met2 = q3020_days * (q3021_hours + q3021_mins) * 4,
  met3 = q3023_days * (q3024_hours + q3024_mins) * 4
)

dataset_post <- dataset_post %>% mutate(
  # Missing values:
  across(all_of(ITEMS_WITH_MISSING_POST), na_if, 8),
  across(all_of(ITEMS_WITH_MISSING_POST), na_if, 9),

  # Recode response categories in disability:
  across(all_of(DISABILITY_RECODE_POST), as.numeric),
  across(all_of(DISABILITY_RECODE_POST), recode, `2` = 3, `4` = 5),

  # Recode physical activity items:
  across(all_of(PHYSICAL_ACTIVITY_POST_ITEMS), as.numeric),
  across(
    all_of(PHYSICAL_ACTIVITY_POST_ITEMS),
    ~if_else(ESTADO_ENTREVISTA ==  1, recode(., `99` = 0), .)
  ),
  across(
    all_of(PHYSICAL_ACTIVITY_POST_ITEMS) & matches("B_H_1$"), `*`, 60
  ),
  met1 = AF1A * (AF1B_H_1 + AF1B_M_1) * 8,
  met2 = AF2A * (AF2B_H_1 + AF2B_M_1) * 4,
  met3 = AF3A * (AF2B_H_1 + AF3B_M_1) * 4
  # See document "Pyshical_activity_SubestudioCOVID.docx" for the 30 denominator
)


## ----reliabilities----

alpha_social_support_pre <- dataset_pre %>%
  select(all_of(SOCIAL_SUPPORT_PRE_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha")) %>%
  format_prop_like()

alpha_loneliness_pre <- dataset_pre %>%
  select(all_of(LONELINESS_PRE_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha")) %>%
  format_prop_like()

alpha_disability_pre <- dataset_pre %>%
  select(all_of(DISABILITY_PRE_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha")) %>%
  format_prop_like()

alpha_physical_activity_pre <- dataset_pre %>%
  select(met1:met3) %>%
  psych::alpha() %>%
  extract2(c("total", "raw_alpha")) %>%
  format_prop_like()


alpha_resilience_post <- dataset_post %>%
  select(all_of(RESILIENCE_POST_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha")) %>%
  format_prop_like()

alpha_social_support_post <- dataset_post %>%
  select(all_of(SOCIAL_SUPPORT_POST_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha")) %>%
  format_prop_like()

alpha_loneliness_post <- dataset_post %>%
  select(all_of(LONELINESS_POST_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha")) %>%
  format_prop_like()

alpha_disability_post <- dataset_post %>%
  select(all_of(DISABILITY_POST_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha")) %>%
  format_prop_like()

alpha_physical_activity_post <- dataset_post %>%
  select(met1:met3) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha")) %>%
  format_prop_like()

