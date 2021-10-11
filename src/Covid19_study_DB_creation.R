# ==============================================================================
# 
# FILE NAME:   Covid19_study_DB_creation.R
# DESCRIPTION: Consolidates a database with all the relevant variables for the
#              COVID-19 confinement study with the Edad con Salud data.
# 
# AUTHOR:      Daniel Morillo (daniel.morillo@uam.es)
# 
# DATE:        01/09/2020
#
# UPDATE (14/07/2021): See lines 87-98
#
# ==============================================================================


## ---- GLOBAL OPTIONS: --------------------------------------------------------

rm(list = ls())


## ---- INCLUDES: --------------------------------------------------------------

# Packages:
library(pacman)
p_load(tidyverse, haven)


## ---- CONSTANTS: -------------------------------------------------------------

# File system:
BASE_DIR <- "~/../UAM"
DB_PATH_MASTER <- file.path(
  BASE_DIR,
  "marta.miret@uam.es - Bases de datos maestras Edad con Salud"
)
DOC_PATH_MASTER <- file.path(
  BASE_DIR,
  "marta.miret@uam.es - Documentacion Edad con Salud"
)
OUTCOME_PATH_BASE <- file.path(
  DOC_PATH_MASTER,
  "Edad con salud - Ola 3/Outcomes/Cohorte 2019",
  "Submuestra_1_preconfinamiento/Outcome datasets"
)
OUTCOME_PATH_COVID <- file.path(
  DOC_PATH_MASTER,
  "Edad con salud - Subestudio COVID/Outcomes/Outcome datasets"
)
OUTPUT_PATH <- file.path(
  DOC_PATH_MASTER,
  "Edad con salud - Subestudio COVID/BBDD_con_outcomes"
)

# Collapsed outcomes file:
OUTPUT_FILE <- file.path(OUTPUT_PATH, "Outcomes_collapsed.dta")

# Variables:
BASELINE_ID_VARS <- c("number_id", "q0002_hhid")
POST_LD_ID <- "ID_CONTACTO"
POST_LD_ID_VARS  <- c(POST_LD_ID, BASELINE_ID_VARS)

# Variable values:
DUPLICATES <- c(2079L)


## ---- MAIN: ------------------------------------------------------------------

## ---- load-master-datasets ----

dataset_master_pre  <- file.path(
  DB_PATH_MASTER,
  "Ola_3/Cohorte_2019/Submuestra_1_preconfinamiento",
  "Edad con Salud ola 3_cohorte 2019_base completa_Stata14.dta"
) %>%
  read_dta()

dataset_master_post <- file.path(
  DB_PATH_MASTER,
  "Subestudio_COVID",
  "Edad_con_salud_Fichero_Completo.dta"
) %>%
  read_dta()


## ---- preprocess-master-pre ----

## UPDATE (14/07/2021)

# In order to avoid confusion, variable `proxy` in the "Pre" master dataset
#   is labelled with the label from `TIPO_ENT` variable inthe "Post" dataset,
#   which univocally identifies that this variable refers to proxy cases in the
#   baseline.
proxy_label <- dataset_master_post %>% pull(TIPO_ENT) %>% attr("label")

dataset_master_pre <- dataset_master_pre %>%
  modify_at("proxy", magrittr::set_attr, "label", proxy_label)

## END UPDATE (14/07/2021)


## ---- preprocess-master-post ----

# Interview end date is in "character" format originally;
#   also, identifiers are not named according to the Edad con Salud convention:
dataset_master_post <- dataset_master_post               %>%
  mutate(FECHAFIN = FECHAFIN %>% parse_date("%d/%m/%Y")) %>%
  rename(!!!c("IDENTIFICA1", "IDENTIFICA2") %>% set_names(BASELINE_ID_VARS))


## ---- additional-outcome-datasets ----

BOOL_LABELS <- c("No" = 0, "Yes" = 1)

# Psychological / pharmacological treatment:
dataset_treatment_post <- dataset_master_post %>%
  select(ID_CONTACTO, number_id, q0002_hhid, SM23, SM24, SM25) %>%
  mutate_at(
    vars(starts_with("SM")),
    ~as.integer(.) %>% recode(1L, 0L)
  ) %>%
  mutate(
    SM23 = SM23 %>% labelled(
      labels = BOOL_LABELS,
      label  = paste(
        "Psychological help or medication",
        "in last 30 days (post lockdown)"
      )
    ),
    SM24 = SM24 %>% labelled(
      labels = BOOL_LABELS,
      label  = paste(
        "Psychological help or medication",
        "interrupted in last 30 days (post lockdown)"
      )
    ),
    SM25 = SM25 %>% labelled(
      labels = BOOL_LABELS,
      label  = paste(
        "Need for psychological help or medication",
        "in last 30 days (post lockdown)"
      )
    ),
  )


## ---- standard-id-vars ----

id_vars <- dataset_master_post %>%
  select_at(POST_LD_ID_VARS)   %>%
  full_join(
    dataset_master_pre %>% select_at(BASELINE_ID_VARS),
    by = BASELINE_ID_VARS
  )


## ---- load-outcome-datasets ----

outcome_datasets <- list(
  
  ## Sociodemographic variables: ----
  
  ### Sociodemographics:
  sociodemographics = file.path(OUTCOME_PATH_COVID, "Sociodemographics.dta") %>%
    read_dta(),

  ### Income:
  income_pre = file.path(OUTCOME_PATH_BASE, "Income.dta") %>%
    read_dta() %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  
  ## Mental health variables: ----
  
  ### Depression:
  depression_idc10_pre = file.path(
    OUTCOME_PATH_BASE,
    "Depression_ICD10.dta"
  ) %>%
    read_dta() %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  depression_comparable_pre = file.path(
    OUTCOME_PATH_COVID,
    "Depression_PRECovid_comparable.dta"
  ) %>%
    read_dta() %>%
    rename_at(vars(-number_id, -q0002_hhid), paste0, "_pre") %>%
    full_join(id_vars, by = BASELINE_ID_VARS) %>%
    mutate(
      depression_pre = depression_pre %>% labelled(
        label  = attr(., "label"),
        labels = BOOL_LABELS
      )
    ),
  
  depression_post = file.path(OUTCOME_PATH_COVID, "Depression_Covid.dta") %>%
    read_dta() %>%
    rename_at(vars(-ID_CONTACTO), paste0, "_post") %>%
    full_join(id_vars, by = POST_LD_ID) %>%
    mutate(
      depression_post = depression_post %>% labelled(
        label  = attr(., "label"),
        labels = BOOL_LABELS
      )
    ),

  ### Suicidal ideation:
  suicidal_ideation_post = file.path(
    OUTCOME_PATH_COVID,
    "Suicidal_ideation.dta"
  ) %>%
    read_dta(),

  ### Psychological/pharmacological treatment:
  dataset_treatment_post = dataset_treatment_post,
  
  ### Resilience:
  resilience_pre = file.path(
    OUTCOME_PATH_BASE,
    "Brief resilience scale.dta"
  ) %>%
    read_dta() %>%
    select(-proxy) %>%
    rename(resilience_pre = resilience_scale) %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  resilience_post = file.path(
    OUTCOME_PATH_COVID,
    "Brief Resilience Scale_ConfinamientoCovid.dta"
  ) %>%
    read_dta() %>%
    select(-ESTADO_ENTREV, -TIPO_ENT) %>%
    rename(resilience_post = resilience_scale) %>%
    full_join(id_vars, by = POST_LD_ID),

  ### SLE (Stressful life events):
  sle_pre = file.path(OUTCOME_PATH_BASE, "Stressful_life_events.dta") %>%
    read_dta() %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  
  ## Affective and wellbeing variables: ----

  ### Eudaimonic wellbeing:
  eudaimonic_pre = file.path(
    OUTCOME_PATH_BASE,
    "Flourishing scale (eudaimonic wellbeing).dta"
  ) %>%
    read_dta() %>%
    select(-proxy) %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  ### Evaluative wellbeing (Cantril ladder):
  evaluative_wb = file.path(OUTCOME_PATH_COVID, "Evaluative_wellbeing.dta") %>%
    read_dta(),
  
  ### Loneliness, social support, and social isolation:
  socialization_pre = file.path(
    OUTCOME_PATH_BASE,
    "Loneliness, social support and social isolation.dta"
  ) %>%
    read_dta() %>%
    rename(
      oslo3_sss_pre = social_support,
      ucla_lon_pre  = loneliness,
      isolation_pre = isolation
    ) %>%
    select(-proxy) %>%
    full_join(id_vars, by = BASELINE_ID_VARS),

  socialization_post = file.path(
    OUTCOME_PATH_COVID,
    "Loneliness and Social Support_ConfinamientoCovid.dta"
  ) %>%
    read_dta() %>%
    rename(oslo3_sss_post = social_support, ucla_lon_post = loneliness) %>%
    select(-ESTADO_ENTREV, -TIPO_ENT) %>%
    full_join(id_vars, by = POST_LD_ID),

  social_network = file.path(OUTCOME_PATH_COVID, "Social_interaction.dta") %>%
    read_dta(),
  
  ### PANAS (Positive and negative affect scale):
  panas_original_pre = file.path(
    OUTCOME_PATH_BASE,
    "Positive and negative affect.dta"
  ) %>%
    read_dta() %>%
    select(-proxy) %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  panas_comparable_pre = file.path(# It had originally a longer name, I created
    OUTCOME_PATH_COVID,            #   a new copy with this name because it gave
    "PANAS_PRE.dta"                #   an error with the other file name
  ) %>%
    read_dta() %>%
    select(-proxy) %>%
    rename_at(vars(ends_with("affect")), paste0, "_pre") %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  panas_post = file.path(
    OUTCOME_PATH_COVID,
    "Positive and Negative Affect_ConfinamientoCovid.dta"
  ) %>%
    read_dta() %>%
    select(-ESTADO_ENTREV, -TIPO_ENT) %>%
    rename_at(vars(ends_with("affect")), paste0, "_post") %>%
    full_join(id_vars, by = POST_LD_ID),
  
  ### WHOQOL (Quality of life assessment):
  whoqol_pre = file.path(OUTCOME_PATH_BASE, "Quality of life.dta") %>%
    read_dta() %>%
    select(-proxy) %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  
  ## Cognitive variables: ----
  
  ### Cognition:
  cognition_pre = file.path(OUTCOME_PATH_BASE, "cognition.dta") %>%
    read_dta() %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  ### Cognitive reserve:
  cognitive_reserve_pre = file.path(
    OUTCOME_PATH_BASE,
    "Cognitive_reserve.dta"
  ) %>%
    read_dta() %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  ### MMSE (Mini-mental state examination):
  mmse_pre = file.path(OUTCOME_PATH_BASE, "MMSE.dta") %>%
    read_dta() %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  
  ## Physical and global health variables: ----
  
  ### Ailment (due to COVID-19 pandemic):
  ailment_post = file.path(OUTCOME_PATH_COVID, "Ailment.dta") %>%
    read_dta(),
  
  ### Disability:
  disability_pre = file.path(OUTCOME_PATH_BASE,  "Disability.dta") %>%
    read_dta() %>%
    select(-proxy, -q0007a_result) %>%
    rename(whodas12_pre = whodas12) %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  disability_post = file.path(OUTCOME_PATH_COVID, "Disability_Covid.dta") %>%
    read_dta() %>%
    rename(whodas12_post = whodas12) %>%
    full_join(id_vars, by = POST_LD_ID),
  
  ### Pain:
  pain_post = file.path(OUTCOME_PATH_COVID, "Pain.dta") %>%
    read_dta(),
  
  ### Global health status:
  health_original_pre = file.path(OUTCOME_PATH_BASE,  "Health_status.dta") %>%
    read_dta() %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  health_comparable_pre = file.path(
    OUTCOME_PATH_COVID,
    "Health_status_PRE.dta"
  ) %>%
    read_dta() %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  health_comparable_post = file.path(
    OUTCOME_PATH_COVID,
    "Health_status_POST.dta"
  ) %>%
    read_dta() %>%
    rename(
      number_id  = IDENTIFICA1,
      q0002_hhid = IDENTIFICA2
    ),
  
  
  ## Lifestyle and behavioral variables: ----
  
  ### MEDAS (Mediterranean diet adherence):
  medas_pre = file.path(
    OUTCOME_PATH_BASE,
    "Mediterranean diet adherence.dta"
  ) %>%
    read_dta() %>%
    rename_at(vars(starts_with("medas")), paste0, "_pre") %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  medas_post = file.path(
    OUTCOME_PATH_COVID,
    "Mediterranean Diet Adeherence_ConfinamientoCovid.dta"
  ) %>%
    read_dta() %>%
    select(-ESTADO_ENTREV, -TIPO_ENT) %>%
    rename_at(vars(starts_with("medas")), paste0, "_post") %>%
    full_join(id_vars, by = POST_LD_ID),
  
  ### Physical activity:
  physical_original_pre = file.path(
    OUTCOME_PATH_BASE,
    "Physical activity.dta"
  ) %>% 
    read_dta() %>%
    select(all_of(BASELINE_ID_VARS), physical) %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  physical_comparable_pre = file.path(
    OUTCOME_PATH_COVID,
    "Physical_activity_PRECovid_comparable.dta"
  ) %>%
    read_dta() %>%
    select(all_of(BASELINE_ID_VARS), physical) %>%
    rename(physical_pre = physical) %>%
    full_join(id_vars, by = BASELINE_ID_VARS),
  
  physical_post = file.path(
    OUTCOME_PATH_COVID,
    "Physical_activity_Covid.dta"
  ) %>%
    read_dta() %>%
    select(all_of(POST_LD_ID), physical) %>%
    rename(physical_post = physical) %>%
    full_join(id_vars, by = POST_LD_ID),
  
  ### Sleep;
  sleep = file.path(OUTCOME_PATH_COVID, "Sleep.dta") %>%
    read_dta(),
  
  ### Use of screens during lockdown:
  screens_post = file.path(OUTCOME_PATH_COVID, "Screens.dta") %>%
    read_dta(),
  
  
  ## Socioeconomic factors, post lockdown: ----
  economic_post = file.path(OUTCOME_PATH_COVID, "Economic_POST.dta") %>%
    read_dta()
  
  
  ## END ----
)


## ---- collapse-datasets ----

dataset_outcome <- outcome_datasets %>%
  reduce(full_join, by = POST_LD_ID_VARS)


## ---- add-weights ----

dataset_weights <- file.path(OUTPUT_PATH, "pesos.dta") %>% read_dta()

dataset_outcome <- dataset_outcome %>%
  full_join(dataset_weights, by = "number_id")


## ----add-interview-dates----
dataset_outcome <- dataset_outcome %>%
  full_join(
    dataset_master_pre %>%
      select(all_of(BASELINE_ID_VARS), date_pre = q0006_date),
    by = BASELINE_ID_VARS
  ) %>%
  full_join(
    dataset_master_post %>%
      select(all_of(POST_LD_ID_VARS), date_post = FECHAFIN),
    by = POST_LD_ID_VARS
  )

## ----add-post-interview-state----
dataset_outcome <- dataset_master_post %>%
  select(all_of(POST_LD_ID_VARS), ESTADO_ENTREVISTA) %>%
  full_join(
    dataset_master_pre %>% select(all_of(BASELINE_ID_VARS), proxy),
    by = BASELINE_ID_VARS
  ) %>%
  full_join(dataset_outcome, by = POST_LD_ID_VARS)


## ----delete-duplicate----

# There is a duplicated case (2079) that needs to be deleted:
dataset_outcome <- dataset_outcome %>% filter(!number_id %in% DUPLICATES)

## ----save-output----

# dataset_outcome %>% write_dta(OUTPUT_FILE, version = 13)


## ---- session-info ----

devtools::session_info()
## - Session info --------------------------------------------------------------
##   setting  value                       
## version  R version 4.0.3 (2020-10-10)
## os       Windows 10 x64              
## system   x86_64, mingw32             
## ui       RStudio                     
## language (EN)                        
## collate  Spanish_Spain.1252          
## ctype    Spanish_Spain.1252          
## tz       Europe/Paris                
## date     2021-01-20                  
## 
## - Packages ------------------------------------------------------------------
##   package     * version date       lib source        
## assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.0.3)
## backports     1.2.1   2020-12-09 [1] CRAN (R 4.0.2)
## broom         0.7.3   2020-12-16 [1] CRAN (R 4.0.3)
## callr         3.5.1   2020-10-13 [1] CRAN (R 4.0.3)
## cellranger    1.1.0   2016-07-27 [1] CRAN (R 4.0.3)
## cli           2.2.0   2020-11-20 [1] CRAN (R 4.0.3)
## colorspace    2.0-0   2020-11-11 [1] CRAN (R 4.0.3)
## crayon        1.3.4   2017-09-16 [1] CRAN (R 4.0.3)
## DBI           1.1.0   2019-12-15 [1] CRAN (R 4.0.3)
## dbplyr        2.0.0   2020-11-03 [1] CRAN (R 4.0.3)
## desc          1.2.0   2018-05-01 [1] CRAN (R 4.0.3)
## devtools      2.3.2   2020-09-18 [1] CRAN (R 4.0.3)
## digest        0.6.27  2020-10-24 [1] CRAN (R 4.0.3)
## dplyr       * 1.0.2   2020-08-18 [1] CRAN (R 4.0.3)
## ellipsis      0.3.1   2020-05-15 [1] CRAN (R 4.0.3)
## fansi         0.4.1   2020-01-08 [1] CRAN (R 4.0.3)
## forcats     * 0.5.0   2020-03-01 [1] CRAN (R 4.0.3)
## fs            1.5.0   2020-07-31 [1] CRAN (R 4.0.3)
## generics      0.1.0   2020-10-31 [1] CRAN (R 4.0.3)
## ggplot2     * 3.3.3   2020-12-30 [1] CRAN (R 4.0.3)
## glue          1.4.2   2020-08-27 [1] CRAN (R 4.0.3)
## gtable        0.3.0   2019-03-25 [1] CRAN (R 4.0.3)
## haven       * 2.3.1   2020-06-01 [1] CRAN (R 4.0.3)
## hms           0.5.3   2020-01-08 [1] CRAN (R 4.0.3)
## httr          1.4.2   2020-07-20 [1] CRAN (R 4.0.3)
## jsonlite      1.7.2   2020-12-09 [1] CRAN (R 4.0.3)
## lifecycle     0.2.0   2020-03-06 [1] CRAN (R 4.0.3)
## lubridate     1.7.9.2 2020-11-13 [1] CRAN (R 4.0.3)
## magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.0.3)
## memoise       1.1.0   2017-04-21 [1] CRAN (R 4.0.3)
## modelr        0.1.8   2020-05-19 [1] CRAN (R 4.0.3)
## munsell       0.5.0   2018-06-12 [1] CRAN (R 4.0.3)
## pacman      * 0.5.1   2019-03-11 [1] CRAN (R 4.0.3)
## pillar        1.4.7   2020-11-20 [1] CRAN (R 4.0.3)
## pkgbuild      1.2.0   2020-12-15 [1] CRAN (R 4.0.3)
## pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.0.3)
## pkgload       1.1.0   2020-05-29 [1] CRAN (R 4.0.3)
## prettyunits   1.1.1   2020-01-24 [1] CRAN (R 4.0.3)
## processx      3.4.5   2020-11-30 [1] CRAN (R 4.0.3)
## ps            1.5.0   2020-12-05 [1] CRAN (R 4.0.3)
## purrr       * 0.3.4   2020-04-17 [1] CRAN (R 4.0.3)
## R6            2.5.0   2020-10-28 [1] CRAN (R 4.0.3)
## Rcpp          1.0.5   2020-07-06 [1] CRAN (R 4.0.3)
## readr       * 1.4.0   2020-10-05 [1] CRAN (R 4.0.3)
## readxl        1.3.1   2019-03-13 [1] CRAN (R 4.0.3)
## remotes       2.2.0   2020-07-21 [1] CRAN (R 4.0.3)
## reprex        0.3.0   2019-05-16 [1] CRAN (R 4.0.3)
## rlang         0.4.10  2020-12-30 [1] CRAN (R 4.0.3)
## rprojroot     2.0.2   2020-11-15 [1] CRAN (R 4.0.3)
## rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.0.3)
## rvest         0.3.6   2020-07-25 [1] CRAN (R 4.0.3)
## scales        1.1.1   2020-05-11 [1] CRAN (R 4.0.3)
## sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 4.0.3)
## stringi       1.5.3   2020-09-09 [1] CRAN (R 4.0.3)
## stringr     * 1.4.0   2019-02-10 [1] CRAN (R 4.0.3)
## testthat      3.0.1   2020-12-17 [1] CRAN (R 4.0.3)
## tibble      * 3.0.4   2020-10-12 [1] CRAN (R 4.0.3)
## tidyr       * 1.1.2   2020-08-27 [1] CRAN (R 4.0.3)
## tidyselect    1.1.0   2020-05-11 [1] CRAN (R 4.0.3)
## tidyverse   * 1.3.0   2019-11-21 [1] CRAN (R 4.0.3)
## usethis       2.0.0   2020-12-10 [1] CRAN (R 4.0.3)
## utf8          1.1.4   2018-05-24 [1] CRAN (R 4.0.3)
## vctrs         0.3.6   2020-12-17 [1] CRAN (R 4.0.3)
## withr         2.3.0   2020-09-22 [1] CRAN (R 4.0.3)
## xml2          1.3.2   2020-04-23 [1] CRAN (R 4.0.3)
## yaml          2.2.1   2020-02-01 [1] CRAN (R 4.0.3)
## 
## [1] C:/Users/Proyecto1/Documents/R/win-library/4.0
## [2] C:/Program Files/R/R-4.0.3/library
