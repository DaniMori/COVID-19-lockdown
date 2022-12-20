## ---- SCRIPT SETUP: ----------------------------------------------------------

## ----global-configuration----

rm(list = ls())

## ----script-configuration----

# Graphical output configuration:
ggplot2::theme_set(ggplot2::theme_minimal())


## ---- MAIN: ------------------------------------------------------------------


## ---- Shared chunks: ---------------------------------------------------------

## ----includes----

library(tidyverse)
library(ecs.data)
library(magrittr)
library(haven)
library(flextable)
library(janitor)
library(broom)
library(psych)
library(GGally)
library(plotly)
library(ggpointdensity)
library(lmerTest)
library(WeMix)
library(lubridate)
library(ggalluvial)
library(ggrepel)
library(MESS)
library(modelr)
library(forcats)
library(RStata)
library(survey)
library(officer)
library(ftExtra)
library(effsize)
library(xfun)
library(rmarkdown)

source("R/Data.R",          encoding = 'UTF-8')
source("R/Output.R",        encoding = 'UTF-8')
source("R/Stats_toolbox.R", encoding = 'UTF-8')
source("R/Stata_output.R",  encoding = 'UTF-8')

source("R/Lang_EN.R", encoding = 'UTF-8')


## ----reliability----

source("src/Reliability_lockdown_measures.R", encoding = 'UTF-8') # Fails in R 4.1.0!


## ----constants----

PROJECT_NAME <- "Edad con Salud" %>% ftext(
  fp_text(font.family = "Times New Roman", font.size = 12)
)

# File system:
DOC_PATH_MASTER <- read_ecs_folder("DOC")
DB_PATH         <- file.path(
  DOC_PATH_MASTER,
  "Edad con salud - Subestudio COVID",
  "BBDD_con_outcomes"
)

# Collapsed outcomes dataset:
OUTCOMES_DB_FILE <- file.path(DB_PATH, "Outcomes_collapsed.dta")


# Variable constants:
ORDINAL_VARS <- c(
  "edu_level_rec",
  "severity",
  # "ucla_lon_pre", # TODO: Treat these as ordinal??
  # "ucla_lon_post",
  "pain_difficulties",
  "d_lifetime_severity",
  "d_12m_severity",
  "depression_severity_pre",
  "depression_severity_post",
  "pers_rel_affected",
  # "physical",
  "physical_pre",
  "physical_post",
  "sleep_how_pre",
  "sleep_quality_pre",
  "sleep_how_post",
  "sleep_quality_post",
  "sleep_change_post"
)

PREDICTORS_ORIGINAL <- c(
  "sex",
  "edu_level_rec",
  "hhincomemid",
  "severity",
  "rel_isolated",
  "rel_concerned",
  "livesalone_pre",
  "livesalone_post",
  "ucla_lon_pre",
  "ucla_lon_post",
  "oslo3_sss_pre",
  "oslo3_sss_post",
  "whodas12_post",
  "physical_pre",
  "quietness",
  "economy",
  "unemployment",
  "resilience_pre",
  "resilience_post",
  "screen_non_work",
  "screen_work",
  "pain_difficulties",
  "pain_new",
  # "pain_physical",
  "pain_worsened",
  "depression_pre", # These two measures are used as predictors in the
  "depression_post" #   `suicidal ideation` model.
)

PREDICTORS_CHANGE <- c(
  "age",
  "physical",
  "resilience"
)

# Fixed predictors for the incidence models:
FIXED_PREDICTORS <- c("sex", "age_pre")

CRITERIA <- c("depression", "suicidal", "sleeping_time", "sleep_quality")


# Value constants:

PRE_LEVEL  <- "Pre"
POST_LEVEL <- "Post"
MEASURE_LEVELS <- c(PRE_LEVEL, POST_LEVEL)

AGE_GROUPS_OUTPUT <- AGE_GROUPS %>% glue_collapse(sep = ", ", last = ", and ")


# Statistical constants:

SIG_LEVEL <- .05
sig_level_print <- paste0(
  "$\\mathrm{\\alpha}$ = ",
  SIG_LEVEL %>% format_prop_like(sig = 2)
)

CI_WIDTH  <- (1 - SIG_LEVEL) %>% percent()
CI_FACTOR <- (SIG_LEVEL / 2) %>% qnorm(lower.tail = FALSE)

MAX_MISSING <- .20
max_missing_print <- MAX_MISSING %>% percent()


## ----load-data----
dataset_outcomes <- OUTCOMES_DB_FILE %>% read_dta()

## ---- compute-new-depr-vars ----
DESCRIPTION_PRE_FILEPATH  <- "notebooks/Description_depression_ICD10_pre"
DESCRIPTION_POST_FILEPATH <- "notebooks/Description_depression_ICD10_post"

Rscript_call(
  render,
  list(input = DESCRIPTION_PRE_FILEPATH |> paste0(".Rmd"))
)
Rscript_call(
  render,
  list(input = DESCRIPTION_POST_FILEPATH |> paste0(".Rmd"))
)

# docx outputs are not necessary so they are deleted:
file.remove(
  c(DESCRIPTION_PRE_FILEPATH, DESCRIPTION_POST_FILEPATH) |> paste0(".docx")
)

depression_pre_new_dataset <- file.path(
  "dat",
  "Outcome_depression_ICD10_pre.dta"
) |>
  read_stata()
depr_pre_new_vars_all_cases <- depression_pre_new_dataset |>
  select(
    q0002_hhid, number_id,
    ends_with(c("lifetime", "12m", "comparable")),
    -matches("severity")
  )
depr_pre_new_vars <- depr_pre_new_vars_all_cases |>
  semi_join(dataset_outcomes, by = c("number_id", "q0002_hhid"))

depression_post_new_dataset <- file.path(
  "dat",
  "Outcome_depression_ICD10_post.dta"
) |>
  read_stata()
depr_post_new_vars <- depression_post_new_dataset |>
  rename(number_id = IDENTIFICA1, q0002_hhid = IDENTIFICA2) |>
  select(-ID_ECS) |>
  semi_join(
    dataset_outcomes,
    by = c("number_id", "q0002_hhid")
  )

n_depr_pre_new_vars  <- depr_pre_new_vars  |> nrow()
n_depr_post_new_vars <- depr_post_new_vars |> nrow() # También igual a 1103

## ---- depr-pre-new-vars-corrected ----
dataset_outcomes <- dataset_outcomes |>
  select(
    everything(),
    -matches("depression"),
    matches("severity") # "Depression severity" raise error if dropped
  ) |>
  full_join(
    depr_pre_new_vars_all_cases |> select(
      q0002_hhid, number_id,
      depression_lifetime, depression_pre = d_12m_comparable
    ),
    by = c("number_id", "q0002_hhid")
  ) |>
  full_join(
    depr_post_new_vars |>
      select(q0002_hhid, number_id, depression_post = depression_30d),
    by = c("number_id", "q0002_hhid"))

## ----compute-interview-dates----

date_ranges <- dataset_outcomes %>%
  summarize(across(starts_with("date"), range, na.rm = TRUE))

## Suppress 'Multiple formats matched: "%Om %d, %Y"(1), "%B %d, %Y"(1)
##             Using: "%B %d, %Y"'
suppressMessages(
  format_date <- stamp_date("January 17, 2019", locale = "C")
)

dates_init     <- date_ranges %>% slice(1)
date_init_pre  <- dates_init %>% pull(date_pre)  %>% format_date()
date_init_post <- dates_init %>% pull(date_post) %>% format_date()

dates_end     <- date_ranges %>% slice(2)
date_end_pre  <- dates_end %>% pull(date_pre)  %>% format_date()
date_end_post <- dates_end %>% pull(date_post) %>% format_date()


## ----subset-cases----

n_initial   <- dataset_outcomes %>% summarize(n()) %>% pull()
n_proxy_pre <- dataset_outcomes %>%
  count(proxy) %>%
  filter(proxy == 1) %>%
  pull()

# Only valid non-proxy cases (in the Pre measure)
dataset_outcomes <- dataset_outcomes %>% filter(proxy == 2)

n_pre_measure <- dataset_outcomes %>% summarize(n()) %>% pull()

n_interview_state <- dataset_outcomes %>% count(ESTADO_ENTREVISTA)
n_missing  <- n_interview_state %>% filter(is.na(ESTADO_ENTREVISTA)) %>% pull()
n_reject   <- n_interview_state %>%
  filter(ESTADO_ENTREVISTA %in% c(2, 10)) %>%
  pull() %>%
  sum()
n_deceased <- n_interview_state %>% filter(ESTADO_ENTREVISTA == 7) %>% pull()
n_incident <- n_interview_state %>%
  filter(ESTADO_ENTREVISTA %in% c(4, 8)) %>%
  pull() %>%
  sum()
n_proxy_post <- n_interview_state %>% filter(ESTADO_ENTREVISTA == 9) %>% pull()
n_unreachable <- n_interview_state %>%
  filter(ESTADO_ENTREVISTA %in% c(6, 11:15)) %>%
  pull() %>%
  sum()
n_excluded <- n_interview_state %>%
  filter(!ESTADO_ENTREVISTA == 1) %>%
  tally(n) %>%
  pull()

n_tota_excluded <- n_missing + n_excluded

dataset_outcomes_compare_excluded <- dataset_outcomes %>%
  mutate(included = (ESTADO_ENTREVISTA == 1) %|% FALSE)

dataset_outcomes <- dataset_outcomes %>% filter(ESTADO_ENTREVISTA == 1)

n_final <- dataset_outcomes %>% summarize(n()) %>% pull()


## ----compare-excluded-cases----

dataset_outcomes_compare_excluded <- dataset_outcomes_compare_excluded %>%
  as_factor() # Facilitates the treatment of the variables


### Attrition by sex:
attr_test_sex <- dataset_outcomes_compare_excluded %>%
  chisq_test_df_var(included, sex)
attr_sex_test_format <- attr_test_sex %>% print_chisq_test()
attr_sex_cramerV     <- attr_test_sex %$% format_prop_like(Cramer_V)

attr_prop_sex <- dataset_outcomes_compare_excluded %>%
  tabyl(included, sex) %>%
  adorn_percentages() # There is a higher proportion of excluded males
prop_males_excluded <- attr_prop_sex %>%
  filter(!included) %>%
  pull(Male) %>%
  percent(1e-1)
prop_males_included <- attr_prop_sex %>%
  filter(included) %>%
  pull(Male) %>%
  percent(1e-1)


### Attrition by age:
attr_test_age <- dataset_outcomes_compare_excluded %>% 
  t.test(age_pre ~ included, data = .) %>%
  tidy()
attr_age_test_format <- attr_test_age %>% print_t_test()

attr_age_hedges_g <- dataset_outcomes_compare_excluded %>%
  cohen.d(age_pre ~ factor(included), data = .)
attr_age_hedges_g_format <- attr_age_hedges_g %>%
  extract2("estimate")                        %>%
  number(1e-3)
attr_age_hedges_g_magnitude <- attr_age_hedges_g %>% extract2("magnitude")

attr_desc_age <- dataset_outcomes_compare_excluded %>%
  group_by(included) %>%
  summarize(
    across(age_pre, lst(mean, sd), .names = "{.fn}"),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.double), number, 1e-1))

age_included      <- attr_desc_age %>% filter(included)
mean_age_included <- age_included %>% pull(mean)
sd_age_included   <- age_included %>% pull(sd)

age_excluded      <- attr_desc_age %>% filter(!included)
mean_age_excluded <- age_excluded %>% pull(mean)
sd_age_excluded   <- age_excluded %>% pull(sd)


### Attrition by depression:
attr_test_depr <- dataset_outcomes_compare_excluded %>%
  chisq_test_df_var(included, depression_pre)
attr_depr_test_format <- attr_test_depr %>% print_chisq_test()
attr_depr_cramerV     <- attr_test_depr %$% format_prop_like(Cramer_V)


### Attrition by suicidal ideation:
attr_test_suic <- dataset_outcomes_compare_excluded %>%
  chisq_test_df_var(included, suicidal_pre, correct = FALSE)
attr_suic_test_format <- attr_test_suic %>% print_chisq_test()
attr_suic_cramerV     <- attr_test_suic %$% format_prop_like(Cramer_V)

attr_prop_suic <- dataset_outcomes_compare_excluded %>%
  tabyl(included, suicidal_pre) %>%
  adorn_percentages() # There is a higher proportion of excluded males
prop_suic_excluded <- attr_prop_suic %>%
  filter(!included) %>%
  pull(Yes) %>%
  percent(1e-2)
prop_suic_included <- attr_prop_suic %>%
  filter(included) %>%
  pull(Yes) %>%
  percent(1e-2)


## ----preprocess-data----

dataset_outcomes <- dataset_outcomes %>%
  mutate_if(is.labelled, as_factor) %>%
  mutate_at(vars(starts_with("physical")), fct_rev) %>% # Revert order of levels
  # As we have agreed to use the original `physical` as the `pre`:
  select(-physical_pre) %>% rename(physical_pre = physical) %>%
  mutate(
    across(all_of(ORDINAL_VARS), as.ordered),
    across(edu_level_rec, fct_relabel, substr, 1, 17)
  ) %>%
  correct_weights(id_var = ID_CONTACTO)

# Omit predictors from time-constant variables that may have the same name as
#   time-varying predictos, to avoid name collisions.
dataset_outcomes <- dataset_outcomes %>% select(!any_of(PREDICTORS_CHANGE))


## ----create-var-labels----

# Get the original variable labels for the predictors, and change the
#   ones that are not appropriate
var_descriptors <- dataset_outcomes %>%
  select(
    all_of(PREDICTORS_ORIGINAL),
    starts_with(PREDICTORS_CHANGE),
    -age_post,
    suicidal_pre, suicidal_post
  ) %>%
  map_chr(~attr(., "label") %||% NA_character_) |>
  c(depression_lifetime = "Depression (lifetime)")

var_descriptors[
  c(
    "edu_level_rec", "hhincomemid", "age_pre",
    "severity",
    "ucla_lon_pre", "ucla_lon_post", "oslo3_sss_pre", "oslo3_sss_post",
    "whodas12_post",
    "physical_pre", "physical_post",
    "resilience_pre", "resilience_post",
    "pain_difficulties", #"pain_physical",
    "depression_pre", "depression_post"
  )
] <- c(
  "Education level", "Household income", "Age",
  "Severity of COVID-19 infection",
  "UCLA loneliness scale (pre lockdown)",
  "UCLA loneliness scale (post lockdown)",
  "Oslo-3 social support scale (pre lockdown)",
  "Oslo-3 social support scale (post lockdown)",
  "12-item WHODAS disability scale (post lockdown)",
  "Physical activity level (pre lockdown)",
  "Physical activity level (post lockdown)",
  "Resilience scale (pre lockdown)",
  "Resilience scale (post lockdown)",
  "Difficulties in everyday activities due to pain? (post lockdown)",
  # "Physical pain (post lockdown)",
  "Depression (pre lockdown)", "Depression (post lockdown)"
)

labels_abbr <- c(
  "Sex", "Education level", "Income",
  "COVID-19 infection", "COVID-19 co-habitant", "COVID-19 concern",
  "Living alone", "Living alone",
  "Loneliness", "Loneliness",
  "Social support", "Social support",
  "Disability",
  "Physical activity",
  "Home quietness",
  "Economy worsened",
  "Unemployed",
  "Resilience", "Resilience",
  "Working screen time (h)",
  "Non-working screen time (h)",
  "Pain-related difficulties",
  "Pain new",
  # "Physical pain",
  "Pain worsened",
  "Depression", "Depression",
  "Age",
  "Physical activity",
  "Suicidal ideation", "Suicidal ideation",
  "Depression (lifetime)"
) %>%
  set_names(names(var_descriptors))

var_measure <- c(
  PRE_LEVEL, PRE_LEVEL, PRE_LEVEL,
  POST_LEVEL, POST_LEVEL, POST_LEVEL,
  PRE_LEVEL, POST_LEVEL,
  PRE_LEVEL, POST_LEVEL,
  PRE_LEVEL, POST_LEVEL,
  POST_LEVEL,
  PRE_LEVEL,
  POST_LEVEL,
  POST_LEVEL,
  POST_LEVEL,
  PRE_LEVEL, POST_LEVEL,
  POST_LEVEL,
  POST_LEVEL,
  POST_LEVEL,
  POST_LEVEL,
  POST_LEVEL,
  PRE_LEVEL, POST_LEVEL,
  PRE_LEVEL,
  POST_LEVEL,
  PRE_LEVEL, POST_LEVEL,
  # POST_LEVEL
  PRE_LEVEL
) %>%
  set_names(names(var_descriptors))

var_properties <- tibble(
  predictor = names(var_descriptors),
  var_descriptors, labels_abbr, var_measure
) %>%
  mutate(
    append_measure = c(
      FALSE %>% rep(6), TRUE  %>% rep(8), FALSE %>% rep(3),
      TRUE  %>% rep(2), FALSE %>% rep(5), TRUE  %>% rep(6),
      FALSE
    ),
    labels_comp = labels_abbr %>% paste0(" (", var_measure, ")"),
    labels_comp = append_measure %>%
      if_else(labels_comp, labels_abbr) %>%
      set_names(predictor)
  )

edu_level_cats <- dataset_outcomes %>% enumerate_levels(edu_level_rec)
severity_cats  <- dataset_outcomes %>% enumerate_levels(severity)
physical_cats  <- dataset_outcomes %>% enumerate_levels(physical_pre)
pain_cats      <- dataset_outcomes %>% enumerate_levels(pain_physical)


## ----missing-responses----
perc_missing <- dataset_outcomes %>%
  transmute(
    across(
      c(
        all_of(PREDICTORS_ORIGINAL),
        starts_with(PREDICTORS_CHANGE)
      ),
      is.na
    )
  ) %>%
  summarize(across(everything(), mean)) %>%
  pivot_longer(everything(), names_to = "Variable") %>%
  mutate(`% Missing` = value %>% percent(.01))

valid_vars <- perc_missing %>%
  filter(
    Variable %in% PREDICTORS_ORIGINAL,
    value <= MAX_MISSING,
  ) %>%
  pull(Variable)


## ----time-varying-predictors----

dataset_outcomes_varying <- dataset_outcomes %>%
  select_at(
    vars(
      ID_CONTACTO,
      starts_with(PREDICTORS_CHANGE)
    )
  ) %>%
  pivot_longer(
    !ID_CONTACTO,
    names_to = c(".value", "Measure"),
    names_sep = "_"
  ) %>%
  mutate(Measure = Measure %>% fct_relabel(chartr, old = "p", new = "P"))


# Dicards `resilience` due to a high rate of missing data (in `resilience_pre`)
time_varying_preds <- dataset_outcomes_varying %>%
  select(-ID_CONTACTO) %>%
  mutate_all(is.na) %>%
  summarize_all(mean) %>%
  gather("Variable") %>%
  filter(value <= MAX_MISSING) %>%
  pull(Variable) %>%
  extract(-1) # Measure does not count as a time varying variable


## ----standardize-predictors----

scaled_preds <- c(
  "ucla_lon_pre",   "ucla_lon_post",
  "oslo3_sss_pre",  "oslo3_sss_post",
  "health_abb_pre", "health_abb_post",
  "whodas12_post",
  "home_score",
  "resilience_post",
  "pain_score"
)

dataset_outcomes_std <- dataset_outcomes %>%
  mutate_at(scaled_preds, ~scale(.) %>% drop()) %>%
  mutate(age_pre = age_pre - 50L, age_post = age_post - 50L)

dataset_outcomes_varying_std <- dataset_outcomes_varying %>%
  mutate(age = age - 50L)


## ----subset-predictors----

# New predictors for the "new models" (models of risk of a dichotomous outcome
#   for the cases that haven't had it previously)
all_preds          <- valid_vars %>% c("age_pre", "physical_post")
dataset_predictors <- dataset_outcomes %>% select(all_of(all_preds))
ordinal_preds      <- dataset_predictors %>%
  select_if(is.ordered) %>%
  colnames()

## ----var-descriptives----

dataset_outcomes <- dataset_outcomes |>
  mutate(
    depression_lifetime  = depression_lifetime |>
      as_factor() |>
      set_attr("label", "Lifetime depression"),
    depression_pre = depression_pre |>
      as_factor() |>
      set_attr("label", "Depression"),
    depression_post = depression_post |>
      as_factor() |>
      set_attr("label", "Depression")
  )

dataset_outcomes_descr_correct <- dataset_outcomes %>% select(
  ID_CONTACTO,
  depression_lifetime, depression_pre, depression_post,
  matches("^suicidal_(pre|post)$"),
  all_of(all_preds)
)

quant_descriptives_out <- dataset_outcomes_descr_correct %>%
  select(-ID_CONTACTO) %>%
  describe(skew = FALSE, omit = TRUE) %>%
  as.data.frame() %>%
  mutate(
    var      = rownames(.),
    Variable = var_descriptors[var],
    n        = n %>% as.integer() # Prevents printing it with 2 decimals
  ) %>%
  as_tibble() %>%
  select(var, Variable, n, mean, sd) %>%
  mutate(across(where(is.double), number, 1e-2))

cat_descriptives_out <- dataset_outcomes_descr_correct %>%
  select(where(is.factor)) %>%
  frequencies_table(missing = FALSE)

sample_contrast_vars <- dataset_outcomes_descr_correct %>%
  select(ID_CONTACTO, ends_with(c("_pre", "post"))) %>%
  pivot_longer(
    -ID_CONTACTO,
    names_to = c(".value", "Measure"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  mutate(
    Measure = Measure %>% chartr("p", "P", x = .) %>% factor(MEASURE_LEVELS)
  )

sample_contrasts <- bind_rows(
  sample_contrast_vars %>% paired_t_tests_df(Measure, ID_CONTACTO),
  sample_contrast_vars %>% mcnemar_tests_df(Measure, ID_CONTACTO)
) %>%
  mutate(# There is always Pre and Post, so it is indifferent which one to use:
    var     = var %>% paste0("_pre"),
    p.value = p.value %>% format_pvalues()
  ) %>%
  left_join(
    var_properties %>% select(var = predictor, labels_abbr),
    by = "var"
  ) %>%
  select(labels_abbr, cat, p.value)

quant_total_out <- quant_descriptives_out %>%
  mutate(
    var_cat = var_properties$labels_abbr[var],
    Measure = var_properties$var_measure[var]
  ) %>%
  select(-var, -Variable) %>%
  pivot_wider(names_from = Measure, values_from = n:sd) %>%
  left_join(sample_contrasts, by = c(var_cat = "labels_abbr")) %>%
  mutate(var_cat = var_cat %>% paste0(", mean (sd)")) %>%
  select(var_cat, ends_with(c("Pre", "Post")), p.value) %>%
  rename_with(str_replace, starts_with("mean"), "mean", "stat1") %>%
  rename_with(str_replace, starts_with("sd"),   "sd",   "stat2") %>%
  mutate(across(starts_with("stat2"), enclose, "(")) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(is_cat = FALSE) %>% # For left-padding the paragraphs of categories
  select(-starts_with("n_")) # Not reporting "N total"

cat_total_out <- cat_descriptives_out %>%
  group_by(Variable) %>%
  mutate(
    var_cat = var_properties$labels_abbr[Variable],
    Measure = var_properties$var_measure[Variable],
    stat1   = if_else(n() != 3 & Level == "Total", NA_integer_, N),
    stat2   = if_else(
      Level == "Total",
      NA_character_,
      `Percent valid` %>% enclose("(")
    )
  ) %>%
  left_join(
    sample_contrasts,
    by = c(var_cat = "labels_abbr", Level = "cat")
  ) %>%
  mutate(across(where(is.numeric), as.character)) %>%
  ungroup() %>%
  select(-Variable, -(N:`Percent valid`)) %>%
  pivot_wider(
    names_from  = Measure,
    # values_from = n:stat2 # Not reporting "N total"
    values_from = stat1:stat2
  ) %>%
  filter(!Level %in% c("Male", "No")) %>%
  select(var_cat, Level, ends_with(c("Pre", "Post")), p.value) %>%
  mutate(aux = var_cat) %>%
  group_by(aux) %>%
  mutate(
    Level = Level %>% if_else(
      condition = !. %in% c("Yes", "Total") & n() == 2,
      true      = enclose(., "(") %>% paste0(", n (%)"),
      false     = .
    ),
    var_cat = if_else(
      Level %in% c("Yes", "Total"),
      var_cat %>% paste0(", n (%)"),
      if_else(
        n() == 2 & !Level %in% c("Yes", "Total"),
        var_cat %>% paste(Level),
        paste0(Level)
      )
    ), # For left-padding the paragraphs of categories:
    is_cat = !Level %in% c("Yes", "Total") & n() != 2,
  ) %>%
  filter(!(n() == 2 & Level == "Total")) %>%
  ungroup() %>%
  select(-aux, -Level)

total_descriptives_out <- quant_total_out %>%
  bind_rows(cat_total_out) %>%
  slice( # Custom order:
    7,     # Age
    11,    # Sex (Female)
    12:16, # Education level (Less than primary, Primary, Secondary, Tertiary)
    8,     # Depression lifetime
    9,     # Depression
    10,    # Suicidal ideation
    4,     # Resilience
    23,    # Living alone
    2,     # Social support
    1,     # Loneliness
    21:22, # COVID-19 co-habitant, COVID-19 concern,
    17:20, # COVID-19 severity (Not infected, Infected, Hospitalized)
    3,     # WHODAS
    31:35, # Physical pain (None, Light, Moderate, Severe)
    5:6,   # Working screen time, Non-working screen time
    28,    # Home quietness
    29:30, # Economy worsened, Unemployed
    24:27  # Physical activity (Low, Moderate, High)
  )

cat_index <- total_descriptives_out %>% pull(is_cat)
total_descriptives_out <- total_descriptives_out %>% select(-is_cat)

extra_footnote  <- c(13, 14, 21)

total_descriptives_out <- total_descriptives_out %>%
  mutate(
    var_cat = var_cat %>% paste0(
      if_else(
        row_number() %in% extra_footnote,
        FOOTNOTE_SYMBOL[1] %>% enclose('^'),
        ""
      )
    )
  ) %>%
  flextable() %>%
  set_header_df(
    tibble(
      col_keys = total_descriptives_out %>% colnames(),
      measure  = c(
        # Not reporting "N total"
        # "Variable / Category", PRE_LEVEL  %>% rep(3), POST_LEVEL %>% rep(3)
        "Variable",
        MEASURE_LEVELS %>% paste0("-confinement") %>% rep(each = 2),
        "*p* value"
      )
    )
  ) %>%
  colformat_md(j = 6, part = "header") %>%
  colformat_md(j = 1, part = "all") %>%
  add_footer(var_cat = DESCRIPTIVES_FOOTER) %>%
  merge_at(part = "footer") %>%
  footnote(
    value       = as_paragraph(
      c(
        FOOTNOTE_VARS_SCALE,
        var_descriptors[c("rel_isolated", "rel_concerned", "severity")] %>%
          paste0('.')
      )
    ),
    i           = c(11, 15:17),
    j           = 1,
    ref_symbols = FOOTNOTE_SYMBOL[1:4]
  ) %>%
  footnote(
    value       = as_paragraph(FOOTNOTE_DESCRIPTIVE_P_VALUES),
    i           = 1,
    j           = 6,
    part        = "header",
    ref_symbols = '*'
  ) %>%
  merge_h(part = "header") %>%
  theme_booktabs() %>%
  flextable::style(i = cat_index, j = 1, pr_p = fp_par(padding.left = 30)) %>%
  align(j = c(2, 4, 6), align = "right", part = "all") %>%
  align(i = 1, align = "center", part = "header") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  padding(padding.top = 1, padding.bottom = 1) %>%
  border(border.bottom = fp_border(style = "none"), part = "footer") %>%
  autofit()

## ---- Prevalence: ----

## ----depression-dataset----

# Drop from the whole predictor set the "Depression" variables:
depr_preds <- all_preds %>% setdiff(c("depression_pre", "depression_post"))

depression <- dataset_outcomes_std %>%
  select(
    ID_CONTACTO,
    depression_pre, depression_post,
    any_of(depr_preds),
    starts_with(time_varying_preds),# These are needed for the linear model with
    weights                         #   negative cases in the Pre measure
  ) %>%
  rename(Pre = depression_pre, Post = depression_post) %>%
  mutate_at(vars(Pre, Post), factor, labels = c("No", "Yes")) %>%
  mutate( # Necessary for the prevalence estimates by age:
    Age = (age_post + 50L) %>% group_ages() # Because it is centered at 50
  )


## ----depression-prevalence-estimates----

# Estimate the prevalence with the whole sample size in the Pre measure.

depression_pre <- dataset_outcomes_compare_excluded %>%
  select(number_id, Pre = depression_pre, sex, age_pre, weights) %>%
  mutate(Age = age_pre %>% group_ages())

depression_prevalence_pre <- depression_pre %>%
  {
    bind_rows(
      bind_cols(
        tibble(Variable = "Total", Group = "Total"),
        estimate_prevalence(., Pre, weights, "Yes")
      ),
      tibble(Variable = "Sex", Group = "Sex"),
      group_by(., Group = sex) %>% estimate_prevalence(Pre, weights, "Yes") %>%
        add_column(Variable = "Sex"),
      tibble(Variable = "Age", Group = "Age"),
      group_by(., Group = Age) %>% estimate_prevalence(Pre, weights, "Yes") %>%
        add_column(Variable = "Age")
    )
  }

depression_prevalence_post <- depression %>% {
  bind_rows(
    bind_cols(
      tibble(Variable = "Total", Group = "Total"),
      estimate_prevalence(., Post, weights, "Yes"),
      mcnemar_test_df_pre_post(., Pre, Post, "Yes", weight = weights)
    ),
    tibble(Variable = "Sex", Group = "Sex"),
    group_by(., Group = sex) %>% {
      full_join(
        estimate_prevalence(., Post, weights, "Yes"),
        mcnemar_test_df_pre_post(., Pre, Post, "Yes", weight = weights),
        by = "Group"
      )
    } %>%
      add_column(Variable = "Sex"),
    tibble(Variable = "Age", Group = "Age"),
    group_by(., Group = Age) %>% {
      full_join(
        estimate_prevalence(., Post, weights, "Yes"),
        mcnemar_test_df_pre_post(., Pre, Post, "Yes", weight = weights),
        by = "Group"
      )
    } %>%
      add_column(Variable = "Age")
  )
}


## ----suicidal-dataset----
suicidal <- dataset_outcomes_std %>%
  select(
    ID_CONTACTO,
    starts_with("suicidal"),
    any_of(all_preds),
    starts_with(time_varying_preds),# These are needed for the linear model with
    weights                         #   negative cases in the Pre measure
  ) %>%
  rename(Pre = suicidal_pre, Post = suicidal_post) %>%
  drop_na(Pre, Post) %>%
  mutate(weights = weights / mean(weights)) %>%
  mutate( # Necessary for the prevalence estimates by age:
    Age = (age_post + 50L) %>% group_ages() # Because it is centered at 50
  )

## ----suicidal-prevalence-estimates----

# Estimate the prevalence with the whole sample size in the Pre measure.

suicidal_pre <- dataset_outcomes_compare_excluded %>%
  select(number_id, Pre = suicidal_pre, sex, age_pre, weights) %>%
  mutate(Age = age_pre %>% group_ages())

suicidal_prevalence_pre <- suicidal_pre %>%
  {
    bind_rows(
      bind_cols(
        tibble(Variable = "Total", Group = "Total"),
        estimate_prevalence(., Pre, weights, "Yes")
      ),
      tibble(Variable = "Sex", Group = "Sex"),
      group_by(., Group = sex) %>% estimate_prevalence(Pre, weights, "Yes") %>%
        add_column(Variable = "Sex"),
      tibble(Variable = "Age", Group = "Age"),
      group_by(., Group = Age) %>% estimate_prevalence(Pre, weights, "Yes") %>%
        add_column(Variable = "Age")
    )
  }

suicidal_prevalence_post <- suicidal %>% {
  bind_rows(
    bind_cols(
      tibble(Variable = "Total", Group = "Total"),
      estimate_prevalence(., Post, weights, "Yes"),
      mcnemar_test_df_pre_post(., Pre, Post, "Yes", weight = weights)
    ),
    tibble(Variable = "Sex", Group = "Sex"),
    group_by(., Group = sex) %>% {
      full_join(
        estimate_prevalence(., Post, weights, "Yes"),
        mcnemar_test_df_pre_post(., Pre, Post, "Yes", weight = weights),
        by = "Group"
      )
    } %>%
      add_column(Variable = "Sex"),
    tibble(Variable = "Age", Group = "Age"),
    group_by(., Group = Age) %>% {
      full_join(
        estimate_prevalence(., Post, weights, "Yes"),
        mcnemar_test_df_pre_post(., Pre, Post, "Yes", weight = weights),
        by = "Group"
      )
    } %>%
      add_column(Variable = "Age")
  )
}


## ----prevalence-estimates-collapsed----

prevalence_tables <- lst(
  depression_prevalence_pre,
  depression_prevalence_post,
  suicidal_prevalence_pre,
  suicidal_prevalence_post
)

prevalence_tables <- prevalence_tables %>%
  map(adorn_pct_formatting, digits = 2, ... = c(Prevalence, Std_err))

prevalence_table_collapsed <- prevalence_tables %>%
  map(mutate, across(matches("Std_err"), enclose, "(")) %>%
  map(unite, prev_sd, Prevalence,  Std_err,  sep = " ") %>%
  map(select, -N, -any_of(c("statistic", "parameter", "method"))) %>%
  reduce2(
    c("_depr_post", "_suic_pre", "_suic_post"),
    ~full_join(..1, ..2, by = c("Variable", "Group"), suffix = c("", ..3))
  ) %>%
  mutate(across(where(is.character), str_replace, "- \\(-\\)", "")) %>%
  rename(prev_sd_depr_pre = prev_sd) %>%
  mutate(across(starts_with("p.value"), format_pvalues)) %>%
  group_by(Variable) %>%
  mutate(is_cat = n() > 1 & Variable != Group)

cat_index <- prevalence_table_collapsed %>% pull(is_cat)

prevalence_table_collapsed <- prevalence_table_collapsed %>%
  ungroup() %>%
  select(-is_cat, -Variable)


prevalence_header <- tibble(
  col_keys = prevalence_table_collapsed %>% colnames(),
  overhead = c("", "Depression" %>% rep(3), "Suicidal ideation" %>% rep(3)),
  main     = c(
    "Segment", c("Pre (sd)", "Post (sd)", "*p* value") %>% rep(2)
  )
)

prevalence_table_output <- prevalence_table_collapsed %>%
  flextable() %>%
  set_header_df(prevalence_header) %>%
  colformat_md(j = c(4, 7), part = "header") %>%
  merge_h(part = "header") %>%
  merge_h(part = "body") %>%
  theme_booktabs() %>%
  add_footer(Group = PREVALENCE_FOOTER) %>%
  merge_at(part = "footer") %>%
  flextable::style(i = cat_index, j = 1, pr_p = fp_par(padding.left = 30)) %>%
  align(j = 2:7, align = "right", part = "all") %>%
  align(i = 1, align = "center", part = "header") %>%
  valign(j = 1, valign = "top") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  padding(padding.top = 1, padding.bottom = 1) %>%
  autofit()


## ----prevalence-values----

prevalences <- prevalence_tables %>%
  map(filter, Group == "Total") %>%
  map(pull, Prevalence)

depression_prev_pre  <- prevalences$depression_prevalence_pre
depression_prev_post <- prevalences$depression_prevalence_post

mcnemar_depr_test <- depression_prevalence_post %>%
  filter(Variable == "Total") %>%
  print_chisq_test()


suicidal_prev_pre    <- prevalences$suicidal_prevalence_pre
suicidal_prev_post   <- prevalences$suicidal_prevalence_post

mcnemar_suic_test <- suicidal_prevalence_post %>%
  filter(Variable == "Total") %>%
  print_chisq_test()


prevalences_young <- prevalence_tables %>%
  map(filter, Group == AGE_GROUPS[1]) %>%
  map(pull, Prevalence)
depression_prev_pre_young  <- prevalences_young$depression_prevalence_pre
depression_prev_post_young <- prevalences_young$depression_prevalence_post
mcnemar_depr_test_young    <- depression_prevalence_post %>%
  filter(Group == AGE_GROUPS[1]) %>%
  print_chisq_test()

prevalences_middle <- prevalence_tables %>%
  map(filter, Group == AGE_GROUPS[2]) %>%
  map(pull, Prevalence)
depression_prev_pre_middle  <- prevalences_middle$depression_prevalence_pre
depression_prev_post_middle <- prevalences_middle$depression_prevalence_post
mcnemar_depr_test_middle    <- depression_prevalence_post %>%
  filter(Group == AGE_GROUPS[2]) %>%
  print_chisq_test()

prevalences_old <- prevalence_tables %>%
  map(filter, Group == AGE_GROUPS[3]) %>%
  map(pull, Prevalence)
depression_prev_pre_old  <- prevalences_old$depression_prevalence_pre
depression_prev_post_old <- prevalences_old$depression_prevalence_post
mcnemar_depr_test_old    <- depression_prevalence_post %>%
  filter(Group == AGE_GROUPS[3]) %>%
  print_chisq_test()


## ---- Depression: ----

## ----depression-selection-no-pre----

depression_neg_pre <- depression %>% filter(Pre == "No")

all_preds <- all_preds %>% str_subset("^depression_", negate = TRUE)

# Survey design, to use with `svyglm` calls:
depression_np_design <- svydesign(
  ids     = ~ID_CONTACTO,
  data    = depression_neg_pre,
  weights = depression_neg_pre %>% pull(weights)
)


## ----depression-no-pre-contingency----
depression_no_pre_contingency <- depression_neg_pre %>%
  select(Post, all_of(all_preds)) %>%
  frequencies_table(.segmentation = Post, missing = FALSE)

## ----univariate-tests-depression----
univariate_tests_depression <- all_preds %>% map_df(
  ~svyglm(
    formula = glue("Post ~ {.x}") %>% as.formula(),
    design  = depression_np_design,
    family  = quasibinomial
  ) %>%
    regTermTest(test.terms = .x, method = "LRT") %$%
    tibble(
      term          = .x,
      lrt_Rao_Scott = .$chisq / mean(.$lambda),
      df            = .$df,
      p.value       = .$p
    )
)

## ----predictor-selection-depression----

selected_preds_depression <- univariate_tests_depression %>%
  filter(p.value < SIG_LEVEL) %>% # No correction
  pull(term) %>%
  union(FIXED_PREDICTORS)

# To have the predictors in the usual order:
selected_preds_depression <- all_preds %>%
  intersect(selected_preds_depression)


## ----ordinal-linearity-tests-depression----

selected_ord_depression <- selected_preds_depression %>%
  intersect(ordinal_preds)

ord_linearity_tests_depression <- selected_ord_depression %>% map_df(
  ~{
    aux_design <- svydesign(
      ids     = ~ID_CONTACTO,
      data    = depression_neg_pre %>%
        mutate(linear = !!sym(.x) %>% as.integer()),
      weights = depression_neg_pre %>% pull(weights)
    )
    
    nested_fit <- svyglm(
      formula = Post ~ linear,
      design  = aux_design,
      family  = quasibinomial
    )
    general_fit <- svyglm(
      formula = glue("Post ~ {.x}") %>% as.formula(),
      design  = aux_design,
      family  = quasibinomial
    )
    
    anova(nested_fit, general_fit, method = "Wald") %$%
      tibble(
        term    = .x,
        F       = .$Ftest %>% drop(),
        df      = .$df,
        dfden   = .$ddf,
        p.value = .$p %>% drop()
      )
  }
)

linear_ord_preds_depression <- ord_linearity_tests_depression %>%
  filter(p.value >= (SIG_LEVEL / n())) %>%
  pull(term)
nonlinear_ord_preds_depression <- selected_ord_depression %>%
  setdiff(linear_ord_preds_depression)

# Trasnform to integer the linear predictors, in order to avoid the use of
#   high-order polynomial terms.
#   The reference (lower) category is set to 0 by substracting 1 from the
#   integer resulting from the transformed category.
depression_neg_pre <- depression_neg_pre %>%
  mutate_at(linear_ord_preds_depression, as.integer) %>%
  mutate_at(linear_ord_preds_depression, `-`, 1L)

linear_ord_preds_depression_labels_out <- var_properties %>%
  pull(labels_comp) %>%
  extract(linear_ord_preds_depression) %>%
  glue_collapse(" and ")


## ----stepwise-glm-depression----

# Initialization:

## Scope (minimum and maximum model):
scope <- list(
  lower = "Post ~" %>% paste(paste0(FIXED_PREDICTORS, collapse = " + ")) %>%
    as.formula(),
  upper = "Post ~" %>% paste(
    glue_collapse(selected_preds_depression, sep = " + ")
  ) %>% as.formula()
)

## Dataset without missing values for the iterations:
depression_neg_pre_fit <- depression_neg_pre %>%
  select(ID_CONTACTO, Post, weights, all_of(selected_preds_depression)) %>%
  drop_na()

depression_np_design <- svydesign( # Redefine the design
  ids     = ~ID_CONTACTO,
  data    = depression_neg_pre_fit,
  weights = depression_neg_pre_fit %>% pull(weights)
)

## Initial fit with all the predictors:
depression_init_fit <- svyglm(
  formula = scope$upper,
  design  = depression_np_design,
  family  = quasibinomial
)

# Iterative dropping of predictors:
depression_final_fit <- depression_init_fit %>%
  step(scope = scope, direction = "backward")

# This gives the result of the iterations, but the dataset has dropped cases
#   that may be re-used in the final model, so it is fit again with the
#   complete dataset


# Final fit:
depression_np_design <- svydesign( # Redefine the design to include all values
  ids     = ~ID_CONTACTO,
  data    = depression_neg_pre %>% mutate(
    across(all_of(nonlinear_ord_preds_depression), factor, ordered = FALSE)
  ),
  weights = depression_neg_pre %>% pull(weights)
)

depression_fit <- svyglm(
  formula = depression_final_fit %>% formula(),
  design  = depression_np_design,
  family  = quasibinomial
) # No gain in participants nevertheless


# Summary:
depression_summ_fit     <- depression_fit %>% summary()
depression_coefficients <- depression_fit %>% tidy()
nobs                    <- depression_fit %>% nobs()


## ----conclusions-pre-computations-depression----

vars_terms <- depression_neg_pre_fit |>
  imap(~paste0(.y, levels(.x)[-1]))  |>
  unlist()                           |>
  enframe("variable", "term")

depression_coefs_sig <- depression_coefficients |>
  left_join(vars_terms, by = "term")            |># Do not use (Intercept)
  filter(term != "(Intercept)")                 |>#   account for correction
  filter(p.value < SIG_LEVEL / n()) # Bilateral, Bonferroni-corrected

depression_sig_terms <- depression_coefs_sig %>%
  pull(variable) %>%
  enclose('`') %>%
  glue_collapse(sep = ', ', last = ', and ')

depression_sig_terms_labels <- depression_coefs_sig %>%
  pull(variable) %>%
  extract(labels_abbr, .)

depr_ucla_lon_post_lab   <- depression_sig_terms_labels[2] %>% paste("(Post)")
depr_resilience_post_lab <- depression_sig_terms_labels[3] %>% paste("(Post)")

depression_sig_term_cov_concern <- depression_sig_terms_labels[1]

depression_sig_terms_labels <- depression_sig_terms_labels[-1] %>%
  glue_collapse(sep = ', ', last = " and ")


# COVID-19 concern:
depr_concern_term <- depression_coefs_sig %>%
  filter(term %>% str_detect("^rel_concerned"))
depr_concern_test <- depr_concern_term %>% print_z_test()
depr_concern_pval <- depr_concern_term %>% pull(p.value) %>% print_pvalue()
depr_concern_coef <- depr_concern_term %>% pull(estimate)
depr_concern_OR   <- depr_concern_coef %>% exp() %>% number(1e-3)
depr_concern_dir  <- depr_concern_coef %>% coef_dir(form = "verb")
depr_concern_incr <- (depr_concern_coef %>% exp() - 1) %>%
  abs() %>%
  percent(.1)

# UCLA Loneliness scale (post):
depr_ucla_lon_post_term <- depression_coefs_sig %>%
  filter(term %>% str_detect("ucla_lon_post$"))
depr_ucla_lon_post_test <- depr_ucla_lon_post_term %>% print_z_test()
depr_ucla_lon_post_pval <- depr_ucla_lon_post_term %>% pull(p.value) %>% print_pvalue()
depr_ucla_lon_post_coef <- depr_ucla_lon_post_term %>% pull(estimate)
depr_ucla_lon_post_OR   <- depr_ucla_lon_post_coef %>% exp() %>% number(1e-3)
depr_ucla_lon_post_dir  <- depr_ucla_lon_post_coef %>% coef_dir(form = "indet")
depr_ucla_lon_post_incr <- (depr_ucla_lon_post_coef %>% exp() - 1) %>%
  abs() %>%
  percent(.1)

depr_ucla_lon_post_scaling <- dataset_outcomes_std %>%
  pull(ucla_lon_post) %>%
  attr("scaled:scale")

depr_ucla_lon_post_coef_abs_score <- (
  depr_ucla_lon_post_coef / depr_ucla_lon_post_scaling * 100 / 6
) %>%
  exp() %>%
  subtract(1) %>%
  percent(1e-1)


# Resilience scale (post):
depr_resilience_post_term <- depression_coefs_sig %>%
  filter(term %>% str_detect("resilience_post$"))
depr_resilience_post_test <- depr_resilience_post_term %>% print_z_test()
depr_resilience_post_pval <- depr_resilience_post_term %>%
  pull(p.value) %>%
  print_pvalue()
depr_resilience_post_coef <- depr_resilience_post_term %>% pull(estimate)
depr_resilience_post_OR   <- depr_resilience_post_coef %>%
  exp() %>%
  number(1e-3)
depr_resilience_post_dir  <- depr_resilience_post_coef %>%
  coef_dir(form = "indet")
depr_resilience_post_decr <- (depr_resilience_post_coef %>% exp() - 1) %>%
  abs() %>%
  percent(.1)

depr_resilience_post_scaling <- dataset_outcomes_std %>%
  pull(resilience_post) %>%
  attr("scaled:scale")

depr_resilience_post_coef_abs_score <- (
  depr_resilience_post_coef / depr_resilience_post_scaling
) %>%
  exp() %>%
  subtract(1) %>%
  abs() %>%
  percent(1e-1)


## ----depression-coefficients----

depression_coefficients <- depression_coefficients %>%
  mutate(
    OR         = exp(estimate) %>% number(1e-2),
    ci.inf     = exp(estimate - std.error * CI_FACTOR),
    ci.sup     = exp(estimate + std.error * CI_FACTOR),
    `(95% CI)` = format_ci(ci.inf, ci.sup, sig = 2, quoting = "("),
    `*p* value`  = p.value   %>% format_pvalues(),
    sig        = p.value %>% is_less_than(SIG_LEVEL / (n() - 1)) %>%
      if_else("*", ""), # -1 to omit intercept
    statistic = statistic %>% number(1e-2),
    across(where(is.numeric), number, 1e-3)
  ) %>%
  format_term_label(
    .data   = depression_neg_pre,
    .labels = var_properties %>% pull(labels_comp),
    add_ref = FALSE
  ) %>%
  order_terms_with_data(dataset_outcomes)

depression_coefficients_table <- depression_coefficients %>%
  select(Term, OR, `(95% CI)`, z = statistic, `*p* value`) %>%
  mutate(across(where(is.numeric), number, 1e-3)) %>% 
  flextable() %>%
  add_footer(Term = DEPRESSION_MODEL_FOOTER) %>%
  merge_at(part = "footer") %>%
  theme_booktabs() %>%
  colformat_md(part = "all") %>%
  align(j = c(2, 4, 5), align = "right", part = "all") %>%
  align(i = 1, align = "center", part = "header") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  border(border.bottom = fp_border(style = "none"), part = "footer") %>%
  autofit()

## ---- Suicidal ideation: ----

## ----suicidal-selection-no-pre----

suicidal_neg_pre <- suicidal %>% filter(Pre == "No")

# Survey design, to use with `svyglm` calls:
suicidal_np_design <- svydesign(
  ids     = ~ID_CONTACTO,
  data    = suicidal_neg_pre,
  weights = suicidal_neg_pre %>% pull(weights)
)


## ----suicidal-no-pre-contingency----
suicidal_no_pre_contingency <- suicidal_neg_pre %>%
  select(Post, all_of(all_preds)) %>%
  frequencies_table(.segmentation = Post, missing = FALSE)


## ----univariate-tests-suicidal----

suicidal_preds <- all_preds %>% c("depression_pre", "depression_post")

univariate_tests_suicidal <- suicidal_preds %>% map_df(
  ~svyglm(
    formula = glue("Post ~ {.x}") %>% as.formula(),
    design  = suicidal_np_design,
    family  = quasibinomial
  ) %>%
    regTermTest(test.terms = .x, method = "LRT") %$%
    tibble(
      term          = .x,
      lrt_Rao_Scott = .$chisq / mean(.$lambda),
      df            = .$df,
      p.value       = .$p
    )
)

## ----predictor-selection-suicidal----

selected_preds_suicidal <- univariate_tests_suicidal %>%
  filter(p.value < SIG_LEVEL) %>% # No correction
  pull(term) %>%
  union(FIXED_PREDICTORS)

# To have the predictors in the usual order:
selected_preds_suicidal <- suicidal_preds %>%
  intersect(selected_preds_suicidal)


## ----drop-complete-separation-terms-suicidal----

# Contingency table for predictors that yield complete separation:
complete_sep_terms_suic <- suicidal_no_pre_contingency %>%
  group_by(Variable) %>%
  summarize(comp_sep = any(No == 0 | Yes == 0)) %>%
  filter(comp_sep) %>%
  pull(Variable)

complete_sep_terms_suic_labels_out <- var_properties %>%
  pull(labels_comp) %>%
  extract(complete_sep_terms_suic) %>%
  glue_collapse(sep = ", ", last = ", and ")


selected_preds_suicidal <- selected_preds_suicidal %>%
  setdiff(complete_sep_terms_suic)


## ----ordinal-linearity-tests-suicidal----

selected_ord_suicidal <- selected_preds_suicidal %>%
  intersect(ordinal_preds)

ord_linearity_tests_suicidal <- selected_ord_suicidal %>% map_df(
  ~{
    aux_design <- svydesign(
      ids     = ~ID_CONTACTO,
      data    = suicidal_neg_pre %>%
        mutate(linear = !!sym(.x) %>% as.integer()),
      weights = suicidal_neg_pre %>% pull(weights)
    )

    nested_fit <- svyglm(
      formula = Post ~ linear,
      design  = aux_design,
      family  = quasibinomial
    )
    general_fit <- svyglm(
      formula = glue("Post ~ {.x}") %>% as.formula(),
      design  = aux_design,
      family  = quasibinomial
    )
    
    anova(nested_fit, general_fit, method = "Wald") %$%
      tibble(
        term    = .x,
        F       = .$Ftest %>% drop(),
        df      = .$df,
        dfden   = .$ddf,
        p.value = .$p %>% drop()
      )
  }
)

linear_ord_preds_suicidal <- ord_linearity_tests_suicidal %>%
  filter(p.value >= (SIG_LEVEL / n())) %>%
  pull(term)
nonlinear_ord_preds_suicidal <- selected_ord_suicidal %>%
  setdiff(linear_ord_preds_suicidal)

linear_ord_preds_suicidal_lab_out <- var_properties %>%
  pull(labels_abbr) %>%
  extract(linear_ord_preds_suicidal)

# Trasnform to integer the linear predictors, in order to avoid the use of
#   high-order polynomial terms.
#   The reference (lower) category is set to 0 by substracting 1 from the
#   integer resulting from the transformed category.
suicidal_neg_pre <- suicidal_neg_pre %>%
  mutate_at(linear_ord_preds_suicidal, as.integer) %>%
  mutate_at(linear_ord_preds_suicidal, `-`, 1L)


## ----stepwise-glm-suicidal----

# Initialization:

## Scope (minimum and maximum model):
scope <- list(
  lower = "Post ~" %>% paste(
    paste0(
      c(FIXED_PREDICTORS, "depression_pre", "depression_post"),
      collapse = " + "
    )
  ) %>%
    as.formula(),
  upper = "Post ~" %>% paste(
    glue_collapse(selected_preds_suicidal, sep = " + ")
  ) %>% as.formula()
)

## Dataset without missing values for the iterations:
suicidal_neg_pre_fit <- suicidal_neg_pre %>%
  select(ID_CONTACTO, Post, weights, all_of(selected_preds_suicidal)) %>%
  drop_na()

suicidal_np_design <- svydesign( # Redefine the design
  ids     = ~ID_CONTACTO,
  data    = suicidal_neg_pre_fit,
  weights = suicidal_neg_pre_fit %>% pull(weights)
)

## Initial fit with all the predictors:
suicidal_init_fit <- svyglm(
  formula = scope$upper,
  design  = suicidal_np_design,
  family  = quasibinomial
)

# Iterative dropping of predictors:
suicidal_final_fit <- suicidal_init_fit %>%
  step(scope = scope, direction = "backward")

# This gives the result of the iterations, but the dataset has dropped cases
#   that may be re-used in the final model, so it is fit again with the
#   complete dataset


# Final fit:
suicidal_np_design <- svydesign( # Redefine the design to include all values
  ids     = ~ID_CONTACTO,
  data    = suicidal_neg_pre %>% mutate(
    across(all_of(nonlinear_ord_preds_suicidal), factor, ordered = FALSE)
  ),
  weights = suicidal_neg_pre %>% pull(weights)
)

suicidal_fit <- svyglm(
  formula = suicidal_final_fit %>% formula(),
  design  = suicidal_np_design,
  family  = quasibinomial
) # No gain in participants nevertheless


# Summary:
suicidal_summ_fit     <- suicidal_fit %>% summary()
suicidal_coefficients <- suicidal_fit %>% tidy()
nobs                  <- suicidal_fit %>% nobs()


## ----conclusions-pre-computations-suicidal----

suicidal_coefs_sig <- suicidal_coefficients %>%
  filter(term != "(Intercept)") %>% # Do not take it into account for correction
  filter(p.value < SIG_LEVEL / n()) # Bilateral, Bonferroni-corrected

suicidal_sig_terms <- suicidal_coefs_sig %>%
  pull(term) %>%
  enclose('`') %>%
  glue_collapse(sep = ', ', last = ', and  ')

suicidal_sig_terms_labels <- suicidal_coefs_sig %>%
  pull(term) %>%
  extract(labels_abbr, .)

suicidal_sig_terms_labels_collapsed <- suicidal_sig_terms_labels %>%
  glue_collapse(sep = " and ")


# Oslo 3:

suic_oslo3_post_label <- suicidal_sig_terms_labels["oslo3_sss_post"]

suic_oslo3_post_term <- suicidal_coefs_sig %>%
  filter(term == "oslo3_sss_post")
suic_oslo3_post_pval <- suic_oslo3_post_term %>%
  pull(p.value) %>%
  print_pvalue()
suic_oslo3_post_coef <- suic_oslo3_post_term %>% pull(estimate) %>% exp()
suic_oslo3_post_dir  <- suic_oslo3_post_coef %>%
  log() %>%
  coef_dir(form = "indet")
suic_oslo3_post_abs  <- (1 - suic_oslo3_post_coef) %>% percent(.1)
suic_oslo3_post_test <- suic_oslo3_post_term %>% print_z_test()

suic_oslo3_post_scaling <- dataset_outcomes_std %>%
  pull(oslo3_sss_post) %>%
  attr("scaled:scale")

# Negative OR per point in the scale
#   (each point == reduction of `oslo3_post_neg_OR` in the odds)
suic_oslo3_post_neg_OR <- (
  1 - exp(log(suic_oslo3_post_coef) * 100 / 11 / suic_oslo3_post_scaling)
) %>%
  percent(.1)


# WHODAS-12:

suic_disability_post_label <- suicidal_sig_terms_labels["whodas12_post"]

suic_disability_post_term <- suicidal_coefs_sig %>%
  filter(term == "whodas12_post")
suic_disability_post_pval <- suic_disability_post_term %>%
  pull(p.value) %>%
  print_pvalue()
suic_disability_post_coef <- suic_disability_post_term %>%
  pull(estimate) %>%
  exp()
suic_disability_post_dir  <- suic_disability_post_coef %>%
  log() %>%
  coef_dir(form = "indet")
suic_disability_post_abs  <- (suic_disability_post_coef - 1) %>% percent(.1)
suic_disability_post_test <- suic_disability_post_term %>% print_z_test()

suic_disability_post_scaling <- dataset_outcomes_std %>%
  pull(whodas12_post) %>%
  attr("scaled:scale")

# Negative OR per point in the scale
#   (each point == increment of `suic_disability_post_OR` in the odds)
suic_disability_post_OR <- (
  exp(
    log(suic_disability_post_coef) * 100 / 11 / suic_disability_post_scaling
  ) - 1
) %>%
  percent(.1)


## ----suicidal-coefficients----

suicidal_coefficients <- suicidal_coefficients %>%
  mutate(
    OR         = exp(estimate) %>% number(1e-2),
    ci.inf     = exp(estimate - std.error * CI_FACTOR),
    ci.sup     = exp(estimate + std.error * CI_FACTOR),
    `(95% CI)` = format_ci(ci.inf, ci.sup, sig = 2, quoting = "("),
    `*p* value`  = p.value   %>% format_pvalues(),
    sig        = p.value %>% is_less_than(SIG_LEVEL / (n() - 1)) %>%
      if_else("*", ""), # -1 to omit intercept
    statistic = statistic %>% number(1e-2),
    across(where(is.numeric), number, 1e-3)
  ) %>%
  format_term_label(
    .data   = suicidal_neg_pre,
    .labels = var_properties %>% pull(labels_comp),
    add_ref = FALSE
  ) %>%
  order_terms_with_data(dataset_outcomes)

indented <- suicidal_coefficients %>% pull(indent)

suicidal_coefficients_table <- suicidal_coefficients %>%
  select(Term, OR, `(95% CI)`, z = statistic, `*p* value`) %>%
  mutate(across(-Term, ~if_else(is.na(.), Term, .))) %>%
  flextable() %>%
  merge_h(part = "body") %>%
  add_footer(Term = SUICIDAL_MODEL_FOOTER) %>%
  merge_at(part = "footer") %>%
  colformat_md(part = "all") %>%
  flextable::style(i = indented, j = 1, pr_p = fp_par(padding.left = 30)) %>%
  theme_booktabs() %>%
  align(j = c(2, 4, 5), align = "right", part = "all") %>%
  align(i = 1, align = "center", part = "header") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  border(border.bottom = fp_border(style = "none"), part = "footer") %>%
  autofit()


## ---- Final: ---------------------------------------------------------

## ----session-info----
devtools::session_info()
