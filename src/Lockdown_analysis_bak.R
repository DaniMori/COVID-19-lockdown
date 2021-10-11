## ---- SCRIPT SETUP: ----------------------------------------------------------

## ----global-configuration----

rm(list = ls())

# Packages:
library(pacman)

# File system constants:
OUT_DIR  <- "output/COVID-19_lockdown"


## ----script-configuration----

# Graphical output configuration:
ggplot2::theme_set(ggplot2::theme_minimal())


# RStata configuration
options(RStata.StataPath = "\"C:\\Program Files (x86)\\Stata15\\StataSE-64\"")
options(RStata.StataVersion = 15)


## ---- MAIN: ------------------------------------------------------------------


## ---- Shared chunks: ---------------------------------------------------------

## ----includes----
p_load(
  tidyverse,
  magrittr,
  haven,
  flextable,
  janitor,
  broom,
  psych,
  GGally,
  plotly,
  ggpointdensity,
  lmerTest,
  WeMix,
  lubridate,
  ggalluvial,
  ggrepel,
  MESS,
  modelr,
  forcats,
  RStata,
  survey,
  officer,
  ftExtra,
  effsize
)

source("R/Output.R",        encoding = 'UTF-8')
source("R/Stats_toolbox.R", encoding = 'UTF-8')
source("R/Stata_output.R",  encoding = 'UTF-8')

source("R/Lang_EN.R", encoding = 'UTF-8')


## ----constants----
# File system:
BASE_DIR <- "~/../UAM"
DOC_PATH_MASTER <- file.path(
  BASE_DIR,
  "marta.miret@uam.es - Documentacion Edad con Salud"
)
DB_PATH <- file.path(
  DOC_PATH_MASTER,
  "Edad con salud - Subestudio COVID/BBDD_con_outcomes"
)

# Collapsed outcomes dataset:
OUTCOMES_DB_FILE <- file.path(DB_PATH, "Outcomes_collapsed.dta")


# Variable constants:
ORDINAL_VARS <- c(
  "edu_level_rec",
  "severity",
  # "ucla_lon_pre", # TODO: Treat these as ordinal??
  # "ucla_lon_post",
  "pain_physical",
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
  # "health_abb_pre", # These are redundant with "WHODAS"
  # "health_abb_post",
  "whodas12_post",
  "physical_pre",
  "quietness",
  "economy",
  "unemployment",
  # "home_score", # Non-significant, saves unnecessary work to omit it
  "resilience_pre",
  "resilience_post",
  "screen_non_work",
  "screen_work",
  # "screen_total",
  # "SM23", # These three variables are deleted from the predictor subset,
  # "SM24", #   according to JM Haro's suggestion
  # "SM25",
  # "pain_score", # This variable has no sense, conceptually
  "pain_difficulties",
  "pain_new",
  "pain_physical",
  "pain_worsened",
  "depression_pre", # These two measures are used as predictors in both the
  "depression_post" #   `sleeping time` and the `suicidal ideation` models.
)

PREDICTORS_CHANGE <- c(
  "age",
  "physical",
  "resilience"
)

# Fixed predictors for the incidence models:
FIXED_PREDICTORS_NEW <- c("sex", "age_pre")

CRITERIA <- c("depression", "suicidal", "sleeping_time", "sleep_quality")


# Value constants:

PRE_LEVEL  <- "Pre"
POST_LEVEL <- "Post"
MEASURE_LEVELS <- c(PRE_LEVEL, POST_LEVEL)


# Statistical constants:

SIG_LEVEL <- .05
sig_level_print <- paste0(
  "$\\alpha$ = ",
  SIG_LEVEL %>% format_prop_like(sig = 2)
)

CI_WIDTH  <- (1 - SIG_LEVEL) %>% percent()
CI_FACTOR <- (SIG_LEVEL / 2) %>% qnorm(lower.tail = FALSE)

MAX_MISSING <- .20
max_missing_print <- MAX_MISSING %>% percent()


## ----load-data----
dataset_outcomes <- OUTCOMES_DB_FILE %>% read_dta()

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
  map_chr(~attr(., "label") %||% NA_character_)

var_descriptors[
  c(
    "edu_level_rec", "hhincomemid", "age_pre",
    "severity",
    "ucla_lon_pre", "ucla_lon_post", "oslo3_sss_pre", "oslo3_sss_post",
    # "health_abb_pre", "health_abb_post",
    "whodas12_post",
    "physical_pre", "physical_post",
    # "home_score",
    "resilience_pre", "resilience_post",
    "pain_difficulties", "pain_physical",
    "depression_pre", "depression_post"
  )
] <- c(
  "Education level", "Household income", "Age",
  "Severity of COVID-19 infection",
  "UCLA loneliness scale (pre lockdown)",
  "UCLA loneliness scale (post lockdown)",
  "Oslo-3 social support scale (pre lockdown)",
  "Oslo-3 social support scale (post lockdown)",
  # "Abbreaviated health scale (pre lockdown)",
  # "Abbreaviated health scale (post lockdown)",
  "12-item WHODAS disability scale (post lockdown)",
  "Physical activity level (pre lockdown)",
  "Physical activity level (post lockdown)",
  # "Home quality score",
  "Resilience scale (pre lockdown)",
  "Resilience scale (post lockdown)",
  "Difficulties in everyday activities due to pain? (post lockdown)",
  "Physical pain (post lockdown)",
  "Depression (pre lockdown)", "Depression (post lockdown)"
)

labels_abbr <- c(
  "Sex", "Education level", "Income",
  "COVID-19 infection", "COVID-19 co-habitant", "COVID-19 concern",
  "Living alone", "Living alone",
  "Loneliness", "Loneliness",
  "Social support", "Social support",
  # "Health", "Health",
  "Disability",
  "Physical activity",
  "Home quietness",
  "Economy worsened",
  "Unemployed",
  # "Home quality",
  "Resilience", "Resilience",
  "Working screen time (h)",
  "Non-working screen time (h)",
  # "Total screen time (h.)",
  # "Mental health care received",
  # "Mental health care interrupted",
  # "Mental health care needed",
  # "Pain",
  "Pain-related difficulties",
  "Pain new",
  "Physical pain",
  "Pain worsened",
  "Depression", "Depression",
  "Age",
  "Physical activity",
  "Suicidal ideation", "Suicidal ideation"
) %>%
  set_names(names(var_descriptors))

var_measure <- c(
  PRE_LEVEL, PRE_LEVEL, PRE_LEVEL,
  POST_LEVEL, POST_LEVEL, POST_LEVEL,
  PRE_LEVEL, POST_LEVEL,
  PRE_LEVEL, POST_LEVEL,
  PRE_LEVEL, POST_LEVEL,
  # PRE_LEVEL, POST_LEVEL, # Corresponding to health_abb_pre / post
  POST_LEVEL,
  PRE_LEVEL,
  POST_LEVEL,
  POST_LEVEL,
  POST_LEVEL,
  # POST_LEVEL, # Corresponding to home_score
  PRE_LEVEL, POST_LEVEL,
  POST_LEVEL,
  POST_LEVEL,
  # POST_LEVEL, # Corresponding to screen_time_total
  # POST_LEVEL, # Corresponding to SM23, SM24, and SM25
  # POST_LEVEL,
  # POST_LEVEL,
  # POST_LEVEL, # Corresponding to pain_score
  POST_LEVEL,
  POST_LEVEL,
  POST_LEVEL,
  POST_LEVEL,
  PRE_LEVEL, POST_LEVEL,
  PRE_LEVEL,
  POST_LEVEL,
  PRE_LEVEL, POST_LEVEL
) %>%
  set_names(names(var_descriptors))

var_properties <- tibble(
  predictor = names(var_descriptors),
  var_descriptors, labels_abbr, var_measure
) %>%
  mutate(
    append_measure = c(
      FALSE %>% rep(6), TRUE  %>% rep(8), FALSE %>% rep(3),
      TRUE  %>% rep(2), FALSE %>% rep(6), TRUE  %>% rep(6)
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


## ----set-equivalent-wd----
# This chunk ensures equivalent behaviour to that in the .Rmd files, by changing
#   the evaluation directory accordingly:
# setwd(file.path(getwd(), OUT_DIR))


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


## ----subset-predictors----

dataset_predictors <- dataset_outcomes %>%
  select(any_of(valid_vars)) %>%
  slice(0) %>%
  bind_cols(
    dataset_outcomes_varying %>% select_at(time_varying_preds) %>% slice(0)
  )

all_preds      <- dataset_predictors %>% colnames()
factor_preds   <- dataset_predictors %>% select_if(is.factor)  %>% colnames()
ordinal_preds  <- dataset_predictors %>% select_if(is.ordered) %>% colnames()
quant_preds    <- dataset_predictors %>% select(!all_of(factor_preds)) %>%
  colnames()
dichotom_preds <- dataset_predictors %>% # All dichotomous predictors
  select_if(~levels(.) %>% identical(c("No", "Yes"))) %>%
  colnames()
binary_preds   <- dataset_predictors %>% # All dichotomous and otherwise nominal
  select_if(is.factor) %>%               #   predictors with 2 levels
  select_if(~!is.ordered(.)) %>%
  select_if(~nlevels(.) == 2) %>%
  colnames()
nonbin_factor_preds <- factor_preds %>% setdiff(binary_preds)
nominal_preds  <- dataset_predictors %>% # All nominal predictors
  select_if(is.factor) %>%
  select(!all_of(dichotom_preds) & !all_of(ordinal_preds)) %>%
  colnames()


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


## ----subset-predictors-new----

# New predictors for the "new models" (models of risk of a dichotomous outcome
#   for the cases that haven't had it previously)
all_preds_new          <- valid_vars %>% c("age_pre", "physical_post")
dataset_predictors_new <- dataset_outcomes %>% select(all_of(all_preds_new))
ordinal_preds_new      <- dataset_predictors_new %>%
  select_if(is.ordered) %>%
  colnames()

## ----var-descriptives----

dataset_outcomes_descr <- dataset_outcomes %>% select(
  ID_CONTACTO,
  depression_pre, depression_post, matches("^suicidal_(pre|post)$"),
  all_of(all_preds_new)
)

quant_descriptives_out <- dataset_outcomes_descr %>%
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

cat_descriptives_out <- dataset_outcomes_descr %>%
  select(where(is.factor)) %>%
  frequencies_table(missing = FALSE)

sample_contrast_vars <- dataset_outcomes_descr %>%
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
    # n       = first(N), # Not reporting "N total"
    # n       = if_else(n() == 3 | Level == "Total", n, NA_real_),
    stat1   = if_else(n() != 3 & Level == "Total", NA_real_, N),
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
  mutate(across(where(is.double), as.character)) %>%
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

# Not reporting "N total"
# cat_header_line <-   tibble(
#   var_cat    = NA_character_,
#   n_Pre      = NA_character_,
#   stat1_Pre  = "n",
#   stat2_Pre  = "(%)",
#   n_Post     = NA_character_,
#   stat1_Post = "n",
#   stat2_Post = "(%)"
# )

# pos_cat_hdr <- nrow(total_descriptives_out) + 1

total_descriptives_out <- quant_total_out %>%
  # bind_rows(cat_header_line, cat_total_out) # Not reporting "N total"
  bind_rows(cat_total_out) %>%
  slice( # Custom order:
    7,     # Age
    10,    # Sex (Female)
    11:15, # Education level (Less than primary, Primary, Secondary, Tertiary)
    8,     # Depression
    9,     # Suicidal ideation
    # 31:30, # Mental health care needed, Mental health care received
    4,     # Resilience
    22,    # Living alone
    2,     # Social support
    1,     # Loneliness
    20:21, # COVID-19 co-habitant, COVID-19 concern,
    16:19, # COVID-19 severity (Not infected, Infected, Hospitalized)
    3,     # WHODAS
    30:34, # Physical pain (None, Light, Moderate, Severe)
    5:6,   # Working screen time, Non-working screen time
    27,    # Home quietness
    28:29, # Economy worsened, Unemployed
    23:26  # Physical activity (Low, Moderate, High)
  )

cat_index <- total_descriptives_out %>% pull(is_cat)
total_descriptives_out <- total_descriptives_out %>% select(-is_cat)

extra_footnote  <- c(12, 13, 20)

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
      ) #,
      # variable = c(
      #   "Variable / Category",
      #   # c("N total", "mean", "(sd)") %>% rep(2) # Not reporting "N total"
      #   c("mean", "(sd)") %>% rep(2)
      # )
    )
  ) %>%
  colformat_md(j = 6, part = "header") %>%
  colformat_md(j = 1, part = "all") %>%
  footnote(
    value       = as_paragraph(
      c(
        FOOTNOTE_VARS_SCALE,
        var_descriptors[c("rel_isolated", "rel_concerned", "severity")]
      ) %>%
        as_i()
    ),
    i           = c(10, 14:16),
    j           = 1,
    ref_symbols = FOOTNOTE_SYMBOL[1:4]
  ) %>%
  merge_h(part = "header") %>%
  theme_booktabs() %>%
  flextable::style(i = cat_index, j = 1, pr_p = fp_par(padding.left = 30)) %>%
  align(j = c(2, 4, 6), align = "right", part = "all") %>%
  align(i = 1, align = "center", part = "header") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  padding(padding.top = 1, padding.bottom = 1) %>%
  autofit()


### Quantitative descriptives output:
quant_descriptives_out <- quant_descriptives_out %>%
  select(-var) %>%
  flextable() %>%
  align(j = 3:4, align = "right") %>%
  autofit()


### Categorical descriptives output:

cat_descriptives_out <- cat_descriptives_out %>%
  slice(1:41, (n()-3):n(), 42:(n()-4)) %>% # Order physical activity (post)
  mutate(
    var      = Variable,
    Variable = var_descriptors[Variable]
  )

border_index <- cat_descriptives_out %>%
  mutate(Variable != Variable %>% lead(1, default = last(.))) %>%
  pull()

cat_descriptives_out <- cat_descriptives_out %>%
  select(-var) %>%
  flextable() %>%
  align(j = 4, align = "right") %>%
  border(i = border_index, border.bottom = fp_border(width = .1)) %>%
  merge_v(1) %>%
  valign(valign = "top") %>%
  fix_border_issues() %>%
  set_table_properties(layout = "autofit")


## ---- Sleeping time: ---------------------------------------------------------

## ----sleep-time-dataset----
sleep_time <- dataset_outcomes_std %>%
  select(
    ID_CONTACTO,
    starts_with("sleeping_time"),
    any_of(all_preds),
    weights
  )

## ----sleep-time-descriptives----
sleep_time_descriptives <- sleep_time %>%
  transmute(
    Pre        = sleeping_time_pre,
    Post       = sleeping_time_post,
    Difference = sleeping_time_post - sleeping_time_pre
  ) %>%
  describe(skew = FALSE) %>%
  as.data.frame() %>%
  rownames_to_column("Sleeping time") %>%
  as_tibble() %>%
  select(-vars, -range, -se) %>%
  mutate(n = n %>% as.integer()) %>%
  mutate(across(where(is.double), number, 1e-2))

## ----sleep-time-histograms----
sleep_time_histograms_output <- sleep_time %>%
  ggplot() +
  geom_histogram(
    aes(x = sleeping_time_pre, y = -..count..),
    alpha = .5, fill = "red", color = "red", binwidth = .5
  ) +
  geom_histogram(
    aes(x = sleeping_time_post, y = ..count..),
    alpha = .5, fill = "blue", color = "blue", binwidth = .5
  ) +
  geom_hline(yintercept = 0, size = .1) +
  coord_flip() +
  scale_y_continuous(
    limits = c(-400, 400),
    labels = abs,
    expand = expansion(),
  ) +
  scale_x_continuous(
    limits = c(0, 24),
    expand = expansion(),
    breaks = 0:6 * 4
  ) +
  ## TODO: Fill legend
  # scale_fill_discrete(
  #   type  = c(Pre = "red", Post = "blue"),
  #   guide = guide_legend()
  # ) +
  theme(plot.margin = margin(5, 8, 5, 0)) +
  xlab(VAR_SLEEP_HOURS) +
  ylab("Frequency")

## ----sleep-time-frequencies-population----
sleep_time_descriptives_est <- sleep_time %>%
  summarize(
    across(
      starts_with("sleeping_time"),
      lst(
        n    = ~sum(!is.na(.)),
        mean = ~weighted.mean(., w = weights, na.rm = TRUE),
        sd   = ~sqrt(
          sum(
            weights * (. - weighted.mean(., w = weights, na.rm = TRUE))^2,
            na.rm = TRUE
          ) /
            (mean(weights, na.rm = TRUE) * (sum(!is.na(.)) - 1))
        )
      )
    )
  ) %>%
  pivot_longer(
    cols         = everything(),
    names_prefix = "sleeping_time_",
    names_sep    = "_",
    names_to     = c("Measure", ".value")
  )

## ----sleep-time-histogram-population----
sleep_time_pop_hist_output <- sleep_time %>%
  ggplot(aes(weight = weights)) +
  geom_histogram(
    aes(x = sleeping_time_pre, y = -..density..),
    alpha = .5, fill = "red", color = "red", binwidth = .5
  ) +
  geom_histogram(
    aes(x = sleeping_time_post, y = ..density..),
    alpha = .5, fill = "blue", color = "blue", binwidth = .5
  ) +
  geom_hline(yintercept = 0, size = .1) +
  coord_flip() +
  scale_y_continuous(
    limits = c(-.7, .7),
    labels = function(x) percent(abs(x)),
    expand = expansion(),
    breaks = -.2 * -3:3
  ) +
  scale_x_continuous(
    limits = c(0, 24),
    expand = expansion(),
    breaks = 4 * 0:6
  ) +
  theme(plot.margin = margin(5, 8, 5, 0)) +
  xlab(VAR_SLEEP_HOURS) +
  ylab("Percentage in population")

## ----filter-sleep-time-values----
# Outliers: 23 hours or more of sleep:
sleep_time_filtered <- sleep_time %>% mutate(
  sleeping_time_pre = sleeping_time_pre %>% {
    if_else(
      . >= 23,
      # . >= 23 | . < 2, # Alternatively: drop also cases with less than 2 hours
      NA_real_,
      .
    )
  }
)

## ----cor-test-sleep-time----

cor_test <- sleep_time_filtered %$%
  cor.test(sleeping_time_pre, sleeping_time_post) %>%
  tidy()
cor_est_ci <- cor_test %>%
  pull(estimate) %>%
  format_prop_like() %>%
  paste(cor_test %$% print_ci(conf.low, conf.high))

cor_test <- cor_test %>% print_cor_test(estimate = FALSE)


## ----weighted-cor-test-sleep-time----

sleep_time_cor_est <- sleep_time_filtered %>%
  select(sleeping_time_pre, sleeping_time_post, weights) %>%
  drop_na()

cor_est <- sleep_time_cor_est %>% {
    cov.wt(select(., -weights), wt = pull(., weights), cor = TRUE)
  } %>%
  extract2("cor") %>%
  extract(2, 1)


## The following is adapted from the code of `cor.test()` (Pearson's method)

conf  <- .95
df    <- sleep_time_cor_est %>% nrow() - 2L
stat  <- sqrt(df) * cor_est / sqrt(1 - cor_est^2)
z     <- atanh(cor_est)
sigma <- 1/sqrt(df - 1L)

# Two-sided confidence interval:
cint <- tanh(z + c(-1, 1) * sigma * qnorm((1 + conf)/2))

# p-value:
p_value <- 2 * min(pt(stat, df), pt(stat, df, lower.tail = FALSE))

cor_test_est <- tibble(
  estimate  = cor_est,
  statistic = stat,
  p.value   = p_value,
  parameter = df
) %>%
  print_cor_test()


## ----change-t-test-sleep-time----

# Test of equality of variances not needed for paired t-test
t_test <- sleep_time_filtered %$%
  t.test(sleeping_time_post, sleeping_time_pre, paired = TRUE) %>%
  tidy()

diff_est_ci <- t_test$estimate %>% number(1e-2) %>% paste(
  t_test %$% print_ci(conf.low, conf.high, sig = 2)
)
diff_est_min <- t_test$estimate %>% multiply_by(60) %>% number(1)

t_test <- t_test %>% print_t_test()


## ----weighted-change-t-test-sleep-time----

# I'm not loading the package `weights` here because of many
#   function name collisions
t_test_wtd <- sleep_time_filtered %$%
  weights::wtd.t.test(sleeping_time_post - sleeping_time_pre, weight = weights)
# Testing the difference is equivalent to testing "Pre - Post"
#   in a paired sample t-test.

diff_est_wtd     <- t_test_wtd$additional %>% extract("Difference")
diff_est_min_wtd <- diff_est_wtd %>% multiply_by(60) %>% number(1)
diff_est_wtd     <- diff_est_wtd %>% number(1e-2)

t_test_wtd <- t_test_wtd %>%
  extract2("coefficients") %>%
  as_tibble_row() %>%
  print_t_test(stat_var = "t.value", df_var = "df")


## ----tidy-sleep-time----
sleep_time_tidy <- sleep_time_filtered %>%
  pivot_longer(
    cols         = starts_with("sleeping_time"),
    names_to     = "Measure",
    names_prefix = "sleeping_time_",
    values_to    = "Sleeping time",
    names_transform = list(Measure = factor)
  ) %>%
  mutate(
    Measure = Measure %>%
      fct_rev() %>%
      fct_relabel(chartr, old = "p", new = "P")
  ) %>%
  full_join(dataset_outcomes_varying_std, by = c("ID_CONTACTO", "Measure"))

## ----univariate-tests-sleep-time----
# Commented out code:
{
# binary_univariate_tests <- binary_preds %>% map_df(
#   ~t.test(as.formula(glue("Diff ~ {.}")), data = sleep_time) %>%
#     tidy()
# ) %>%
#   add_column(Predictor = binary_preds, .before = 1)
# 
# aov_univariate_tests <- nonbin_factor_preds %>% map_df(
#   ~aov(
#     formula = as.formula(glue("Diff ~ {.}")),
#     data    = sleep_time,
#     weights = weights
#   ) %>%
#     tidy() %>%
#     pivot_wider(names_from = term, values_from = df:`p.value`) %>%
#     select_if(is_not_na) %>%
#     rename_at(vars(ends_with(.x)), str_remove, glue("_{.x}"))
# ) %>%
#   add_column(Predictor = nonbin_factor_preds, .before = 1)

# Note: As this is a one-way anova (no interaction factors),
#   unbalanced observations give the same result as balanced observations would
#   give; no need for Type 3 SS or fitting a linear model whatsoever.

# cat_univariate_tests <- factor_preds %>% map_df(
#   ~{
#     sleep_time %>% aov(
#       formula = glue("Diff ~ {.x}") %>% as.formula(),
#       weights = weights
#     ) %>%
#       tidy() %>%
#       pivot_wider(names_from = term, values_from = df:`p.value`) %>%
#       select_if(is_not_na) %>%
#       rename_at(vars(ends_with(.x)), str_remove, glue("_{.x}")) %>%
#       add_column(term = .x, .before = 1) %>%
#       add_column(alternative = "one sided")
#   }
# )
}
univariate_tests_sleeptime <- all_preds %>% map_df(
  ~{
    dataset_univariate_test <- sleep_time_tidy %>%
      select(`Sleeping time`, Measure, all_of(.x), ID_CONTACTO, weights) %>%
      drop_na() %>%
      correct_weights(id_var = ID_CONTACTO)
    
    nested_fit <- lmer(
      formula = glue("`Sleeping time` ~ Measure + {.x} + (1|ID_CONTACTO)") %>%
        as.formula(),
      data    = dataset_univariate_test,
      weights = weights,
      REML    = FALSE
    )
    general_fit <- lmer(
      formula = glue("`Sleeping time` ~ Measure * {.x} + (1|ID_CONTACTO)") %>%
        as.formula(),
      data    = dataset_univariate_test,
      weights = weights,
      REML    = FALSE
    )
    
    ## Supress "In tidy.anova(.) :
    ##            The following column names in ANOVA output were not
    ##            recognized or transformed: npar"
    suppressWarnings(
      anova(general_fit, nested_fit) %>%
        tidy() %>%
        select(term, statistic:p.value) %>%
        slice(2) %>%
        mutate(term = .x)
    )
  }
)

# Commented out code:
{
# `resilience_pre` is discarded from the dataset because there is a very high
#   rate of missing values, so it is not used.

# ## Supress "In tidy.anova(.) :
# ##            The following column names in ANOVA output were not
# ##            recognized or transformed: npar"
# suppressWarnings(
#   res2_fit <- anova(
#     lmer(
#       formula = `Sleeping time` ~ Measure +
#         resilience_pre +
#         I(resilience_pre^2) +
#         (1|ID_CONTACTO),
#       data    = sleep_time_tidy,
#       weights = weights,
#       REML    = FALSE
#     ),
#     lmer(
#       formula = `Sleeping time` ~ Measure *
#         (resilience_pre + I(resilience_pre^2)) +
#         (1|ID_CONTACTO),
#       data    = sleep_time_tidy,
#       weights = weights,
#       REML    = FALSE
#     )
#   ) %>%
#     tidy() %>%
#     select(term, statistic:p.value) %>%
#     slice(2) %>%
#     mutate(term = "resilience_pre^2^")
# )
# 
# # Significance test of the squared term of "resilience_pre"
# res2_pval <- res2_fit %>% print_chisq_test(df_var = "df")
}

## ----predictor-selection-sleep-time----
selected_preds <- univariate_tests_sleeptime %>%
  filter(p.value < SIG_LEVEL) %>%
  pull(term)


## ----ordinal-linearity-tests-sleep-time----
selected_ord <- selected_preds %>% intersect(ordinal_preds)

ord_linearity_tests <- selected_ord %>% map_df(
  ~{
    aux_dataset <- sleep_time_tidy %>%
      mutate(linear = !!sym(.x) %>% as.integer())
    
    nested_fit <- lmer(
      formula = glue("`Sleeping time` ~ Measure * linear + (1|ID_CONTACTO)") %>%
        as.formula(),
      data    = aux_dataset,
      weights = weights,
      REML    = FALSE
    )
    general_fit <- lmer(
      formula = glue("`Sleeping time` ~ Measure * {.x} + (1|ID_CONTACTO)") %>%
        as.formula(),
      data    = aux_dataset,
      weights = weights,
      REML    = FALSE
    )
    
    ## Supress "In tidy.anova(.) :
    ##            The following column names in ANOVA output were not
    ##            recognized or transformed: npar"
    suppressWarnings(
      anova(general_fit, nested_fit) %>%
        tidy() %>%
        select(term, statistic:p.value) %>%
        slice(2) %>%
        mutate(term = .x)
    )
  }
)

linear_ord_preds_sleep_time <- ord_linearity_tests %>%
  filter(p.value >= SIG_LEVEL) %>%
  pull(term)

sleep_time_tidy <- sleep_time_tidy %>%
  mutate_at(linear_ord_preds_sleep_time, as.integer) %>%
  mutate_at(linear_ord_preds_sleep_time, `-`, 1L)


## ----stepwise-mixed-effects-sleep-time----

sleep_time_tidy <- sleep_time_tidy %>% select(
  ID_CONTACTO, weights, `Sleeping time`, Measure, all_of(selected_preds)
)

scope <- list(
  lower = "`Sleeping time` ~ Measure + (1|ID_CONTACTO)",
  upper = paste0(
    "`Sleeping time` ~ Measure + Measure : (",
    glue_collapse(selected_preds, sep = " + "),
    ") + (1|ID_CONTACTO)"
  )
)

sleep_init_fit <- lmer(
  formula = scope$upper %>% as.formula(),
  data    = sleep_time_tidy,
  weights = weights,
  REML    = FALSE
)

# Same backward step procedure as implemented by lmerTest::step, but
#   allowing for different sample sizes in reduced models due to missing data.
sleep_final_fit <- step_backwards_lmer(
  sleep_init_fit,
  min_model = scope$lower,
  alpha_fixed = SIG_LEVEL,
  bonf_correct = TRUE
)


## ----mixed-effects-final-model-config-sleep-time----

sleep_final_terms <- sleep_final_fit %>%
  terms() %>%
  attr("term.labels") %>%
  extract(-1) %>%
  str_remove("Measure:")

sleep_fit_final_dataset <- sleep_time_tidy %>%
  drop_na(`Sleeping time`, all_of(sleep_final_terms)) %>%
  mutate(mweight = 1) %>%
  as.data.frame()

formula_final <- paste0(
  "`Sleeping time` ~ Measure * (",
  glue_collapse(sleep_final_terms, sep = " + "),
  ") + (1|ID_CONTACTO)"
)


## ----mixed-effects-final-model-sleep-time----

sleep_fit <- mix(
  formula  = formula_final %>% as.formula(),
  data     = sleep_fit_final_dataset,
  weights  = c("mweight", "weights"),
  cWeights = TRUE
)

save(sleep_fit, file = "sleep_fit.RData")


## ----mixed-effects-sleep-time-model-processing----

sleep_summ_fit <- sleep_fit %>% summary()

sleep_coefficients <- sleep_summ_fit$coef %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  as_tibble() %>%
  mutate(p.value = `t value` %>% abs() %>% pnorm(lower.tail = FALSE) * 2)

nobs    <- sleep_fit %>% extract2(c("ngroups", "Number of obs"))
ngroups <- sleep_fit %>% extract2(c("ngroups", "ID_CONTACTO"))


## ----mixed-effects-final-sleep-time-model-Stata----

sleep_final_terms_stata <- sleep_final_terms %>% {
  
  select_at(sleep_fit_final_dataset, .) %>%
    map_lgl(is.numeric) %>%
    { paste0(if_else(., 'c.', ''), names(.)) }
}
sleep_predictors_stata <- "Measure##" %>%
  paste0(sleep_final_terms_stata, collapse = ' ')

sleeptime_syntax <- paste(
  "mixed sleeping_time",
  sleep_predictors_stata,
  "[pweight=mweight] || ID_CONTACTO:, pweight(weights)"
)

stata_out_sleep_time <- capture.output(
  stata(
    src     = sleeptime_syntax,
    data.in = sleep_fit_final_dataset %>%
      rename(sleeping_time = `Sleeping time`)
  )
)
# Results are identical to the output of function `WeMix::mix`


## ----conclusions-pre-computations-sleep-time----
sleep_change_coefs <- sleep_coefficients %>%
  filter(term %>% str_detect("^MeasurePost:"))

sleep_change_coefs_sig <- sleep_change_coefs %>%
  filter(p.value < SIG_LEVEL / n()) # Bilateral, Bonferroni-corrected

sleep_change_sig_terms <- sleep_change_coefs_sig %>%
  pull(term) %>%
  enclose('`') %>%
  glue_collapse(sep = ', ', last = ', and  ')


# Measure main effect (conditional for men):
main_change_coef <- sleep_coefficients %>% filter(term == "MeasurePost")

main_pval <- main_change_coef %>% pull() %>% print_pvalue()
main_coef <- main_change_coef %>% pull(Estimate)

main_change_dir <- main_coef %>% coef_dir(form = "noun")
main_change_abs <- main_coef %>% abs() %>% dhours %>% minsec_format()


# Age:
age_term <- sleep_change_coefs %>%
  filter(term %>% str_detect("age$"))

age_pval <- age_term %>% pull() %>% print_pvalue()
age_coef <- age_term %>% pull(Estimate)

age_change_dir      <- age_coef %>% coef_dir()
age_change_dir_noun <- age_coef %>% coef_dir(form = "noun")
age_change_abs      <- age_coef %>% abs() %>% dhours() %>% sec_format()

AGE_MIN_EXAMPLE  <- 50
AGE_DIFF_EXAMPLE <- 30
AGE_MAX_EXAMPLE  <- AGE_MIN_EXAMPLE + AGE_DIFF_EXAMPLE

age_example <- age_coef %>%
  abs() %>%
  dhours() %>%
  multiply_by(AGE_DIFF_EXAMPLE) %>%
  minsec_format()

# UCLA Loneliness scale (post):
ucla_lon_post_term <- sleep_change_coefs %>%
  filter(term %>% str_detect("ucla_lon_post$"))

ucla_lon_post_pval <- ucla_lon_post_term %>% pull() %>% print_pvalue()
ucla_lon_post_coef <- ucla_lon_post_term %>%
  pull(Estimate)
ucla_lon_post_change_dir <- ucla_lon_post_coef %>% coef_dir(form = "noun")
ucla_lon_post_coef_abs <- ucla_lon_post_coef %>% 
  abs() %>%
  dhours() %>%
  minsec_format()

ucla_lon_post_scaling <- dataset_outcomes_std %>%
  pull(ucla_lon_post) %>%
  attr("scaled:scale")

ucla_lon_post_coef_abs_score <- (
  ucla_lon_post_coef %>% abs() / ucla_lon_post_scaling * 100 / 6
) %>% dhours() %>% minsec_format()


## ----sleep-time-histogram-population-by-age----
sleep_time_pop_hist_by_age_output <- sleep_time_tidy %>%
  mutate(
    age_group = (age + 50) %>% cut(
      # breaks         = quantile(., (0:5)/5, na.rm = TRUE),
      breaks         = c(18, seq(25, 85, 10), Inf),
      include.lowest = TRUE,
      ordered_result = TRUE
    )
  ) %>%
  ggplot(aes(weight = weights, fill = Measure, color = Measure)) +
  geom_histogram(
    aes(x = `Sleeping time`, y = ..density..),
    alpha = .5, binwidth = 1
  ) +
  geom_hline(yintercept = 0, size = .1) +
  facet_grid(rows = vars(age_group), cols = vars(Measure)) +
  scale_y_continuous(
    limits = c(0, .5),
    labels = percent,
    expand = expansion(),
    breaks = 0:2 * .2,
    minor_breaks = NULL
  ) +
  scale_x_continuous(
    limits = c(0, 13),
    expand = expansion(),
    breaks = 1:12,
    minor_breaks = NULL
  ) +
  scale_color_manual(
    values     = c(Pre = "blue", Post = "red"),
    aesthetics = c("colour", "fill"),
    guide      = NULL
  ) +
  theme(plot.margin = margin(5, 8, 5, 0)) +
  xlab(VAR_SLEEP_HOURS) +
  ylab("Percentage in population")


## ---- Suicidal ideation: ----

## ----suicidal-dataset----
suicidal <- dataset_outcomes_std %>%
  select(
    ID_CONTACTO,
    starts_with("suicidal"),
    any_of(all_preds),
    starts_with(time_varying_preds),# These are needed for the linear model with
    weights                         #   negative cases in the Pre measure
  ) %>%
  rename(Pre = suicidal_pre, Post = suicidal_post)

## ----suicidal-descriptives ----

suicidal_props <- suicidal %>% {
  full_join(
    tabyl(., Pre) %>%
      adorn_pct_formatting() %>%
      rename(`Suicidal ideation` = Pre),
    tabyl(., Post) %>%
      adorn_pct_formatting() %>%
      rename(`Suicidal ideation` = Post),
    by = "Suicidal ideation",
    suffix = c("_pre", "_post")
  )
} %>% as_tibble()

suicidal_prop_pre <- suicidal_props %>%
  filter(`Suicidal ideation` == "Yes") %>%
  pull(valid_percent_pre)
suicidal_prop_post <- suicidal_props %>%
  filter(`Suicidal ideation` == "Yes") %>%
  pull(valid_percent_post)


## ----suicidal-prevalence-population----

suicidal_est <- suicidal %>%
  select(Pre, Post, weights) %>%
  drop_na() %>%
  mutate(suic_weights = weights / mean(weights))

suicidal_prevalence_est <- suicidal_est %>% {
  full_join(
    count(., Pre,  wt = suic_weights) %>% rename(Suicidal = Pre,  Pre  = n),
    count(., Post, wt = suic_weights) %>% rename(Suicidal = Post, Post = n),
    by = "Suicidal"
  )
} %>%
  mutate(across(-Suicidal, ~. / sum(.))) %>%
  adorn_pct_formatting()

suicidal_prev_pre <- suicidal_prevalence_est %>%
  filter(Suicidal == "Yes") %>%
  pull(Pre)
suicidal_prev_post <- suicidal_prevalence_est %>%
  filter(Suicidal == "Yes") %>%
  pull(Post)


## ----suicidal-prevalence-estimates----

# This version estimates the prevalence in the population with the whole
#   sample size in the Pre measure.

suicidal_pre <- dataset_outcomes_compare_excluded %>%
  select(number_id, Pre = suicidal_pre, sex, age_pre, weights) %>%
  mutate(
    Age = (age_pre < 50) %>%
      factor(levels = c(TRUE, FALSE), labels = c("18 - 49", "50 +"))
  )

suicidal_prevalence_pre <- suicidal_pre %>%
  {
    bind_rows(
      bind_cols(
        tibble(Variable = "Total", Group = ""),
        estimate_prevalence(., Pre, weights, "Yes")
      ),
      group_by(., Group = sex) %>% estimate_prevalence(Pre, weights, "Yes") %>%
        add_column(Variable = "Sex"),
      group_by(., Group = Age) %>% estimate_prevalence(Pre, weights, "Yes") %>%
        add_column(Variable = "Age")
    )
  } %>%
  adorn_pct_formatting(digits = 2, ... = c(Prevalence, Std_err))

suicidal_prevalence_post <- suicidal %>%
  mutate(
    Age = (age_post < 0) %>% # Because it is centered at 50
      factor(levels = c(TRUE, FALSE), labels = c("18 - 49", "50 +"))
  ) %>% 
  {
    bind_rows(
      bind_cols(
        tibble(Variable = "Total", Group = ""),
        estimate_prevalence(., Post, weights, "Yes")
      ),
      group_by(., Group = sex) %>% estimate_prevalence(Post, weights, "Yes") %>%
        add_column(Variable = "Sex"),
      group_by(., Group = Age) %>% estimate_prevalence(Post, weights, "Yes") %>%
        add_column(Variable = "Age")
    )
  } %>%
  adorn_pct_formatting(digits = 2, ... = c(Prevalence, Std_err))


## ----suicidal-association----

suic_contingency_complete <- suicidal %>%
  mutate_at(vars(Pre, Post), fct_explicit_na) %>%
  tabyl(Pre, Post)

suic_contingency_valid <- suicidal %>%
  filter_at(vars(Pre, Post), ~!is.na(.)) %>%
  tabyl(Pre, Post)

## Not used:
chisq_suic <- suic_contingency_valid %>%
  chisq.test(simulate.p.value = TRUE) %>%
  tidy() %>%
  print_chisq_test()

mcnemar_suic <- suic_contingency_valid %>%
  select(-Pre) %>%
  as.matrix() %>%
  mcnemar.test() %>%
  tidy() %>%
  print_chisq_test()


## ----suicidal-sankey----
suic_count <- suicidal %>% group_by_all() %>% count()

suic_count %$% plot_ly(
    type        = "sankey",
    orientation = "h",

    node        = list(
      label     = c(Pre %>% levels(), Post %>% levels()),
      color     = c("blue", "red", "blue", "red")
    ),

    link = list(
      source = as.integer(Pre)  - 1,
      target = as.integer(Post) - 1 + nlevels(Pre),
      value =  n
    )
  )


## ----suicidal-association-population----

suicidal_contingency_est <- suicidal_est %>%
  count(Pre, Post, wt = suic_weights) %>%
  spread(Post, n)

chisq_suic_est <- suicidal_contingency_est %>%
  select(-Pre) %>%
  chisq.test(simulate.p.value = TRUE) %>%
  tidy() %>%
  print_chisq_test() # Association in the population (result not used)

mcnemar_suic_est <- suicidal_contingency_est %>%
  select(-Pre) %>%
  as.matrix() %>%
  mcnemar.test(correct = FALSE) %>%
  tidy() %>%
  print_chisq_test() # No hay cambio en la tasa de prevalencia

suicidal_contingency_est <- suicidal_contingency_est %>%
  mutate(across(where(is.double), `/`, sum(c_across(No:Yes))))

# Power test:
p01 <- suicidal_contingency_est %>% filter(Pre == "No")  %>% pull(Yes)
p10 <- suicidal_contingency_est %>% filter(Pre == "Yes") %>% pull(No)

p_ratio <- p10 / p01
if (p_ratio < 1) {
  
  p_ratio <- 1 / p_ratio
  p01 <- p10
}

power_test_suic <- power_mcnemar_test(
  n    = suicidal_est %>% nrow(),
  paid = p01,
  psi  = p_ratio
) %>%
  tidy() %>%
  pull(power) %>%
  format_pvalues()


## ----suicidal-alluvial-population----
suicidal_alluvial_plot <- suicidal_contingency_est %>%
  gather(Post, Frequency, -Pre) %>%
  mutate(Change = (Post != Pre) %>% factor(labels = c("No", "Yes"))) %>%
  to_lodes_form(Pre:Post, key = "Measure", value = "Suicidal") %>%
  mutate(
    freq_label = if_else(
      Measure == "Pre",
      Frequency %>% percent(.1) %>% enclose("("), "")
  ) %>%
  full_join(
    suicidal_prevalence_est %>% gather(Measure, Prevalence, Pre:Post),
    by = c("Measure", "Suicidal")
  ) %>%
  ggplot(
    aes(x = Measure, y = Frequency, alluvium = alluvium, stratum = Suicidal)
  ) +
  geom_flow(aes(fill = Suicidal)) +
  geom_stratum(aes(fill = Suicidal), color = NA) +
  geom_text(stat = "stratum", aes(label = Prevalence), size = 4.5) +
  scale_x_discrete(
    limits = c("Pre", "Post"),
    expand = expansion(),
    name   = NULL
  ) +
  scale_y_continuous(breaks = NULL, name = NULL) +
  scale_fill_brewer(
    palette    = "Set1",
    direction  = -1
  ) +
  guides(fill = guide_legend(title = "Suicidal ideation")) +
  geom_text_repel(
    aes(label = freq_label),
    stat          = "flow",
    size          = 2.5,
    nudge_x       = .3,
    segment.color = NA
  ) +
  ggtitle(
    CAPTION_ALLUVIAL_SUICIDAL,
    subtitle = glue("N = {suicidal_est %>% nrow()}")
  ) +
  theme(
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    plot.title         = element_text(hjust = .5),
    plot.subtitle      = element_text(hjust = .5)
  )


## ----tidy-suicidal----
suicidal_tidy <- suicidal %>%
  pivot_longer(
    cols      = Pre:Post,
    names_to  = "Measure",
    values_to = "Suicidal",
    names_transform = list(Measure = factor)
  ) %>%
  mutate(Measure = Measure %>% fct_rev) %>%
  full_join(dataset_outcomes_varying_std, by = c("ID_CONTACTO", "Measure"))


## ----optimizer-configuration-suicidal----
# All of them are essentially equal for the nested model; for the general one,
#   `nloptwrap.NLOPT_LN_NELDERMEAD` is still the best.
#
# FINALLY: We opt for the optimizer `nloptwrap.NLOPT_LN_NELDERMEAD`; however,
#   for the general model in `pain_physical`, `bobyqa` needs to be used.
glmer_control <- function(predictor = "", model = c("nested", "general")) {

  model <- match.arg(model)
  
  if (predictor == "pain_physical") {

    glmerControl(optimizer = "bobyqa")
    
  } else {
    
    glmerControl(
      optimizer = "nloptwrap",
      optCtrl   = list(
        algorithm = "NLOPT_LN_NELDERMEAD",
        ftol_abs  = 1e-8,
        xtol_abs  = 1e-8
      )
    )
  }
}


## ----univariate-tests-suicidal----

## TODO: Check if `edu_level_rec` has been converted to linear?

# This function reports convergence checking issues, although convergence has
#   already been assessed and ensured in chunk `optimizer-tests-suicidal`.
warning_func <- function(predictor, model = c("nested", "general")) {

  model <- match.arg(model)

  function(w) {

    warning_msg <- gettext(w)

    if (
      warning_msg != paste(
        "simpleWarning in eval(family$initialize, rho):",
        "non-integer #successes in a binomial glm!\n"
      )
    ) {
      cat(predictor, model, warning_msg, sep = "\n", fill = TRUE)
    }
  }
}

univariate_tests_suicidal <- all_preds %>% map_df(
  ~{
    withCallingHandlers(
      nested_fit <- glmer(
        formula = glue("Suicidal ~ Measure + {.x} + (1|ID_CONTACTO)") %>%
          as.formula(),
        data    = suicidal_tidy,
        family  = binomial,
        weights = weights,
        control = glmer_control()
      ),
      warning = warning_func(.x, "nested")
    )
    withCallingHandlers(
      general_fit <- glmer(
        formula = glue("Suicidal ~ Measure * {.x} + (1|ID_CONTACTO)") %>%
          as.formula(),
        data    = suicidal_tidy,
        family  = binomial,
        weights = weights,
        control = glmer_control(predictor = .x, model = "general")
      ),
      warning = warning_func(.x, "general")
    )

    ## Supress "In tidy.anova(.) :
    ##            The following column names in ANOVA output were not
    ##            recognized or transformed: npar"
    suppressWarnings(
      anova(general_fit, nested_fit) %>%
        tidy() %>%
        select(term, statistic:p.value) %>%
        slice(2) %>%
        mutate(term = .x)
    )
  }
)

save(univariate_tests_suicidal, file = "univariate_tests_suicidal.RData")


## ----predictor-selection-suicidal----
## Bonferroni correction is applied in order to try
##   to avoid complete separation.
selected_preds_suicidal <- univariate_tests_suicidal %>%
  # filter(p.value < SIG_LEVEL) %>% # No correction
  # filter(p.value < SIG_LEVEL / n()) %>%   # Bonferroni correction
  arrange(desc(p.value)) %>%              # Holm-Bonferroni correction
  mutate(
    sig  = p.value < SIG_LEVEL / row_number(),
    keep = p.value < min(p.value[!sig])
  ) %>%
  filter(keep) %>%
  pull(term)

# To have the predictors in the usual order:
selected_preds_suicidal <- all_preds %>% intersect(selected_preds_suicidal)


## ----ordinal-linearity-tests-suicidal----
selected_ord_suicidal <- selected_preds_suicidal %>% intersect(ordinal_preds)

ord_linearity_tests_suicidal <- selected_ord_suicidal %>% map_df(
  ~{
    aux_dataset <- suicidal_tidy %>%
      mutate(linear = !!sym(.x) %>% as.integer())

    nested_fit <- glmer(
      formula = glue("Suicidal ~ Measure * linear + (1|ID_CONTACTO)") %>%
        as.formula(),
      data    = aux_dataset,
      family  = binomial,
      weights = weights,
      control = glmer_control()
    )
    general_fit <- glmer(
      formula = glue("Suicidal ~ Measure * {.x} + (1|ID_CONTACTO)") %>%
        as.formula(),
      data    = aux_dataset,
      family  = binomial,
      weights = weights,
      control = glmer_control(.x)
    )

    ## Supress "In tidy.anova(.) :
    ##            The following column names in ANOVA output were not
    ##            recognized or transformed: npar"
    suppressWarnings(
      anova(general_fit, nested_fit) %>%
        tidy() %>%
        select(term, statistic:p.value) %>%
        slice(2) %>%
        mutate(term = .x)
    )
  }
)

save(ord_linearity_tests_suicidal, file = "ord_linearity_tests_suicidal.RData")


## ----ordinal-linearity-tests-processing-suicidal----
# Bonferroni correction is applied.
linear_ord_preds_suicidal <- ord_linearity_tests_suicidal %>%
  filter(p.value >= (SIG_LEVEL / n())) %>%
  pull(term)

# Trasnform to integer the linear predictors, in order to avoid the use of
#   high-order polynomial terms.
#   The reference (lower) category is set to 0 by substracting 1 from the
#   integer resulting from the transformed category.
suicidal_tidy <- suicidal_tidy %>%
  mutate_at(c(linear_ord_preds_sleep_time, "severity"), as.integer) %>%
  mutate_at(c(linear_ord_preds_sleep_time, "severity"), `-`, 1L)
# (`severity` fails when used as ordinal due to complete separation)


## ----stepwise-mixed-effects-suicidal----
best_fit_suicidal <- function(formula, weights = NULL) {
  
  # optimizers to try initially
  optimizers <- c("init", "bobyqa", "Nelder_Mead", "nlminbwrap", "nmkbw")

  fit_init <- NULL
  optimizer <- NULL

  while (is_non_empty(optimizers)) {

    tryCatch(
      {
        fit_init <- if (optimizers[1] == "init") {

          glmer(
            formula = formula,
            data    = suicidal_fit_dataset,
            family  = binomial,
            weights = weights
          )

        } else {

          glmer(
            formula = formula,
            data    = suicidal_fit_dataset,
            family  = binomial,
            weights = weights,
            control = glmerControl(optimizer = optimizers[1])
          )
        }
        break
      },
      error = function(e) {

        optimizers <- optimizers[-1]
      }
    )
  }

  if (is_null(fit_init)) stop("All initial optimizers failed.")

  fit_all <- fit_init %>% allFit()

  LL_diffs <- fit_all %>%
    summary() %>%
    extract2("llik") %>%
    enframe("optimizer", "LL") %>%
    bind_rows(
      tibble(
        optimizer = glue("initial_{optimizers[1]}"),
        LL        = fit_init %>% logLik() %>% as.numeric()
      )
    ) %>%
    mutate(LL_diff = max(LL) - LL) %>%
    arrange(LL_diff)

  which_best <- LL_diffs %>% slice(which.max(LL)) %>% pull(optimizer)

  fit_best <- if (which_best == glue("initial_{optimizers[1]}")) {

    fit_init

  } else {

    fit_all[[which_best]]
  }

  attr(fit_best, "LL_diffs") <- LL_diffs

  fit_best
}

## Setpwise algorithm:

iterate_fit_suicidal <- function(init_fit, candidate_terms) {

  current_fit <- init_fit

  history <- tibble(
    iteration        = 0L,
    chosen_predictor = NA_character_,
    candidates_LL    = list(current_fit %>% attr("LL_diff")),
    candidates_AIC   = list(current_fit %>% AIC()),
    model_LR         = list(tibble()),
    comp_optims_LL   = list(tibble()),
    n_subjects       = current_fit %>% summary() %>% extract2("ngrps"),
    n_obs            = current_fit@frame %>% nrow()
  )

  repeat{

    cat(
      "\nIteration ", history %>% slice_tail() %>% pull(iteration) + 1, ":\n\n",
      sep = ""
    )

    current_formula <- current_fit %>%
      formula() %>%
      as.character() %>%
      extract(c(2, 1, 3)) %>% glue_collapse(sep = " ")
    current_terms   <- current_formula %>%
      as.formula() %>%
      terms() %>%
      attr("term.labels") %>%
      str_subset("^Measure:") %>%
      str_remove("^Measure:")

    candidate_terms <- candidate_terms %>% setdiff(current_terms)

    if (is_empty(candidate_terms)) break

    candidate_models <- candidate_terms %>% map(
      ~{
        cat("\nFitting model for candidate predictor `", .x, "`:\n\n", sep = "")

        suicidal_fit_dataset <<- suicidal_tidy %>%
          drop_na(Suicidal, all_of(c(current_terms, .x))) %>%
          correct_weights(ID_CONTACTO)

        formula_step <- glue("{current_formula} + Measure : {.x}")

        suicidal_step_fit <- best_fit_suicidal(
          formula = formula_step %>% as.formula(),
          weights = weights
        )
      }
    ) %>%
      set_names(candidate_terms)

    candidates_opt_fits <- candidate_models %>%
      map_df(attr, "LL_diffs", .id = "candidate") %>%
      group_by(candidate)
    candidates_AICs     <- candidate_models %>% map_dbl(AIC)
    chosen_predictor    <- candidates_AICs %>% which.min() %>% names()

    chosen_fit <- candidate_models[[chosen_predictor]]

    cat(
      "\nNew candidate predictor chosen is `", chosen_predictor, "`\n\n",
      sep = ""
    )

    suicidal_fit_dataset <<- suicidal_tidy %>%
      drop_na(Suicidal, all_of(c(current_terms, chosen_predictor))) %>%
      correct_weights(ID_CONTACTO)
    current_fit_comp <- best_fit_suicidal(
      formula = current_formula %>% as.formula(),
      weights = weights
    )

    model_lr <- chosen_fit %>%
      anova(current_fit_comp) %>%
      tidy() %>%
      slice(2) %>%
      select(statistic:p.value)

    history <- history %>% bind_rows(
      tibble(
        iteration        = nrow(.),
        chosen_predictor = chosen_predictor,
        candidates_LL    = list(candidates_opt_fits),
        candidates_AIC   = list(candidates_AICs),
        model_LR         = list(model_lr),
        comp_optims_LL   = list(current_fit_comp %>% attr("LL_diff")),
        n_subjects       = chosen_fit %>% summary() %>% extract2("ngrps"),
        n_obs            = chosen_fit@frame %>% nrow()
      )
    )

    if (model_lr %>% pull() > SIG_LEVEL) break

    cat(
      "\nModel with predictor `", chosen_predictor,
      "` fits significantly better than previous iteration model; ",
      "predictor added.\n\n",
      sep = ""
    )

    current_fit <- chosen_fit
  }

  cat("Stepwise regression complete.\n\n")

  attr(current_fit, "history") <- history

  current_fit
}


# Initialization:

suicidal_fit_dataset <- suicidal_tidy %>%
  drop_na(Suicidal, Measure) %>%
  correct_weights(ID_CONTACTO)


formula_init <- "Suicidal ~ Measure + (1|ID_CONTACTO)"


# Initial fit with only the `Measure` variable:
suicidal_init_fit <- best_fit_suicidal(
  formula = formula_init %>% as.formula(),
  weights = weights
)

# Iterative addition of predictors:
suicidal_final_fit <- iterate_fit_suicidal(
  suicidal_init_fit,
  selected_preds_suicidal
)

# Check differences in convergence among optimizers
suicidal_final_fit %>% attr("LL_diff") # A difference of 0.221 can be considered
                                       #   good enough.
save(suicidal_final_fit, file = "suicidal_final_fit.RData")


## ----mixed-effects-final-suicidal-model-Stata----

suicidal_final_terms <- suicidal_final_fit %>%
  terms() %>%
  attr("term.labels") %>%
  extract(-1) %>%
  str_remove("Measure:")

formula_final <- paste0(
  "Suicidal ~ Measure * (",
  glue_collapse(suicidal_final_terms, sep = " + "),
  ") + (1|ID_CONTACTO)"
)

suicidal_fit_final_dataset <- suicidal_tidy %>%
  drop_na(Suicidal, all_of(suicidal_final_terms)) %>%
  mutate(
    mweight = 1,
    Suicidal = (Suicidal == "Yes") %>% as.numeric()
  )


## Does not work:
# suicidal_fit <- mix(
#   formula  = formula_final %>% as.formula(),
#   data     = suicidal_fit_final_dataset,
#   weights  = c("mweight", "weights"),
#   cWeights = TRUE,
#   family   = binomial(link = "logit")
# )
# 
# save(suicidal_fit, file = "suicidal_fit.RData")

suicidal_final_terms_stata <- suicidal_final_terms %>% {
  
  select_at(suicidal_fit_final_dataset, .) %>%
    map_lgl(is.numeric) %>%
    { paste0(if_else(., 'c.', ''), names(.)) }
}
suicidal_predictors_stata <- "Measure##" %>%
  paste0(suicidal_final_terms_stata, collapse = ' ')

suicidal_syntax <- paste(
  "melogit Suicidal",
  suicidal_predictors_stata,
  "[pweight=mweight] || ID_CONTACTO:, pweight(weights) or"
)

stata_out_suicidal <- capture.output(
  stata(
    src     = suicidal_syntax,
    data.in = suicidal_fit_final_dataset
  )
)


## ----mixed-effects-suicidal-model-processing----

nobs    <- suicidal_fit_final_dataset %>% nrow()
ngroups <- suicidal_final_fit@Gp[2]

suicidal_coefficients <- stata_out_suicidal %>% get_tables() %>% extract2(1)


## ----conclusions-pre-computations-suicidal----

suicidal_change_coefs <- suicidal_coefficients %>%
  filter(Suicidal %>% str_detect("^Measure#"))

suicidal_change_coefs_sig <- suicidal_change_coefs %>%
  filter(`P>|z|` < SIG_LEVEL / n()) # Bilateral, Bonferroni-corrected
# arrange(desc(`P>|z|`)) %>%              # Holm-Bonferroni correction
# mutate(
#   sig  = `P>|z|` < SIG_LEVEL / row_number(),
#   keep = `P>|z|` < min(`P>|z|`[!sig])
# ) %>%
# filter(keep)

suicidal_change_sig_terms <- suicidal_change_coefs_sig %>%
  pull(Suicidal) %>%
  enclose('`') %>%
  glue_collapse(sep = ', ', last = ', and  ')


# Measure main effect (conditional for men):
main_change_coef <- suicidal_coefficients %>%
  filter(Suicidal == "Measure", level == "Post")

main_pval <- main_change_coef %>% pull(`P>|z|`) %>% print_pvalue()
main_coef <- main_change_coef %>% pull(`Odds Ratio`)


# Oslo 3:
oslo3_post_term <- suicidal_change_coefs_sig %>%
  filter(Suicidal %>% str_detect("oslo3_sss_"))

oslo3_post_pval <- oslo3_post_term %>% pull(`P>|z|`) %>% print_pvalue()
oslo3_post_coef <- oslo3_post_term %>% pull(`Odds Ratio`)

oslo3_post_change_dir <- oslo3_post_coef %>% log() %>% coef_dir(form = "noun")
oslo3_post_change_abs <- (1 - oslo3_post_coef) %>% percent(.1)

oslo3_post_scaling <- dataset_outcomes_std %>%
  pull(oslo3_sss_post) %>%
  attr("scaled:scale")

# Negative OR per point in the scale
#   (each point == reduction of `oslo3_post_neg_OR` in the odds)
oslo3_post_neg_OR <- (
  1 - exp(log(oslo3_post_coef) * 100 / 11 / oslo3_post_scaling)
) %>%
  percent(.1)

## TODO: Drop this?

# depr_pre_term <- suicidal_change_coefs_sig %>%
#   filter(Suicidal %>% str_detect("depression~e"))
# depr_pre_pval <- depr_pre_term %>% pull(`P>|z|`) %>% print_pvalue()
# depr_pre_coef <- depr_pre_term %>% pull(`Odds Ratio`)
# depr_pre_coef_dir <- depr_pre_coef %>% log() %>% coef_dir(form = "noun")
# depr_pre_neg_OR <- (1 - depr_pre_coef) %>% percent(.1)


## ----suicidal-depression-contingency----

## TODO: Chunk not needed?

suicidal_depression_pre_contingency <- suicidal %>%
  select(depression_pre, Pre, Post) %>%
  drop_na() %>%
  tabyl(Post, depression_pre, Pre) %>%
  bind_rows(.id = "Pre")

suic_depr_pre_cont_perc <- suicidal_depression_pre_contingency %>%
  adorn_percentages(denominator = "col")

suicidal_depression_pre_contingency <- suicidal_depression_pre_contingency %>%
  full_join(
    suic_depr_pre_cont_perc,
    by     = c("Pre", "Post"),
    suffix = c("", "_perc")
  ) %>%
  adorn_totals(where = "row", fill = "") %>%
  adorn_pct_formatting(... = ends_with("_perc"), digits = 2) %>%
  select(Pre, Post, starts_with("No"), starts_with("Yes"))

suic_yes_to_no_depr_yes <- suicidal_depression_pre_contingency %>% 
  filter(Pre == "Yes", Post == "No") %>% pull(Yes_perc)


## ----suicidal-selection-no-pre----

# # Drop terms that lead to complete separation:
# complete_sep_suicidal_terms <-
#   c("livesalone_pre", "livesalone_post", "severity")
# all_preds_suicidal_new <- all_preds_new %>% setdiff(complete_sep_suicidal_terms)

suicidal_neg_pre <- suicidal %>% filter(Pre == "No")

# Survey design, to use with `svyglm` calls:
suicidal_np_design <- svydesign(
  ids     = ~ID_CONTACTO,
  data    = suicidal_neg_pre,
  weights = suicidal_neg_pre %>% pull(weights)
)


## ----suicidal-no-pre-contingency----
suicidal_no_pre_contingency <- suicidal_neg_pre %>%
  select(Post, all_of(all_preds_new)) %>%
  frequencies_table(.segmentation = Post, missing = FALSE)

## ----univariate-tests-new-suicidal----
univariate_tests_suicidal_new <- all_preds_new %>% map_df(
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

## ----predictor-selection-new-suicidal----

## Bonferroni correction is applied in order to try
##   to avoid complete separation. # TODO: Check whether this comment is true
selected_preds_suicidal_new <- univariate_tests_suicidal_new %>%
  filter(p.value < SIG_LEVEL) %>% # No correction
  # filter(p.value < SIG_LEVEL / n()) %>%   # Bonferroni correction
  # arrange(desc(p.value)) %>%              # Holm-Bonferroni correction
  # mutate(
  #   sig  = p.value < SIG_LEVEL / row_number(),
  #   keep = p.value < min(p.value[!sig])
  # ) %>%
  # filter(keep) %>%
  pull(term) %>%
  union(FIXED_PREDICTORS_NEW)

# To have the predictors in the usual order:
selected_preds_suicidal_new <- all_preds_new %>%
  intersect(selected_preds_suicidal_new)


## ----ordinal-linearity-tests-new-suicidal----

selected_ord_suicidal_new <- selected_preds_suicidal_new %>%
  intersect(ordinal_preds_new)

ord_linearity_tests_suicidal_new <- selected_ord_suicidal_new %>% map_df(
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

linear_ord_preds_suicidal_new <- ord_linearity_tests_suicidal_new %>%
  filter(p.value >= (SIG_LEVEL / n())) %>%
  pull(term)
nonlinear_ord_preds_suicidal_new <- selected_ord_suicidal_new %>%
  setdiff(linear_ord_preds_suicidal_new)

linear_ord_preds_suicidal_new_lab_out <- var_properties %>%
  pull(labels_abbr) %>%
  extract(linear_ord_preds_suicidal_new) %>%
  enclose("*")

# Trasnform to integer the linear predictors, in order to avoid the use of
#   high-order polynomial terms.
#   The reference (lower) category is set to 0 by substracting 1 from the
#   integer resulting from the transformed category.
suicidal_neg_pre <- suicidal_neg_pre %>%
  mutate_at(linear_ord_preds_suicidal_new, as.integer) %>%
  mutate_at(linear_ord_preds_suicidal_new, `-`, 1L)


## ----stepwise-glm-new-suicidal----

# Initialization:

## Scope (minimum and maximum model):
scope <- list(
  lower = "Post ~" %>% paste(paste0(FIXED_PREDICTORS_NEW, collapse = " + ")) %>%
    as.formula(),
  upper = "Post ~" %>% paste(
    glue_collapse(selected_preds_suicidal_new, sep = " + ")
  ) %>% as.formula()
)

## Dataset without missing values for the iterations:
suicidal_neg_pre_fit <- suicidal_neg_pre %>%
  select(ID_CONTACTO, Post, weights, all_of(selected_preds_suicidal_new)) %>%
  drop_na()

suicidal_np_design <- svydesign( # Redefine the design
  ids     = ~ID_CONTACTO,
  data    = suicidal_neg_pre_fit,
  weights = suicidal_neg_pre_fit %>% pull(weights)
)

## Initial fit with all the predictors:
suicidal_init_fit_new <- svyglm(
  formula = scope$upper,
  design  = suicidal_np_design,
  family  = quasibinomial
)

# Iterative dropping of predictors:
suicidal_final_fit_new <- suicidal_init_fit_new %>%
  step(scope = scope, direction = "backward")

# This gives the result of the iterations, but the dataset has dropped cases
#   that may be re-used in the final model, so it is fit again with the
#   complete dataset


# Final fit:
suicidal_np_design <- svydesign( # Redefine the design to include all values
  ids     = ~ID_CONTACTO,
  data    = suicidal_neg_pre %>% mutate(
    across(all_of(nonlinear_ord_preds_suicidal_new), factor, ordered = FALSE)
  ),
  weights = suicidal_neg_pre %>% pull(weights)
)

suicidal_fit_new <- svyglm(
  formula = suicidal_final_fit_new %>% formula(),
  design  = suicidal_np_design,
  family  = quasibinomial
) # No gain in participants nevertheless


# Summary:
suicidal_summ_fit_new     <- suicidal_fit_new %>% summary()
suicidal_coefficients_new <- suicidal_fit_new %>% tidy()
nobs                      <- suicidal_fit_new %>% nobs()


## ----conclusions-pre-computations-suicidal-new----

suicidal_coefs_sig_new <- suicidal_coefficients_new %>%
  filter(term != "(Intercept)") %>% # Do not take it into account for correction
  filter(p.value < SIG_LEVEL / n()) # Bilateral, Bonferroni-corrected
# arrange(desc(p.value)) %>%              # Holm-Bonferroni correction
# mutate(
#   sig  = p.value < SIG_LEVEL / row_number(),
#   keep = p.value < min(p.value[!sig])
# ) %>%
# filter(keep)

suicidal_sig_terms_new <- suicidal_coefs_sig_new %>%
  pull(term) %>%
  enclose('`') %>%
  glue_collapse(sep = ', ', last = ', and  ')


# # Severity:
# severity_term <- suicidal_coefs_sig_new %>% filter(term == "severity")
# severity_pval <- severity_term %>% pull(p.value)  %>% print_pvalue()
# severity_coef <- severity_term %>% pull(estimate) %>% exp()
# severity_dir  <- severity_coef %>% log() %>% coef_dir(form = "indet")
# severity_abs  <- (1 - severity_coef) %>% percent(.1)

# Lives alone (Pre):
livesalone_pre_term <- suicidal_coefs_sig_new %>%
  filter(term == "livesalone_preYes")
livesalone_pre_pval <- livesalone_pre_term %>% pull(p.value)  %>% print_pvalue()
livesalone_pre_coef <- livesalone_pre_term %>% pull(estimate) %>% exp()
livesalone_pre_dir  <- livesalone_pre_coef %>% log() %>% coef_dir(form = "comp")
livesalone_pre_abs  <- (1 - livesalone_pre_coef) %>% percent(.1)

# Lives alone (Post):
# livesalone_post_term <- suicidal_coefs_sig_new %>%
#   filter(term == "livesalone_postYes")
# livesalone_post_pval <- livesalone_post_term %>%
#   pull(p.value) %>%
#   print_pvalue()
# livesalone_post_coef <- livesalone_post_term %>% pull(estimate) %>% exp()
# livesalone_post_dir  <- livesalone_post_coef %>%
#   log() %>%
#   coef_dir(form = "comp")
# livesalone_post_abs  <- (1 - livesalone_post_coef) %>% percent(.1)

# # Oslo 3:
# oslo3_post_term <- suicidal_coefs_sig_new %>% filter(term == "oslo3_sss_post")
# oslo3_post_pval <- oslo3_post_term %>% pull(p.value)  %>% print_pvalue()
# oslo3_post_coef <- oslo3_post_term %>% pull(estimate) %>% exp()
# oslo3_post_dir  <- oslo3_post_coef %>% log() %>% coef_dir(form = "indet")
# oslo3_post_abs  <- (1 - oslo3_post_coef) %>% percent(.1)
# 
# oslo3_post_scaling <- dataset_outcomes_std %>%
#   pull(oslo3_sss_post) %>%
#   attr("scaled:scale")
# 
# # Negative OR per point in the scale
# #   (each point == reduction of `oslo3_post_neg_OR` in the odds)
# oslo3_post_neg_OR <- (
#   1 - exp(log(oslo3_post_coef) * 100 / 11 / oslo3_post_scaling)
# ) %>%
#   percent(.1)


# Contingency table for predictors that yield complete separation:
complete_sep_terms_suic <- c(
  "livesalone_pre", "livesalone_post" #, "physical_post"
)

contingency_complete_separation_suicidal_new <- suicidal_neg_pre_fit %>%
  select(all_of(complete_sep_terms_suic), Post) %>%
  frequencies_table(.segmentation = Post)

complete_sep_terms_suic_labels_out <- var_properties %>%
  pull(labels_comp) %>%
  extract(complete_sep_terms_suic) %>%
  enclose("*") %>%
  glue_collapse(sep = ", ", last = ", and ")


## ----drop-complete-separation-terms-suicidal-new----
selected_preds_suicidal_new <- selected_preds_suicidal_new %>%
  setdiff(complete_sep_terms_suic)

## ----conclusions-pre-computations-suicidal-new-bis----

suicidal_coefs_sig_new <- suicidal_coefficients_new %>%
  filter(term != "(Intercept)") %>% # Do not take it into account for correction
  filter(p.value < SIG_LEVEL / n()) # Bilateral, Bonferroni-corrected
# arrange(desc(p.value)) %>%              # Holm-Bonferroni correction
# mutate(
#   sig  = p.value < SIG_LEVEL / row_number(),
#   keep = p.value < min(p.value[!sig])
# ) %>%
# filter(keep)

suicidal_sig_terms_new <- suicidal_coefs_sig_new %>%
  pull(term) %>%
  enclose('`') %>%
  glue_collapse(sep = ', ', last = ', and  ')

suicidal_sig_terms_labels_new <- suicidal_coefs_sig_new %>%
  pull(term) %>%
  extract(labels_abbr, .)

suicidal_sig_terms_labels_new_collapsed <- suicidal_sig_terms_labels_new %>%
  enclose("*") %>%
  glue_collapse(sep = " and ")


# Oslo 3:

suic_oslo3_post_label <- suicidal_sig_terms_labels_new["oslo3_sss_post"]

suic_oslo3_post_term <- suicidal_coefs_sig_new %>%
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


# Resilience:

suic_resilience_post_label <- suicidal_sig_terms_labels_new["resilience_post"]

suic_resilience_post_term <- suicidal_coefs_sig_new %>%
  filter(term == "resilience_post")
suic_resilience_post_pval <- suic_resilience_post_term %>%
  pull(p.value) %>%
  print_pvalue()
suic_resilience_post_coef <- suic_resilience_post_term %>%
  pull(estimate) %>%
  exp()
suic_resilience_post_dir  <- suic_resilience_post_coef %>%
  log() %>%
  coef_dir(form = "indet")
suic_resilience_post_abs  <- (1 - suic_resilience_post_coef) %>% percent(.1)
suic_resilience_post_test <- suic_resilience_post_term %>% print_z_test()

suic_resilience_post_scaling <- dataset_outcomes_std %>%
  pull(resilience_post) %>%
  attr("scaled:scale")

# Negative OR per point in the scale
#   (each point == reduction of `oslo3_post_neg_OR` in the odds)
suic_resilience_post_neg_OR <- (
  1 - exp(
    log(suic_resilience_post_coef) * 100 / 11 / suic_resilience_post_scaling
  )
) %>%
  percent(.1)


## ----suicidal-coefficients-new----

suicidal_coefficients_new <- suicidal_coefficients_new %>%
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

suicidal_coefficients_new_table <- suicidal_coefficients_new %>%
  select(Term, OR, `(95% CI)`, z = statistic, `*p* value`) %>%
  mutate(across(where(is.numeric), number, 1e-3)) %>%
  flextable() %>%
  add_footer(Term = REG_MODEL_FOOTER) %>%
  merge_at(part = "footer") %>%
  colformat_md(part = "all") %>%
  theme_booktabs() %>%
  align(j = c(2, 4, 5), align = "right", part = "all") %>%
  align(i = 1, align = "center", part = "header") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  border(border.bottom = fp_border(style = "none"), part = "footer") %>%
  autofit()


## ---- Depression: ----

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
  mutate(depr_weights = weights / mean(weights))


## ----depression-descriptives ----
depression_props <- depression %>% {
  full_join(
    tabyl(., Pre) %>%
      adorn_pct_formatting() %>%
      rename(Depression = Pre),
    tabyl(., Post) %>%
      adorn_pct_formatting() %>%
      rename(Depression = Post),
    by = "Depression",
    suffix = c("_pre", "_post")
  )
} %>%
  as_tibble()

depression_prop_pre <- depression_props %>%
  filter(Depression == "Yes") %>%
  pull(percent_pre)
depression_prop_post <- depression_props %>%
  filter(Depression == "Yes") %>%
  pull(percent_post)


## ----depression-association----
depr_contingency_complete <- depression %>%
  mutate_at(vars(Pre, Post), fct_explicit_na) %>%
  tabyl(Pre, Post)

depr_contingency_valid <- depression %>%
  filter_at(vars(Pre, Post), ~!is.na(.)) %>%
  tabyl(Pre, Post)

chisq_depr <- depr_contingency_valid %>% ## Not used
  chisq.test(simulate.p.value = TRUE) %>%
  tidy() %>%
  print_chisq_test()

mcnemar_depr <- depr_contingency_valid %>%
  select(-Pre) %>%
  as.matrix() %>%
  mcnemar.test() %>%
  tidy() %>%
  print_chisq_test()


## ----depression-sankey----
depr_count <- depression %>% group_by_all() %>% count()

depr_count %$% plot_ly(
  type        = "sankey",
  orientation = "h",

  node        = list(
    label     = c(Pre %>% levels(), Post %>% levels()),
    color     = c("blue", "red", "blue", "red")
  ),

  link = list(
    source = as.integer(Pre)  - 1,
    target = as.integer(Post) - 1 + nlevels(Pre),
    value =  n
  )
)


## ----depression-prevalence-population----
depression_prevalence_est <- depression %>% {
    full_join(
      count(., Pre,  wt = depr_weights) %>% rename(Depression = Pre,  Pre  = n),
      count(., Post, wt = depr_weights) %>% rename(Depression = Post, Post = n),
      by = "Depression"
    )
  } %>%
  mutate(across(-Depression, ~. / sum(.))) %>%
  adorn_pct_formatting()

depression_prev_pre <- depression_prevalence_est %>%
  filter(Depression == "Yes") %>%
  pull(Pre)
depression_prev_post <- depression_prevalence_est %>%
  filter(Depression == "Yes") %>%
  pull(Post)


## ----depression-prevalence-estimates----

# This version estimates the prevalence in the population with the whole
#   sample size in the Pre measure.

depression_pre <- dataset_outcomes_compare_excluded %>%
  select(number_id, Pre = depression_pre, sex, age_pre, weights) %>%
  mutate(
    Age = (age_pre < 50) %>%
      factor(levels = c(TRUE, FALSE), labels = c("18 - 49", "50 +"))
  )

depression_prevalence_pre <- depression_pre %>%
  {
    bind_rows(
      bind_cols(
        tibble(Variable = "Total", Group = ""),
        estimate_prevalence(., Pre, weights, "Yes")
      ),
      group_by(., Group = sex) %>% estimate_prevalence(Pre, weights, "Yes") %>%
        add_column(Variable = "Sex"),
      group_by(., Group = Age) %>% estimate_prevalence(Pre, weights, "Yes") %>%
        add_column(Variable = "Age")
    )
  } %>%
  adorn_pct_formatting(digits = 2, ... = c(Prevalence, Std_err))

depression_prevalence_post <- depression %>%
  mutate(
    Age = (age_post < 0) %>% # Because it is centered at 50
      factor(levels = c(TRUE, FALSE), labels = c("18 - 49", "50 +"))
  ) %>% 
  {
    bind_rows(
      bind_cols(
        tibble(Variable = "Total", Group = ""),
        estimate_prevalence(., Post, weights, "Yes")
      ),
      group_by(., Group = sex) %>% estimate_prevalence(Post, weights, "Yes") %>%
        add_column(Variable = "Sex"),
      group_by(., Group = Age) %>% estimate_prevalence(Post, weights, "Yes") %>%
        add_column(Variable = "Age")
    )
  } %>%
  adorn_pct_formatting(digits = 2, ... = c(Prevalence, Std_err))


## ----depression-association-population----
depression_contingency_est <- depression %>%
  count(Pre, Post, wt = depr_weights) %>%
  spread(Post, n)

chisq_depr_est <- depression_contingency_est %>%
  select(-Pre) %>%
  chisq.test(simulate.p.value = TRUE) %>%
  tidy() %>%
  print_chisq_test() # Hay asociación en la población

mcnemar_depr_est <- depression_contingency_est %>%
  select(-Pre) %>%
  as.matrix() %>%
  mcnemar.test(correct = FALSE) %>%
  tidy() %>%
  print_chisq_test() # No hay cambio en la tasa de prevalencia

depression_contingency_est <- depression_contingency_est %>%
  mutate(across(where(is.double), `/`, sum(c_across(No:Yes))))


# Power test:
p01 <- depression_contingency_est %>% filter(Pre == "No")  %>% pull(Yes)
p10 <- depression_contingency_est %>% filter(Pre == "Yes") %>% pull(No)

power_test_depr <- power_mcnemar_test(
  n    = depression %>% nrow(),
  paid = p10,
  psi  = p01 / p10
) %>%
  tidy() %>%
  pull(power) %>%
  format_pvalues()


## ----depression-alluvial-population----
depression_alluvial_plot <- depression_contingency_est %>%
  gather(Post, Frequency, -Pre) %>%
  mutate(Change = (Post != Pre) %>% factor(labels = c("No", "Yes"))) %>%
  to_lodes_form(Pre:Post, key = "Measure", value = "Depression") %>%
  mutate(
    freq_label = if_else(
      Measure == "Pre",
      Frequency %>% percent(.1) %>% enclose("("), "")
  ) %>%
  full_join(
    depression_prevalence_est %>% gather(Measure, Prevalence, Pre:Post),
    by = c("Measure", "Depression")
  ) %>%
  ggplot(
    aes(x = Measure, y = Frequency, alluvium = alluvium, stratum = Depression)
  ) +
  geom_flow(aes(fill = Depression)) +
  geom_stratum(aes(fill = Depression), color = NA) +
  geom_text(stat = "stratum", aes(label = Prevalence), size = 4.5) +
  scale_x_discrete(
    limits = c("Pre", "Post"),
    expand = expansion(),
    name   = NULL
  ) +
  scale_y_continuous(breaks = NULL, name = NULL) +
  scale_fill_brewer(
    palette    = "Set1",
    direction  = -1
  ) +
  geom_text(aes(label = freq_label), stat = "flow", size = 3, nudge_x = .3) +
  ggtitle(
    CAPTION_ALLUVIAL_DEPRESSION,
    subtitle = glue("N = {depression %>% nrow()}")
  ) +
  theme(
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    plot.title         = element_text(hjust = .5),
    plot.subtitle      = element_text(hjust = .5)
  )


## ----tidy-depression----
depression_tidy <- depression %>%
  pivot_longer(
    cols      = Pre:Post,
    names_to  = "Measure",
    values_to = "Depression",
    names_transform = list(Measure = factor)
  ) %>%
  mutate(Measure = Measure %>% fct_rev) %>%
  full_join(dataset_outcomes_varying_std, by = c("ID_CONTACTO", "Measure"))


## ----optimizer-configuration-depression----
# All of them are essentially equal for the nested model; for the general one,
#   `nloptwrap.NLOPT_LN_NELDERMEAD` is still the best.
#
# FINALLY: We opt for the optimizer `nloptwrap.NLOPT_LN_NELDERMEAD`;
# however, for the general model in `pain_physical`, `bobyqa` needs to be used.
glmer_control <- function(predictor = "") {

  glmerControl(
    optimizer = "nloptwrap",
    optCtrl   = list(
      algorithm = "NLOPT_LN_NELDERMEAD",
      ftol_abs  = 1e-8,
      xtol_abs  = 1e-8
    )
  )
}
## TODO: Did not check the best optimizer for "univariate" predictor models


## ----univariate-tests-depression----
warning_func <- function(predictor, model = c("nested", "general")) {

  model <- match.arg(model)

  function(w) {

    warning_msg <- gettext(w)

    if (
      warning_msg != paste(
        "simpleWarning in eval(family$initialize, rho):",
        "non-integer #successes in a binomial glm!\n"
      )
    ) {
      cat(predictor, model, warning_msg, sep = "\n", fill = TRUE)
    }
  }
}

univariate_tests_depression <- depr_preds %>% map_df(
  ~{
    withCallingHandlers(
      nested_fit <- glmer(
        formula = glue("Depression ~ Measure + {.x} + (1|ID_CONTACTO)") %>%
          as.formula(),
        data    = depression_tidy,
        family  = binomial,
        weights = weights,
        control = glmer_control()
      ),
      warning = warning_func(.x, "nested")
    )
    withCallingHandlers(
      general_fit <- glmer(
        formula = glue("Depression ~ Measure * {.x} + (1|ID_CONTACTO)") %>%
          as.formula(),
        data    = depression_tidy,
        family  = binomial,
        weights = weights,
        control = glmer_control(.x)
      ),
      warning = warning_func(.x, "general")
    )

    ## Supress "In tidy.anova(.) :
    ##            The following column names in ANOVA output were not
    ##            recognized or transformed: npar"
    suppressWarnings(
      anova(general_fit, nested_fit) %>%
        tidy() %>%
        select(term, statistic:p.value) %>%
        slice(2) %>%
        mutate(term = .x)
    )
  }
)

save(univariate_tests_depression, file = "univariate_tests_depression.RData")


## ----predictor-selection-depression----
## Bonferroni correction is applied in order to try
##   to avoid complete separation.
selected_preds_depression <- univariate_tests_depression %>%
  # filter(p.value < SIG_LEVEL) %>% # No correction
  # filter(p.value < SIG_LEVEL / n()) %>%   # Bonferroni correction
  arrange(desc(p.value)) %>%              # Holm-Bonferroni correction
  mutate(
    sig  = p.value < SIG_LEVEL / row_number(),
    keep = p.value < min(p.value[!sig])
  ) %>%
  filter(keep) %>%
  pull(term)

# To have the predictors in the usual order:
selected_preds_depression <- depr_preds %>% intersect(selected_preds_depression)


## ----ordinal-linearity-tests-depression----

selected_ord_depression <- selected_preds_depression %>%
  intersect(ordinal_preds)

ord_linearity_tests_depression <- selected_ord_depression %>% map_df(
  ~{
    aux_dataset <- depression_tidy %>%
      mutate(linear = !!sym(.x) %>% as.integer())

    nested_fit <- glmer(
      formula = glue("Depression ~ Measure * linear + (1|ID_CONTACTO)") %>%
        as.formula(),
      data    = aux_dataset,
      family  = binomial,
      weights = weights,
      control = glmer_control()
    )
    general_fit <- glmer(
      formula = glue("Depression ~ Measure * {.x} + (1|ID_CONTACTO)") %>%
        as.formula(),
      data    = aux_dataset,
      family  = binomial,
      weights = weights,
      control = glmer_control(.x)
    )

    ## Supress "In tidy.anova(.) :
    ##            The following column names in ANOVA output were not
    ##            recognized or transformed: npar"
    suppressWarnings(
      anova(general_fit, nested_fit) %>%
        tidy() %>%
        select(term, statistic:p.value) %>%
        slice(2) %>%
        mutate(term = .x)
    )
  }
)

save(
  ord_linearity_tests_depression,
  file = "ord_linearity_tests_depression.RData"
)


## ----ordinal-linearity-tests-processing-depression----
# Bonferroni correction is applied.
linear_ord_preds_depression <- ord_linearity_tests_depression %>%
  filter(p.value >= (SIG_LEVEL / n())) %>%
  pull(term)

# Trasnform to integer the linear predictors, in order to avoid the use of
#   high-order polynomial terms.
#   The reference (lower) category is set to 0 by substracting 1 from the
#   integer resulting from the transformed category.
depression_tidy <- depression_tidy %>%
  mutate_at(c(linear_ord_preds_sleep_time, "severity"), as.integer) %>%
  mutate_at(c(linear_ord_preds_sleep_time, "severity"), `-`, 1L)
# (`severity` fails when used as ordinal due to complete separation)


## ----stepwise-mixed-effects-depression----
best_fit_depression <- function(formula, weights = NULL) {
  
  # optimizers to try initially
  optimizers <- c("init", "bobyqa", "Nelder_Mead", "nlminbwrap", "nmkbw")

  fit_init <- NULL
  optimizer <- NULL

  while (is_non_empty(optimizers)) {

    tryCatch(
      {
        fit_init <- if (optimizers[1] == "init") {

          glmer(
            formula = formula,
            data    = depression_fit_dataset,
            family  = binomial,
            weights = weights
          )

        } else {

          glmer(
            formula = formula,
            data    = depression_fit_dataset,
            family  = binomial,
            weights = weights,
            control = glmerControl(optimizer = optimizers[1])
          )
        }
        break
      },
      error = function(e) {

        optimizers <- optimizers[-1]
      }
    )
  }

  if (is_null(fit_init)) stop("All initial optimizers failed.")

  fit_all <- fit_init %>% allFit()

  LL_diffs <- fit_all %>%
    summary() %>%
    extract2("llik") %>%
    enframe("optimizer", "LL") %>%
    bind_rows(
      tibble(
        optimizer = glue("initial_{optimizers[1]}"),
        LL        = fit_init %>% logLik() %>% as.numeric()
      )
    ) %>%
    mutate(LL_diff = max(LL) - LL) %>%
    arrange(LL_diff)

  which_best <- LL_diffs %>% slice(which.max(LL)) %>% pull(optimizer)

  fit_best <- if (which_best == glue("initial_{optimizers[1]}")) {

    fit_init

  } else {

    fit_all[[which_best]]
  }

  attr(fit_best, "LL_diffs") <- LL_diffs

  fit_best
}

## Setpwise algorithm:

iterate_fit_depression <- function(init_fit, candidate_terms) {

  current_fit <- init_fit

  history <- tibble(
    iteration        = 0L,
    chosen_predictor = NA_character_,
    candidates_LL    = list(current_fit %>% attr("LL_diff")),
    candidates_AIC   = list(current_fit %>% AIC()),
    model_LR         = list(tibble()),
    comp_optims_LL   = list(tibble()),
    n_subjects       = current_fit %>% summary() %>% extract2("ngrps"),
    n_obs            = current_fit@frame %>% nrow()
  )

  repeat{

    cat(
      "\nIteration ", history %>% slice_tail() %>% pull(iteration) + 1, ":\n\n",
      sep = ""
    )

    current_formula <- current_fit %>%
      formula() %>%
      as.character() %>%
      extract(c(2, 1, 3)) %>% glue_collapse(sep = " ")
    current_terms   <- current_formula %>%
      as.formula() %>%
      terms() %>%
      attr("term.labels") %>%
      str_subset("^Measure:") %>%
      str_remove("^Measure:")

    candidate_terms <- candidate_terms %>% setdiff(current_terms)

    if (is_empty(candidate_terms)) break

    candidate_models <- candidate_terms %>% map(
      ~{
        cat("\nFitting model for candidate predictor `", .x, "`:\n\n", sep = "")

        depression_fit_dataset <<- depression_tidy %>%
          drop_na(Depression, all_of(c(current_terms, .x))) %>%
          correct_weights(ID_CONTACTO)

        formula_step <- glue("{current_formula} + Measure : {.x}")

        depression_step_fit <- best_fit_depression(
          formula = formula_step %>% as.formula(),
          weights = weights
        )
      }
    ) %>%
      set_names(candidate_terms)

    candidates_opt_fits <- candidate_models %>%
      map_df(attr, "LL_diffs", .id = "candidate") %>%
      group_by(candidate)
    candidates_AICs     <- candidate_models %>% map_dbl(AIC)
    chosen_predictor    <- candidates_AICs %>% which.min() %>% names()

    chosen_fit <- candidate_models[[chosen_predictor]]

    cat(
      "\nNew candidate predictor chosen is `", chosen_predictor, "`\n\n",
      sep = ""
    )

    depression_fit_dataset <<- depression_tidy %>%
      drop_na(Depression, all_of(c(current_terms, chosen_predictor))) %>%
      correct_weights(ID_CONTACTO)
    current_fit_comp <- best_fit_depression(
      formula = current_formula %>% as.formula(),
      weights = weights
    )

    model_lr <- chosen_fit %>%
      anova(current_fit_comp) %>%
      tidy() %>%
      slice(2) %>%
      select(statistic:p.value)

    history <- history %>% bind_rows(
      tibble(
        iteration        = nrow(.),
        chosen_predictor = chosen_predictor,
        candidates_LL    = list(candidates_opt_fits),
        candidates_AIC   = list(candidates_AICs),
        model_LR         = list(model_lr),
        comp_optims_LL   = list(current_fit_comp %>% attr("LL_diff")),
        n_subjects       = chosen_fit %>% summary() %>% extract2("ngrps"),
        n_obs            = chosen_fit@frame %>% nrow()
      )
    )

    if (model_lr %>% pull() > SIG_LEVEL) break

    cat(
      "\nModel with predictor `", chosen_predictor,
      "` fits significantly better than previous iteration model; ",
      "predictor added.\n\n",
      sep = ""
    )

    current_fit <- chosen_fit
  }

  cat("Stepwise regression complete.\n\n")

  attr(current_fit, "history") <- history

  current_fit
}


# Initialization:

depression_fit_dataset <- depression_tidy %>% ## TODO: Needed this?
  drop_na(Depression, Measure) %>%
  correct_weights(ID_CONTACTO)


formula_init <- "Depression ~ Measure + (1|ID_CONTACTO)"


# Initial fit with only the `Measure` variable:
depression_init_fit <- best_fit_depression(
  formula = formula_init %>% as.formula(),
  weights = weights
)

# Iterative addition of predictors:
depression_final_fit <- iterate_fit_depression(
  depression_init_fit,
  selected_preds_depression
)

# Check differences in convergence among optimizers
depression_final_fit %>% attr("LL_diff") # Difference of 2.03e-11, evidence of
                                         #   convergence found.

save(depression_final_fit, file = "depression_final_fit.RData")


## ----mixed-effects-final-depression-model-Stata----

depression_final_terms <- depression_final_fit %>%
  terms() %>%
  attr("term.labels") %>%
  extract(-1) %>%
  str_remove("Measure:")

formula_final <- paste0(
  "Depression ~ Measure * (",
  glue_collapse(depression_final_terms, sep = " + "),
  ") + (1|ID_CONTACTO)"
)

depression_fit_final_dataset <- depression_tidy %>%
  drop_na(Depression, all_of(depression_final_terms)) %>%
  mutate(
    mweight = 1,
    Depression = (Depression == "Yes") %>% as.numeric()
  ) %>%
  as.data.frame()


## Does not work:
# depression_fit <- mix(
#   formula  = formula_final %>% as.formula(),
#   data     = depression_fit_final_dataset,
#   weights  = c("mweight", "weights"),
#   cWeights = TRUE,
#   family   = binomial(link = "logit")
# )
# 
# save(depression_fit, file = "depression_fit.RData")

depression_final_terms_stata <- depression_final_terms %>% {
  
  select_at(depression_fit_final_dataset, .) %>%
    map_lgl(is.numeric) %>%
    { paste0(if_else(., 'c.', ''), names(.)) }
}
depression_predictors_stata <- "Measure##" %>%
  paste0(depression_final_terms_stata, collapse = ' ')

depression_syntax <- paste(
  "melogit Depression",
  depression_predictors_stata,
  "[pweight=mweight] || ID_CONTACTO:, pweight(weights) or"
)

stata_out_depression <- capture.output(
  stata(
    src     = depression_syntax,
    data.in = depression_fit_final_dataset
  )
)


## ----mixed-effects-depression-model-processing----

nobs    <- depression_fit_final_dataset %>% nrow()
ngroups <- depression_final_fit@Gp[2]

depression_coefficients <- stata_out_depression %>% get_tables() %>% extract2(1)


## ----conclusions-pre-computations-depression----

depression_change_coefs <- depression_coefficients %>%
  filter(Depression %>% str_detect("^Measure#"))

depression_change_coefs_sig <- depression_change_coefs %>%
  filter(`P>|z|` < SIG_LEVEL / n()) # Bilateral, Bonferroni-corrected
# arrange(desc(`P>|z|`)) %>%              # Holm-Bonferroni correction
# mutate(
#   sig  = `P>|z|` < SIG_LEVEL / row_number(),
#   keep = `P>|z|` < min(`Pr(>|z|)`[!sig])
# ) %>%
# filter(keep)

depression_change_sig_terms <- depression_change_coefs_sig %>%
  pull(Depression) %>%
  enclose('`') %>%
  glue_collapse(sep = ', ', last = ', and  ')


# Measure main effect (conditional for men):
main_change_coef <- depression_coefficients %>%
  filter(Depression == "Measure", level == "Post")

main_pval <- main_change_coef %>% pull(`P>|z|`) %>% print_pvalue()
main_coef <- main_change_coef %>% pull(`Odds Ratio`)


# Health abbreviated (Pre):
health_abb_pre_term <- depression_change_coefs %>%
  filter(Depression %>% str_detect("health_abb~e$"))

health_abb_pre_pval <- health_abb_pre_term %>% pull(`P>|z|`) %>% print_pvalue()
health_abb_pre_coef <- health_abb_pre_term %>% pull(`Odds Ratio`)# OR per stddev
health_abb_pre_abs  <- health_abb_pre_coef %>% number(1e-3)
health_abb_pre_dir  <- health_abb_pre_coef %>%
  log() %>%
  coef_dir(form = "indet")
health_abb_pre_incr <- (health_abb_pre_coef - 1) %>% percent(.1)



# UCLA loneliness (Pre):
ucla_lon_pre_term <- depression_change_coefs %>%
  filter(Depression %>% str_detect("ucla_lon_pre$"))

ucla_lon_pre_pval <- ucla_lon_pre_term %>% pull(`P>|z|`) %>% print_pvalue()
ucla_lon_pre_coef <- ucla_lon_pre_term %>% pull(`Odds Ratio`)
ucla_lon_pre_abs  <- ucla_lon_pre_coef %>% number(1e-3)
ucla_lon_pre_dir  <- ucla_lon_pre_coef %>% log() %>% coef_dir(form = "indet")
ucla_lon_pre_decr <- (1 - ucla_lon_pre_coef) %>% abs() %>% percent(.1)


ucla_lon_pre_scaling <- dataset_outcomes_std %>%
  pull(ucla_lon_pre) %>%
  attr("scaled:scale")

ucla_lon_pre_neg_OR <- (
  1 -
    exp(log(ucla_lon_pre_coef) / ucla_lon_pre_scaling * 100 / 6)
) %>%
  percent(.1)
 
 
# # UCLA loneliness (Post):
# ucla_lon_post_term <- depression_change_coefs %>%
#   filter(Depression %>% str_detect("ucla_lon_p~t$"))
# 
# ucla_lon_post_pval <- ucla_lon_post_term %>% pull(`P>|z|`) %>% print_pvalue()
# ucla_lon_post_coef <- ucla_lon_post_term %>% pull(`Odds Ratio`)
# 
# ucla_lon_post_scaling <- dataset_outcomes_std %>%
#   pull(ucla_lon_post) %>%
#   attr("scaled:scale")
# 
# ucla_lon_post_OR <- (
#   exp(log(ucla_lon_post_coef) / ucla_lon_post_scaling * 100 / 6) -
#     1
# ) %>%
#   percent(.1)


## ----depression-selection-no-pre----

depression_neg_pre <- depression %>% filter(Pre == "No")

all_preds_new <- all_preds_new %>% str_subset("^depression_", negate = TRUE)

# Survey design, to use with `svyglm` calls:
depression_np_design <- svydesign(
  ids     = ~ID_CONTACTO,
  data    = depression_neg_pre,
  weights = depression_neg_pre %>% pull(weights)
)


## ----depression-no-pre-contingency----
depression_no_pre_contingency <- depression_neg_pre %>%
  select(Post, all_of(all_preds_new)) %>%
  frequencies_table(.segmentation = Post, missing = FALSE)

## ----univariate-tests-new-depression----
univariate_tests_depression_new <- all_preds_new %>% map_df(
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

## ----predictor-selection-new-depression----

## Bonferroni correction is applied in order to try
##   to avoid complete separation. # TODO: Check whether this comment is true
selected_preds_depression_new <- univariate_tests_depression_new %>%
  filter(p.value < SIG_LEVEL) %>% # No correction
  # filter(p.value < SIG_LEVEL / n()) %>%   # Bonferroni correction
  # arrange(desc(p.value)) %>%              # Holm-Bonferroni correction
  # mutate(
  #   sig  = p.value < SIG_LEVEL / row_number(),
  #   keep = p.value < min(p.value[!sig])
  # ) %>%
  # filter(keep) %>%
  pull(term) %>%
  union(FIXED_PREDICTORS_NEW)

# To have the predictors in the usual order:
selected_preds_depression_new <- all_preds_new %>%
  intersect(selected_preds_depression_new)


## ----ordinal-linearity-tests-new-depression----

selected_ord_depression_new <- selected_preds_depression_new %>%
  intersect(ordinal_preds_new)

ord_linearity_tests_depression_new <- selected_ord_depression_new %>% map_df(
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

linear_ord_preds_depression_new <- ord_linearity_tests_depression_new %>%
  filter(p.value >= (SIG_LEVEL / n())) %>%
  pull(term)
nonlinear_ord_preds_depression_new <- selected_ord_depression_new %>%
  setdiff(linear_ord_preds_depression_new)

# Trasnform to integer the linear predictors, in order to avoid the use of
#   high-order polynomial terms.
#   The reference (lower) category is set to 0 by substracting 1 from the
#   integer resulting from the transformed category.
depression_neg_pre <- depression_neg_pre %>%
  mutate_at(linear_ord_preds_depression_new, as.integer) %>%
  mutate_at(linear_ord_preds_depression_new, `-`, 1L)

linear_ord_preds_depression_labels_new_out <- var_properties %>%
  pull(labels_comp) %>%
  extract(linear_ord_preds_depression_new) %>%
  enclose("*") %>%
  glue_collapse(" and ")


## ----stepwise-glm-new-depression----

# Initialization:

## Scope (minimum and maximum model):
scope <- list(
  lower = "Post ~" %>% paste(paste0(FIXED_PREDICTORS_NEW, collapse = " + ")) %>%
    as.formula(),
  upper = "Post ~" %>% paste(
    glue_collapse(selected_preds_depression_new, sep = " + ")
  ) %>% as.formula()
)

## Dataset without missing values for the iterations:
depression_neg_pre_fit <- depression_neg_pre %>%
  select(ID_CONTACTO, Post, weights, all_of(selected_preds_depression_new)) %>%
  drop_na()

depression_np_design <- svydesign( # Redefine the design
  ids     = ~ID_CONTACTO,
  data    = depression_neg_pre_fit,
  weights = depression_neg_pre_fit %>% pull(weights)
)

## Initial fit with all the predictors:
depression_init_fit_new <- svyglm(
  formula = scope$upper,
  design  = depression_np_design,
  family  = quasibinomial
)

# Iterative dropping of predictors:
depression_final_fit_new <- depression_init_fit_new %>%
  step(scope = scope, direction = "backward")

# # Same backward step procedure as implemented by stats::step, but
# #   allowing for different sample sizes in reduced models due to missing data
# #   (It also corrects an apparent issue with the AIC computation).
# sleep_final_fit <- step_backwards_svyglm(
#   depression_init_fit_new,
#   min_model = scope$lower,
#   alpha_fixed = SIG_LEVEL,
#   bonf_correct = TRUE
# )

# This gives the result of the iterations, but the dataset has dropped cases
#   that may be re-used in the final model, so it is fit again with the
#   complete dataset


# Final fit:
depression_np_design <- svydesign( # Redefine the design to include all values
  ids     = ~ID_CONTACTO,
  data    = depression_neg_pre %>% mutate(
    across(all_of(nonlinear_ord_preds_depression_new), factor, ordered = FALSE)
  ),
  weights = depression_neg_pre %>% pull(weights)
)

depression_fit_new <- svyglm(
  formula = depression_final_fit_new %>% formula(),
  design  = depression_np_design,
  family  = quasibinomial
) # No gain in participants nevertheless


# Summary:
depression_summ_fit_new     <- depression_fit_new %>% summary()
depression_coefficients_new <- depression_fit_new %>% tidy()
nobs                        <- depression_fit_new %>% nobs()


## ----conclusions-pre-computations-depression-new----

depression_coefs_sig_new <- depression_coefficients_new %>%
  filter(term != "(Intercept)") %>% # Do not take it into account for correction
  filter(p.value < SIG_LEVEL / n()) # Bilateral, Bonferroni-corrected
# arrange(desc(p.value)) %>%              # Holm-Bonferroni correction
# mutate(
#   sig  = p.value < SIG_LEVEL / row_number(),
#   keep = p.value < min(p.value[!sig])
# ) %>%
# filter(keep)

depression_sig_terms_new <- depression_coefs_sig_new %>%
  pull(term) %>%
  enclose('`') %>%
  glue_collapse(sep = ', ', last = ', and  ')

depression_sig_terms_labels_new <- depression_coefs_sig_new %>%
  pull(term) %>%
  extract(labels_abbr, .) %>%
  enclose("*")

depr_ucla_lon_post_lab   <- depression_sig_terms_labels_new[1] %>%
  paste("*(Post)*")
depr_resilience_post_lab <- depression_sig_terms_labels_new[2] %>%
  paste("*(Post)*")
depr_age_lab             <- depression_sig_terms_labels_new[3]

depression_sig_terms_labels_new <- depression_sig_terms_labels_new[-3] %>%
  glue_collapse(last = " and ")


# Age:
depr_age_term <- depression_coefs_sig_new %>%
  filter(term %>% str_detect("age"))
depr_age_test <- depr_age_term %>% print_z_test()
depr_age_pval <- depr_age_term %>% pull() %>% print_pvalue()
depr_age_coef <- depr_age_term %>% pull(estimate)
depr_age_OR   <- depr_age_coef %>% exp() %>% number(1e-3)
depr_age_dir  <- depr_age_coef %>% coef_dir(form = "verb")
depr_age_decr <- (depr_age_coef %>% exp() - 1) %>%
  abs() %>%
  percent(.1)


# UCLA Loneliness scale (post):
depr_ucla_lon_post_term <- depression_coefs_sig_new %>%
  filter(term %>% str_detect("ucla_lon_post$"))
depr_ucla_lon_post_test <- depr_ucla_lon_post_term %>% print_z_test()
depr_ucla_lon_post_pval <- depr_ucla_lon_post_term %>% pull() %>% print_pvalue()
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
depr_resilience_post_term <- depression_coefs_sig_new %>%
  filter(term %>% str_detect("resilience_post$"))
depr_resilience_post_test <- depr_resilience_post_term %>% print_z_test()
depr_resilience_post_pval <- depr_resilience_post_term %>%
  pull() %>%
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


## This category is no longer a significant predictor. Thus it does not yield
##   complete separation.

# # Extreme physical pain (post):
# extreme_pain_term <- depression_coefs_sig_new %>%
#   filter(term %>% str_detect("^pain_physical"))
# 
# extreme_pain_pval <- extreme_pain_term %>% pull() %>% print_pvalue()
# extreme_pain_coef <- extreme_pain_term %>% pull(estimate)
# extreme_pain_OR   <- extreme_pain_coef %>%
#   exp() %>%
#   format(scientific = TRUE, digits = 3) %>%
#   str_replace("e\\+", " $\\\\times$ 10\\^") %>%
#   paste0("^")
# extreme_pain_dir  <- extreme_pain_coef %>% coef_dir(form = "indet")
# extreme_pain_incr <- (extreme_pain_coef %>% exp() - 1) %>%
#   abs() %>%
#   format(scientific = TRUE, digits = 3) %>%
#   str_replace("e\\+", " $\\\\times$ 10\\^") %>%
#   paste0("^")
# 
# 
# # Contingency table for predictors that yield complete separation:
# complete_sep_terms_depr <- c("pain_physical")
# 
# contingency_complete_separation_depression_new <- depression_neg_pre_fit %>%
#   select(all_of(complete_sep_terms_depr), Post) %>%
#   frequencies_table(.segmentation = Post)


## ----drop-complete-separation-terms-depression-new----

# selected_preds_depression_new <- selected_preds_depression_new %>%
#   setdiff(complete_sep_terms_depr)


## ----conclusions-pre-computations-depression-new-bis----

# depression_coefs_sig_new <- depression_coefficients_new %>%
#   filter(term != "(Intercept)") %>% # Do not take it into account for correction
#   filter(p.value < SIG_LEVEL / n()) # Bilateral, Bonferroni-corrected
# # arrange(desc(p.value)) %>%              # Holm-Bonferroni correction
# # mutate(
# #   sig  = p.value < SIG_LEVEL / row_number(),
# #   keep = p.value < min(p.value[!sig])
# # ) %>%
# # filter(keep)
# 
# depression_sig_terms_new <- depression_coefs_sig_new %>%
#   pull(term) %>%
#   enclose('`') %>%
#   glue_collapse(sep = ', ', last = ', and  ')
# 
# 
# # UCLA Loneliness scale (post):
# ucla_lon_post_term <- depression_coefs_sig_new %>%
#   filter(term %>% str_detect("ucla_lon_post$"))
# 
# ucla_lon_post_pval <- ucla_lon_post_term %>% pull() %>% print_pvalue()
# ucla_lon_post_coef <- ucla_lon_post_term %>% pull(estimate)
# ucla_lon_post_OR   <- ucla_lon_post_coef %>% exp() %>% number(1e-3)
# ucla_lon_post_dir  <- ucla_lon_post_coef %>% coef_dir(form = "indet")
# ucla_lon_post_incr <- (ucla_lon_post_coef %>% exp() - 1) %>%
#   abs() %>%
#   percent(.1)
# 
# ucla_lon_post_scaling <- dataset_outcomes_std %>%
#   pull(ucla_lon_post) %>%
#   attr("scaled:scale")
# 
# ucla_lon_post_coef_abs_score <- (
#   ucla_lon_post_coef / ucla_lon_post_scaling * 100 / 6
# ) %>%
#   exp() %>%
#   subtract(1) %>%
#   percent(1e-1)
# 
# 
# # Resilience scale (post):
# resilience_post_term <- depression_coefs_sig_new %>%
#   filter(term %>% str_detect("resilience_post$"))
# 
# resilience_post_pval <- resilience_post_term %>% pull() %>% print_pvalue()
# resilience_post_coef <- resilience_post_term %>% pull(estimate)
# resilience_post_OR   <- resilience_post_coef %>% exp() %>% number(1e-3)
# resilience_post_dir  <- resilience_post_coef %>% coef_dir(form = "indet")
# resilience_post_decr <- (resilience_post_coef %>% exp() - 1) %>%
#   abs() %>%
#   percent(.1)
# 
# resilience_post_scaling <- dataset_outcomes_std %>%
#   pull(resilience_post) %>%
#   attr("scaled:scale")
# 
# resilience_post_coef_abs_score <- (
#   resilience_post_coef / resilience_post_scaling
# ) %>%
#   exp() %>%
#   subtract(1) %>%
#   abs() %>%
#   percent(1e-1)


## ----depression-coefficients-new----

depression_coefficients_new <- depression_coefficients_new %>%
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

depression_coefficients_new_table <- depression_coefficients_new %>%
  select(Term, OR, `(95% CI)`, z = statistic, `*p* value`) %>%
  mutate(across(where(is.numeric), number, 1e-3)) %>% 
  flextable() %>%
  add_footer(Term = REG_MODEL_FOOTER) %>%
  merge_at(part = "footer") %>%
  colformat_md(part = "all") %>%
  theme_booktabs() %>%
  align(j = c(2, 4, 5), align = "right", part = "all") %>%
  align(i = 1, align = "center", part = "header") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  border(border.bottom = fp_border(style = "none"), part = "footer") %>%
  autofit()


## ---- Intro (final): ---------------------------------------------------------

## ----issues-pre-computations----
sm23_label <- dataset_outcomes %>% pull(SM25) %>% attr("label")
sm24_label <- dataset_outcomes %>% pull(SM24) %>% attr("label")


## ----restore-wd----
# Opposite operation of chunk `set-equivalent-wd`.
#   the evaluation directory accordingly:
setwd("../..")


## ----session-info----
devtools::session_info()
