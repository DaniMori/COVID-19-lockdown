# ==============================================================================
# 
# FILE NAME:   Lang_ES.R
# DESCRIPTION: Language verbatims (en)
# 
# AUTHOR:      Mori (danivmorillo@gmail.com)
# 
# DATE:        22/09/2020
# 
# ==============================================================================


## ---- CONSTANTS: -------------------------------------------------------------

# Variable names: ----

VAR_SLEEP_HOURS <- "Hours of sleep"


# Value labels: ----

# BOOL_FALSE <- "No"
# BOOL_TRUE  <- "Yes"


# Table headers: ----


# Figure titles: ----


# Tables: ----

## Table footers: ----

DESCRIPTIVES_FOOTER <-
  "Note. n = number of participants; sd = standard deviation."
PREVALENCE_FOOTER   <- "Note. std. dev. = standard deviation."

DEPRESSION_MODEL_FOOTER <- paste(
  "Note. OR = Odds ratio; CI = Confidence interval;",
  "Resilience = Brief Resilience Scale;",
  "Social support = OSLO3 Social Support Scale;",
  "Loneliness = UCLA Loneliness Scale;",
  "Disability = 12-item WHO Disability Assessment Schedule",
  "Physical activity = GPAQ-2 abbreviated."
)

SUICIDAL_MODEL_FOOTER <- paste(
  "Note. OR = Odds ratio; CI = Confidence interval;",
  "Resilience = Brief Resilience Scale;",
  "Social support = OSLO3 Social Support Scale;",
  "Disability = 12-item WHO Disability Assessment Schedule",
  "Physical activity = GPAQ-2 abbreviated."
)


## Descriptives: ----

CAPTION_MISSING_RATES <-
  "Missing response rate in variables considered for analysis"

CAPTION_DESC_QUANTITATIVE_PREDICTORS <-
  "Descriptive statistics of the quantitative predictors"

CAPTION_DESC_CATEGORICAL_PREDICTORS <-
  "Category frequencies of the categorical predictors"

CAPTION_DESC_VARIABLES <- paste(
  "Sociodemographic and health characteristics",
  "before and after the confinement"
)

FOOTNOTE_VARS_SCALE <- "These variables are measured in a 0-100 scale."

FOOTNOTE_DESCRIPTIVE_P_VALUES <- paste(
  "p-values correspond to a paired-sample T-test",
  "for the quantitative variables,",
  "and a McNemar’s test of symmetry for the categorical ones."
)

FOOTNOTE_LINEAR_COVARIATE <- "Included as linear covariate."


## Prevalence: ----

CAPTION_PREVALENCES <- paste(
  "Prevalence rate estimates in the Pre and Post measures",
  "of depression and suicidal ideation,",
  "for the population, and disaggregated by sex and group age."
)


## Depression: ----

CAPTION_DESC_DEPRESSION_IDEATION <-
  "Descriptive statistics of the reported depression"

CAPTION_PREVALENCE_DEPRESSION_IDEATION <-
  "Estimated population prevalence of the reported depression"

CAPTION_DEPRESSION_IDEATION_CONTINGENCY <-
  "Contingency table between the Pre and Post measures of depression"

CAPTION_DEPRESSION_IDEATION_ESTIMATED_CONTINGENCY <- paste(
  "Estimated contingency between the Pre and Post measures",
  "of depression in the population"
)

CAPTION_ALLUVIAL_DEPRESSION <- "Change of prevalence in depression"

CAPTION_UNVIARIATE_TESTS_DEPRESSION <-
  "Univariate tests of the predictors of change in depression"

CAPTION_ORDINAL_LINEARITY_TESTS_DEPRESSION <- paste(
  "Tests of non-linearity of",
  "the ordinal predictors of change in depression"
)

CAPTION_SLEEP_FIT_HISTORY <-
  "History of terms dropped from the model of change in depression"

CAPTION_DEPRESSION_CHANGE_FIT_TERMS <-
  "Predictors of change in depression"


### Depression (prediction of Post measure in participants without
### depression in the Pre measure): ----

CAPTION_DEPRESSION_CONTINGENCY_PREDS_NEW <- paste(
  "Frequency of depression per category of categorical predictors,",
  "in participants without depression in the Pre measure"
)

CAPTION_UNVIARIATE_TESTS_DEPRESSION_NEW <- paste(
  "Univariate tests of the predictors of depression in the Post measure",
  "in participants without depression in the Pre measure"
)

CAPTION_ORDINAL_LINEARITY_TESTS_DEPRESSION_NEW <- paste(
  "Tests of non-linearity of",
  "the ordinal predictors of depression in the Post measure",
  "in participants without depression in the Pre measure"
)

CAPTION_DEPRESSION_HISTORY_NEW_FIT_HISTORY <- paste(
  "Fit history terms for the model of depression in the Post measure in",
  "the participants without depression in the Pre measure"
)

CAPTION_DEPRESSION_FIT_NEW_TERMS <- paste(
  "Logistic regression model of depression after the confinement",
  "in participants without depression before the confinement"
)

CAPTION_DEPRESSION_NEW_COMPLETE_SEPARATION_PREDICTORS <- paste(
  "Contingency table between the Post measure of depression and",
  "the predictors that yield to complete separation"
)

CAPTION_DEPRESSION_HISTORY_NEW_FIT_HISTORY_WITHOUT_COMP_SEP <- paste(
  "Fit history terms for the model of depression in the Post measure in",
  "the participants without depression in the Pre measure",
  "(after dropping terms that yield to complete separation)"
)

CAPTION_DEPRESSION_FIT_NEW_TERMS_WITHOUT_COMP_SEP <- paste(
  "Logistic regression model of depression after the confinement",
  "in participants without depression before the confinement",
  "(after dropping terms that yield to complete separation)"
)


## Suicidal ideation: ----

CAPTION_DESC_SUICIDAL_IDEATION <-
  "Descriptive statistics of the reported suicidal ideation"

CAPTION_PREVALENCE_SUICIDAL_IDEATION <-
  "Estimated population prevalence of the reported suicidal ideation"

CAPTION_SUICIDAL_IDEATION_CONTINGENCY <-
  "Contingency table between the Pre and Post measures of suicidal ideation"

CAPTION_SUICIDAL_IDEATION_ESTIMATED_CONTINGENCY <- paste(
  "Estimated contingency between the Pre and Post measures",
  "of suicidal ideation in the population"
)

CAPTION_ALLUVIAL_SUICIDAL <- "Change of prevalence in suicidal ideation"

CAPTION_UNVIARIATE_TESTS_SUICIDAL <-
  "Univariate tests of the predictors of change in suicidal ideation"

CAPTION_ORDINAL_LINEARITY_TESTS_SUICIDAL <- paste(
  "Tests of non-linearity of",
  "the ordinal predictors of change in suicidal ideation"
)

CAPTION_SLEEP_FIT_HISTORY <-
  "History of terms dropped from the model of change in suicidal ideation"

CAPTION_SUICIDAL_CHANGE_FIT_TERMS <-
  "Predictors of change in suicidal ideation"


### Suicidal ideation (prediction of Post measure in participants without
### suicidal ideation in the Pre measure): ----

CAPTION_SUICIDAL_IDEATION_CONTINGENCY_PREDS_NEW <- paste(
  "Frequency of suicidal ideation per category of categorical predictors,",
  "in participants without suicidal ideation in the Pre measure"
)

CAPTION_UNVIARIATE_TESTS_SUICIDAL_NEW <- paste(
  "Univariate tests of the predictors of suicidal ideation in the Post measure",
  "in participants without suicidal ideation in the Pre measure"
)

CAPTION_ORDINAL_LINEARITY_TESTS_SUICIDAL_NEW <- paste(
  "Tests of non-linearity of",
  "the ordinal predictors of suicidal ideation in the Post measure",
  "in participants without suicidal ideation in the Pre measure"
)

CAPTION_SUICIDAL_HISTORY_NEW_FIT_HISTORY <- paste(
  "Fit history terms for the model of suicidal ideation in the Post measure in",
  "the participants without suicidal ideation in the Pre measure"
)

CAPTION_SUICIDAL_FIT_NEW_TERMS <- paste(
  "Logistic regression model of suicidal ideation after the confinement",
  "in participants without suicidal ideation before the confinement"
)

CAPTION_SUICIDAL_NEW_COMPLETE_SEPARATION_PREDICTORS <- paste(
  "Contingency table between the Post measure of suicidal ideation and",
  "the predictors that yield to complete separation"
)

CAPTION_SUICIDAL_HISTORY_NEW_FIT_HISTORY_WITHOUT_COMP_SEP <- paste(
  "Fit history terms for the model of suicidal ideation in the Post measure in",
  "the participants without suicidal ideation in the Pre measure",
  "(after dropping terms that yield to complete separation)"
)

CAPTION_SUICIDAL_FIT_NEW_TERMS_WITHOUT_COMP_SEP <- paste(
  "Logistic regression model of suicidal ideation after the confinement",
  "in participants without suicidal ideation before the confinement",
  "(after dropping terms that yield to complete separation)"
)


## Sleeping time: ----

CAPTION_DESC_SLEEPING_TIME <-
  "Descriptive statistics of the sleeping time measures"

CAPTION_POP_EST_SLEEPING_TIME <-
  "Population estimates (weighted statistics) of the sleeping time measures"

CAPTION_HISTOGRAM_SLEEP_TIME <- "Histogram of reported average hours of sleep"

CAPTION_POP_HISTOGRAM_SLEEP_TIME <-
  "Population histogram estimate of reported average hours of sleep"

CAPTION_UNVIARIATE_TESTS_SLEEP <-
  "Univariate tests of the predictors of change in sleeping time"

CAPTION_ORDINAL_LINEARITY_TESTS_SLEEP <-
  "Tests of non-linearity of the ordinal predictors of change in sleeping time"

CAPTION_SLEEP_FIT_HISTORY <-
  "History of terms dropped from the model of change in sleeping time"

CAPTION_SLEEP_FIT_TERMS <-
  "Fixed-effect coefficients in the model of change in sleeping time"

CAPTION_SLEEP_FIT_TERMS_STATA <- paste(
  "Fixed-effect coefficients in the model of change in sleeping time",
  "(fit with Stata 15)"
)

CAPTION_POP_HISTOGRAM_SLEEP_TIME_BY_AGE <- paste(
  "Population histogram estimate of reported average hours of sleep,",
  "segmented by age groups"
)
