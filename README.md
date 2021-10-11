# COVID-19-lockdown

Analyses for the paper http://doi.org/10.1017/S2045796021000408


# Most relevant files

- Perform the study analyses: src/Lockdown_analysis.R

- Generate the output document with the Tables:
output/Tables.Rmd

- Generate the output document with the methods & results sections:
output/Methods_results_Review.Rmd

The latter generates the published version of these sections.
A previous version can be found in output/Methods_results.Rmd


# Package installation

This repository uses package `renv` in order to improve reproducibility.
`renv` should be activated when opening the project `COVID-19-lockdown.Rproj`.
In order to restore the package environment,
type the following syntax into the R console:

```r
renv::restore()
```
