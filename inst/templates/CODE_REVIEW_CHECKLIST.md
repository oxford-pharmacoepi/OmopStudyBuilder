# Code Review Checklist

Repository: {{REPO_NAME}}
Created: {{DATE}}

Based on the [OxInfer study code checklist](https://oxford-pharmacoepi.github.io/Oxinfer/onboarding/code_review.html).

## Study Code Organisation
- [ ] Structured as an R project (`.Rproj`), with no nested R projects
- [ ] There is a `codeToRun.R` file to run the study code
- [ ] There is a`runStudy.R` file to coordinate the study code
- [ ] There is a `cohorts` directory  with cohort definitions and an `instantiateCohorts.R` file
- [ ] Independent analyses are separated into individual files in the `analyses/` directory
- [ ] There is a `results` directory for study results to be saved
- [ ] Code is formatted and styled with `styler::style_dir()` and `lintr::lint_dir()`

## Standard Analyses
- [ ] Database snapshot using `OmopSketch::summariseOmopSnapshot()`
- [ ] Observation period summary using `OmopSketch::summariseObservationPeriod()`
- [ ] Cohort code usage summary using `CodelistGenerator::summariseCohortCodeUse()`
- [ ] Cohort count summary using `CohortCharacteristics::summariseCohortCounts()`
- [ ] Cohort attrition summary using `CohortCharacteristics::summariseCohortAttrition()`

## Shiny App
- [ ] There is a shiny app to review results
- [ ] The app has sensible defaults for input choices
- [ ] The app does not have redundant UI inputs
- [ ] There is no particularly slow performance when generating tables or plots

## Reproducibility
- [ ] `renv` is used to list all dependencies needed
- [ ] `renv.lock`  file only includes packages from CRAN
- [ ] `MASS` and `Matrix` are excluded from study code (because of dependency of R version)
- [ ] Most recent versions of analytic packages are being used

## Documentation
- [ ] There is a central readme explaining how to run the study code and review the results

## Efficient and Robust Study Code
- [ ] The code runs on synthetic test database
- [ ] Ensure any warning messages that could be resolved when running
- [ ] Check whether any code is repeated unnecessarily or could be simplified
- [ ] Review the SQL that gets executed for any obvious inefficiencies
- [ ] Review the `renv.lock` - can any packages be removed?
- [ ] Use `readr::read_csv()` for importing CSVs and specify column types
- [ ] Use `stringr` for string manipulation
- [ ] Study code doesn’t have a lot of complex, custom code unless unavoidable
- [ ] Study code does not have if-else statements based on data partner names unless unavoidable
- [ ] Output of the study is saved as a CSV

## Review Results for Plausibility
- [ ] Does the code and results shown match the protocol?
- [ ] Have any analyses been missed?
- [ ] For each analysis, are cohorts defined in the right way (e.g., typically no exclusion criteria for an incidence outcome)?

---
*This checklist was opened automatically by [OmopStudyBuilder](https://github.com/oxford-pharmacoepi/OmopStudyBuilder).*
