
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OmopStudyBuilder <a href="https://github.com/oxford-pharmacoepi/OmopStudyBuilder"><img src="man/figures/image.jfif" align="right" height="138" alt="OmopStudyBuilder logo" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/oxford-pharmacoepi/OmopStudyBuilder/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/oxford-pharmacoepi/OmopStudyBuilder/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/OmopStudyBuilder)](https://CRAN.R-project.org/package=OmopStudyBuilder)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/oxford-pharmacoepi/OmopStudyBuilder/branch/main/graph/badge.svg)](https://app.codecov.io/gh/oxford-pharmacoepi/OmopStudyBuilder?branch=main)

<!-- badges: end -->

The goal of **OmopStudyBuilder** is to help you prepare a study for
network studies using the OMOP Common Data Model (CDM). The package sets
up an R project for your study with a default folder structure and
template code typically required for network studies. This allows you to
focus on the parts of the analysis that are specific to your study
design.

In addition to project setup, the package also provides utilities for
reviewing study code and its dependencies, helping ensure
reproducibility, consistency, and alignment with best practices.

The package is highly opinionated and designed to align with the OxInfer
study code checklist. For further details, please refer to the formal
documentation at:
<https://oxford-pharmacoepi.github.io/Oxinfer/onboarding/code_review.html>

# Installation

You can install the development version of the package from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("oxford-pharmacoepi/OmopStudyBuilder")
```

# Example Usage

To illustrate how OmopStudyBuilder works, we begin by loading the
package:

``` r
library(OmopStudyBuilder)
```

Next, we create a new study project. You can provide any study name; the
example below creates `"SampleStudy"` in the current working directory:

``` r
OmopStudyBuilder::createStudy(here::here("SampleStudy"))
#> ✔ /Users/foluwa/Desktop/NDORMS/projects/OmopStudyBuilder/SampleStudy prepared as root folder for study.
#> ✔ /Users/foluwa/Desktop/NDORMS/projects/OmopStudyBuilder/SampleStudy/diagnostics_code prepared for study diagnostics code
#> ✔ /Users/foluwa/Desktop/NDORMS/projects/OmopStudyBuilder/SampleStudy/diagnostics_shiny prepared for diagnostics shiny app
#> ✔ /Users/foluwa/Desktop/NDORMS/projects/OmopStudyBuilder/SampleStudy/study_code prepared for study study code
#> ✔ /Users/foluwa/Desktop/NDORMS/projects/OmopStudyBuilder/SampleStudy/study_shiny prepared for study shiny app
```

After creating the project, open the study directory and use the
[`renv`](https://rstudio.github.io/renv/) package to initialise a
reproducible environment:

``` r
renv::init()
```

This will generate an `renv.lock` file, which OmopStudyBuilder uses to
analyse the study dependencies:

``` r
OmopStudyBuilder::summariseStudyDependencies(here::here("SampleStudy"))
```
