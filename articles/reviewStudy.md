# reviewStudy

``` r
library(OmopStudyBuilder)
```

## Overview

When you receive a study package (or before you ship one), it is useful
to quickly inspect:

- What files are included (R scripts, JSON, CSV, Excel)
- Whether `renv.lock` exists, and what it contains
- Whether any dependencies look surprising for the study type

`OmopStudyBuilder` provides two helpers:

- `reviewStudyCode()` summarises the contents of a directory
- `reviewStudyDependencies()` summarises dependencies based on
  `renv.lock`

## Review files in a study folder

Point `reviewStudyCode()` at the folder you want to inspect (often
`study_code/` or `diagnostics_code/`).

The simplest pattern is to run these commands from within the project
you want to review (for example, open `study_code.Rproj` or
`diagnostics_code.Rproj`).

``` r
reviewStudyCode(".")
```

## Review dependencies

`reviewStudyDependencies()` expects a `renv.lock` file in the directory.
Use `type = "analysis"` for analysis code, or `type = "reporting"` for
reporting code.

``` r
reviewStudyDependencies(
  dir = ".",
  type = "analysis"
)
```
