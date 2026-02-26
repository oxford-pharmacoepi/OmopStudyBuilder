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

- [`reviewStudyCode()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/reviewStudyCode.md)
  summarises the contents of a directory
- [`reviewStudyDependencies()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/reviewStudyDependencies.md)
  summarises dependencies based on `renv.lock`

## Review files in a study folder

Point
[`reviewStudyCode()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/reviewStudyCode.md)
at the folder you want to inspect (often `study_code/` or
`diagnostics_code/`).

The simplest pattern is to run these commands from within the project
you want to review (for example, open `StudyCode.Rproj` or
`Diagnostics.Rproj`).

``` r
reviewStudyCode(".")
```

## Review dependencies

[`reviewStudyDependencies()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/reviewStudyDependencies.md)
expects a `renv.lock` file in the directory. Use `type = "analysis"` for
analysis code, or `type = "reporting"` for reporting code.

``` r
reviewStudyDependencies(
  dir = ".",
  type = "analysis"
)
```
