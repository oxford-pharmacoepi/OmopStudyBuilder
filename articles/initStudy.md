# initStudy

## Introduction

[`initStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/initStudy.md)
is the starting point for building a study project with the **OMOP
Common Data Model (CDM)** using the `OmopStudyBuilder` package.

It generates a fully structured project folder containing:

- Templates for study diagnostics
- Templates for analysis code
- Shiny apps for diagnostics and reporting
- A project README
- A reproducible, opinionated layout aligned with
  [OxInfer](https://oxford-pharmacoepi.github.io/Oxinfer/onboarding/code_review.html)
  conventions

This allows you to immediately focus on writing study-specific code
instead of manually assembling the surrounding infrastructure.

In this vignette you will learn how to:

- Create a new study project using
  [`initStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/initStudy.md)
- Explore the folder structure that is generated
- Control whether diagnostics and/or study analysis templates are
  included
- Understand next steps after project creation

------------------------------------------------------------------------

## Basic usage

The simplest way to use
[`initStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/initStudy.md)
is to provide a directory path. If the directory:

- **Does not exist**: it will be created
- **Exists but is empty**: it will be used
- **Exists and contains files**: an error will be thrown

To avoid modifying your real project directories, this vignette uses a
temporary folder.

``` r
# Create a temporary root for this vignette
study_root <- file.path(tempdir(), "SampleStudy")

initStudy(study_root)

# Show top-level contents created by initStudy()
list.files(study_root)
#> [1] "diagnosticsCode"  "diagnosticsShiny" "INSTRUCTIONS.md"  "README.md"       
#> [5] "studyCode"        "studyShiny"
```

You should see something like:

- `diagnosticsCode/` – diagnostic scripts and helpers
- `diagnosticsShiny/` – skeleton diagnostics Shiny app
- `studyCode/` – scripts for running the main analyses
- `studyShiny/` – skeleton reporting Shiny app
- `README.md` – overview of the study project

To inspect the full structure:

``` r
list.files(study_root, recursive = TRUE)
#>  [1] "diagnosticsCode/codeToRun.R"                 
#>  [2] "diagnosticsCode/cohorts/instantiateCohorts.R"
#>  [3] "diagnosticsCode/diagnosticsCode.Rproj"       
#>  [4] "diagnosticsCode/README.md"                   
#>  [5] "diagnosticsCode/results/README.md"           
#>  [6] "diagnosticsCode/runStudy.R"                  
#>  [7] "diagnosticsShiny/diagnosticsShiny.Rproj"     
#>  [8] "diagnosticsShiny/README.md"                  
#>  [9] "INSTRUCTIONS.md"                             
#> [10] "README.md"                                   
#> [11] "studyCode/analyses/cohortCharacteristics.R"  
#> [12] "studyCode/analyses/cohortSurvival.R"         
#> [13] "studyCode/analyses/drugUtilisation.R"        
#> [14] "studyCode/analyses/incidencePrevalence.R"    
#> [15] "studyCode/codelist/codelistCreation.R"       
#> [16] "studyCode/codeToRun.R"                       
#> [17] "studyCode/cohorts/instantiateCohorts.R"      
#> [18] "studyCode/README.md"                         
#> [19] "studyCode/Results/README.md"                 
#> [20] "studyCode/runStudy.R"                        
#> [21] "studyCode/studyCode.Rproj"                   
#> [22] "studyShiny/README.md"                        
#> [23] "studyShiny/studyShiny.Rproj"
```

------------------------------------------------------------------------

## Diagnostics-only and study-only setups

You can control which parts of the template are created with the
`diagnostics` and `study` arguments.

### Diagnostics-only project

``` r
diag_root <- file.path(tempdir(), "DiagnosticsOnly")

initStudy(
  directory   = diag_root,
  diagnostics = TRUE,
  study       = FALSE
)

list.files(diag_root)
#> [1] "diagnosticsCode"  "diagnosticsShiny" "INSTRUCTIONS.md"  "README.md"
```

### Study-only project (no diagnostics templates)

``` r
study_only_root <- file.path(tempdir(), "StudyOnly")

initStudy(
  directory   = study_only_root,
  diagnostics = FALSE,
  study       = TRUE
)

list.files(study_only_root)
#> [1] "INSTRUCTIONS.md" "README.md"       "studyCode"       "studyShiny"
```

------------------------------------------------------------------------

## What to do after `initStudy()`

After generating the project structure, you will mainly work with these
folders and files:

- `studyCode/`
  - `studyCode.Rproj`: open this in RStudio to work on the main analysis
    code. It sets the working directory so all relative paths behave
    correctly.  
  - `codeToRun.R`: where you add database connection details, schemas,
    and global settings
  - `cohorts/instantiateCohorts.R`: defines the study cohorts using your
    codelists and cohort-building functions. You customise this to match
    your phenotype definitions.  
  - `runStudy.R`: orchestrates the analysis steps (cohort creation,
    summaries, analyses, export).
- `diagnosticsCode/`
  - `diagnosticsCode.Rproj`: open this project to work on diagnostics.
    It keeps diagnostics code separate from the main analysis project.  
  - `codeToRun.R`: similar to the study version, but dedicated to
    diagnostics. You set connection details and schemas used for running
    phenotype diagnostics.  
  - `cohorts/instantiateCohorts.R`: defines cohorts for diagnostic
    checking, often mirroring or simplifying the main study cohorts.  
  - `runStudy.R`: runs the PhenotypeDiagnostics workflow and exports
    diagnostic results. You might tweak options, but the overall flow is
    usually left intact.
- `studyShiny/`
  - For a shiny app to explore study results. You extend this with
    plots, tables, and filters built on top of your exported outputs.
- `diagnosticsShiny/`
  - Contains a minimal Shiny app for exploring diagnostic outputs. Use
    it to review cohort characteristics and diagnostic results
    interactively across data partners.

------------------------------------------------------------------------

## Building and Distributing Your Study

Once your study code is ready, `OmopStudyBuilder` provides functions to
package it into a Docker image and distribute it to data partners.

### Building the Docker Image

``` r
# Build a Docker image containing your study code
dockeriseStudy(
  image_name = "omop-study-study-code",
  path = file.path(study_root, "studyCode")
)
#> Building Docker image: omop-study-study-code
#> This may take some minutes on first build...
#> ✔ Image built successfully: omop-study-study-code
```

The image contains: - Your exact R version - All package dependencies
(from renv.lock) - Your study code - Database drivers and system
dependencies

### Distribution Options

#### Option 1: Docker Hub (Online)

Push to Docker Hub for easy sharing with connected partners:

``` r
pushDockerImage(
  image_name = "omop-study-study-code",
  repo = "yourusername/myomopstudy"
)
#> ✔ Image pushed to Docker Hub!
```

Data partners can then pull and run the image.

They can then run either an interactive RStudio Server session, or run
the study in automated mode.

``` r
library(OmopStudyBuilder)

# Interactive (requires image built with dockeriseStudy(useRStudio = TRUE)):
runRStudio(
  image_name = "yourusername/myomopstudy:latest",
  results_path = "./results"
)
```

### Interactive vs Automated Execution

**RStudio Server** runs the Docker image and opens a browser to the
RStudio Server URL:

``` r
# Requires image built with dockeriseStudy(useRStudio = TRUE)
runRStudio(
  image_name = "omop-study-study-code",
  results_path = "./results"
)
```

Partners edit credentials in `codeToRun.R` and run the study
interactively.

**Automated execution** (for programmatic workflows):

``` r
runStudy(
  image_name = "omop-study-study-code",
  results_path = "./results",
  data_path = "path/to/data"
)
```

To stop any running OmopStudyBuilder containers, use:

``` r
stopStudy(image_name = "omop-study-study-code")
```

For more operational details, see the README files generated into your
study folders (for example `studyCode/README.md` and
`diagnosticsCode/README.md`).

------------------------------------------------------------------------
