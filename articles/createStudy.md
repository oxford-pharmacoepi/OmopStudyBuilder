# createStudy

## Introduction

[`createStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/createStudy.md)
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
  [`createStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/createStudy.md)
- Explore the folder structure that is generated
- Control whether diagnostics and/or study analysis templates are
  included
- Understand next steps after project creation

------------------------------------------------------------------------

## Basic usage

The simplest way to use
[`createStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/createStudy.md)
is to provide a directory path. If the directory:

- **Does not exist**: it will be created
- **Exists but is empty**: it will be used
- **Exists and contains files**: an error will be thrown

To avoid modifying your real project directories, this vignette uses a
temporary folder.

``` r
# Create a temporary root for this vignette
study_root <- file.path(tempdir(), "SampleStudy")

createStudy(study_root)

# Show top-level contents created by createStudy()
list.files(study_root)
#> [1] "diagnostics_code"  "diagnostics_shiny" "README.md"        
#> [4] "study_code"        "study_shiny"
```

You should see something like:

- `diagnostics_code/` – diagnostic scripts and helpers
- `diagnostics_shiny/` – skeleton diagnostics Shiny app
- `study_code/` – scripts for running the main analyses
- `study_shiny/` – skeleton reporting Shiny app
- `README.md` – overview of the study project

To inspect the full structure:

``` r
list.files(study_root, recursive = TRUE)
#>  [1] "diagnostics_code/code_to_run.R"                
#>  [2] "diagnostics_code/cohorts/instantiate_cohorts.R"
#>  [3] "diagnostics_code/README.md"                    
#>  [4] "diagnostics_code/results/readme.md"            
#>  [5] "diagnostics_code/run_study.R"                  
#>  [6] "diagnostics_shiny/README.md"                   
#>  [7] "README.md"                                     
#>  [8] "study_code/analyses/cohort_characteristics.R"  
#>  [9] "study_code/analyses/cohort_survival.R"         
#> [10] "study_code/analyses/drug_utilisation.R"        
#> [11] "study_code/analyses/incidence_prevalence.R"    
#> [12] "study_code/code_to_run.R"                      
#> [13] "study_code/codelist/codelist_creation.R"       
#> [14] "study_code/cohorts/instantiate_cohorts.R"      
#> [15] "study_code/Dockerfile"                         
#> [16] "study_code/README.md"                          
#> [17] "study_code/Results/README.md"                  
#> [18] "study_code/run_study.R"                        
#> [19] "study_shiny/README.md"
```

------------------------------------------------------------------------

## Diagnostics-only and study-only setups

You can control which parts of the template are created with the
`diagnostics` and `study` arguments.

### Diagnostics-only project

``` r
diag_root <- file.path(tempdir(), "DiagnosticsOnly")

createStudy(
  directory   = diag_root,
  diagnostics = TRUE,
  study       = FALSE
)

list.files(diag_root)
#> [1] "diagnostics_code"  "diagnostics_shiny" "README.md"
```

### Study-only project (no diagnostics templates)

``` r
study_only_root <- file.path(tempdir(), "StudyOnly")

createStudy(
  directory   = study_only_root,
  diagnostics = FALSE,
  study       = TRUE
)

list.files(study_only_root)
#> [1] "README.md"   "study_code"  "study_shiny"
```

------------------------------------------------------------------------

## What to do after `createStudy()`

After generating the project structure, you will mainly work with these
folders and files:

- `study_code/`
  - `StudyCode.Rproj`: open this in RStudio to work on the main analysis
    code. It sets the working directory so all relative paths behave
    correctly.  
  - `code_to_run.R`: where you add database connection details, schemas,
    and global settings
  - `cohorts/instantiate_cohorts.R`: defines the study cohorts using
    your codelists and cohort-building functions. You customise this to
    match your phenotype definitions.  
  - `run_study.R`: orchestrates the analysis steps (cohort creation,
    summaries, analyses, export).
- `diagnostics_code/`
  - `diagnostics.Rproj`: open this project to work on diagnostics. It
    keeps diagnostics code separate from the main analysis project.  
  - `code_to_run.R`: similar to the study version, but dedicated to
    diagnostics. You set connection details and schemas used for running
    phenotype diagnostics.  
  - `cohorts/instantiate_cohorts.R`: defines cohorts for diagnostic
    checking, often mirroring or simplifying the main study cohorts.  
  - `run_study.R`: runs the PhenotypeDiagnostics workflow and exports
    diagnostic results. You might tweak options, but the overall flow is
    usually left intact.
- `study_shiny/`
  - For a shiny app to explore study results. You extend this with
    plots, tables, and filters built on top of your exported outputs.
- `diagnostics_shiny/`
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
buildStudy(
  image_name = "omop-study-study-code",
  path = file.path(study_root, "study_code")
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
pushStudyImage(
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

# Interactive (requires image built with buildStudy(useRStudio = TRUE)):
runRStudio(
  image_name = "yourusername/myomopstudy:latest",
  results_path = "./results"
)
```

### Interactive vs Automated Execution

**RStudio Server** runs the Docker image and opens a browser to the
RStudio Server URL:

``` r
# Requires image built with buildStudy(useRStudio = TRUE)
runRStudio(
  image_name = "omop-study-study-code",
  results_path = "./results"
)
```

Partners edit credentials in `code_to_run.R` and run the study
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
study folders (for example `study_code/README.md` and
`diagnostics_code/README.md`).

------------------------------------------------------------------------
