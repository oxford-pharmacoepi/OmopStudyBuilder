
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
OmopStudyBuilder::reviewStudyDependencies(here::here("SampleStudy"))
```

To get a summary of the study code and its dependencies, you can use:

``` r
OmopStudyBuilder::reviewStudyCode(here::here("SampleStudy"))
```

# Docker Execution Engine

OmopStudyBuilder includes a Docker-based execution system that allows
studies to be run in reproducible, isolated environments. This ensures
that studies produce consistent results across different systems and
eliminates “works on my machine” problems.

## Complete Workflow

The typical workflow combines study creation with Docker
containerization:

``` r
# 1. Create study structure
library(OmopStudyBuilder)
createStudy("MyStudy")

# 2. Initialize package management
setwd("MyStudy/study_code")
renv::init()

# 3. Develop your study code
# Edit files in study_code/ directory
# Install required packages: install.packages("dplyr")

# 4. Lock package versions
renv::snapshot()

# 5. Build Docker image
buildStudy(".")

# 6. Run study in Docker
runStudy(
  image_name = "omop-study-study-code",
  config_file = "config.yml",
  output_path = "../results"
)
```

## Setup Requirements

Before using the Docker execution features, you need:

1.  Docker Desktop installed and running on your system
2.  OmopStudyBuilder package loaded

``` r
library(OmopStudyBuilder)
# or in development
devtools::load_all()
```

## Core Functions

### checkDocker()

Verifies that Docker is installed and accessible from R. Returns TRUE if
Docker is available, FALSE otherwise.

``` r
checkDocker()
#> Docker 28.3.2 detected
#> [1] TRUE
```

### buildStudy(study_path, image_name = NULL)

Creates a Docker image containing the study code and all required R
packages. The image is a self-contained environment that can be
distributed to data partners.

Arguments: - `study_path`: Path to the study directory created by
createStudy() - `image_name`: Optional custom name for the Docker image.
If NULL, generates name automatically.

The function automatically detects if renv.lock exists and uses it to
install exact package versions. If no renv.lock is found, it installs a
default set of packages.

``` r
buildStudy("path/to/study")
#> Building Docker image: omop-study-mystudy
#> This may take 5-15 minutes on first build...
#> Image built successfully: omop-study-mystudy
```

### runStudy(image_name, config_file, data_path, output_path)

Executes the study inside a Docker container. The study runs in complete
isolation and saves results to the specified output directory.

Arguments: - `image_name`: Name of the Docker image to run -
`config_file`: Path to YAML configuration file (default: “config.yml”) -
`data_path`: Optional path to data directory to mount in container -
`output_path`: Directory where results will be saved (default:
“./results”)

``` r
runStudy(
  image_name = "omop-study-mystudy",
  config_file = "database_config.yml",
  output_path = "./results"
)
#> Running study in Docker container...
#> Results will be saved to: ./results
#> Study completed successfully!
```

## Package Version Management with renv

To ensure reproducibility, lock package versions using renv before
building the Docker image:

``` r
# In your study directory
setwd("path/to/study")

# Initialize renv (first time only)
renv::init()

# Install required packages
install.packages(c("dplyr", "ggplot2", "DatabaseConnector"))

# Lock package versions
renv::snapshot()

# Build Docker image with locked versions
buildStudy("path/to/study")
```

The Docker image will contain the exact package versions specified in
renv.lock, ensuring identical environments across all execution sites.

## Additional Functions

Additional utilities available:

### initRenv(study_path)

Initializes renv in a study directory, setting up package version
management.

### snapshotPackages(study_path)

Takes a snapshot of currently installed packages and locks their
versions in renv.lock.

### syncStudy(study_path, force_rebuild, run_test, output_path)

One-stop command to sync local environment with Docker. Snapshots
packages, builds image, verifies compatibility, and optionally tests
execution.

``` r
syncStudy(
  study_path = "path/to/study",
  force_rebuild = FALSE,
  run_test = TRUE
)
#> ══ SYNCING STUDY ENVIRONMENT ══
#> 
#> ── [1/4] Snapshotting package versions...
#> ✔ Packages updated in renv.lock
#> 
#> ── [2/4] Building Docker image...
#> ✔ Image up-to-date, skipping rebuild
#> 
#> ── [3/4] Validating Docker environment...
#> ✔ Environment validated successfully!
#>   • Local: 45 packages
#>   • Docker: 45 packages
#> 
#> ── [4/4] Testing study execution in Docker...
#> ✔ Study executed successfully in Docker!
#> 
#> ══ SYNC COMPLETE - Ready for distribution! ══
```

## How It Works

The Docker execution system operates through these steps:

1.  Study code and renv.lock are packaged with a Dockerfile template
2.  Docker builds an image containing R 4.5.2, system dependencies, and
    all required packages
3.  The image can be distributed to data partners via Docker registry or
    offline transfer
4.  Partners run the study using runStudy() without installing any R
    packages
5.  Results are extracted from the container to the local filesystem

This approach guarantees that: - All execution environments are
identical - Package versions are locked and reproducible - System
dependencies are included - Studies run independently of local R
installations - Results are consistent across all sites
