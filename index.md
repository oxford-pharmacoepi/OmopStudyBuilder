# OmopStudyBuilder [![OmopStudyBuilder logo](reference/figures/image.jfif)](https://github.com/oxford-pharmacoepi/OmopStudyBuilder)

The **OmopStudyBuilder** package helps a study prepare for network
studies using the OMOP Common Data Model (CDM). The package sets up an R
project for your study with a default folder structure and template code
typically required for network studies. This allows you to focus on the
parts of the analysis that are specific to your study design.

In addition to project setup, the package reviews code and dependencies,
supports renv locking for consistent package versions, and can build and
run a Docker image for reproducible execution aligned with best
practices.

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

To illustrate how OmopStudyBuilder works, start by creating the study
folder and reviewing what it contains:

``` r
library(OmopStudyBuilder)
createStudy(here::here("SampleStudy"))

reviewStudyCode(here::here("SampleStudy/study_code"))
reviewStudyDependencies(here::here("SampleStudy/study_code"))
```

Lock package versions with renv so everyone runs the same environment,
then build the study image from the study folder.

``` r
renv::init(here::here("SampleStudy/study_code"))
install.packages(c("dplyr", "CDMConnector", "IncidencePrevalence"))
renv::snapshot(here::here("SampleStudy/study_code"))

buildStudy(path = here::here("SampleStudy/study_code"))
```

Run the study interactively in RStudio Server or as an automated script.
If you use a `.env` file for credentials, pass `env_file = ".env"`.

``` r
runRStudio()
runStudy()
```

Use optional inputs only if you need them:

``` r
runStudy(
  image_name = "omop-study-study-code",
  data_path = "path/to/data",
  results_path = "./results"
)
```

To distribute the study, share the **study folder** created by
[`createStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/createStudy.md)
(including `study_code/` and `renv.lock`). Partners can build and run
using the same commands.

``` r
install.packages("OmopStudyBuilder")
library(OmopStudyBuilder)

buildStudy(path = here::here("SampleStudy/study_code"))
runStudy()
runRStudio()
stopStudy()
```
