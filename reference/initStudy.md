# Creates initial directory for an OMOP CDM network study

Creates initial directory for an OMOP CDM network study

## Usage

``` r
initStudy(
  directory,
  diagnostics = TRUE,
  study = TRUE,
  studyTitle = NULL,
  studyLeads = NULL,
  studyDescription = NULL
)
```

## Arguments

- directory:

  Path to a directory that will be used as the root folder for the
  study. If it does not exist, it will be created. The directory must be
  empty if it already exists.

- diagnostics:

  A single TRUE or FALSE value. If TRUE (the default), the function
  creates the \`diagnostics_code/\` and \`diagnostics_shiny/\` folders
  using the package templates. If FALSE, these diagnostics folders are
  not created.

- study:

  A single TRUE or FALSE value. If TRUE (the default), the function
  creates the \`study_code/\` and \`study_shiny/\` folders using the
  package templates. If FALSE, these study folders are not created.

- studyTitle:

  Character string with the study title. If NULL (default), uses the
  directory basename.

- studyLeads:

  Character string with study leads. If NULL (default), leaves a
  placeholder.

- studyDescription:

  Character string with study description. If NULL (default), leaves a
  placeholder.

## Value

Project directory will be created

## Examples

``` r
# Create a study called "SampleStudy" in a temporary directory
study_root <- file.path(tempdir(), "SampleStudy")
initStudy(study_root)
#> ✔ /tmp/RtmponZ5qW/SampleStudy prepared as root folder for study.
#> ✔ /tmp/RtmponZ5qW/SampleStudy/diagnostics_code prepared for study diagnostics code
#> ✔ /tmp/RtmponZ5qW/SampleStudy/diagnostics_shiny prepared for diagnostics shiny app
#> ✔ /tmp/RtmponZ5qW/SampleStudy/study_code prepared for study study code
#> ✔ /tmp/RtmponZ5qW/SampleStudy/study_shiny prepared for study shiny app

# Inspect the top-level contents
list.files(study_root)
#> [1] "README.md"         "diagnostics_code"  "diagnostics_shiny"
#> [4] "study_code"        "study_shiny"      

# Create another study with custom metadata
study_root2 <- file.path(tempdir(), "DiabetesStudy")
initStudy(study_root2,
          studyTitle = "Diabetes Prevalence Study",
          studyLeads = "Dr. Smith, Dr. Jones")
#> ✔ /tmp/RtmponZ5qW/DiabetesStudy prepared as root folder for study.
#> ✔ /tmp/RtmponZ5qW/DiabetesStudy/diagnostics_code prepared for study diagnostics code
#> ✔ /tmp/RtmponZ5qW/DiabetesStudy/diagnostics_shiny prepared for diagnostics shiny app
#> ✔ /tmp/RtmponZ5qW/DiabetesStudy/study_code prepared for study study code
#> ✔ /tmp/RtmponZ5qW/DiabetesStudy/study_shiny prepared for study shiny app
```
