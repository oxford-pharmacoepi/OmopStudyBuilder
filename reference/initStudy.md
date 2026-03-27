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
  creates the \`diagnosticsCode/\` and \`diagnosticsShiny/\` folders
  using the package templates. If FALSE, these diagnostics folders are
  not created.

- study:

  A single TRUE or FALSE value. If TRUE (the default), the function
  creates the \`studyCode/\` and \`studyShiny/\` folders using the
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
#> ✔ /tmp/RtmpSbB663/SampleStudy prepared as root folder for study.
#> ✔ /tmp/RtmpSbB663/SampleStudy/diagnosticsCode prepared for study diagnostics code
#> ✔ /tmp/RtmpSbB663/SampleStudy/diagnosticsShiny prepared for diagnostics shiny app
#> ✔ /tmp/RtmpSbB663/SampleStudy/studyCode prepared for study study code
#> ✔ /tmp/RtmpSbB663/SampleStudy/studyShiny prepared for study shiny app

# Inspect the top-level contents
list.files(study_root)
#> [1] "INSTRUCTIONS.md"  "README.md"        "diagnosticsCode"  "diagnosticsShiny"
#> [5] "studyCode"        "studyShiny"      

# Create another study with custom metadata
study_root2 <- file.path(tempdir(), "DiabetesStudy")
initStudy(study_root2,
          studyTitle = "Diabetes Prevalence Study",
          studyLeads = "Dr. Smith, Dr. Jones")
#> ✔ /tmp/RtmpSbB663/DiabetesStudy prepared as root folder for study.
#> ✔ /tmp/RtmpSbB663/DiabetesStudy/diagnosticsCode prepared for study diagnostics code
#> ✔ /tmp/RtmpSbB663/DiabetesStudy/diagnosticsShiny prepared for diagnostics shiny app
#> ✔ /tmp/RtmpSbB663/DiabetesStudy/studyCode prepared for study study code
#> ✔ /tmp/RtmpSbB663/DiabetesStudy/studyShiny prepared for study shiny app
```
