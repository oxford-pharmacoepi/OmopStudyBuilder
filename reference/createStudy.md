# Creates initial directory for an OMOP CDM network study

Creates initial directory for an OMOP CDM network study

## Usage

``` r
createStudy(directory, diagnostics = TRUE, study = TRUE)
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

## Value

Project directory will be created

## Examples

``` r
# Create a study called "SampleStudy" in a temporary directory
study_root <- file.path(tempdir(), "SampleStudy")
createStudy(study_root)
#> ✔ /tmp/RtmppyrheP/SampleStudy prepared as root folder for study.
#> ✔ /tmp/RtmppyrheP/SampleStudy/diagnostics_code prepared for study diagnostics code
#> ✔ /tmp/RtmppyrheP/SampleStudy/diagnostics_shiny prepared for diagnostics shiny app
#> ✔ /tmp/RtmppyrheP/SampleStudy/study_code prepared for study study code
#> ✔ /tmp/RtmppyrheP/SampleStudy/study_shiny prepared for study shiny app

# Inspect the top-level contents
list.files(study_root)
#> [1] "README.md"         "diagnostics_code"  "diagnostics_shiny"
#> [4] "study_code"        "study_shiny"      
```
