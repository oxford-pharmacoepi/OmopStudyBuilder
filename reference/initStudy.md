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
  studyDescription = NULL,
  repository = NULL,
  organisation = NULL,
  private = TRUE
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

- repository:

  Optional GitHub repository name. If provided, creates a GitHub
  repository and links it to the study. Requires the `gh` and `gert`
  packages and GitHub authentication (for example via `GITHUB_PAT`).

- organisation:

  Optional GitHub organisation name. If NULL (default), creates
  repository under your personal account. Only used when `repository` is
  provided.

- private:

  Logical. If TRUE (default), creates a private GitHub repository. Only
  used when `repository` is provided.

## Value

Project directory will be created

## Examples

``` r
# Create a study called "SampleStudy" in a temporary directory
study_root <- file.path(tempdir(), "SampleStudy")
initStudy(study_root)
#> ✔ /tmp/RtmpI30Pvl/SampleStudy prepared as root folder for study.
#> ✔ /tmp/RtmpI30Pvl/SampleStudy/diagnosticsCode prepared for study diagnostics code
#> ✔ /tmp/RtmpI30Pvl/SampleStudy/diagnosticsShiny prepared for diagnostics shiny app
#> ✔ /tmp/RtmpI30Pvl/SampleStudy/studyCode prepared for study study code
#> ✔ /tmp/RtmpI30Pvl/SampleStudy/studyShiny prepared for study shiny app

# Inspect the top-level contents
list.files(study_root)
#> [1] "INSTRUCTIONS.md"  "README.md"        "diagnosticsCode"  "diagnosticsShiny"
#> [5] "studyCode"        "studyShiny"      

# Create another study with custom metadata
study_root2 <- file.path(tempdir(), "DiabetesStudy")
initStudy(study_root2,
          studyTitle = "Diabetes Prevalence Study",
          studyLeads = "Dr. Smith, Dr. Jones")
#> ✔ /tmp/RtmpI30Pvl/DiabetesStudy prepared as root folder for study.
#> ✔ /tmp/RtmpI30Pvl/DiabetesStudy/diagnosticsCode prepared for study diagnostics code
#> ✔ /tmp/RtmpI30Pvl/DiabetesStudy/diagnosticsShiny prepared for diagnostics shiny app
#> ✔ /tmp/RtmpI30Pvl/DiabetesStudy/studyCode prepared for study study code
#> ✔ /tmp/RtmpI30Pvl/DiabetesStudy/studyShiny prepared for study shiny app

if (FALSE) { # \dontrun{
# Create study with GitHub integration (requires GITHUB_PAT)
# Set PAT for current session:
Sys.setenv(GITHUB_PAT = "your_token_here")

study_root3 <- file.path(tempdir(), "GitHubStudy")
initStudy(
  directory = study_root3,
  repository = "my-omop-study",
  organisation = "oxford-pharmacoepi",
  private = TRUE
)
} # }
```
