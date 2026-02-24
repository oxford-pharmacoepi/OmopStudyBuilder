# Build a Docker image for an OMOP study

Build a Docker image for an OMOP study

## Usage

``` r
buildStudy(
  image_name = NULL,
  path = ".",
  useRStudio = FALSE,
  r_version = NULL,
  snapshot = TRUE,
  github_token = NULL
)
```

## Arguments

- image_name:

  Name for the Docker image (default: auto-detected from directory)

- path:

  Path to study directory (default: current directory)

- useRStudio:

  Use RStudio Server base (TRUE) or r-ver base (FALSE, default)

- r_version:

  R version override (default: auto-detected from renv.lock)

- snapshot:

  Update renv.lock before building (default: TRUE)

- github_token:

  Optional GitHub token for installing GitHub packages during build

## Value

Image name (invisibly - already printed to console)
