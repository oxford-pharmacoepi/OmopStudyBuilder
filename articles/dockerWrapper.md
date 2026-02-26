# dockerWrapper

``` r
library(OmopStudyBuilder)
```

## Overview

This vignette describes the Docker helper functions in
`OmopStudyBuilder`:

- [`buildStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/buildStudy.md)
  builds a Docker image for your study
- [`runRStudio()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/runRStudio.md)
  starts an interactive RStudio Server session inside the image
- [`runStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/runStudy.md)
  runs your study script in automated mode (streaming logs)
- [`stopStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/stopStudy.md)
  stops containers started by
  [`runRStudio()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/runRStudio.md)/[`runStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/runStudy.md)
- [`pushStudyImage()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/pushStudyImage.md)
  tags and pushes your image to Docker Hub

All Docker calls require Docker to be installed and the Docker daemon to
be running.

## Build an image

[`buildStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/buildStudy.md)
expects an `renv.lock` (and renv activation files) in the directory you
point it at. By default it will also snapshot your dependencies before
building.

``` r
# From the study_code folder (or pass path=...)
buildStudy(
  image_name = "my-study",
  path = "./study_code",
  useRStudio = FALSE
)
```

If you want
[`runRStudio()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/runRStudio.md),
build with an RStudio base image:

``` r
buildStudy(
  image_name = "my-study",
  path = "./study_code",
  useRStudio = TRUE
)
```

If your `renv.lock` includes GitHub packages and you hit GitHub rate
limits during the build, pass a token:

``` r
buildStudy(
  path = "./study_code",
  github_token = Sys.getenv("GITHUB_PAT")
)
```

## Run interactively (RStudio Server)

[`runRStudio()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/runRStudio.md)
runs the Docker image and opens a browser to the RStudio Server URL.
Results are written to `results_path` on your host.

``` r
runRStudio(
  image_name = "my-study",
  results_path = "./results"
)
```

To pass runtime settings to the container, create a `.env` file in your
working directory and it will be used automatically (or provide
`env_file = "path/to/.env"`).

## Run in automated mode

[`runStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/runStudy.md)
executes an R script in the container (default `code_to_run.R`) and
streams stdout/stderr to your console.

``` r
runStudy(
  image_name = "my-study",
  results_path = "./results",
  data_path = "/path/to/cdm/data",
  script_path = "code_to_run.R"
)
```

## Stop containers

Use
[`stopStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/stopStudy.md)
to stop containers started by
[`runRStudio()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/runRStudio.md)
and/or
[`runStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/runStudy.md).

``` r
stopStudy(image_name = "my-study")
```

## Push to Docker Hub

[`pushStudyImage()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/pushStudyImage.md)
will tag `image_name:latest` and push it to a Docker Hub repository.

``` r
pushStudyImage(
  image_name = "my-study",
  repo = "yourusername/my-study"
)
```
