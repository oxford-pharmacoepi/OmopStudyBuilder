# Stop a running OmopStudyBuilder container (automated or RStudio)

Stops containers started by
[`runStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/runStudy.md)
and/or
[`runRStudio()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/runRStudio.md).
By default it stops containers for the image inferred from the current
directory name.

## Usage

``` r
stopStudy(
  container = NULL,
  image_name = NULL,
  mode = c("any", "rstudio", "run"),
  all = FALSE
)
```

## Arguments

- container:

  Optional container name or ID to stop directly.

- image_name:

  Optional Docker image name. Defaults to current directory name.

- mode:

  Which container(s) to stop: "any" (default), "rstudio", or "run".

- all:

  If TRUE, stops all running containers started by OmopStudyBuilder.

## Value

TRUE if at least one container was stopped (invisibly)
