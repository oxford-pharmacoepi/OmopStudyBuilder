# Verify Docker image exists and provide helpful error

Verify Docker image exists and provide helpful error

## Usage

``` r
verifyImageExists(image_name, check_rstudio = FALSE)
```

## Arguments

- image_name:

  Name of Docker image

- check_rstudio:

  If TRUE, also verify RStudio Server is present

## Value

TRUE (invisibly) if checks pass, otherwise throws error
