# Run RStudio Server for interactive study execution

Note: The Docker image must include RStudio Server (i.e., be built with
\`buildStudy(useRStudio = TRUE)\` which uses a \`rocker/rstudio\` base
image).

## Usage

``` r
runRStudio(
  image_name = NULL,
  results_path = "./results",
  env_file = NULL,
  port = 8787,
  password = NULL
)
```

## Arguments

- image_name:

  Name of Docker image to run (default: auto-detected from directory)

- results_path:

  Path to save results (default: "./results")

- env_file:

  Optional path to a .env file (passed to Docker via –env-file). If NULL
  and a \`.env\` file exists in the current working directory, it will
  be used automatically.

- port:

  Port for RStudio Server (default: 8787, auto-finds next available if
  busy)

- password:

  RStudio password (default: auto-generated and displayed)

## Value

Container ID (invisibly)
