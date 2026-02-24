# Run study in automated mode with real-time log streaming

Run study in automated mode with real-time log streaming

## Usage

``` r
runStudy(
  image_name = NULL,
  results_path = "./results",
  env_file = NULL,
  data_path = NULL,
  script_path = "code_to_run.R"
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

- data_path:

  Optional path to data directory (mounted at /data)

- script_path:

  Path to R script to execute (default: "code_to_run.R")

## Value

Exit status (0 = success, non-zero = failure)
