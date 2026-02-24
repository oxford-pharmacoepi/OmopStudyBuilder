# Process environment file argument

Process environment file argument

## Usage

``` r
processEnvFile(env_file, default_env_file = ".env")
```

## Arguments

- env_file:

  Path to .env file or NULL

- default_env_file:

  Default env file path to use when env_file is NULL

## Value

Character vector of Docker env-file arguments (empty if no file)
