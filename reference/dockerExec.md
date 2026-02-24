# Execute a Docker command

Execute a Docker command

## Usage

``` r
dockerExec(args, error_message = "Docker command failed")
```

## Arguments

- args:

  Character vector of Docker command arguments

- error_message:

  Custom error message if command fails

## Value

Command output as character vector (returned invisibly to avoid console
clutter)
