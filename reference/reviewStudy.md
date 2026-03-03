# Review a study directory

Summarises the code and/or renv dependencies in a study directory.

## Usage

``` r
reviewStudy(dir, code = TRUE, dependencies = TRUE, type = "analysis")
```

## Arguments

- dir:

  Path to the study directory.

- code:

  If \`TRUE\`, summarises R, JSON, CSV, and Excel files found in the
  directory.

- dependencies:

  If \`TRUE\`, summarises the renv.lock dependencies.

- type:

  Whether the R project is for analysis code or study reporting. Only
  used when \`dependencies = TRUE\`.

## Value

Invisibly returns \`NULL\`. Called for its side effects of printing
summaries to the console.
