# Push a Docker image to Docker Hub

Push a Docker image to Docker Hub

## Usage

``` r
pushStudyImage(
  image_name = NULL,
  repo,
  tag = "latest",
  username = NULL,
  password = NULL,
  logout = TRUE
)
```

## Arguments

- image_name:

  Name of Docker image to push (default: auto-detected from directory)

- repo:

  Docker Hub repository (e.g., "username/repo" or "repo")

- tag:

  Tag to push (default: "latest")

- username:

  Docker Hub username (prompted if NULL)

- password:

  Docker Hub password or token (prompted if NULL, hidden input)

- logout:

  If TRUE, logs out after pushing

## Value

Pushed image reference (invisibly)
