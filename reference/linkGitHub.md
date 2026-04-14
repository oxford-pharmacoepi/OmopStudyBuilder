# Link study directory to GitHub repository

Creates a new GitHub repository and connects it to an existing study
directory. Initializes git, creates .gitignore, commits all files, and
pushes to GitHub.

## Usage

``` r
linkGitHub(
  directory,
  repository,
  organisation = NULL,
  private = TRUE,
  description = NULL
)
```

## Arguments

- directory:

  Path to study directory

- repository:

  GitHub repository name (will be sanitized if needed)

- organisation:

  GitHub organisation name. If NULL (default), creates repo under your
  personal account

- private:

  Logical. If TRUE (default), creates a private repository

- description:

  Repository description. If NULL, auto-generated from directory name

## Value

GitHub repository URL (invisibly)

## Requirements

- **GitHub authentication**: Set up via GITHUB_PAT, gh CLI, or git
  credentials

- **R package 'gh'**: Install with `install.packages("gh")`

- **R package 'gert'**: Install with `install.packages("gert")`

## Authentication

This function needs credentials for both:

- **GitHub API access**: used by `gh` to check your account and create
  the repository

- **Git transport authentication**: used by `gert` to push the local
  repository to GitHub

Recommended setup:

- **GITHUB_PAT environment variable**: recommended for HTTPS
  authentication and works for both GitHub API calls and Git pushes to
  GitHub

- **Stored Git credentials**: credentials in your OS credential store or
  Git credential helper can also work for both the API and push steps

- **gh CLI**: may help set up GitHub authentication, but you may still
  need Git credentials available for the final push

- **SSH**: supported when your remote/auth setup is configured
  accordingly

To create a Personal Access Token (PAT):

1.  Visit <https://github.com/settings/tokens>

2.  Generate a token with `repo` scope

3.  Set for current session:
    `Sys.setenv(GITHUB_PAT = "your_token_here")`

4.  Or add to .Renviron: `GITHUB_PAT='your_token_here'` and restart R

## Examples

``` r
if (FALSE) { # \dontrun{
library(OmopStudyBuilder)
library(here)

# Authenticate (choose one method):
# 1. Set GITHUB_PAT for current session
Sys.setenv(GITHUB_PAT = "your_token_here")
# 2. Or use gh CLI: gh auth login

# Create repo under personal account
linkGitHub(
  directory = here::here(),
  repository = "my-omop-study"
)

# Create repo under organisation
linkGitHub(
  directory = here::here(),
  repository = "diabetes-study",
  organisation = "oxford-pharmacoepi",
  private = TRUE
)
} # }
```
