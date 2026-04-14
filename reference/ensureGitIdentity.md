# Ensure Git identity is configured

Checks if Git user.name and user.email are configured for commits. If
not configured, automatically sets them using GitHub account info
provided by the caller. This allows seamless Git operations without
requiring manual \`git config\` setup.

## Usage

``` r
ensureGitIdentity(directory, user_info = NULL)
```
