# GitHub Integration for OMOP Studies

#' Check if Git is installed
#' @return TRUE if Git is available, throws error otherwise
#' @keywords internal
ensureGit <- function() {
  git_bin <- Sys.which("git")
  if (!nzchar(git_bin)) {
    cli::cli_abort(c(
      "Git not found on PATH",
      "i" = "Git is required for GitHub integration",
      "i" = "Install Git from: {.url https://git-scm.com/downloads}",
      "i" = "After installing, restart R"
    ))
  }
  return(invisible(TRUE))
}


#' Link study directory to GitHub repository
#'
#' Creates a new GitHub repository and connects it to an existing study directory.
#' Initializes git, creates .gitignore, commits all files, and pushes to GitHub.
#' 
#' @section Requirements:
#' \itemize{
#'   \item \strong{Git must be installed}: Download from \url{https://git-scm.com/downloads}
#'   \item \strong{GitHub Personal Access Token (PAT)}: Set \code{GITHUB_PAT} environment variable
#'   \item \strong{R package 'gh'}: Install with \code{install.packages("gh")}
#' }
#' 
#' @section Authentication:
#' This function uses your GitHub Personal Access Token (PAT) for authentication.
#' If Git user identity is not configured, it will automatically be set using your
#' GitHub account information from the PAT.
#' 
#' To set up authentication:
#' \enumerate{
#'   \item Create a PAT at \url{https://github.com/settings/tokens}
#'   \item Set for current session: \code{Sys.setenv(GITHUB_PAT = "your_token_here")}
#'   \item Or add to .Renviron: \code{GITHUB_PAT='your_token_here'} and restart R
#' }
#'
#' @param directory Path to study directory
#' @param repository GitHub repository name (will be sanitized if needed)
#' @param organisation GitHub organisation name. If NULL (default), creates repo
#'   under your personal account
#' @param private Logical. If TRUE (default), creates a private repository
#' @param description Repository description. If NULL, auto-generated from directory name
#' @param createIssue Logical. If TRUE (default), creates a code review checklist issue
#'   in the newly created GitHub repository
#'
#' @return GitHub repository URL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' library(OmopStudyBuilder)
#' 
#' # Set GitHub token for current session
#' Sys.setenv(GITHUB_PAT = "your_token_here")
#' 
#' # Create repo under personal account
#' linkGitHub(
#'   directory = here::here(),
#'   repository = "my-omop-study"
#' )
#'
#' # Create repo under organisation
#' linkGitHub(
#'   directory = here::here(),
#'   repository = "diabetes-study",
#'   organisation = "oxford-pharmacoepi",
#'   private = TRUE
#' )
#' }
linkGitHub <- function(directory,
                       repository,
                       organisation = NULL,
                       private = TRUE,
                       description = NULL,
                       createIssue = TRUE) {
  
  # Check Git is installed
  ensureGit()
  
  # Validate inputs
  if (!dir.exists(directory)) {
    cli::cli_abort("Directory does not exist: {.path {directory}}")
  }
  directory <- normalizePath(directory, mustWork = TRUE)
  
  if (missing(repository) || is.null(repository) || !nzchar(repository)) {
    cli::cli_abort("repository name is required")
  }
  
  # Check gh package
  if (!requireNamespace("gh", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg gh} is required for GitHub integration",
      "i" = "Install it with: install.packages('gh')"
    ))
  }
  
  # Validate repository name
  validateRepoName(repository)
  
  # Check authentication
  cli::cli_alert_info("Checking GitHub authentication...")
  user_info <- checkGitHubAuth()
  owner <- if (!is.null(organisation)) organisation else user_info$login
  
  # Check if repo available
  if (!checkRepoAvailable(owner, repository)) {
    cli::cli_abort("Repository {.val {owner}/{repository}} already exists on GitHub")
  }
  
  # Initialize git locally
  cli::cli_alert_info("Initializing git repository...")
  default_branch <- initializeGitRepo(directory)
  
  # Create .gitignore
  createStudyGitIgnore(directory)
  
  # Create GitHub repository
  cli::cli_alert_info("Creating GitHub repository: {.val {owner}/{repository}}")
  if (is.null(description)) {
    description <- paste("OMOP CDM study:", basename(directory))
  }
  repo_info <- createGitHubRepo(repository, organisation, private, description)
  repo_url <- repo_info$html_url
  
  # Setup remote and push
  cli::cli_alert_info("Pushing to GitHub...")
  result <- setupGitRemote(directory, repo_info$clone_url, default_branch, user_info)
  
  # Check if user cancelled
  if (is.null(result)) {
    cli::cli_alert_warning("GitHub repository created but not linked to local directory")
    cli::cli_alert_info("Repository URL: {.url {repo_url}}")
    cli::cli_alert_info("To link later, run: git remote add origin {repo_info$clone_url}")
    return(invisible(NULL))
  }
  
  cli::cli_alert_success("Study linked to GitHub: {.url {repo_url}}")
  
  # Create code review checklist issue
  if (isTRUE(createIssue)) {
    cli::cli_alert_info("Creating code review checklist issue...")
    issue_result <- try(createCodeReviewIssue(owner, repository), silent = TRUE)
    
    if (inherits(issue_result, "try-error")) {
      cli::cli_alert_warning("Failed to create code review checklist issue")
      cli::cli_alert_info("You can manually create an issue with the checklist later")
    }
  }
  
  return(invisible(repo_url))
}


#' Check GitHub authentication
#' @keywords internal
checkGitHubAuth <- function() {
  # Check if GITHUB_PAT is set
  if (!nzchar(Sys.getenv("GITHUB_PAT"))) {
    cli::cli_abort(c(
      "GitHub authentication failed",
      "i" = "Set GITHUB_PAT environment variable with a personal access token",
      "i" = "Create token at: {.url https://github.com/settings/tokens}",
      "i" = "For current session, run: {.code Sys.setenv(GITHUB_PAT = \"your_token_here\")}",
      "i" = "Or add to .Renviron: {.code GITHUB_PAT='your_token_here'}"
    ))
  }
  
  user <- gh::gh("GET /user")
  if (is.null(user) || is.null(user$login)) {
    cli::cli_abort("Failed to fetch GitHub user information")
  }
  
  cli::cli_alert_success("Authenticated as: {.val {user$login}}")
  return(user)
}


#' Check if repository name is available
#' @keywords internal
checkRepoAvailable <- function(owner, repo) {
  # Try to get the repo - if it exists, gh will return the repo info
  # If it doesn't exist, gh will throw a 404 error
  repo_check <- try(gh::gh("GET /repos/{owner}/{repo}", owner = owner, repo = repo), silent = TRUE)
  
  if (inherits(repo_check, "try-error")) {
    error_msg <- attr(repo_check, "condition")$message
    if (grepl("404", error_msg)) {
      return(TRUE)  # Repo doesn't exist (available)
    } else {
      cli::cli_abort("Failed to check repository availability: {error_msg}")
    }
  }
  
  return(FALSE)  # Repo exists
}


#' Validate repository name for GitHub
#'
#' Checks if a repository name follows GitHub naming rules.
#' Throws an error with helpful suggestions if invalid.
#'
#' @param name Repository name to validate
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validateRepoName <- function(name) {
  # Check not empty
  if (!nzchar(name)) {
    cli::cli_abort(c(
      "Repository name cannot be empty",
      "i" = "Choose a descriptive name like 'my-study' or 'diabetes-analysis'"
    ))
  }
  
  # Check length
  if (nchar(name) > 100) {
    cli::cli_abort(c(
      "Repository name too long: {nchar(name)} characters (max 100)",
      "i" = "Shorten to: {.val {substr(name, 1, 100)}}"
    ))
  }
  
  # Check for spaces
  if (grepl("\\s", name)) {
    cli::cli_abort(c(
      "Repository name cannot contain spaces: {.val {name}}",
      "i" = "Use hyphens instead: {.val {gsub('\\\\s+', '-', name)}}"
    ))
  }
  
  # Check for invalid characters (allow letters, numbers, hyphens, underscores, dots)
  if (grepl("[^a-zA-Z0-9._-]", name)) {
    cli::cli_abort(c(
      "Repository name contains invalid characters: {.val {name}}",
      "i" = "Only letters, numbers, hyphens (-), underscores (_), and periods (.) are allowed"
    ))
  }
  
  # Check leading/trailing hyphens
  if (grepl("^-|-$", name)) {
    cli::cli_abort(c(
      "Repository name cannot start or end with hyphen: {.val {name}}",
      "i" = "Remove leading/trailing hyphens"
    ))
  }
  
  return(invisible(TRUE))
}


#' Create GitHub repository via API
#' @keywords internal
createGitHubRepo <- function(repository, organisation, private, description) {
  if (is.null(organisation)) {
    # Create under user account
    repo <- gh::gh(
      "POST /user/repos",
      name = repository,
      private = private,
      description = description,
      auto_init = FALSE
    )
  } else {
    # Create under organisation
    repo <- gh::gh(
      "POST /orgs/{org}/repos",
      org = organisation,
      name = repository,
      private = private,
      description = description,
      auto_init = FALSE
    )
  }
  
  if (is.null(repo) || is.null(repo$clone_url)) {
    cli::cli_abort("Failed to create GitHub repository - invalid response")
  }
  
  return(repo)
}


#' Initialize local git repository
#' @keywords internal
initializeGitRepo <- function(directory) {
  git_dir <- file.path(directory, ".git")
  
  if (!dir.exists(git_dir)) {
    # Initialize git
    result <- system2("git", c("init", shQuote(directory)), stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
      cli::cli_abort("Failed to initialize git repository: {paste(result, collapse = '\n')}")
    }
  }
  
  # Get default branch name
  default_branch <- getDefaultBranch(directory)
  
  return(default_branch)
}


#' Create .gitignore for OMOP studies
#' @keywords internal
createStudyGitIgnore <- function(directory) {
  gitignore_path <- file.path(directory, ".gitignore")
  
  gitignore_content <- c(
    "# R artifacts",
    ".Rproj.user",
    ".RData",
    ".Rhistory",
    ".Ruserdata",
    "",
    "# Study outputs (don't commit results)",
    "results/",
    "",
    "# renv library (only commit lockfile)",
    "renv/library/",
    "renv/staging/",
    "renv/local/",
    "",
    "# OS files",
    ".DS_Store",
    "Thumbs.db",
    "",
    "# Docker (generated files)",
    "Dockerfile",
    ".dockerignore"
  )
  
  if (file.exists(gitignore_path)) {
    existing <- readLines(gitignore_path, warn = FALSE)
    # Only add missing lines
    to_add <- setdiff(gitignore_content, existing)
    if (length(to_add) > 0) {
      writeLines(c(existing, "", to_add), gitignore_path)
    }
  } else {
    writeLines(gitignore_content, gitignore_path)
  }
  
  return(invisible(gitignore_path))
}


#' Ensure Git identity is configured
#' 
#' Checks if Git user.name and user.email are configured for commits.
#' If not configured, automatically sets them using GitHub account info
#' from the authenticated user (via GITHUB_PAT). This allows seamless
#' Git operations without requiring manual `git config` setup.
#' 
#' @keywords internal
ensureGitIdentity <- function(directory, user_info = NULL) {
  # Get current git config (returns empty on failure)
  get_config <- function(key) {
    result <- suppressWarnings(system2("git", c("-C", shQuote(directory), "config", key), 
                                       stdout = TRUE, stderr = FALSE))
    if (length(result) > 0 && nzchar(result[1])) return(result[1])
    return(NULL)
  }
  
  # Check if already configured
  if (!is.null(get_config("user.name")) && !is.null(get_config("user.email"))) {
    return(invisible(TRUE))
  }
  
  # Get GitHub user info if not provided
  if (is.null(user_info)) {
    user_fetch <- try(gh::gh("GET /user"), silent = TRUE)
    if (inherits(user_fetch, "try-error")) {
      user_info <- NULL
    } else {
      user_info <- user_fetch
    }
  }
  
  if (is.null(user_info)) {
    cli::cli_abort(c(
      "Git identity not configured and could not fetch from GitHub",
      "i" = "Run: git config --global user.name 'Your Name'",
      "i" = "Run: git config --global user.email 'your@email.com'"
    ))
  }
  
  # Configure user.name (use name or fallback to login)
  if (is.null(get_config("user.name"))) {
    name <- if (!is.null(user_info$name) && nzchar(user_info$name)) user_info$name else user_info$login
    system2("git", c("-C", shQuote(directory), "config", "user.name", shQuote(name)), 
            stdout = FALSE, stderr = FALSE)
    cli::cli_alert_info("Configured Git user.name: {.val {name}}")
  }
  
  # Configure user.email (with warning if unavailable)
  if (is.null(get_config("user.email"))) {
    if (!is.null(user_info$email) && nzchar(user_info$email)) {
      system2("git", c("-C", shQuote(directory), "config", "user.email", shQuote(user_info$email)), 
              stdout = FALSE, stderr = FALSE)
      cli::cli_alert_info("Configured Git user.email: {.val {user_info$email}}")
    } else {
      cli::cli_alert_warning("GitHub email is hidden or unavailable")
      cli::cli_alert_warning("Git commits may fail without configured email")
      cli::cli_alert_info("Run: git config --global user.email 'your@email.com'")
    }
  }
  
  return(invisible(TRUE))
}


#' Setup git remote and push
#' @keywords internal
setupGitRemote <- function(directory, clone_url, default_branch, user_info = NULL) {
  # Ensure Git identity is configured (uses GitHub account info if available)
  ensureGitIdentity(directory, user_info)
  
  # Setup remote (update if exists, add if new)
  existing_remote <- suppressWarnings(
    system2("git", c("-C", shQuote(directory), "remote", "get-url", "origin"), 
            stdout = TRUE, stderr = FALSE)
  )
  
  if (!is.null(existing_remote) && length(existing_remote) > 0 && nzchar(existing_remote[1])) {
    # Remote exists - ask user before updating
    cli::cli_alert_warning(
      "Directory already linked to: {.url {existing_remote[1]}}"
    )
    cli::cli_alert_warning(
      "This will switch remote to: {.url {clone_url}}"
    )
    
    # Check if running interactively
    if (!interactive()) {
      cli::cli_alert_info("Non-interactive session detected. Remote not changed.")
      cli::cli_alert_info("To update, run interactively or use: git remote set-url origin {clone_url}")
      return(invisible(NULL))
    }
    
    response <- readline("Continue? (y/n): ")
    if (!tolower(trimws(response)) %in% c("y", "yes")) {
      cli::cli_alert_info("Cancelled. Remote not changed.")
      return(invisible(NULL))
    }
    
    system2("git", c("-C", shQuote(directory), "remote", "set-url", "origin", clone_url), 
            stdout = FALSE, stderr = FALSE)
    cli::cli_alert_success("Remote updated successfully")
  } else {
    # No remote - add it
    system2("git", c("-C", shQuote(directory), "remote", "add", "origin", clone_url), 
            stdout = FALSE, stderr = FALSE)
  }
  
  # Stage all files
  result <- system2("git", c("-C", shQuote(directory), "add", "-A"), stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    cli::cli_abort("Failed to stage files: {paste(result, collapse = '\n')}")
  }
  
  # Commit
  commit_msg <- paste0("Initialize study: ", basename(directory))
  result <- system2("git", c("-C", shQuote(directory), "commit", "-m", shQuote(commit_msg)), 
                   stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status")
  if (!is.null(status) && status != 0) {
    # Check if it's just "nothing to commit"
    if (!any(grepl("nothing to commit", result, ignore.case = TRUE))) {
      cli::cli_abort("Failed to commit: {paste(result, collapse = '\n')}")
    }
  }
  
  # Check if there are any commits to push
  has_commits <- suppressWarnings(
    system2("git", c("-C", shQuote(directory), "rev-parse", "HEAD"), 
            stdout = FALSE, stderr = FALSE)
  )
  if (!is.null(attr(has_commits, "status")) && attr(has_commits, "status") != 0) {
    # No commits yet - create a README to ensure there's something to commit
    readme_path <- file.path(directory, "README.md")
    if (!file.exists(readme_path)) {
      readme_content <- c(
        paste0("# ", basename(directory)),
        "",
        "This is an OMOP CDM study repository.",
        "",
        paste0("Created: ", Sys.Date())
      )
      writeLines(readme_content, readme_path)
      
      # Stage and commit the README
      system2("git", c("-C", shQuote(directory), "add", "README.md"), 
              stdout = FALSE, stderr = FALSE)
      result <- system2("git", c("-C", shQuote(directory), "commit", "-m", 
                                 "Initialize repository with README"), 
                       stdout = TRUE, stderr = TRUE)
      if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
        cli::cli_abort("Failed to create initial commit: {paste(result, collapse = '\n')}")
      }
    }
  }
  
  # Push
  result <- system2("git", c("-C", shQuote(directory), "push", "-u", "origin", default_branch), 
                   stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    cli::cli_abort("Failed to push to GitHub: {paste(result, collapse = '\n')}")
  }
  
  return(invisible(TRUE))
}


#' Get default branch name
#' @keywords internal
getDefaultBranch <- function(directory) {
  # First check if repo has commits and what branch we're on
  current_branch <- suppressWarnings(
    system2("git", c("-C", shQuote(directory), "branch", "--show-current"),
            stdout = TRUE, stderr = FALSE)
  )
  
  # If we have a current branch, use it
  if (length(current_branch) > 0 && nzchar(current_branch[1])) {
    return(trimws(current_branch[1]))
  }
  
  # If no current branch (new repo), try to get from git config
  result <- suppressWarnings(
    system2("git", c("-C", shQuote(directory), "config", "--get", "init.defaultBranch"),
            stdout = TRUE, stderr = FALSE)
  )
  
  if (!is.null(attr(result, "status")) || length(result) == 0 || !nzchar(result[1])) {
    cli::cli_alert_info("No configured default branch. Using: {.val main}")
    return("main")
  }
  
  return(trimws(result[1]))
}


#' Get code review checklist template
#' @keywords internal
getCodeReviewChecklistBody <- function() {
  paste0(
    "## Code Review Checklist\n",
    "\n",
    "Complete these items before submitting study code for review.\n",
    "\n",
    "### Study Code Organisation\n",
    "- [ ] The study code is organised as an R project\n",
    "- [ ] There is a `code_to_run.R` file to run the study code\n",
    "- [ ] There is a `run_analysis.R` file to coordinate the study code\n",
    "- [ ] There is a `cohorts` directory with cohort definitions and `instantiate_cohorts.R` file\n",
    "- [ ] Independent analyses are split up into separate files (e.g., `incidence_analysis.R`)\n",
    "- [ ] There is a `results` directory for study results to be saved\n",
    "- [ ] Code is formatted and styled using `styler::style_dir()` and `lintr::lint_dir()`\n",
    "\n",
    "### Standard Analyses\n",
    "- [ ] Database snapshot using `OmopSketch::summariseOmopSnapshot()`\n",
    "- [ ] Observation period summary using `OmopSketch::summariseObservationPeriod()`\n",
    "- [ ] Cohort count summary using `CodelistGenerator::summariseCohortCodeUse()`\n",
    "- [ ] Cohort attrition summary using `CohortCharacteristics::summariseCohortCounts()`\n",
    "- [ ] Cohort attrition summary using `CohortCharacteristics::summariseCohortAttrition()`\n",
    "\n",
    "### Shiny App\n",
    "- [ ] There is a shiny app to review results\n",
    "- [ ] The app has sensible defaults for input choices\n",
    "- [ ] The app does not have redundant UI inputs\n",
    "- [ ] There is no particularly slow performance when generating tables or plots\n",
    "\n",
    "### Reproducibility\n",
    "- [ ] Renv is used to list all dependencies needed\n",
    "- [ ] Renv file only includes packages from CRAN\n",
    "- [ ] Study code renv does not include MASS or Matrix (dependency conflicts with R version)\n",
    "- [ ] Most recent versions of analytic packages are being used\n",
    "\n",
    "### Documentation\n",
    "- [ ] There is a central README explaining how to run the study code and review the results\n",
    "\n",
    "### Efficient and Robust Study Code\n",
    "- [ ] The code runs on synthetic test database\n",
    "- [ ] Ensure any warning messages that could be resolved when running\n",
    "- [ ] Check whether any code is repeated unnecessarily or could be simplified\n",
    "- [ ] Review the SQL that gets executed for any obvious inefficiencies\n",
    "- [ ] Review the renv lock - can any packages be removed?\n",
    "- [ ] Use `readr::read_csv` for importing CSVs and specify column types\n",
    "- [ ] Use `stringr` for string manipulation\n",
    "- [ ] Study code doesn't have a lot of complex, custom code unless unavoidable\n",
    "- [ ] Study code does not have if-else statements based on data partner names unless unavoidable\n",
    "- [ ] Output of the study is saved as a CSV\n",
    "\n",
    "### Review Results for Plausibility\n",
    "- [ ] Does the code and results shown match the protocol?\n",
    "- [ ] Have any analyses been missed?\n",
    "- [ ] For each analysis, are cohorts defined in the right way (e.g., typically no exclusion criteria for an incidence outcome)?\n",
    "\n",
    "---\n",
    "\n",
    "📚 **Reference:** [OxInfer Code Review Guide](https://oxford-pharmacoepi.github.io/Oxinfer/onboarding/code_review.html)\n"
  )
}


#' Create code review checklist issue on GitHub
#' @keywords internal
createCodeReviewIssue <- function(owner, repo) {
  checklist_body <- getCodeReviewChecklistBody()
  
  result <- gh::gh(
    "POST /repos/{owner}/{repo}/issues",
    owner = owner,
    repo = repo,
    title = "Code Review Checklist",
    body = checklist_body,
    labels = list("documentation", "code-review")
  )
  
  cli::cli_alert_success("Code review checklist issue created: {.url {result$html_url}}")
  return(invisible(result))
}
