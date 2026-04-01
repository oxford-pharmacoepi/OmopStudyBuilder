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
#'   \item Add to .Renviron: \code{GITHUB_PAT='your_token_here'}
#'   \item Restart R
#' }
#'
#' @param directory Path to study directory
#' @param repository GitHub repository name (will be sanitized if needed)
#' @param organisation GitHub organisation name. If NULL (default), creates repo
#'   under your personal account
#' @param private Logical. If TRUE (default), creates a private repository
#' @param description Repository description. If NULL, auto-generated from directory name
#'
#' @return GitHub repository URL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' # Create repo under personal account
#' linkGitHub(
#'   directory = "MyStudy",
#'   repository = "my-study"
#' )
#'
#' # Create repo under organisation
#' linkGitHub(
#'   directory = "MyStudy",
#'   repository = "diabetes-study",
#'   organisation = "oxford-pharmacoepi",
#'   private = TRUE
#' )
#' }
linkGitHub <- function(directory,
                       repository,
                       organisation = NULL,
                       private = TRUE,
                       description = NULL) {
  
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
  
  # Sanitize repository name
  repo_clean <- sanitizeRepoName(repository)
  if (repo_clean != repository) {
    cli::cli_alert_info("Repository name sanitized: {.val {repository}} -> {.val {repo_clean}}")
  }
  
  # Check authentication
  cli::cli_alert_info("Checking GitHub authentication...")
  user_info <- checkGitHubAuth()
  owner <- if (!is.null(organisation)) organisation else user_info$login
  
  # Check if repo available
  if (!checkRepoAvailable(owner, repo_clean)) {
    cli::cli_abort("Repository {.val {owner}/{repo_clean}} already exists on GitHub")
  }
  
  # Initialize git locally
  cli::cli_alert_info("Initializing git repository...")
  default_branch <- initializeGitRepo(directory)
  
  # Create .gitignore
  createStudyGitIgnore(directory)
  
  # Create GitHub repository
  cli::cli_alert_info("Creating GitHub repository: {.val {owner}/{repo_clean}}")
  if (is.null(description)) {
    description <- paste("OMOP CDM study:", basename(directory))
  }
  repo_info <- createGitHubRepo(repo_clean, organisation, private, description)
  repo_url <- repo_info$html_url
  
  # Setup remote and push
  cli::cli_alert_info("Pushing to GitHub...")
  setupGitRemote(directory, repo_info$clone_url, default_branch, user_info)
  
  cli::cli_alert_success("Study linked to GitHub: {.url {repo_url}}")
  return(invisible(repo_url))
}


#' Check GitHub authentication
#' @keywords internal
checkGitHubAuth <- function() {
  tryCatch(
    {
      user <- gh::gh("GET /user")
      cli::cli_alert_success("Authenticated as: {.val {user$login}}")
      return(user)
    },
    error = function(e) {
      cli::cli_abort(c(
        "GitHub authentication failed",
        "i" = "Set GITHUB_PAT environment variable with a personal access token",
        "i" = "Create token at: https://github.com/settings/tokens",
        "i" = "Add to .Renviron: GITHUB_PAT='your_token_here'",
        "x" = conditionMessage(e)
      ))
    }
  )
}


#' Check if repository name is available
#' @keywords internal
checkRepoAvailable <- function(owner, repo) {
  result <- tryCatch(
    {
      gh::gh("GET /repos/{owner}/{repo}", owner = owner, repo = repo)
      FALSE  # Repo exists
    },
    error = function(e) {
      if (grepl("404", conditionMessage(e))) {
        TRUE  # Repo doesn't exist (available)
      } else {
        cli::cli_abort("Failed to check repository availability: {conditionMessage(e)}")
      }
    }
  )
  return(result)
}


#' Sanitize repository name for GitHub
#' @keywords internal
sanitizeRepoName <- function(name) {
  # Convert to lowercase
  name <- tolower(name)
  # Replace spaces and underscores with hyphens
  name <- gsub("\\s+", "-", name)
  name <- gsub("_+", "-", name)
  # Remove invalid characters (keep only alphanumeric and hyphens)
  name <- gsub("[^a-z0-9\\-]", "", name)
  # Remove leading hyphens
  name <- gsub("^-+", "", name)
  # Remove trailing hyphens
  name <- gsub("-+$", "", name)
  # Collapse multiple hyphens
  name <- gsub("-+", "-", name)
  # Truncate to 100 characters
  if (nchar(name) > 100) name <- substr(name, 1, 100)
  # Ensure not empty
  if (!nzchar(name)) name <- "study"
  return(name)
}


#' Create GitHub repository via API
#' @keywords internal
createGitHubRepo <- function(repository, organisation, private, description) {
  tryCatch(
    {
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
      return(repo)
    },
    error = function(e) {
      cli::cli_abort(c(
        "Failed to create GitHub repository",
        "x" = conditionMessage(e)
      ))
    }
  )
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
  if (is.null(user_info)) user_info <- tryCatch(gh::gh("GET /user"), error = function(e) NULL)
  if (is.null(user_info)) {
    cli::cli_abort(c("Git identity not configured",
                     "i" = "Run: git config --global user.name 'Your Name'",
                     "i" = "Run: git config --global user.email 'your@email.com'"))
  }
  
  # Configure user.name (use name or fallback to login)
  if (is.null(get_config("user.name"))) {
    name <- if (!is.null(user_info$name) && nzchar(user_info$name)) user_info$name else user_info$login
    system2("git", c("-C", shQuote(directory), "config", "user.name", shQuote(name)), 
            stdout = FALSE, stderr = FALSE)
    cli::cli_alert_info("Configured Git user.name: {.val {name}}")
  }
  
  # Configure user.email
  if (is.null(get_config("user.email")) && !is.null(user_info$email) && nzchar(user_info$email)) {
    system2("git", c("-C", shQuote(directory), "config", "user.email", shQuote(user_info$email)), 
            stdout = FALSE, stderr = FALSE)
    cli::cli_alert_info("Configured Git user.email: {.val {user_info$email}}")
  }
  
  return(invisible(TRUE))
}


#' Setup git remote and push
#' @keywords internal
setupGitRemote <- function(directory, clone_url, default_branch, user_info = NULL) {
  orig_wd <- getwd()
  on.exit(setwd(orig_wd), add = TRUE)
  setwd(directory)
  
  # Ensure Git identity is configured (uses GitHub account info if available)
  ensureGitIdentity(directory, user_info)
  
  # Setup remote (update if exists, add if new)
  existing_remote <- system2("git", c("remote", "get-url", "origin"), 
                            stdout = TRUE, stderr = FALSE)
  if (!is.null(existing_remote) && length(existing_remote) > 0 && nzchar(existing_remote[1])) {
    # Remote exists - update it
    system2("git", c("remote", "set-url", "origin", clone_url), stdout = FALSE, stderr = FALSE)
    cli::cli_alert_info("Updated existing git remote to: {.val {clone_url}}")
  } else {
    # No remote - add it
    system2("git", c("remote", "add", "origin", clone_url), stdout = FALSE, stderr = FALSE)
  }
  
  # Stage all files
  result <- system2("git", c("add", "-A"), stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    cli::cli_abort("Failed to stage files: {paste(result, collapse = '\n')}")
  }
  
  # Commit
  commit_msg <- paste0("Initialize study: ", basename(directory))
  result <- system2("git", c("commit", "-m", shQuote(commit_msg)), stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status")
  if (!is.null(status) && status != 0) {
    # Check if it's just "nothing to commit"
    if (!any(grepl("nothing to commit", result, ignore.case = TRUE))) {
      cli::cli_abort("Failed to commit: {paste(result, collapse = '\n')}")
    }
  }
  
  # Push
  result <- system2("git", c("push", "-u", "origin", default_branch), stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    cli::cli_abort("Failed to push to GitHub: {paste(result, collapse = '\n')}")
  }
  
  return(invisible(TRUE))
}


#' Get default branch name
#' @keywords internal
getDefaultBranch <- function(directory) {
  # Try to get from git config
  result <- suppressWarnings(
    system2("git", c("-C", shQuote(directory), "config", "--get", "init.defaultBranch"),
            stdout = TRUE, stderr = FALSE)
  )
  
  if (!is.null(attr(result, "status")) || length(result) == 0 || !nzchar(result[1])) {
    return("main")
  }
  
  return(trimws(result[1]))
}
