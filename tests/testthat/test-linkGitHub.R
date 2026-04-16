test_that("validateRepoName accepts valid names", {
  # Valid names should not throw errors
  expect_silent(OmopStudyBuilder:::validateRepoName("my-study"))
  expect_silent(OmopStudyBuilder:::validateRepoName("MyStudy"))
  expect_silent(OmopStudyBuilder:::validateRepoName("my_study"))
  expect_silent(OmopStudyBuilder:::validateRepoName("my.study"))
  expect_silent(OmopStudyBuilder:::validateRepoName("study-123"))
  expect_silent(OmopStudyBuilder:::validateRepoName("Study_2024.v1"))
})

test_that("validateRepoName rejects invalid names", {
  # Spaces
  expect_error(
    OmopStudyBuilder:::validateRepoName("My Study"),
    "Repository name cannot contain spaces"
  )

  # Special characters
  expect_error(
    OmopStudyBuilder:::validateRepoName("study@2024"),
    "Repository name contains invalid characters"
  )

  expect_error(
    OmopStudyBuilder:::validateRepoName("study#test"),
    "invalid characters"
  )

  # Leading hyphen
  expect_error(
    OmopStudyBuilder:::validateRepoName("-study"),
    "cannot start or end with hyphen"
  )

  # Trailing hyphen
  expect_error(
    OmopStudyBuilder:::validateRepoName("study-"),
    "cannot start or end with hyphen"
  )

  # Too long (>30 chars)
  long_name <- paste0(rep("a", 31), collapse = "")
  expect_error(
    OmopStudyBuilder:::validateRepoName(long_name),
    "Repository name too long"
  )
})

test_that("linkGitHub fails on non-existent directory", {
  expect_error(
    linkGitHub(
      directory = "nonexistent_dir_12345",
      repository = "test-repo"
    ),
    "Directory does not exist"
  )
})

test_that("linkGitHub requires repository parameter", {
  temp_dir <- tempdir()
  expect_error(
    linkGitHub(directory = temp_dir),
    "missing, with no default"
  )
})

test_that("createStudyGitIgnore creates .gitignore file", {
  temp_dir <- file.path(tempdir(), "test_gitignore")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  OmopStudyBuilder:::createStudyGitIgnore(temp_dir)
  expect_true(file.exists(file.path(temp_dir, ".gitignore")))
})

test_that("createStudyGitIgnore has correct content", {
  temp_dir <- file.path(tempdir(), "test_gitignore_content")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  OmopStudyBuilder:::createStudyGitIgnore(temp_dir)
  content <- readLines(file.path(temp_dir, ".gitignore"))

  # Check for essential patterns
  expect_true(any(grepl("results/", content, fixed = TRUE)))
  expect_true(any(grepl(".Rproj.user", content, fixed = TRUE)))
  expect_true(any(grepl(".RData", content, fixed = TRUE)))
  expect_true(any(grepl("renv/library/", content, fixed = TRUE)))
  expect_true(any(grepl(".DS_Store", content, fixed = TRUE)))
  expect_true(any(grepl("Dockerfile", content, fixed = TRUE)))

  # Check for comments
  expect_true(any(grepl("# R artifacts", content, fixed = TRUE)))
  expect_true(any(grepl("# Study outputs", content, fixed = TRUE)))
})

test_that("createStudyGitIgnore merges with existing .gitignore", {
  temp_dir <- file.path(tempdir(), "test_gitignore_merge")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create existing .gitignore with custom content
  existing_content <- c("custom_file.txt", "my_folder/", "*.log")
  writeLines(existing_content, file.path(temp_dir, ".gitignore"))

  # Add study .gitignore
  OmopStudyBuilder:::createStudyGitIgnore(temp_dir)

  # Read final content
  final_content <- readLines(file.path(temp_dir, ".gitignore"))

  # Check custom content preserved
  expect_true("custom_file.txt" %in% final_content)
  expect_true("my_folder/" %in% final_content)
  expect_true("*.log" %in% final_content)

  # Check study content added
  expect_true(any(grepl("results/", final_content, fixed = TRUE)))
  expect_true(any(grepl(".Rproj.user", final_content, fixed = TRUE)))

  # Check no duplicates created
  expect_equal(sum(final_content == "custom_file.txt"), 1)
})

test_that("createStudyGitIgnore handles duplicate entries gracefully", {
  temp_dir <- file.path(tempdir(), "test_gitignore_duplicates")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create existing .gitignore with some study patterns already present
  existing_content <- c(".RData", "results/", "custom.txt")
  writeLines(existing_content, file.path(temp_dir, ".gitignore"))

  # Add study .gitignore
  OmopStudyBuilder:::createStudyGitIgnore(temp_dir)

  # Read final content
  final_content <- readLines(file.path(temp_dir, ".gitignore"))

  # Check no duplicates of existing entries
  expect_equal(sum(final_content == ".RData"), 1)
  expect_equal(sum(final_content == "results/"), 1)

  # Check custom entry preserved
  expect_true("custom.txt" %in% final_content)
})

test_that("createStudyGitIgnore trims whitespace before checking duplicates", {
  temp_dir <- file.path(tempdir(), "test_gitignore_trimws")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  existing_content <- c(".RData   ", "  results/**/*.csv", "custom.txt  ")
  writeLines(existing_content, file.path(temp_dir, ".gitignore"))

  OmopStudyBuilder:::createStudyGitIgnore(temp_dir)

  final_content <- readLines(file.path(temp_dir, ".gitignore"))

  expect_equal(sum(final_content == ".RData"), 1)
  expect_equal(sum(final_content == "results/**/*.csv"), 1)
  expect_true("custom.txt" %in% final_content)
})

test_that("ensureGitIdentity errors when git identity is missing and user info is absent", {
  skip_if_not_installed("gert")

  temp_dir <- tempfile("test_git_identity_missing-")
  temp_home <- tempfile("git-home-")
  old_home <- Sys.getenv("HOME", unset = NA_character_)
  old_userprofile <- Sys.getenv("USERPROFILE", unset = NA_character_)

  on.exit({
    unlink(temp_dir, recursive = TRUE)
    unlink(temp_home, recursive = TRUE)
    Sys.unsetenv(c("GIT_CONFIG_NOSYSTEM", "XDG_CONFIG_HOME", "HOME", "USERPROFILE"))

    if (!is.na(old_home)) {
      Sys.setenv(HOME = old_home)
    }

    if (!is.na(old_userprofile)) {
      Sys.setenv(USERPROFILE = old_userprofile)
    }
  }, add = TRUE)

  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(temp_home, showWarnings = FALSE, recursive = TRUE)
  Sys.setenv(
    GIT_CONFIG_NOSYSTEM = "1",
    XDG_CONFIG_HOME = temp_home,
    HOME = temp_home,
    USERPROFILE = temp_home
  )

  try(gert::git_config_global_set("user.name", NULL), silent = TRUE)
  try(gert::git_config_global_set("user.email", NULL), silent = TRUE)
  gert::git_init(temp_dir)

  expect_error(
    OmopStudyBuilder:::ensureGitIdentity(temp_dir, NULL),
    "Git identity not configured and no GitHub user information was provided"
  )
})

test_that("setupGitRemote pushes the actual local branch to a bare remote", {
  skip_if_not_installed("gert")

  repo_dir <- tempfile("test-linkGitHub-local-")
  remote_dir <- tempfile("test-linkGitHub-remote-")

  on.exit({
    unlink(repo_dir, recursive = TRUE)
    unlink(remote_dir, recursive = TRUE)
  }, add = TRUE)

  dir.create(repo_dir, recursive = TRUE)
  dir.create(remote_dir, recursive = TRUE)

  gert::git_init(repo_dir)
  gert::git_init(remote_dir, bare = TRUE)
  gert::git_config_set("user.name", "Test User", repo = repo_dir)
  gert::git_config_set("user.email", "test@example.com", repo = repo_dir)

  writeLines("hello", file.path(repo_dir, "README.md"))

  expect_no_error(
    OmopStudyBuilder:::setupGitRemote(
      directory = repo_dir,
      clone_url = remote_dir,
      default_branch = "master",
      user_info = list(login = "test", name = "Test User", email = "test@example.com")
    )
  )

  remote_branches <- gert::git_branch_list(repo = remote_dir)
  expect_true("master" %in% remote_branches$name)
})

test_that("linkGitHub validates directory parameter", {
  expect_error(
    linkGitHub(directory = NULL, repository = "test"),
    class = "error"
  )

  expect_error(
    linkGitHub(directory = "", repository = "test"),
    "Directory does not exist"
  )
})

test_that("linkGitHub validates repository parameter", {
  temp_dir <- tempdir()

  expect_error(
    linkGitHub(directory = temp_dir, repository = NULL),
    "can not be NULL"
  )

  expect_error(
    linkGitHub(directory = temp_dir, repository = ""),
    "must have at least 1 characters"
  )
})

# ============================================================================
# MANUAL/INTEGRATION TESTS
# These tests require real GitHub authentication and network access
# Run manually with: testthat::test_file("test-linkGitHub.R", filter = "manual")
# ============================================================================

test_that("linkGitHub full workflow (MANUAL TEST - requires GITHUB_PAT)", {
  skip_if_not(Sys.getenv("RUN_MANUAL_TESTS") == "true", 
              "Manual test - set RUN_MANUAL_TESTS=true to run (requires GITHUB_PAT)")
  skip_if_not(nzchar(Sys.getenv("GITHUB_PAT")), "GITHUB_PAT not set")
  skip_if_not_installed("gh")

  # WARNING: This creates a real GitHub repository!
  # You'll need to delete it manually after testing
  test_repo_name <- paste0("test-omop-study-", format(Sys.time(), "%Y%m%d-%H%M%S"))
  temp_dir <- file.path(tempdir(), test_repo_name)
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create a dummy file to commit
  writeLines("# Test Study", file.path(temp_dir, "README.md"))

  # Run linkGitHub
  result <- linkGitHub(
    directory = temp_dir,
    repository = test_repo_name,
    organisation = NULL,  # Creates under personal account
    private = TRUE
  )

  # Verify result
  expect_true(grepl("github.com", result))
  expect_true(grepl(test_repo_name, result))

  # Cleanup: Delete the test repository
  # NOTE: You may need to delete this manually from GitHub
  message("Test repository created: ", result)
  message("Please delete manually from GitHub if needed")
})

test_that("initStudy with GitHub integration (MANUAL TEST)", {
  skip_if_not(Sys.getenv("RUN_MANUAL_TESTS") == "true", 
              "Manual test - set RUN_MANUAL_TESTS=true to run (requires GITHUB_PAT)")
  skip_if_not(nzchar(Sys.getenv("GITHUB_PAT")), "GITHUB_PAT not set")
  skip_if_not_installed("gh")

  test_repo_name <- paste0("test-init-study-", format(Sys.time(), "%Y%m%d-%H%M%S"))
  temp_dir <- file.path(tempdir(), test_repo_name)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create study with GitHub integration
  initStudy(
    directory = temp_dir,
    studyTitle = "Test Study",
    studyLeads = "Test Lead",
    repository = test_repo_name,
    organisation = NULL,
    private = TRUE
  )

  # Verify directory structure
  expect_true(dir.exists(temp_dir))
  expect_true(file.exists(file.path(temp_dir, "README.md")))
  expect_true(dir.exists(file.path(temp_dir, ".git")))
  expect_true(file.exists(file.path(temp_dir, ".gitignore")))

  message("Test repository created: https://github.com/YOUR_USERNAME/", test_repo_name)
  message("Please delete manually from GitHub")
})
