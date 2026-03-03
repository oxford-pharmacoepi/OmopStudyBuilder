test_that("sanitizeRepoName converts names correctly", {
  expect_equal(sanitizeRepoName("My Study"), "my-study")
  expect_equal(sanitizeRepoName("-study-"), "study")
  expect_equal(sanitizeRepoName("Special@#$Chars"), "specialchars")
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
    "repository name is required"
  )
})

test_that("createStudyGitIgnore creates .gitignore file", {
  temp_dir <- file.path(tempdir(), "test_gitignore")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  createStudyGitIgnore(temp_dir)
  expect_true(file.exists(file.path(temp_dir, ".gitignore")))
})
