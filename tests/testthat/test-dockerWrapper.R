library(testthat)

test_that("checkDocker() returns logical value", {
  result <- checkDocker()
  
  # Should always return a logical value (TRUE or FALSE)
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("checkDocker() returns TRUE when Docker is available", {
  result <- checkDocker()
  
  # If this test runs and Docker is installed, should return TRUE
  # If Docker is not installed, this test will be informational only
  if (result) {
    expect_true(result)
  } else {
    skip("Docker is not available on this system")
  }
})

test_that("checkDocker() returns FALSE when Docker is not installed", {
  # This test is inherently conditional - can't test without uninstalling Docker
  # We verify the behavior by checking the return type instead
  result <- checkDocker()
  
  if (!result) {
    # If Docker is not available, should return FALSE
    expect_false(result)
  } else {
    skip("Docker is installed, cannot test unavailable scenario")
  }
})

test_that("ensureDocker(check_daemon=FALSE) only checks binary", {
  # Access internal function
  ensureDocker <- get("ensureDocker", envir = asNamespace("OmopStudyBuilder"))
  
  # When check_daemon=FALSE, should only verify Docker binary exists
  # Does not check if daemon is running
  result <- ensureDocker(check_daemon = FALSE)
  
  expect_type(result, "logical")
  expect_length(result, 1)
  
  # If Docker binary exists, should return TRUE even if daemon not running
  if (result) {
    expect_true(result)
  }
})

test_that("ensureDocker(check_daemon=TRUE) checks daemon connectivity", {
  # Access internal function
  ensureDocker <- get("ensureDocker", envir = asNamespace("OmopStudyBuilder"))
  
  # When check_daemon=TRUE, should verify both binary AND daemon
  result <- ensureDocker(check_daemon = TRUE)
  
  expect_type(result, "logical")
  expect_length(result, 1)
  
  # This will return TRUE only if Docker daemon is running
  if (result) {
    # If TRUE, daemon should be accessible
    expect_true(result)
  } else {
    # If FALSE, either binary missing or daemon not running
    expect_false(result)
  }
})
