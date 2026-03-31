test_that("reviewStudy summarises code files", {
  temp_dir <- here::here(tempdir(), omopgenerics::uniqueTableName())
  dir.create(path = temp_dir, recursive = TRUE)
  initStudy(directory = temp_dir)
  expect_no_error(
    reviewStudy(here::here(temp_dir, "diagnosticsCode"), code = TRUE, dependencies = FALSE)
  )
})

test_that("reviewStudy summarises dependencies", {
  temp_dir <- here::here(tempdir(), omopgenerics::uniqueTableName())
  dir.create(path = temp_dir, recursive = TRUE)
  initStudy(directory = temp_dir)

  # no renv yet — should warn that renv.lock is missing
  expect_warning(
    reviewStudy(here::here(temp_dir, "diagnosticsCode"), code = FALSE, dependencies = TRUE)
  )

  renv::init(here::here(temp_dir, "diagnosticsCode"),
    restart = FALSE,
    load = FALSE
  )
  expect_no_error(
    reviewStudy(here::here(temp_dir, "diagnosticsCode"), code = FALSE, dependencies = TRUE)
  )
})

test_that("reviewStudy works for both code and dependencies", {
  temp_dir <- here::here(tempdir(), omopgenerics::uniqueTableName())
  dir.create(path = temp_dir, recursive = TRUE)
  initStudy(directory = temp_dir)

  renv::init(here::here(temp_dir, "diagnosticsCode"),
    restart = FALSE,
    load = FALSE
  )
  expect_no_error(
    reviewStudy(here::here(temp_dir, "diagnosticsCode"), code = TRUE, dependencies = TRUE)
  )
})
