test_that("reviewStudy summarises dependencies", {

  temp_dir <- here::here(tempdir(), omopgenerics::uniqueTableName())
  dir.create(path = temp_dir, recursive = TRUE)
  initStudy(directory = temp_dir)

  # no renv yet — should warn that renv.lock is missing
  expect_warning(reviewStudy(here::here(temp_dir, "diagnostics_code"), code = FALSE, dependencies = TRUE))

  renv::init(here::here(temp_dir, "diagnostics_code"),
             restart = FALSE,
             load = FALSE)
  expect_no_error(
    reviewStudy(here::here(temp_dir, "diagnostics_code"), code = FALSE, dependencies = TRUE)
  )

})
