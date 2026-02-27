test_that("reviewStudy summarises code files", {
  temp_dir <- here::here(tempdir(), omopgenerics::uniqueTableName())
  dir.create(path = temp_dir, recursive = TRUE)
  initStudy(directory = temp_dir)
  expect_no_error(reviewStudy(here::here(temp_dir, "diagnostics_code"), code = TRUE, dependencies = FALSE))
})
