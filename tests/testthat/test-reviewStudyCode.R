test_that("multiplication works", {
  temp_dir <- here::here(tempdir(), omopgenerics::uniqueTableName())
  dir.create(path = temp_dir, recursive = TRUE)
  createStudy(directory = temp_dir)
  expect_no_error(reviewStudyCode(here::here(temp_dir, "diagnostics_code")))
})
