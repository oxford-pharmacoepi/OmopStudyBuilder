test_that("summarise renv", {

  temp_dir <- here::here(tempdir(), omopgenerics::uniqueTableName())
  dir.create(path = temp_dir, recursive = TRUE)
  createStudy(directory = temp_dir)

  # no renv yet
  expect_warning(summariseStudyDependencies(here::here(temp_dir, "diagnostics_code")))

  renv::init(here::here(temp_dir, "diagnostics_code"),
             restart = FALSE,
             load = FALSE)
  expect_no_error(
    summariseStudyDependencies(here::here(temp_dir, "diagnostics_code"))
  )

})
