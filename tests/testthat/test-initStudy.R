test_that("directory set up", {

  # user can provide a directory that already exists (and is empty)
  temp_dir <- here::here(tempdir(), omopgenerics::uniqueTableName())
  dir.create(path = temp_dir, recursive = TRUE)
  expect_true(dir.exists(temp_dir))
  expect_no_error(initStudy(directory = temp_dir))
  expect_true("README.md" %in% list.files(temp_dir))

  # if directory does not exist, it should be created and used
  temp_dir <- here::here(tempdir(), omopgenerics::uniqueTableName())
  expect_false(dir.exists(temp_dir))
  expect_no_error(initStudy(directory = temp_dir))
  expect_true(dir.exists(temp_dir))
  expect_true("README.md" %in% list.files(temp_dir))

  # error if directory exists and contains files
  temp_dir_non_empty <- here::here(tempdir(), omopgenerics::uniqueTableName())
  dir.create(path = temp_dir_non_empty, recursive = TRUE)
  writeLines("some text", file.path(temp_dir_non_empty, "notes.txt"))
  expect_error(initStudy(directory = temp_dir_non_empty))

  # only diagnostics directories
  temp_dir_diag <- here::here(tempdir(), omopgenerics::uniqueTableName())
  dir.create(path = temp_dir_diag, recursive = TRUE)
  expect_no_error(initStudy(
    directory   = temp_dir_diag,
    diagnostics = TRUE,
    study       = FALSE
  ))
  expect_true("README.md" %in% list.files(temp_dir_diag))
  expect_true("diagnostics_code" %in% list.files(temp_dir_diag))
  expect_true("diagnostics_shiny" %in% list.files(temp_dir_diag))
  expect_false("study_code" %in% list.files(temp_dir_diag))
  expect_false("study_shiny" %in% list.files(temp_dir_diag))

  # only study directories
  temp_dir_study <- here::here(tempdir(), omopgenerics::uniqueTableName())
  dir.create(path = temp_dir_study, recursive = TRUE)
  expect_no_error(initStudy(
    directory   = temp_dir_study,
    diagnostics = FALSE,
    study       = TRUE
  ))
  expect_true("README.md" %in% list.files(temp_dir_study))
  expect_false("diagnostics_code" %in% list.files(temp_dir_study))
  expect_false("diagnostics_shiny" %in% list.files(temp_dir_study))
  expect_true("study_code" %in% list.files(temp_dir_study))
  expect_true("study_shiny" %in% list.files(temp_dir_study))
})
