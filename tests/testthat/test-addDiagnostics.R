test_that("add diagnostics", {

  # we require user to provide an empty, existing folder
  temp_dir <- here::here(tempdir(), "study")
  dir.create(path = temp_dir, recursive = TRUE)
  createStudy(directory = temp_dir)
  expect_no_error(addDiagnostics(directory = temp_dir))

  expect_true("README.md" %in% list.files(temp_dir, recursive = TRUE))
  expect_true("diagnostics/README.md" %in% list.files(temp_dir, recursive = TRUE))
  expect_true("diagnostics/README.md" %in% list.files(temp_dir, recursive = TRUE))
  expect_true("diagnostics/diagnostics.Rproj" %in% list.files(temp_dir, recursive = TRUE))

  reviewRenv(temp_dir)


  # expected error
  # if folder already exists
  expect_error(addDiagnostics(directory = temp_dir))

  # folder missing root readme
  expect_error(addDiagnostics(directory = here::here(tempdir(), "study_2")))

})

