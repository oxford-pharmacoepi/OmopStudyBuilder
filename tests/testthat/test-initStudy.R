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
  expect_true("diagnosticsCode" %in% list.files(temp_dir_diag))
  expect_true("diagnosticsShiny" %in% list.files(temp_dir_diag))
  expect_false("studyCode" %in% list.files(temp_dir_diag))
  expect_false("studyShiny" %in% list.files(temp_dir_diag))

  # only study directories
  temp_dir_study <- here::here(tempdir(), omopgenerics::uniqueTableName())
  dir.create(path = temp_dir_study, recursive = TRUE)
  expect_no_error(initStudy(
    directory   = temp_dir_study,
    diagnostics = FALSE,
    study       = TRUE
  ))
  expect_true("README.md" %in% list.files(temp_dir_study))
  expect_false("diagnosticsCode" %in% list.files(temp_dir_study))
  expect_false("diagnosticsShiny" %in% list.files(temp_dir_study))
  expect_true("studyCode" %in% list.files(temp_dir_study))
  expect_true("studyShiny" %in% list.files(temp_dir_study))
})

test_that("INSTRUCTIONS.md is created with correct content", {

  # INSTRUCTIONS.md should be created with both diagnostics and study
  temp_dir_both <- here::here(tempdir(), omopgenerics::uniqueTableName())
  dir.create(path = temp_dir_both, recursive = TRUE)
  expect_no_error(initStudy(
    directory   = temp_dir_both,
    diagnostics = TRUE,
    study       = TRUE
  ))
  expect_true("INSTRUCTIONS.md" %in% list.files(temp_dir_both))
  
  # Check content includes both diagnostics and study instructions
  instructions_both <- readLines(file.path(temp_dir_both, "INSTRUCTIONS.md"))
  instructions_text <- paste(instructions_both, collapse = " ")
  expect_true(grepl("Running the Diagnostics Code", instructions_text))
  expect_true(grepl("Running the Study Code", instructions_text))
  expect_true(grepl("diagnosticsCode\\.Rproj", instructions_text))
  expect_true(grepl("studyCode\\.Rproj", instructions_text))

  # INSTRUCTIONS.md with only diagnostics
  temp_dir_diag_only <- here::here(tempdir(), omopgenerics::uniqueTableName())
  dir.create(path = temp_dir_diag_only, recursive = TRUE)
  expect_no_error(initStudy(
    directory   = temp_dir_diag_only,
    diagnostics = TRUE,
    study       = FALSE
  ))
  expect_true("INSTRUCTIONS.md" %in% list.files(temp_dir_diag_only))
  
  instructions_diag <- readLines(file.path(temp_dir_diag_only, "INSTRUCTIONS.md"))
  instructions_diag_text <- paste(instructions_diag, collapse = " ")
  expect_true(grepl("Running the Diagnostics Code", instructions_diag_text))
  expect_false(grepl("Running the Study Code", instructions_diag_text))

  # INSTRUCTIONS.md with only study
  temp_dir_study_only <- here::here(tempdir(), omopgenerics::uniqueTableName())
  dir.create(path = temp_dir_study_only, recursive = TRUE)
  expect_no_error(initStudy(
    directory   = temp_dir_study_only,
    diagnostics = FALSE,
    study       = TRUE
  ))
  expect_true("INSTRUCTIONS.md" %in% list.files(temp_dir_study_only))
  
  instructions_study <- readLines(file.path(temp_dir_study_only, "INSTRUCTIONS.md"))
  instructions_study_text <- paste(instructions_study, collapse = " ")
  expect_false(grepl("Running the Diagnostics Code", instructions_study_text))
  expect_true(grepl("Running the Study Code", instructions_study_text))
})

test_that("README.md files point to INSTRUCTIONS.md", {
  temp_dir <- here::here(tempdir(), omopgenerics::uniqueTableName())
  dir.create(path = temp_dir, recursive = TRUE)
  initStudy(directory = temp_dir, diagnostics = TRUE, study = TRUE)
  
  # Check that subfolder READMEs reference the central instructions
  study_readme <- readLines(file.path(temp_dir, "studyCode", "README.md"))
  expect_true(any(grepl("\\.\\./INSTRUCTIONS\\.md", study_readme)))
})

