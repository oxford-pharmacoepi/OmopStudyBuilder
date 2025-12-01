#' Creates initial directory for an OMOP CDM network study
#'
#' @param directory Path to a directory that will be used as the root folder for
#' the study. If it does not exist, it will be created. It must be empty.
#' @param diagnostics If TRUE, directories for diagnositcs will be created
#' @param study If TRUE, directories for the study will be created.
#'
#' @returns Project directory will be created
#' @export
#'
#' @examples
createStudy <- function(directory,
                        diagnostics = TRUE,
                        study = TRUE) {
  validateRootDirectory(directory)
  omopgenerics::assertLogical(diagnostics, length = 1)
  omopgenerics::assertLogical(study, length = 1)


  # Add README file
  invisible(file.copy(
    from = system.file("README.md", package = "OmopStudyBuilder"),
    to = directory
  ))

  cli::cli_alert_success("{.strong {directory}} prepared as root folder for study.")

  if (isTRUE(diagnostics)) {
    directoryDiagnosticsCode <- file.path(directory, "diagnostics_code")
    copyDirectory(
      from = system.file("diagnostics_code", package = "OmopStudyBuilder"),
      to = directoryDiagnosticsCode
    )
    cli::cli_alert_success("{.strong {directoryDiagnosticsCode}} prepared for study diagnostics code")

    directoryDiagnosticsShiny <- file.path(directory, "diagnostics_shiny")
    copyDirectory(
      from = system.file("diagnostics_shiny", package = "OmopStudyBuilder"),
      to = directoryDiagnosticsShiny
    )
    cli::cli_alert_success("{.strong {directoryDiagnosticsShiny}} prepared for diagnostics shiny app")
  }

  if (isTRUE(study)) {
    directoryStudyCode <- file.path(directory, "study_code")
    copyDirectory(
      from = system.file("study_code", package = "OmopStudyBuilder"),
      to = directoryStudyCode
    )
    cli::cli_alert_success("{.strong {directoryStudyCode}} prepared for study study code")

    directoryStudyShiny <- file.path(directory, "study_shiny")
    copyDirectory(
      from = system.file("study_shiny", package = "OmopStudyBuilder"),
      to = directoryStudyShiny
    )
    cli::cli_alert_success("{.strong {directoryStudyShiny}} prepared for study shiny app")
  }


  return(invisible())
}


copyDirectory <- function(from, to) {
  # files to copy
  oldFiles <- list.files(path = from, full.names = TRUE, recursive = TRUE)

  files <- list.files(path = from, full.names = FALSE, recursive = TRUE)
  NewFiles <- paste0(to, "/", files)

  dirsToCreate <- unique(dirname(NewFiles))
  sapply(dirsToCreate, dir.create, recursive = TRUE, showWarnings = FALSE)

  # copy files
  file.copy(from = oldFiles, to = NewFiles)
}

validateRootDirectory <- function(dir) {
  # root directory must be empty and create it if it does not exist
  if (!dir.exists(dir)) {
    dirCreated <- dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (!dirCreated) {
      cli::cli_abort(c("Provided directory {.pkg {dir}} does not exist and could not be created."))
    }
  }

  if (length(list.files(dir, recursive = TRUE)) > 0) {
    cli::cli_abort(c("Provided directory {.pkg {dir}} is not empty."))
  }

  return(invisible())
}

