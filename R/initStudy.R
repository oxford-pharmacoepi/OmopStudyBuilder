#' Creates initial directory for an OMOP CDM network study
#'
#' @param directory Path to a directory that will be used as the root folder
#' for the study. If it does not exist, it will be created. The directory
#' must be empty if it already exists.
#'
#' @param diagnostics A single TRUE or FALSE value.
#'   If TRUE (the default), the function creates the `diagnostics_code/`
#'   and `diagnostics_shiny/` folders using the package templates.
#'   If FALSE, these diagnostics folders are not created.
#'
#' @param study A single TRUE or FALSE value.
#'   If TRUE (the default), the function creates the `study_code/`
#'   and `study_shiny/` folders using the package templates.
#'   If FALSE, these study folders are not created.
#'
#' @param studyTitle Character string with the study title. If NULL (default),
#'   uses the directory basename.
#'
#' @param studyLeads Character string with study leads. If NULL (default),
#'   leaves a placeholder.
#'
#' @param studyDescription Character string with study description. If NULL (default),
#'   leaves a placeholder.
#'
#' @returns Project directory will be created
#' @export
#'
#' @examples
#' # Create a study called "SampleStudy" in a temporary directory
#' study_root <- file.path(tempdir(), "SampleStudy")
#' initStudy(study_root)
#'
#' # With metadata
#' createStudy(study_root,
#'             studyTitle = "Diabetes Prevalence Study",
#'             studyLeads = "Dr. Smith, Dr. Jones")
#'
#' # Inspect the top-level contents
#' list.files(study_root)
initStudy <- function(directory,
                        diagnostics = TRUE,
                        study = TRUE,
                        studyTitle = NULL,
                        studyLeads = NULL,
                        studyDescription = NULL) {
  validateRootDirectory(directory)
  omopgenerics::assertLogical(diagnostics, length = 1)
  omopgenerics::assertLogical(study, length = 1)

  # Set smart defaults for template variables
  if (is.null(studyTitle)) {
    studyTitle <- basename(normalizePath(directory, mustWork = FALSE))
  }
  if (is.null(studyLeads)) {
    studyLeads <- "[Add study leads]"
  }
  if (is.null(studyDescription)) {
    studyDescription <- "[Add study description here]"
  }

  # Build folder structure description dynamically
  folderStructure <- c()
  if (isTRUE(diagnostics)) {
    folderStructure <- c(
      folderStructure,
      "- **[diagnostics_code/](diagnostics_code/)**: Contains diagnostic code needed before running the main study",
      "- **[diagnostics_shiny/](diagnostics_shiny/)**: Shiny app for exploring diagnostic outputs"
    )
  }
  if (isTRUE(study)) {
    folderStructure <- c(
      folderStructure,
      "- **[study_code/](study_code/)**: Contains the main study analysis code",
      "- **[study_shiny/](study_shiny/)**: Shiny app for exploring study results"
    )
  }
  folderStructure <- paste(folderStructure, collapse = "\n")

  # Prepare template data
  templateData <- list(
    STUDY_TITLE = studyTitle,
    STUDY_LEADS = studyLeads,
    STUDY_DESCRIPTION = studyDescription,
    STUDY_START = as.character(Sys.Date()),
    STUDY_END = "[Ongoing]",
    STUDY_STATUS = "Started",
    PUBLICATIONS = "[None yet]",
    FOLDER_STRUCTURE = folderStructure
  )

  # Process README template
  readmeTemplate <- readLines(system.file("README.md", package = "OmopStudyBuilder"))
  readmeContent <- whisker::whisker.render(readmeTemplate, templateData)
  writeLines(readmeContent, file.path(directory, "README.md"))

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