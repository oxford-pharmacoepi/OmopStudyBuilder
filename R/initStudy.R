#' Creates initial directory for an OMOP CDM network study
#'
#' @param directory Path to a directory that will be used as the root folder
#' for the study. If it does not exist, it will be created. The directory
#' must be empty if it already exists.
#'
#' @param diagnostics A single TRUE or FALSE value.
#'   If TRUE (the default), the function creates the `diagnosticsCode/`
#'   and `diagnosticsShiny/` folders using the package templates.
#'   If FALSE, these diagnostics folders are not created.
#'
#' @param study A single TRUE or FALSE value.
#'   If TRUE (the default), the function creates the `studyCode/`
#'   and `studyShiny/` folders using the package templates.
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
#' @param repository Optional GitHub repository name. If provided, creates a GitHub
#'   repository and links it to the study. Requires the \code{gh} package and
#'   GitHub authentication (GITHUB_PAT environment variable).
#'
#' @param organisation Optional GitHub organisation name. If NULL (default), creates
#'   repository under your personal account. Only used when \code{repository} is provided.
#'
#' @param private Logical. If TRUE (default), creates a private GitHub repository.
#'   Only used when \code{repository} is provided.
#'
#' @returns Project directory will be created
#' @export
#'
#' @examples
#' # Create a study called "SampleStudy" in a temporary directory
#' study_root <- file.path(tempdir(), "SampleStudy")
#' initStudy(study_root)
#'
#' # Inspect the top-level contents
#' list.files(study_root)
#'
#' # Create another study with custom metadata
#' study_root2 <- file.path(tempdir(), "DiabetesStudy")
#' initStudy(study_root2,
#'           studyTitle = "Diabetes Prevalence Study",
#'           studyLeads = "Dr. Smith, Dr. Jones")
#'
#' \donttest{
#' # Create study with GitHub integration
#' initStudy(
#'   directory = here::here(),
#'   repository = "my-omop-study",
#'   organisation = "oxford-pharmacoepi",
#'   private = TRUE
#' )
#' }
initStudy <- function(directory,
                        diagnostics = TRUE,
                        study = TRUE,
                        studyTitle = NULL,
                        studyLeads = NULL,
                        studyDescription = NULL,
                        repository = NULL,
                        organisation = NULL,
                        private = TRUE) {
  validateRootDirectory(directory)
  omopgenerics::assertLogical(diagnostics, length = 1)
  omopgenerics::assertLogical(study, length = 1)
  
  # Validate optional parameters
  if (!is.null(studyTitle)) {
    omopgenerics::assertCharacter(studyTitle, length = 1)
  }
  if (!is.null(studyLeads)) {
    omopgenerics::assertCharacter(studyLeads, length = 1)
  }
  if (!is.null(studyDescription)) {
    omopgenerics::assertCharacter(studyDescription, length = 1)
  }
  if (!is.null(repository)) {
    omopgenerics::assertCharacter(repository, length = 1)
  }
  if (!is.null(organisation)) {
    omopgenerics::assertCharacter(organisation, length = 1)
  }
  omopgenerics::assertLogical(private, length = 1)

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
      "- **[diagnosticsCode/](diagnosticsCode/)**: Contains diagnostic code needed before running the main study",
      "- **[diagnosticsShiny/](diagnosticsShiny/)**: Shiny app for exploring diagnostic outputs"
    )
  }
  if (isTRUE(study)) {
    folderStructure <- c(
      folderStructure,
      "- **[studyCode/](studyCode/)**: Contains the main study analysis code",
      "- **[studyShiny/](studyShiny/)**: Shiny app for exploring study results"
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

  # Process INSTRUCTIONS template
  instructionsTemplateData <- list(
    HAS_DIAGNOSTICS = isTRUE(diagnostics),
    HAS_STUDY = isTRUE(study)
  )
  instructionsTemplate <- readLines(system.file("INSTRUCTIONS.md", package = "OmopStudyBuilder"))
  instructionsContent <- whisker::whisker.render(instructionsTemplate, instructionsTemplateData)
  writeLines(instructionsContent, file.path(directory, "INSTRUCTIONS.md"))

  cli::cli_alert_success("{.strong {directory}} prepared as root folder for study.")

  if (isTRUE(diagnostics)) {
    directoryDiagnosticsCode <- file.path(directory, "diagnosticsCode")
    copyDirectory(
      from = system.file("diagnosticsCode", package = "OmopStudyBuilder"),
      to = directoryDiagnosticsCode
    )
    cli::cli_alert_success("{.strong {directoryDiagnosticsCode}} prepared for study diagnostics code")

    directoryDiagnosticsShiny <- file.path(directory, "diagnosticsShiny")
    copyDirectory(
      from = system.file("diagnosticsShiny", package = "OmopStudyBuilder"),
      to = directoryDiagnosticsShiny
    )
    cli::cli_alert_success("{.strong {directoryDiagnosticsShiny}} prepared for diagnostics shiny app")
  }

  if (isTRUE(study)) {
    directoryStudyCode <- file.path(directory, "studyCode")
    copyDirectory(
      from = system.file("studyCode", package = "OmopStudyBuilder"),
      to = directoryStudyCode
    )
    cli::cli_alert_success("{.strong {directoryStudyCode}} prepared for study study code")

    directoryStudyShiny <- file.path(directory, "studyShiny")
    copyDirectory(
      from = system.file("studyShiny", package = "OmopStudyBuilder"),
      to = directoryStudyShiny
    )
    cli::cli_alert_success("{.strong {directoryStudyShiny}} prepared for study shiny app")
  }

  # GitHub integration (optional)
  if (!is.null(repository)) {
    linkGitHub(
      directory = directory,
      repository = repository,
      organisation = organisation,
      private = private
    )
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