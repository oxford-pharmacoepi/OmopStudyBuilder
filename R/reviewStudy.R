#' Review a study directory
#'
#' Summarises the code and/or renv dependencies in a study directory.
#'
#' @param dir Path to the study directory.
#' @param code If `TRUE`, summarises R, JSON, CSV, and Excel files found in
#'   the directory.
#' @param dependencies If `TRUE`, summarises the renv.lock dependencies.
#' @param type Whether the R project is for analysis code or study reporting.
#'   Only used when `dependencies = TRUE`.
#'
#' @returns Invisibly returns `NULL`. Called for its side effects of printing
#'   summaries to the console.
#' @export
#'
reviewStudy <- function(dir, code = TRUE, dependencies = TRUE, type = "analysis") {

  dir <- normalizePath(dir, mustWork = TRUE)

  if (isTRUE(dependencies)) {
    omopgenerics::assertChoice(type, choices = c("analysis", "reporting"))
  }

  if (isTRUE(code)) {
    cli::cli_h1("Code Summary")
    summariseRFiles(dir)
    summariseJsonFiles(dir)
    summariseCsvFiles(dir)
    summariseExcelFiles(dir)
  }

  if (isTRUE(dependencies)) {
    cli::cli_h1("Dependencies")

    if (!file.exists(file.path(dir, "renv.lock"))) {
      cli::cli_warn(c("renv.lock file not found in {dir}",
                      i = "A renv file should be added to ensure reproducibility"))
      return(invisible(NULL))
    }

    reportDependencies(dir)
    lock <- renv::lockfile_read(file.path(dir, "renv.lock"))
    reportLockPkgs(lock, type)
    checkPkgSource(lock)
    checkLatestVersion(lock)
  }

  return(invisible(NULL))
}
