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
#' @examples
#' \dontrun{
#' # Review current study directory
#' reviewStudy(".")
#'
#' # Review only code (no dependencies)
#' reviewStudy(".", dependencies = FALSE)
#'
#' # Review only dependencies
#' reviewStudy(".", code = FALSE)
#'
#' # Review reporting project dependencies
#' reviewStudy(".", type = "reporting")
#'
#' # Review specific directory
#' reviewStudy("/path/to/study")
#' }
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

# =============================================================================
# Code file helpers
# =============================================================================

summariseRFiles <- function(dir) {
  cli::cat_line()
  cli::cli_h2("R scripts")

  rFiles <- list.files(dir,
    recursive = TRUE,
    pattern = "\\.R$",
    ignore.case = TRUE,
    full.names = TRUE
  ) |>
    stringr::str_subset(pattern = "renv/", negate = TRUE)

  rFilesShort <- list.files(dir,
    recursive = TRUE,
    pattern = "\\.R$",
    ignore.case = TRUE,
    full.names = FALSE
  ) |>
    stringr::str_subset(pattern = "renv/", negate = TRUE)

  rFilesShortLoc <- paste0(
    rFilesShort,
    " (loc: ",
    purrr::map_int(rFiles, ~ get_loc(.x)) |> scales::comma(),
    ")"
  )
  totalLoc <- sum(purrr::map_int(rFiles, ~ get_loc(.x))) |> scales::comma()

  cli::cli_inform(c(
    "{dir} contains {length(rFiles)} R scripts (total lines of code {totalLoc})",
    i = "{rFilesShortLoc}"
  ))
}

summariseJsonFiles <- function(dir) {
  cli::cat_line()
  cli::cli_h2("JSON files")

  jsonFiles <- list.files(dir,
    recursive = TRUE,
    pattern = "\\.json$",
    ignore.case = TRUE,
    full.names = TRUE
  ) |>
    stringr::str_subset(pattern = "renv/", negate = TRUE)

  jsonFilesShort <- list.files(dir,
    recursive = TRUE,
    pattern = "\\.json$",
    ignore.case = TRUE,
    full.names = FALSE
  ) |>
    stringr::str_subset(pattern = "renv/", negate = TRUE)

  cli::cli_inform(c(
    "{dir} contains {length(jsonFiles)} JSON files",
    i = "{jsonFilesShort}"
  ))
}

summariseCsvFiles <- function(dir) {
  cli::cat_line()
  cli::cli_h2("CSV files")

  csvFiles <- list.files(dir,
    recursive = TRUE,
    pattern = "\\.csv$",
    ignore.case = TRUE,
    full.names = TRUE
  ) |>
    stringr::str_subset(pattern = "renv/", negate = TRUE)

  csvFilesShort <- list.files(dir,
    recursive = TRUE,
    pattern = "\\.csv$",
    ignore.case = TRUE,
    full.names = FALSE
  ) |>
    stringr::str_subset(pattern = "renv/", negate = TRUE)

  cli::cli_inform(c(
    "{dir} contains {length(csvFiles)} CSV files",
    i = "{csvFilesShort}"
  ))
}

summariseExcelFiles <- function(dir) {
  cli::cat_line()
  cli::cli_h2("Excel files")

  excelFiles <- list.files(dir,
    recursive = TRUE,
    pattern = "\\.xlsx$|\\.xls$",
    ignore.case = TRUE,
    full.names = TRUE
  ) |>
    stringr::str_subset(pattern = "renv/", negate = TRUE)

  excelFilesShort <- list.files(dir,
    recursive = TRUE,
    pattern = "\\.xlsx$|\\.xls$",
    ignore.case = TRUE,
    full.names = FALSE
  ) |>
    stringr::str_subset(pattern = "renv/", negate = TRUE)

  cli::cli_inform(c(
    "{dir} contains {length(excelFiles)} Excel files",
    i = "{excelFilesShort}"
  ))
}

get_loc <- function(file_path) {
  length(readLines(file_path, warn = FALSE))
}

# =============================================================================
# Dependency helpers
# =============================================================================

reportDependencies <- function(dir) {
  cli::cat_line()
  cli::cli_h2("Dependencies used in code")

  directDependencies <- renv::dependencies(dir, quiet = TRUE) |>
    dplyr::pull("Package") |>
    unique() |>
    sort()
  directDependenciesChr <- paste0(directDependencies, collapse = "; ")

  cli::cli_inform(c(
    "In total, {length(directDependencies)} packages are used in the code.",
    i = "{directDependenciesChr}"
  ))

  return(invisible())
}

reportLockPkgs <- function(lock, type) {
  cli::cat_line()
  cli::cli_h2("Packages in renv lock file")

  lockPkgs <- names(lock$Packages) |> sort()
  lockPkgsVer <- lockPkgs |>
    purrr::map_chr(~ {
      workingPkg <- .x
      paste0(workingPkg, " (", lock$Packages[[workingPkg]]$Version, ")")
    })
  lockPkgsChr <- paste0(lockPkgsVer, collapse = "; ")
  cli::cli_inform(c(
    "The renv.lock file contains {length(lockPkgs)} packages",
    i = "{lockPkgsChr}"
  ))

  if (type == "analysis") {
    warnJavaPkgs <- c("SqlRender", "CirceR")
    if (any(warnJavaPkgs %in% lockPkgs)) {
      cli::cat_line()
      cli::cli_inform(c(
        "Can {.code {intersect(warnJavaPkgs, lockPkgs)}} be removed?",
        i = "Use requires JAVA installation."
      ))
    }

    warnRVerPkgs <- c("MASS", "Matrix")
    if (any(warnRVerPkgs %in% lockPkgs)) {
      cli::cat_line()
      cli::cli_inform(c(
        "Can {.code {intersect(warnRVerPkgs, lockPkgs)}} be removed?",
        i = "Packages tied to a specific version of R and can be challenging for data partners to restore."
      ))
    }

    warnVisPkgs <- c("ggplot2", "gt", "flextable", "datatable", "reactable", "tinytable", "plotly")
    if (any(warnVisPkgs %in% lockPkgs)) {
      cli::cat_line()
      cli::cli_inform(c(
        "Can {.code {intersect(warnVisPkgs, lockPkgs)}} be removed?",
        i = "Reporting packages should not need to be installed by a data partner to run analysis code."
      ))
    }
  }

  return(invisible())
}

checkPkgSource <- function(lock) {
  cli::cat_line()
  cli::cli_h2("Checking package sources")

  pkgs <- names(lock$Packages) |> sort()
  sources <- purrr::map_dfr(pkgs, ~ dplyr::tibble(
    pkg    = .x,
    source = lock$Packages[[.x]]$Source
  ))

  unknownPkgs <- sources |>
    dplyr::filter(.data$source == "unknown") |>
    dplyr::pull("pkg")

  if (length(unknownPkgs) > 0) {
    cli::cli_inform(c(
      "{length(unknownPkgs)} packages come from an unknown source",
      i = "{unknownPkgs}"
    ))
  } else {
    cli::cli_inform("All packages are from a known source")
  }

  return(invisible())
}

checkLatestVersion <- function(lock) {
  cli::cat_line()
  cli::cli_h2("Checking package versions")

  packages <- names(lock$Packages) |>
    sort() |>
    purrr::map_dfr(~ dplyr::tibble(
      pkg          = .x,
      renv_version = lock$Packages[[.x]]$Version
    ))

  packages <- packages |>
    dplyr::left_join(
      utils::available.packages(repos = "https://cran.rstudio.com/") |>
        dplyr::as_tibble() |>
        dplyr::select("pkg" = "Package", "cran_version" = "Version"),
      by = "pkg"
    )

  discordantPackages <- packages |>
    dplyr::filter(.data$renv_version != .data$cran_version) |>
    dplyr::mutate(compare = paste0(
      .data$pkg, " ", .data$renv_version,
      " (renv) vs ", .data$cran_version, " (cran)"
    ))

  if (nrow(discordantPackages) > 0) {
    cli::cli_inform(c(
      "{length(discordantPackages$pkg)} packages do not correspond to the current CRAN version",
      i = "{discordantPackages$compare}"
    ))
  } else {
    cli::cli_inform("All packages are from latest cran version")
  }

  return(invisible())
}