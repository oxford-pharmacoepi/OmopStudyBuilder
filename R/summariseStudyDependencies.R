#' Summarise dependencies in renv lock file
#'
#' @param dir Path to folder containing R project with renv.lock file
#' @param type Whether R project is for analysis code or study reporting.
#'
#' @returns Summary of state of dependencies
#' @export
#'
summariseStudyDependencies  <- function(dir, type = "analysis"){

  omopgenerics::assertChoice(type, choices = c("analysis", "reporting"))

  if(!file.exists(here::here(dir, "renv.lock"))){
    cli::cli_warn(c("renv.lock file not found in {dir}",
                    i = "A renv file should be added to ensure reproducibility"))
    return(invisible(NULL))
  }

  reportDependencies(dir, type = type)

  lock <- renv::lockfile_read(here::here(dir, "renv.lock"))
  reportLockPkgs(lock)
  checkPkgSource(lock)
  checkLatestVersion(lock)

  return(invisible())
}

reportDependencies <- function(dir, type){
  cli::cat_line()
  cli::cli_h2("Dependencies used in code")

  directDependencies <- renv::dependencies(dir, quiet = TRUE) |>
    dplyr::pull("Package") |>
    unique() |>
    sort()
  directDependenciesChr <- paste0(directDependencies, collapse = "; ")

  cli::cli_inform(c("In total, {length(directDependencies)} packages are used in the code.",
                    i = "{directDependenciesChr}"))

  if(type == "analysis"){
  warnJavaPkgs <- c("SqlRender", "CirceR")
  if(any(warnJavaPkgs %in% directDependencies)){
    cli::cat_line()
    cli::cli_inform(c("Can {.code {intersect(warnJavaPkgs, directDependencies)}} be removed?",
                      i = "Use requires JAVA installation."))

  }

  warnVisPkgs <- c("ggplot2", "gt")
  if(any(warnVisPkgs %in% directDependencies)){
    cli::cat_line()
    cli::cli_inform(c("Can {.code {intersect(warnVisPkgs, directDependencies)}} be removed?",
                      i = "Reporting packages should not need to be installed by a data partner to run analysis code."))
  }

  }

  return(invisible())

}

reportLockPkgs <- function(lock){
  cli::cat_line()
  cli::cli_h2("Packages in renv lock file")

  lockPkgs <- names(lock$Packages) |>
    sort() |>
    purrr::map_chr(~ {
      wokrkingPkg <- .x
      paste0(wokrkingPkg, " (", lock$Packages[[wokrkingPkg]]$Version, ")")
    })
  lockPkgsChr <- paste0(lockPkgs, collapse = "; ")
  cli::cli_inform(c("The renv.lock file contains {length(lockPkgs)} packages",
                    i = "{lockPkgsChr}"))

  return(invisible())

}

checkPkgSource <- function(lock){

  cli::cat_line()
  cli::cli_h2("Checking package sources")

  pkgs <- names(lock$Packages) |> sort()
  sources <- list()
  for(i in seq_along(pkgs)){
  workingPkg <- pkgs[i]
  sources[[workingPkg]] <- dplyr::tibble(
    pkg = workingPkg,
    source = lock$Packages[[workingPkg]]$Source
  )
  }
  sources <- dplyr::bind_rows(sources)

  unknownPkgs <- sources |>
    dplyr::filter(.data$source == "unknown") |>
    dplyr::pull("pkg")
  if(length(unknownPkgs) > 0){
    cli::cli_inform(c("{length(unknownPkgs)} packages come from an unknown source",
                      i = "{unknownPkgs}"))
  } else {
    cli::cli_inform("All packages are from a known source")
  }

  return(invisible())

}

checkLatestVersion <- function(lock){

cli::cat_line()
cli::cli_h2("Checking package versions")

packages <- names(lock$Packages) |>
    sort() |>
    purrr::map_dfr(~ {
      wokrkingPkg <- .x
      dplyr::tibble(
        pkg = wokrkingPkg,
        renv_version = lock$Packages[[wokrkingPkg]]$Version
      )
    })

packages <- packages |>
  dplyr::left_join(
    utils::available.packages(repos = "https://cran.rstudio.com/") |>
      dplyr::as_tibble() |>
      dplyr::select("pkg" = "Package",
             "cran_version" = "Version"),
    by = "pkg"
  )

discordantPackages <- packages |>
  dplyr::filter(.data$renv_version != .data$cran_version) |>
  dplyr::mutate(compare = paste0(.data$pkg, " ", .data$renv_version,
                       " (renv) vs ",
                       .data$cran_version,
                       " (cran)"))

if(nrow(discordantPackages) > 0){
  cli::cli_inform(c("{length(discordantPackages$pkg)} packages do not correspond to the current CRAN version",
                    i = "{discordantPackages$compare}"))
} else {
  cli::cli_inform("All packages are from latest cran version")
}

return(invisible())

}
