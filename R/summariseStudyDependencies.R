summariseStudyDependencies  <- function(dir){

  if(!file.exists(here::here(dir, "renv.lock"))){
    cli::cli_abort("renv.lock file not found in {dir}")
  }

  reportDependencies(dir)

  lock <- renv::lockfile_read(here::here(dir, "renv.lock"))
  reportLockPkgs(lock)
  checkPkgSource(lock)
  checkLatestVersion(lock)

  return(invisible())
}

reportDependencies <- function(dir){
  cli::cat_line()
  cli::cli_h2("Dependencies used in code")

  directDependencies <- renv::dependencies(dir, quiet = TRUE) |>
    dplyr::pull("Package") |>
    unique() |>
    sort()
  directDependenciesChr <- paste0(directDependencies, collapse = "; ")

  cli::cli_inform(c("In total, {length(directDependencies)} packages are used in the code.",
                    i = "{directDependenciesChr}"))

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
    utils::available.packages() |>
      dplyr::as_tibble() |>
      dplyr::select("pkg" = "Package",
             "cran_version" = "Version"),
    by = "pkg"
  )

discordantPackages <- packages |>
  dplyr::filter(renv_version != cran_version) |>
  dplyr::mutate(compare = paste0(pkg, " ", renv_version,
                       " (renv) vs ",
                       cran_version,
                       " (cran)"))

if(nrow(discordantPackages) > 0){
  cli::cli_inform(c("{length(discordantPackages$pkg)} packages do not correspond to the current CRAN version",
                    i = "{discordantPackages$compare}"))
} else {
  cli::cli_inform("All packages are from latest cran version")
}

return(invisible())

}
