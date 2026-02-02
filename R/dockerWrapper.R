# Docker Wrapper Functions for OMOP Studies
# Abstracts Docker CLI commands into simple R functions


#' Execute expression in study directory
#' @keywords internal
withStudyDir <- function(study_path, expr) {
  original_dir <- getwd()
  on.exit(setwd(original_dir), add = TRUE)
  setwd(study_path)
  force(expr)
}

#' Run a docker command and capture output
#'
#' @param args Character vector of docker CLI args
#' @param stream If TRUE, stream stdout/stderr to console
#' @return list(ok, status, out, cmd)
#' @keywords internal
dockerExec <- function(args, stream = FALSE) {
  cmd <- paste(c("docker", args), collapse = " ")

  if (isTRUE(stream)) {
    status <- suppressWarnings(system2("docker", args = args, stdout = "", stderr = ""))
    return(list(ok = identical(status, 0L), status = status, out = character(), cmd = cmd))
  }

  out <- suppressWarnings(system2("docker", args = args, stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  list(ok = identical(status, 0L), status = status, out = out, cmd = cmd)
}

#' Test if packages can be loaded in Docker
#'
#' Attempts to load all specified packages in the Docker container
#' to catch namespace conflicts, missing system dependencies, etc.
#'
#' @param image_name Docker image name
#' @param packages Character vector of package names to test
#' @return List with ok status and failed package names
#' @keywords internal
testPackageLoading <- function(image_name, packages) {
  if (length(packages) == 0) {
    return(list(ok = TRUE, failed = character()))
  }

  # Generate R script that tries to load each package
  # Captures which ones fail and why
  pkg_list <- paste0("'", packages, "'", collapse = ", ")
  test_script <- sprintf("
    failures <- character()
    for (pkg in c(%s)) {
      result <- tryCatch({
        suppressPackageStartupMessages(library(pkg, character.only = TRUE))
        NULL
      }, error = function(e) {
        failures <<- c(failures, pkg)
        NULL
      })
    }
    if (length(failures) > 0) {
      cat('FAILED:', paste(failures, collapse=','))
    } else {
      cat('SUCCESS')
    }
  ", pkg_list)

  # Run test script in Docker
  res <- dockerExec(c(
    "run", "--rm", image_name,
    "Rscript", "-e", test_script
  ))
  
  # Parse results
  if (!res$ok || length(res$out) == 0) {
    return(list(ok = NA, failed = character(), message = "Could not run load test"))
  }
  
  if (grepl("SUCCESS", res$out[1])) {
    return(list(ok = TRUE, failed = character()))
  } else {
    failed_str <- gsub("FAILED: ", "", res$out[1])
    failed_pkgs <- strsplit(failed_str, ",", fixed = TRUE)[[1]]
    return(list(ok = FALSE, failed = failed_pkgs))
  }
}

#' Check docker binary and (optionally) daemon (internal)
#' @keywords internal
ensureDocker <- function(check_daemon = FALSE) {
  ver <- tryCatch(system2("docker", "--version", stdout = TRUE, stderr = TRUE),
                  error = function(e) NULL)

  if (is.null(ver)) {
    cli::cli_alert_danger("Docker not found. Please install Docker Desktop.")
    cli::cli_alert_info("Download from: {.url https://www.docker.com/products/docker-desktop}")
    return(FALSE)
  }

  version <- gsub("Docker version ([0-9.]+).*", "\\1", ver[1])
  cli::cli_alert_success("Docker {version} detected")

  if (!isTRUE(check_daemon)) return(TRUE)

  daemon <- dockerExec(c("info"), stream = FALSE)
  if (!daemon$ok) {
    cli::cli_alert_danger("Cannot connect to the Docker daemon. Is Docker Desktop running?")
    return(FALSE)
  }
  TRUE
}

#' Snapshot packages and detect changes
#' @keywords internal
syncSnapshot <- function(study_path) {
  cli::cli_h3("[1/4] Snapshotting package versions...")

  lock_file <- file.path(study_path, "renv.lock")
  if (!file.exists(lock_file)) {
    cli::cli_alert_info("No renv.lock found - initializing renv first...")
    initRenv(study_path)
  }

  old_hash <- if (file.exists(lock_file)) tools::md5sum(lock_file) else ""
  snapshotPackages(study_path)
  new_hash <- tools::md5sum(lock_file)
  packages_changed <- old_hash != new_hash

  if (packages_changed) {
    cli::cli_alert_success("Packages updated in renv.lock")
  } else {
    cli::cli_alert_info("No package changes detected")
  }
  cli::cli_text("")
  
  packages_changed
}

#' Build Docker image conditionally
#' @keywords internal
syncBuild <- function(study_path, image_name, force_rebuild, packages_changed) {
  cli::cli_h3("[2/4] Building Docker image...")

  img <- dockerExec(c("images", "-q", image_name))
  should_rebuild <- isTRUE(force_rebuild) || isTRUE(packages_changed) || length(img$out) == 0

  if (should_rebuild) {
    if (force_rebuild) cli::cli_alert_info("Force rebuild requested")
    else if (packages_changed) cli::cli_alert_info("Packages changed - rebuilding required")
    else cli::cli_alert_info("No existing image - building fresh")

    buildStudy(study_path, image_name = image_name)
  } else {
    cli::cli_alert_info("Image up-to-date, skipping rebuild")
    cli::cli_alert_info("(Use force_rebuild=TRUE to rebuild anyway)")
  }
  cli::cli_text("")
  
  return(invisible(TRUE))
}

#' Validate Docker environment with rebuild option
#' @keywords internal
syncValidate <- function(study_path, image_name) {
  cli::cli_h3("[3/4] Validating Docker environment...")
  cli::cli_alert_info("Running deep compatibility check (this may take 30-60 seconds)...")

  validate_result <- validateDockerEnvironment(study_path, image_name, deep_check = TRUE)

  if (is.na(validate_result$ok)) {
    cli::cli_alert_danger(validate_result$message)
    cli::cli_alert_warning("Docker may not be running properly")
    cli::cli_text("")
    return(FALSE)
  }
  
  # Report validation results and get issue status
  report <- reportValidationResults(validate_result)
  has_issues <- report$has_issues
  
  # If issues found, offer to rebuild
  if (has_issues) {
    cli::cli_text("")
    response <- readline(prompt = "Rebuild Docker image to fix these issues? (y/n): ")
    
    if (tolower(trimws(response)) == "y") {
      cli::cli_text("")
      cli::cli_alert_info("Rebuilding Docker image...")
      buildStudy(study_path, image_name = image_name)
      
      # Re-validate after rebuild
      cli::cli_text("")
      cli::cli_alert_info("Re-validating after rebuild...")
      validate_result <- validateDockerEnvironment(study_path, image_name, deep_check = TRUE)
      
      if (!validate_result$ok) {
        cli::cli_alert_danger("Issues persist after rebuild. Manual intervention may be required.")
        cli::cli_text("")
        return(FALSE)
      }
      cli::cli_alert_success("All issues resolved!")
    } else {
      cli::cli_alert_warning("Skipping rebuild. Docker image may not work correctly.")
      cli::cli_text("")
      return(FALSE)
    }
  }
  cli::cli_text("")
  
  return(TRUE)
}

#' Test study execution in Docker
#' @keywords internal
syncTest <- function(image_name, run_test, output_path) {
  if (isTRUE(run_test)) {
    cli::cli_h3("[4/4] Testing study execution in Docker...")

    if (is.null(output_path)) output_path <- file.path(tempdir(), "sync_test_results")
    if (dir.exists(output_path)) unlink(output_path, recursive = TRUE)
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

    ok <- tryCatch({
      runStudy(image_name = image_name, output_path = output_path)
      TRUE
    }, error = function(e) {
      cli::cli_alert_danger("Study failed to run in Docker")
      cli::cli_alert_info(e$message)
      FALSE
    })

    if (ok) {
      cli::cli_alert_success("Study executed successfully in Docker!")
    } else {
      cli::cli_text("")
      return(FALSE)
    }
  } else {
    cli::cli_h3("[4/4] Skipping test run (run_test=FALSE)")
  }
  
  return(TRUE)
}

#' Detect and sync R version for Docker
#' @keywords internal
detectRVersion <- function(dockerfile_dest) {
  docker_r_version <- DOCKER_R_VERSION  # Default from config
  
  if (!file.exists(dockerfile_dest)) {
    # Detect local R version
    local_r_version <- paste(R.version$major, R.version$minor, sep = ".")
    
    # Check for mismatch
    if (local_r_version != DOCKER_R_VERSION) {
      cli::cli_alert_warning("R version mismatch detected:")
      cli::cli_bullets(c(
        "*" = "Your machine:  R {local_r_version}",
        "*" = "Docker config: R {DOCKER_R_VERSION}",
        "i" = "Using a different R version in Docker may cause compatibility issues."
      ))
      
      response <- readline(prompt = sprintf(
        "Use your R version (%s) in Docker instead? (y/n): ",
        local_r_version
      ))
      
      if (tolower(trimws(response)) == "y") {
        docker_r_version <- local_r_version
        cli::cli_alert_success("Using R {docker_r_version} for Docker build")
      } else {
        cli::cli_alert_info("Continuing with configured R {DOCKER_R_VERSION}")
        cli::cli_alert_info("(To change default, edit DOCKER_R_VERSION in R/dockerConfig.R)")
      }
    } else {
      cli::cli_alert_success("Using R {docker_r_version} (matches your local version)")
    }
  }
  
  return(docker_r_version)
}

#' Prepare Dockerfile from template
#' @keywords internal
prepareDockerfile <- function(study_path, docker_r_version) {
  dockerfile_dest <- file.path(study_path, "Dockerfile")
  
  if (!file.exists(dockerfile_dest)) {
    dockerfile_template <- system.file("docker", "Dockerfile.template", package = "OmopStudyBuilder")
    if (!file.exists(dockerfile_template) || dockerfile_template == "") {
      dockerfile_template <- here::here("inst", "docker", "Dockerfile.template")
    }
    if (!file.exists(dockerfile_template)) {
      cli::cli_abort("Dockerfile.template not found. Ensure it's in inst/docker/ directory.")
    }

    template_content <- readLines(dockerfile_template)
    template_content <- gsub("{{DOCKER_R_VERSION}}", docker_r_version, template_content, fixed = TRUE)
    template_content <- gsub("{{DOCKER_DEFAULT_PACKAGES_STR}}", DOCKER_DEFAULT_PACKAGES_STR, template_content, fixed = TRUE)
    writeLines(template_content, dockerfile_dest)

    cli::cli_alert_success("Created Dockerfile in study directory (R version: {docker_r_version})")
  }
  
  invisible(dockerfile_dest)
}

#' Report validation results
#' @keywords internal
reportValidationResults <- function(validate_result, verbose = TRUE) {
  if (!verbose) return(invisible(validate_result))
  
  has_issues <- FALSE
  
  # Check name issues
  if (length(validate_result$name_issues) > 0) {
    has_issues <- TRUE
    cli::cli_alert_danger("Missing packages in Docker ({length(validate_result$name_issues)}):")
    cli::cli_bullets(c("*" = "{paste(head(validate_result$name_issues, 5), collapse = ', ')}"))
    if (length(validate_result$name_issues) > 5) {
      cli::cli_alert_info("... and {length(validate_result$name_issues) - 5} more")
    }
  }
  
  # Check version issues
  if (length(validate_result$version_issues) > 0) {
    has_issues <- TRUE
    cli::cli_alert_warning("Version mismatches ({length(validate_result$version_issues)}):")
    for (pkg in head(names(validate_result$version_issues), 5)) {
      ver_info <- validate_result$version_issues[[pkg]]
      cli::cli_bullets(c("*" = "{pkg}: local={ver_info$local}, docker={ver_info$docker}"))
    }
    if (length(validate_result$version_issues) > 5) {
      cli::cli_alert_info("... and {length(validate_result$version_issues) - 5} more")
    }
  }
  
  # Check load failures
  if (length(validate_result$load_failures) > 0) {
    has_issues <- TRUE
    cli::cli_alert_danger("Packages failed to load in Docker ({length(validate_result$load_failures)}):")
    cli::cli_bullets(c("*" = "{paste(head(validate_result$load_failures, 5), collapse = ', ')}"))
    if (length(validate_result$load_failures) > 5) {
      cli::cli_alert_info("... and {length(validate_result$load_failures) - 5} more")
    }
  }
  
  # Success message if no issues
  if (!has_issues) {
    cli::cli_alert_success("Environment validated successfully!")
    cli::cli_bullets(c(
      "*" = "Local: {validate_result$local_count} packages",
      "*" = "Docker: {validate_result$docker_count} packages"
    ))
  }
  
  invisible(list(has_issues = has_issues, result = validate_result))
}

#' Validate Docker environment matches local
#'
#' Performs multi-step validation:
#' - Step 1: Check package names exist 
#' - Step 2: Check package versions match 
#' - Step 3: Test that packages actually load
#'
#' @param study_path Path to study directory
#' @param image_name Name of Docker image
#' @param deep_check If TRUE, runs load test (Step 3)
#' @return List with validation results
#' @keywords internal
validateDockerEnvironment <- function(study_path, image_name, deep_check = FALSE) {
  lock_file <- file.path(study_path, "renv.lock")
  if (!file.exists(lock_file)) {
    return(list(
      ok = NA,
      message = "renv.lock not found",
      name_issues = character(),
      version_issues = list(),
      load_failures = character()
    ))
  }

  # Read local package information from renv.lock
  lock_data <- jsonlite::read_json(lock_file)
  local_packages <- names(lock_data$Packages)
  local_count <- length(local_packages)

  # Check if all local packages exist in Docker
  res_names <- dockerExec(c(
    "run", "--rm", image_name,
    "Rscript", "-e",
    "suppressMessages({pkgs<-installed.packages()[,'Package']; cat(paste(sort(pkgs), collapse='|'))})"
  ))

  docker_result <- res_names$out |>
    stringr::str_subset(pattern = "project is out-of-sync|renv::status", negate = TRUE)

  if (!res_names$ok || length(docker_result) == 0) {
    return(list(
      ok = NA,
      message = "Could not query Docker environment",
      name_issues = character(),
      version_issues = list(),
      load_failures = character()
    ))
  }

  docker_packages <- strsplit(docker_result[1], "|", fixed = TRUE)[[1]]
  docker_count <- length(docker_packages)
  missing_packages <- setdiff(local_packages, docker_packages)

  # Compare exact versions between local and Docker
  version_mismatches <- list()
  
  # Get Docker package versions
  res_versions <- dockerExec(c(
    "run", "--rm", image_name,
    "Rscript", "-e",
    "suppressMessages({pkgs<-installed.packages()[,c('Package','Version')]; cat(jsonlite::toJSON(as.data.frame(pkgs, stringsAsFactors=FALSE)))})"
  ))
  
  if (res_versions$ok && length(res_versions$out) > 0) {
    docker_versions <- tryCatch({
      jsonlite::fromJSON(res_versions$out[1])
    }, error = function(e) NULL)
    
    if (!is.null(docker_versions)) {
      # Compare versions for each package
      for (pkg_name in local_packages) {
        if (pkg_name %in% missing_packages) next  # Skip missing packages
        
        local_version <- lock_data$Packages[[pkg_name]]$Version
        docker_idx <- which(docker_versions$Package == pkg_name)
        
        if (length(docker_idx) > 0) {
          docker_version <- docker_versions$Version[docker_idx[1]]
          
          if (!identical(local_version, docker_version)) {
            version_mismatches[[pkg_name]] <- list(
              local = local_version,
              docker = docker_version
            )
          }
        }
      }
    }
  }

  # Test package loading if requested
  load_failures <- character()
  if (isTRUE(deep_check)) {
    load_result <- testPackageLoading(image_name, local_packages)
    if (!is.na(load_result$ok)) {
      load_failures <- load_result$failed
    }
  }

  # Determine overall status
  has_name_issues <- length(missing_packages) > 0
  has_version_issues <- length(version_mismatches) > 0
  has_load_issues <- length(load_failures) > 0
  
  overall_ok <- !has_name_issues && !has_version_issues && !has_load_issues

  list(
    ok = overall_ok,
    local_count = local_count,
    docker_count = docker_count,
    name_issues = missing_packages,
    version_issues = version_mismatches,
    load_failures = load_failures,
    deep_check_run = deep_check
  )
}

# -------------------------------------------------------------------------
# Exported Functions
# -------------------------------------------------------------------------

#' Check if Docker is available
#'
#' @return Logical. TRUE if Docker is installed and reachable, FALSE otherwise
#' @export
checkDocker <- function() {
  ensureDocker(check_daemon = FALSE)
}

#' Build Docker image for an OMOP study
#'
#' @param study_path Path to the study directory
#' @param image_name Optional custom image name. Auto-generated if NULL
#' @return Image name (invisibly)
#' @export
buildStudy <- function(study_path, image_name = NULL) {
  if (!ensureDocker(check_daemon = TRUE)) cli::cli_abort("Docker is not available")

  if (!dir.exists(study_path)) cli::cli_abort("Study path does not exist: {study_path}")

  if (is.null(image_name)) {
    study_name <- basename(normalizePath(study_path))
    image_name <- paste0(DOCKER_IMAGE_PREFIX, tolower(gsub("[^a-z0-9]", "-", study_name)))
  }

  # Detect R version and prepare Dockerfile
  dockerfile_dest <- file.path(study_path, "Dockerfile")
  docker_r_version <- detectRVersion(dockerfile_dest)
  prepareDockerfile(study_path, docker_r_version)

  # Check for renv.lock
  renv_lock <- file.path(study_path, "renv.lock")
  if (file.exists(renv_lock)) {
    cli::cli_alert_info("Using renv.lock for package version control")
  } else {
    cli::cli_alert_warning("No renv.lock found - using default package versions")
    cli::cli_alert_info("Run renv::snapshot() in your study to lock package versions")
  }

  cli::cli_h3("Building Docker image: {image_name}")
  cli::cli_alert_info("This may take 5-15 minutes on first build...")

  res <- dockerExec(c(
    "build",
    "-t", image_name,
    "--label", "omop-study=true",
    "--label", paste0("study-path=", study_path),
    normalizePath(study_path)
  ))

  if (!res$ok) {
    error_text <- paste(res$out, collapse = "\n")
    if (grepl("Cannot connect to the Docker daemon", error_text)) {
      cli::cli_abort(c("Docker daemon is not running.", "i" = "Please start Docker Desktop and try again."))
    }
    if (grepl("permission denied", error_text, ignore.case = TRUE)) {
      cli::cli_abort(c("Permission denied accessing Docker.", "i" = "You may need to run Docker Desktop or check Docker permissions."))
    }
    cli::cli_abort(c("Docker build failed.", "i" = "Check that your Dockerfile is valid and Docker is running properly."))
  }

  cli::cli_alert_success("Image built successfully: {image_name}")

  invisible(image_name)
}

#' Run OMOP study in Docker container
#'
#' @param image_name Name of the Docker image to run
#' @param config_file Path to config.yml file (default: "config.yml")
#' @param data_path Optional path to data directory to mount
#' @param output_path Path where results should be saved (default: "./results")
#' @return Output path (invisibly)
#' @export
runStudy <- function(image_name,
                      config_file = "config.yml",
                      data_path = NULL,
                      output_path = "./results") {
  if (!ensureDocker(check_daemon = TRUE)) cli::cli_abort("Docker is not available")

  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    cli::cli_alert_success("Created output directory: {output_path}")
  }

  mounts <- c("-v", paste0(normalizePath(output_path), ":/output"))

  if (!is.null(data_path)) {
    if (!dir.exists(data_path)) cli::cli_abort("Data path does not exist: {data_path}")
    mounts <- c(mounts, "-v", paste0(normalizePath(data_path), ":/data"))
    cli::cli_alert_info("Mounting data directory: {data_path}")
  }

  if (file.exists(config_file)) {
    cfg_path <- normalizePath(config_file, winslash = "/")
    mounts <- c(mounts, "-v", paste0(cfg_path, ":/study/config.yml"))
    cli::cli_alert_info("Using config file: {config_file}")
  } else {
    cli::cli_alert_warning("Config file not found: {config_file}")
  }

  args <- c("run", "--rm", mounts, image_name, "Rscript", "/study/run_study.R")

  cli::cli_h3("Running study in Docker container...")
  cli::cli_alert_info("Results will be saved to: {output_path}")
  cli::cli_rule()

  res <- dockerExec(args, stream = TRUE)

  cli::cli_rule()
  if (!res$ok) {
    cli::cli_abort(c(
      "Study execution failed.",
      "i" = "Check the output above for error details.",
      "i" = "Ensure your run_study.R file exists and runs without errors."
    ))
  }

  cli::cli_alert_success("Study completed successfully!")
  invisible(output_path)
}

#' Initialize renv in a study directory
#' @param study_path Path to the study directory
#' @return Invisibly returns TRUE
#' @export
initRenv <- function(study_path) {
  if (!dir.exists(study_path)) cli::cli_abort("Study path does not exist: {study_path}")

  if (!requireNamespace("renv", quietly = TRUE)) {
    cli::cli_alert_info("Installing renv package...")
    install.packages("renv")
  }

  cli::cli_alert_info("Initializing renv in: {study_path}")
  withStudyDir(study_path, renv::init())
  cli::cli_alert_success("renv initialized")

  return(invisible(TRUE))
}

#' Snapshot package versions to renv.lock
#' @param study_path Path to the study directory
#' @return Invisibly returns TRUE
#' @export
snapshotPackages <- function(study_path) {
  if (!dir.exists(study_path)) cli::cli_abort("Study path does not exist: {study_path}")

  renv_dir <- file.path(study_path, "renv")
  if (!dir.exists(renv_dir)) {
    cli::cli_abort("renv not initialized. Run init_renv('{study_path}') first")
  }

  cli::cli_alert_info("Taking snapshot of package versions...")
  withStudyDir(study_path, renv::snapshot())
  cli::cli_alert_success("Package versions locked in renv.lock")
  cli::cli_alert_info("Docker builds will now use these exact versions")

  return(invisible(TRUE))
}

#' Sync study: snapshot -> (re)build image if needed -> verify -> optional test run
#' @param study_path Path to the study directory
#' @param force_rebuild If TRUE, forces Docker image rebuild even if up-to-date
#' @param run_test If TRUE, runs test execution in Docker after validation
#' @param output_path Optional path for test execution results
#' @return Invisibly returns TRUE on success, FALSE on failure
#' @export
syncStudy <- function(study_path,
                       force_rebuild = FALSE,
                       run_test = TRUE,
                       output_path = NULL) {

  if (!dir.exists(study_path)) cli::cli_abort("Study path does not exist: {study_path}")

  cli::cli_rule("SYNCING STUDY ENVIRONMENT", line = 2)
  cli::cli_text("")

  # Snapshot packages
  packages_changed <- syncSnapshot(study_path)

  # Build Docker image (conditionally)
  image_name <- paste0(DOCKER_IMAGE_PREFIX, tolower(basename(study_path)))
  syncBuild(study_path, image_name, force_rebuild, packages_changed)

  # Validate Docker environment
  validation_ok <- syncValidate(study_path, image_name)
  if (!validation_ok) return(invisible(FALSE))

  # Test execution (optional)
  test_ok <- syncTest(image_name, run_test, output_path)
  if (!test_ok) return(invisible(FALSE))

  cli::cli_text("")
  cli::cli_rule("SYNC COMPLETE", line = 2)
  cli::cli_text("")
  cli::cli_alert_success("Study environment synchronized and validated")

  return(invisible(TRUE))
}
