# Docker Wrapper Functions for OMOP Studies
# Minimal set of functions for RStudio-based workflow

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


#' Detect and sync R version for Docker
#' @keywords internal
detectRVersion <- function(dockerfile_dest, study_path = NULL) {
  # Always use renv.lock as the source of truth for R version
  if (is.null(study_path)) {
    cli::cli_abort("study_path is required to detect R version from renv.lock")
  }
  
  lock_file <- file.path(study_path, "renv.lock")
  if (!file.exists(lock_file)) {
    cli::cli_abort(c(
      "x" = "renv.lock not found in {study_path}",
      "i" = "Navigate to your study directory first: {.code setwd('{study_path}')}",
      "i" = "Then run: {.code renv::snapshot()}",
      "i" = "This ensures reproducible package versions and R version across all environments"
    ))
  }
  
  # Read R version from renv.lock
  tryCatch({
    lock_data <- jsonlite::read_json(lock_file)
    if (!is.null(lock_data$R$Version)) {
      docker_r_version <- lock_data$R$Version
      cli::cli_alert_success("Using R {docker_r_version} from renv.lock")
      return(docker_r_version)
    } else {
      cli::cli_abort(c(
        "x" = "R version not found in renv.lock",
        "i" = "Your renv.lock may be corrupted. Try running {.code renv::snapshot()} again"
      ))
    }
  }, error = function(e) {
    cli::cli_abort(c(
      "x" = "Could not read renv.lock: {e$message}",
      "i" = "Your renv.lock may be corrupted. Try running {.code renv::snapshot()} again"
    ))
  })
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
    writeLines(template_content, dockerfile_dest)

    cli::cli_alert_success("Created Dockerfile in study directory (R version: {docker_r_version})")
  }
  
  # Copy .dockerignore template
  dockerignore_dest <- file.path(study_path, ".dockerignore")
  if (!file.exists(dockerignore_dest)) {
    dockerignore_template <- system.file("docker", ".dockerignore", package = "OmopStudyBuilder")
    if (!file.exists(dockerignore_template) || dockerignore_template == "") {
      dockerignore_template <- here::here("inst", "docker", ".dockerignore")
    }
    if (file.exists(dockerignore_template)) {
      file.copy(dockerignore_template, dockerignore_dest)
      cli::cli_alert_success("Created .dockerignore")
    }
  }
  
  invisible(dockerfile_dest)
}

# -------------------------------------------------------------------------
# Exported Functions
# -------------------------------------------------------------------------

#' Build Docker image for an OMOP study
#'
#' @param study_path Path to the study directory (defaults to current working directory)
#' @param image_name Optional custom image name. Auto-generated if NULL
#' @return Image name (invisibly)
#' @export
buildStudy <- function(study_path = here::here(), image_name = NULL) {
  if (!ensureDocker(check_daemon = TRUE)) cli::cli_abort("Docker is not available")

  if (!dir.exists(study_path)) cli::cli_abort("Study path does not exist: {study_path}")

  if (is.null(image_name)) {
    study_name <- basename(normalizePath(study_path))
    image_name <- tolower(gsub("[^a-z0-9]", "-", study_name))
  }

  # Detect R version and prepare Dockerfile
  dockerfile_dest <- file.path(study_path, "Dockerfile")
  docker_r_version <- detectRVersion(dockerfile_dest, study_path)
  prepareDockerfile(study_path, docker_r_version)

  # Enforce renv.lock requirement
  renv_lock <- file.path(study_path, "renv.lock")
  if (!file.exists(renv_lock)) {
    cli::cli_abort(c(
      "x" = "renv.lock not found in {study_path}",
      "i" = "Navigate to your study directory first: {.code setwd('{study_path}')}",
      "i" = "Then run: {.code renv::snapshot()}",
      "i" = "This ensures reproducible package versions across all environments"
    ))
  }
  cli::cli_alert_info("Using renv.lock for package version control")

  cli::cli_h3("Building Docker image: {image_name}")
  cli::cli_alert_info("This may take some minutes on first build...")
  
  # Detect if we need platform flag for ARM64 Macs
  platform_args <- character()
  is_arm <- grepl("arm64|aarch64", Sys.info()["machine"], ignore.case = TRUE)
  if (is_arm) {
    cli::cli_alert_info("ARM64 Mac detected - using x86_64 emulation for RStudio compatibility")
    cli::cli_alert_warning("RStudio Server may take some minutes to start due to emulation")
    platform_args <- c("--platform", "linux/amd64")
  }

  res <- dockerExec(c(
    "build",
    platform_args,
    "-t", image_name,
    "--label", "omop-study=true",
    "--label", paste0("study-path=", study_path),
    normalizePath(study_path)
  ), stream = TRUE)

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

#' Run RStudio Server for interactive study execution
#'
#' @param image_name Name of the Docker image to run. If NULL, auto-detected from current directory.
#' @param data_path Optional path to data directory (mounted at /data in container)
#' @param results_path Path to save results (default: "./results")
#' @param port Port for RStudio Server (default: 8787)
#' @param password RStudio password. If NULL, auto-generated and displayed.
#' @return Container ID (invisibly)
#' @export
runRStudio <- function(image_name = NULL,
                        data_path = NULL,
                        results_path = "./results",
                        port = 8787,
                        password = NULL) {
  if (!ensureDocker(check_daemon = TRUE)) cli::cli_abort("Docker is not available")
  
  # Auto-detect image name from current directory if not provided
  if (is.null(image_name)) {
    study_name <- basename(here::here())
    image_name <- tolower(gsub("[^a-z0-9]", "-", study_name))
    cli::cli_alert_info("Using image: {image_name}")
  }
  
  # Generate password if not provided
  if (is.null(password)) {
    password <- paste0("study", sample(1000:9999, 1))
  }
  
  # Create results directory
  if (!dir.exists(results_path)) {
    dir.create(results_path, recursive = TRUE)
  }
  
  # Build mounts
  mounts <- c("-v", paste0(normalizePath(results_path), ":/home/rstudio/study/results"))
  
  if (!is.null(data_path)) {
    if (!dir.exists(data_path)) cli::cli_abort("Data path does not exist: {data_path}")
    mounts <- c(mounts, "-v", paste0(normalizePath(data_path), ":/data"))
    cli::cli_alert_info("Mounting data: {data_path} -> /data")
  }
  
  # Detect if we need platform flag for ARM64 Macs
  platform_args <- character()
  is_arm <- grepl("arm64|aarch64", Sys.info()["machine"], ignore.case = TRUE)
  if (is_arm) {
    platform_args <- c("--platform", "linux/amd64")
  }
  
  # Start RStudio Server
  args <- c("run", "-d", 
            platform_args,
            "-p", paste0(port, ":8787"),
            "-e", paste0("PASSWORD=", password),
            mounts, 
            image_name)
  
  cli::cli_h3("Starting RStudio Server...")
  res <- dockerExec(args)
  
  if (!res$ok) {
    cli::cli_abort("Failed to start RStudio Server")
  }
  
  container_id <- trimws(res$out[1])
  
  if (is_arm) {
    cli::cli_alert_success("RStudio Server starting...")
    cli::cli_alert_warning("ARM64 emulation detected - RStudio may take some minutes to be ready")
    cli::cli_alert_info("Please wait, then try opening the browser...")
  } else {
    cli::cli_alert_success("RStudio Server started!")
  }
  cli::cli_text("")
  cli::cli_bullets(c(
    "!" = "Open browser: {.url http://localhost:{port}}",
    "!" = "Username: {.strong rstudio}",
    "!" = "Password: {.strong {password}}"
  ))
  cli::cli_text("")
  cli::cli_alert_info("Edit {.file code_to_run.R} to configure database credentials")
  cli::cli_alert_info("Results will be saved to: {results_path}")
  cli::cli_text("")
  cli::cli_alert_info("To stop: docker stop {substr(container_id, 1, 12)}")
  
  invisible(container_id)
}
