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
  ver <- tryCatch(system2("docker", "--version", stdout = TRUE, stderr = TRUE), error = function(e) NULL)

  if (is.null(ver)) {
    cli::cli_abort(c(
      "x" = "Docker not found. Please install Docker Desktop.",
      "i" = "Download from: {.url https://www.docker.com/products/docker-desktop}"
    ))
  }

  version <- gsub("Docker version ([0-9.]+).*", "\\1", ver[1])
  cli::cli_alert_success("Docker {version} detected")

  if (!isTRUE(check_daemon)) return(TRUE)

  daemon <- dockerExec(c("info"), stream = FALSE)
  if (!daemon$ok) {
    cli::cli_abort("x" = "Cannot connect to Docker daemon. Is Docker Desktop running?")
  }
  return(TRUE)
}

#' Get auto-generated image name from directory
#' @keywords internal
autoImageName <- function(path = here::here()) {
  return(tolower(gsub("[^a-z0-9]", "-", basename(normalizePath(path)))))
}

#' Read R version from renv.lock
#' @keywords internal
getRVersionFromLock <- function(study_path) {
  lock_file <- file.path(study_path, "renv.lock")
  
  if (!file.exists(lock_file)) {
    cli::cli_abort(c(
      "x" = "renv.lock not found in {study_path}",
      "i" = "Run {.code renv::snapshot()} to create it"
    ))
  }
  
  tryCatch({
    lock_data <- jsonlite::read_json(lock_file)
    r_version <- lock_data$R$Version
    if (is.null(r_version)) {
      cli::cli_abort("x" = "R version not found in renv.lock")
    }
    cli::cli_alert_success("Using R {r_version} from renv.lock")
    return(r_version)
  }, error = function(e) {
    cli::cli_abort(c(
      "x" = "Could not read renv.lock: {e$message}",
      "i" = "Try {.code renv::snapshot()} again"
    ))
  })
}

#' Map R version to Ubuntu codename
#' @keywords internal
getUbuntuCodename <- function(r_version) {
  parts <- as.numeric(strsplit(r_version, "\\.")[[1]])
  
  if (parts[1] > 4 || (parts[1] == 4 && parts[2] >= 5)) return("noble")
  if (parts[1] == 4 && parts[2] >= 2) return("jammy")
  return("focal")
}

#' Copy template file with variable substitution
#' @keywords internal
copyTemplate <- function(template_name, dest_path, replacements = list()) {
  template <- system.file("docker", template_name, package = "OmopStudyBuilder")
  if (template == "") template <- here::here("inst", "docker", template_name)
  
  if (!file.exists(template)) {
    cli::cli_abort("{template_name} not found in package")
  }
  
  if (length(replacements) > 0) {
    content <- readLines(template)
    for (key in names(replacements)) {
      content <- gsub(key, replacements[[key]], content, fixed = TRUE)
    }
    writeLines(content, dest_path)
  } else {
    file.copy(template, dest_path)
  }
  
  invisible(dest_path)
}

#' Prepare Dockerfile from template
#' @keywords internal
prepareDockerfile <- function(study_path, r_version) {
  dockerfile_dest <- file.path(study_path, "Dockerfile")
  
  if (!file.exists(dockerfile_dest)) {
    ubuntu_codename <- getUbuntuCodename(r_version)
    
    copyTemplate(
      "Dockerfile.template",
      dockerfile_dest,
      list(
        "{{DOCKER_R_VERSION}}" = r_version,
        "{{UBUNTU_CODENAME}}" = ubuntu_codename
      )
    )
    
    cli::cli_alert_success("Created Dockerfile (R {r_version}, Ubuntu {ubuntu_codename})")
  }
  
  # Copy .dockerignore
  dockerignore_dest <- file.path(study_path, ".dockerignore")
  if (!file.exists(dockerignore_dest)) {
    tryCatch({
      copyTemplate(".dockerignore", dockerignore_dest)
      cli::cli_alert_success("Created .dockerignore")
    }, error = function(e) NULL)
  }
  
  invisible(dockerfile_dest)
}

#' Find next available port starting from a given port
#' @keywords internal
findAvailablePort <- function(start_port = 8787, max_tries = 10) {
  for (i in 0:(max_tries - 1)) {
    port <- start_port + i
    check <- dockerExec(c("ps", "-q", "--filter", paste0("publish=", port)))
    if (check$ok && (length(check$out) == 0 || all(check$out == ""))) {
      return(port)
    }
  }
  cli::cli_abort("No available ports found between {start_port} and {start_port + max_tries - 1}")
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
  ensureDocker(check_daemon = TRUE)

  if (!dir.exists(study_path)) cli::cli_abort("Study path does not exist: {study_path}")

  image_name <- if (is.null(image_name)) autoImageName(study_path) else image_name

  # Get R version and prepare Dockerfile
  r_version <- getRVersionFromLock(study_path)
  prepareDockerfile(study_path, r_version)

  cli::cli_h3("Building Docker image: {image_name}")
  cli::cli_alert_info("This may take some minutes on first build...")
  
  # Detect if we need platform flag for ARM64 Macs
  platform_args <- character()
  is_arm <- grepl("arm64|aarch64", Sys.info()["machine"], ignore.case = TRUE)
  if (is_arm) {
    parts <- as.numeric(strsplit(r_version, "\\.")[[1]])
    
    if (parts[1] < 4 || (parts[1] == 4 && parts[2] < 3)) {
      cli::cli_alert_warning("ARM64 Mac detected with R {r_version}")
      cli::cli_alert_danger("RStudio Server does NOT work reliably with R < 4.3 on ARM64 Macs")
      cli::cli_text("")
      cli::cli_bullets(c(
        "x" = "The Docker image will build, but RStudio Server may not be accessible",
        "i" = "Recommendation: Upgrade to R 4.3+ in your renv.lock for native ARM64 support",
        "i" = "Alternative: Run on Intel Mac or Windows, or use command-line execution only"
      ))
      cli::cli_text("")
      cli::cli_alert_info("Continuing build with x86_64 emulation...")
      platform_args <- c("--platform", "linux/amd64")
    } else {
      cli::cli_alert_success("ARM64 Mac detected - using native ARM64 image (R {r_version})")
    }
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
      cli::cli_abort(c("x" = "Docker daemon not running", "i" = "Please start Docker Desktop"))
    }
    if (grepl("permission denied", error_text, ignore.case = TRUE)) {
      cli::cli_abort(c("x" = "Permission denied", "i" = "Check Docker permissions"))
    }
    cli::cli_abort("Docker build failed. Check Dockerfile and Docker daemon.")
  }

  cli::cli_alert_success("Image built successfully: {image_name}")
  invisible(image_name)
}

#' Run RStudio Server for interactive study execution
#'
#' @param image_name Name of the Docker image to run. If NULL, auto-detected from current directory.
#' @param data_path Optional path to data directory (mounted at /data in container)
#' @param results_path Path to save results (default: "./results")
#' @param port Port for RStudio Server (default: 8787, auto-detects next available if in use)
#' @param password RStudio password. If NULL, auto-generated and displayed.
#' @return Container ID (invisibly)
#' @export
runRStudio <- function(image_name = NULL,
                        data_path = NULL,
                        results_path = "./results",
                        port = 8787,
                        password = NULL) {
  ensureDocker(check_daemon = TRUE)
  
  # Auto-detect image name
  if (is.null(image_name)) {
    image_name <- autoImageName()
    cli::cli_alert_info("Using image: {image_name}")
  }
  
  # Check if port is available, find next available if not
  port_check <- dockerExec(c("ps", "-q", "--filter", paste0("publish=", port)))
  if (port_check$ok && length(port_check$out) > 0 && any(port_check$out != "")) {
    original_port <- port
    port <- findAvailablePort(start_port = port)
    cli::cli_alert_warning("Port {original_port} in use, using port {port} instead")
  }
  
  # Generate password if not provided
  if (is.null(password)) password <- paste0("study", sample(1000:9999, 1))
  
  # Create results directory
  if (!dir.exists(results_path)) dir.create(results_path, recursive = TRUE)
  
  # Build mounts
  mounts <- c("-v", paste0(normalizePath(results_path), ":/home/rstudio/study/results"))
  
  if (!is.null(data_path)) {
    if (!dir.exists(data_path)) cli::cli_abort("Data path does not exist: {data_path}")
    mounts <- c(mounts, "-v", paste0(normalizePath(data_path), ":/data"))
    cli::cli_alert_info("Mounting data: {data_path} -> /data")
  }
  
  # Generate unique container name
  container_name <- image_name
  i <- 0
  while (i < 100) {
    check <- dockerExec(c("ps", "-aq", "-f", paste0("name=^", container_name, "$")))
    if (!check$ok || length(check$out) == 0 || all(check$out == "")) break
    i <- i + 1
    container_name <- paste0(image_name, "-", sprintf("%02d", i))
  }
  
  # Start RStudio Server
  args <- c("run", "-d", 
            "--name", container_name,
            "-p", paste0(port, ":8787"),
            "-e", paste0("PASSWORD=", password),
            mounts, 
            image_name)
  
  cli::cli_h3("Starting RStudio Server...")
  res <- dockerExec(args)
  
  if (!res$ok) {
    error_msg <- paste(res$out, collapse = "\n")
    if (grepl("port is already allocated|address already in use", error_msg, ignore.case = TRUE)) {
      cli::cli_abort(c(
        "x" = "Port {port} is already in use",
        "i" = "Try different port: {.code runRStudio(port = 8788)}"
      ))
    }
    if (grepl("No such image", error_msg, ignore.case = TRUE)) {
      cli::cli_abort(c(
        "x" = "Image '{image_name}' not found",
        "i" = "Build it first: {.code buildStudy()}"
      ))
    }
    cli::cli_abort(c("x" = "Failed to start RStudio Server", "i" = "Error: {error_msg}"))
  }
  
  # Extract container ID (filter out warnings, look for hex ID)
  container_id <- NULL
  for (line in res$out) {
    clean_line <- trimws(line)
    if (grepl("^[a-f0-9]{12,64}$", clean_line)) {
      container_id <- clean_line
      break
    }
  }
  
  if (is.null(container_id)) {
    cli::cli_abort(c("x" = "Could not extract container ID", "i" = "Output: {paste(res$out, collapse = ', ')}"))
  }
  
  # Wait and verify
  Sys.sleep(2)
  
  status_check <- dockerExec(c("ps", "-q", "-f", paste0("id=", container_id)))
  if (!status_check$ok || length(status_check$out) == 0 || all(status_check$out == "")) {
    cli::cli_abort(c(
      "x" = "Container exited unexpectedly",
      "i" = "Check logs: {.code docker logs {container_id}}"
    ))
  }
  
  cli::cli_alert_success("RStudio Server started!")
  cli::cli_text("")
  cli::cli_bullets(c(
    "!" = "Open browser: {.url http://localhost:{port}}",
    "!" = "Username: {.strong rstudio}",
    "!" = "Password: {.strong {password}}"
  ))
  cli::cli_text("")
  cli::cli_alert_info("Results will be saved to: {results_path}")
  cli::cli_alert_info("To stop: docker stop {substr(container_id, 1, 12)}")
  
  invisible(container_id)
}


#' Push Docker image to Docker Hub
#'
#' @param image_name Local image name. If NULL, auto-detected from current directory.
#' @param repo Docker Hub repository (e.g., "username/imagename"). If NULL, prompts for username.
#' @return Repository name (invisibly)
#' @export
pushImage <- function(image_name = NULL, repo = NULL) {
  ensureDocker(check_daemon = TRUE)

  # Auto-detect image name
  image_name <- if (is.null(image_name)) autoImageName() else image_name
  
  # Verify image exists
  check <- dockerExec(c("image", "inspect", image_name))
  if (!check$ok) cli::cli_abort("Image '{image_name}' not found. Run buildStudy() first.")
  
  # Get Docker Hub credentials
  if (is.null(repo)) {
    cli::cli_text("")
    username <- readline("Docker Hub username: ")
    repo <- paste0(username, "/", image_name)
  }
  
  cli::cli_text("")
  password <- getPass::getPass("Docker Hub password/token: ")
  
  # Login
  cli::cli_alert_info("Logging in to Docker Hub...")
  login <- suppressWarnings(system2("docker", c("login", "-u", username, "--password-stdin"),
                                    input = password, stdout = TRUE, stderr = TRUE))
  if (!is.null(attr(login, "status")) && attr(login, "status") != 0) {
    cli::cli_abort("Docker Hub login failed. Check credentials.")
  }
  
  # Tag and push
  cli::cli_alert_info("Tagging image as {repo}...")
  dockerExec(c("tag", image_name, repo))
  
  cli::cli_alert_info("Pushing to Docker Hub...")
  res <- dockerExec(c("push", repo), stream = TRUE)
  
  if (!res$ok) cli::cli_abort("Push failed.")
  
  cli::cli_alert_success("Pushed: {repo}")
  invisible(repo)
}
