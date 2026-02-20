# Docker Wrapper Functions for OMOP Studies

#' Execute a Docker command
#' @param args Character vector of Docker command arguments
#' @param error_message Custom error message if command fails
#' @return Command output as character vector (returned invisibly to avoid console clutter)
#' @keywords internal
dockerExec <- function(args, error_message = "Docker command failed") {
  result <- system2("docker", args, stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    stop(error_message, "\n", paste(result, collapse = "\n"), call. = FALSE)
  }
  return(invisible(result))
}

#' Stop a running OmopStudyBuilder container (automated or RStudio)
#'
#' Stops containers started by \code{runStudy()} and/or \code{runRStudio()}.
#' By default it stops containers for the image inferred from the current
#' directory name.
#'
#' @param container Optional container name or ID to stop directly.
#' @param image_name Optional Docker image name. Defaults to current directory name.
#' @param mode Which container(s) to stop: "any" (default), "rstudio", or "run".
#' @param all If TRUE, stops all running containers started by OmopStudyBuilder.
#' @return TRUE if at least one container was stopped (invisibly)
#' @export
stopStudy <- function(container = NULL, image_name = NULL, mode = c("any", "rstudio", "run"), all = FALSE) {
  ensureDocker()
  mode <- match.arg(mode)
  
  if (!is.null(container) && nzchar(container)) {
    message("Stopping container...")
    dockerExec(c("stop", container), paste0("Failed to stop container: ", container))
    message("Container stopped: ", substr(container, 1, 12))
    return(invisible(TRUE))
  }
  
  if (!isTRUE(all)) {
    image_name <- autoDetectImageName(image_name)
  }
  
  filters <- c("-f", "label=omopstudybuilder=true")
  if (!isTRUE(all)) filters <- c(filters, "-f", paste0("label=omopstudybuilder.image=", image_name))
  if (mode != "any") filters <- c(filters, "-f", paste0("label=omopstudybuilder.mode=", mode))

  ids <- system2("docker", c("ps", "-q", filters), stdout = TRUE, stderr = TRUE)
  ids_status <- attr(ids, "status")
  if (!is.null(ids_status) && !identical(as.integer(ids_status), 0L)) ids <- character(0)
  ids <- ids[nzchar(ids)]
  
  if (length(ids) == 0 && !isTRUE(all) && mode %in% c("any", "rstudio")) {
    ids <- system2(
      "docker",
      c("ps", "-q", "-f", paste0("ancestor=", image_name)),
      stdout = TRUE,
      stderr = TRUE
    )
    ids_status <- attr(ids, "status")
    if (!is.null(ids_status) && !identical(as.integer(ids_status), 0L)) ids <- character(0)
    ids <- ids[nzchar(ids)]
  }
  
  if (length(ids) == 0) {
    stop("No running OmopStudyBuilder containers found",
         if (!isTRUE(all)) paste0(" for image: ", image_name) else "", call. = FALSE)
  }
  
  message("Stopping ", length(ids), " container(s)...")
  for (id in ids) dockerExec(c("stop", id), paste0("Failed to stop: ", id))
  message("Stopped.")
  return(invisible(TRUE))
}

#' Check if Docker daemon is running
#' @return TRUE if Docker is available, throws error otherwise
#' @keywords internal
ensureDocker <- function() {
  docker_bin <- Sys.which("docker")
  if (!nzchar(docker_bin)) {
    stop(
      "Docker CLI not found on PATH.\n",
      "Install Docker Desktop (or ensure 'docker' is on PATH) and try again.",
      call. = FALSE
    )
  }

  result <- suppressWarnings(system2(docker_bin, "info", stdout = TRUE, stderr = TRUE))
  status <- attr(result, "status")
  if (is.null(status)) status <- 0L

  if (!identical(as.integer(status), 0L)) {
    stop(
      "Docker daemon is not running.\n",
      "Please start Docker Desktop and try again.\n\n",
      paste(result, collapse = "\n"),
      call. = FALSE
    )
  }

  return(TRUE)
}

#' Build a Docker image for an OMOP study
#' @param image_name Name for the Docker image (default: auto-detected from directory)
#' @param path Path to study directory (default: current directory)
#' @param useRStudio Use RStudio Server base (TRUE) or r-ver base (FALSE, default)
#' @param r_version R version override (default: auto-detected from renv.lock)
#' @param snapshot Update renv.lock before building (default: TRUE)
#' @param github_token Optional GitHub token for installing GitHub packages during build
#' @return Image name (invisibly - already printed to console)
#' @export
buildStudy <- function(image_name = NULL, 
                       path = ".", 
                       useRStudio = FALSE,
                       r_version = NULL,
                       snapshot = TRUE,
                       github_token = NULL) {
  # Ensure Docker is running
  ensureDocker()

  if (!requireNamespace("renv", quietly = TRUE)) {
    stop(
      "Package 'renv' is required for buildStudy().\n",
      "Install it with: install.packages('renv')",
      call. = FALSE
    )
  }
  
  # Update renv.lock to capture all dependencies
  if (snapshot) {
    message("Updating renv.lock (project dependencies only)...")
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(path)
    
    # Check for and repair broken symlinks in renv cache
    message("Checking renv integrity...")
    suppressWarnings(suppressMessages(try(renv::repair(), silent = TRUE)))
    
    # Check for missing packages and auto-install them
    deps <- try(renv::dependencies(), silent = TRUE)
    if (inherits(deps, "try-error")) deps <- NULL
    if (!is.null(deps) && nrow(deps) > 0) {
      required_pkgs <- unique(deps$Package)
      installed_pkgs <- rownames(utils::installed.packages())
      missing_pkgs <- setdiff(required_pkgs, installed_pkgs)
      
      if (length(missing_pkgs) > 0) {
        message("Found ", length(missing_pkgs), " missing package(s): ", paste(missing_pkgs, collapse = ", "))
        
        # Try to install each package, but don't fail if some can't install on host
        # (database drivers like odbc/RPostgres often fail on Mac but work fine in Docker)
        for (pkg in missing_pkgs) {
          message("Attempting to install ", pkg, "...")
          install_res <- try(renv::install(pkg, prompt = FALSE), silent = TRUE)
          if (inherits(install_res, "try-error")) {
            cond <- attr(install_res, "condition")
            msg <- if (!is.null(cond) && nzchar(cond$message)) cond$message else as.character(install_res)
            message("  Warning: Could not install ", pkg, " on host machine (", msg, ")")
            message("  This is OK if ", pkg, " is only needed inside Docker (e.g., database drivers)")
          }
        }
      }
    }
    
    # Snapshot all dependencies (non-interactive, force include all dependencies)
    snapshot_res <- try({
      # Force snapshot even if some packages couldn't install on host
      # Use type = "all" to include all dependencies found in code, regardless of installation status
      old_interactive <- getOption("renv.config.auto.snapshot")
      options(renv.config.auto.snapshot = FALSE)
      on.exit(options(renv.config.auto.snapshot = old_interactive), add = TRUE)
      
      renv::snapshot(type = "all", prompt = FALSE, force = TRUE)
      message("renv.lock updated successfully")
      
      # Quick status check (don't halt on inconsistencies)
      status_msg <- try(utils::capture.output(renv::status()), silent = TRUE)
      if (inherits(status_msg, "try-error")) status_msg <- character(0)
      if (length(status_msg) > 0 && any(grepl("inconsistent", status_msg, ignore.case = TRUE))) {
        message("Note: Some packages couldn't install on your host machine but are in renv.lock for Docker to install.")
      }
      TRUE
    }, silent = TRUE)

    if (inherits(snapshot_res, "try-error")) {
      cond <- attr(snapshot_res, "condition")
      msg <- if (!is.null(cond) && nzchar(cond$message)) cond$message else as.character(snapshot_res)
      stop("Failed to snapshot dependencies: ", msg, call. = FALSE)
    }
  }

  # If the lockfile includes GitHub packages, Docker builds may require a token
  # (rate limits / restricted network environments).
  if (is.null(github_token) || !nzchar(github_token)) {
    lock_file_for_check <- file.path(path, "renv.lock")
    if (file.exists(lock_file_for_check)) {
      lock_data_for_check <- try(jsonlite::read_json(lock_file_for_check), silent = TRUE)
      if (inherits(lock_data_for_check, "try-error")) lock_data_for_check <- NULL
      pkgs <- NULL
      if (is.list(lock_data_for_check)) pkgs <- lock_data_for_check$Packages
      if (is.list(pkgs) && length(pkgs) > 0) {
        has_github <- any(vapply(pkgs, function(rec) {
          src <- rec[["Source"]]
          isTRUE(identical(src, "GitHub"))
        }, logical(1)))
        if (isTRUE(has_github) && !nzchar(Sys.getenv("GITHUB_PAT"))) {
          warning(
            "renv.lock contains GitHub packages. If Docker build fails downloading from GitHub, ",
            "provide github_token=... (or set GITHUB_PAT) when calling buildStudy().",
            call. = FALSE
          )
        }
      }
    }
  }
  
  # Auto-detect R version from renv.lock if not provided
  if (is.null(r_version)) {
    lock_file <- file.path(path, "renv.lock")
    lock_data <- jsonlite::read_json(lock_file)
    r_version <- lock_data$R$Version
    if (is.null(r_version)) {
      stop("Could not find R version in renv.lock", call. = FALSE)
    }
    message("Detected R version from renv.lock: ", r_version)
  }
  
  # Verify required renv files exist
  required_files <- c("renv.lock", ".Rprofile", file.path("renv", "activate.R"))
  missing <- required_files[!file.exists(file.path(path, required_files))]
  if (length(missing) > 0) {
    stop(
      "Missing required renv files: ", paste(missing, collapse = ", "), "\n",
      "Initialize renv with: renv::init()",
      call. = FALSE
    )
  }
  
  # Ensure .dockerignore exists (prevents copying large/unnecessary files)
  dockerignore_path <- file.path(path, ".dockerignore")
  if (!file.exists(dockerignore_path)) {
    writeLines(c(
      "# RStudio / R artifacts",
      ".Rproj.user",
      ".Rhistory",
      ".RData",
      ".Ruserdata",
      "*.Rproj",
      "",
      "# OS files",
      "Thumbs.db",
      ".DS_Store",
      "",
      "# Build artifacts",
      "Dockerfile",
      "",
      "# Output directories",
      "results/",
      "",
      "# renv library (rebuilt inside container)",
      "renv/library/",
      "renv/staging/",
      "renv/local/",
      "",
      "# Git",
      ".git",
      ".gitignore"
    ), dockerignore_path)
    message("Generated .dockerignore")
  }
  
  # Auto-detect image name from directory if not provided
  if (is.null(image_name)) {
    image_name <- tolower(basename(normalizePath(path)))
    message("Auto-detected image name: ", image_name)
  }
  
  # Determine base image
  base_type <- if (useRStudio) "rstudio" else "r-ver"
  base_image <- paste0("rocker/", base_type, ":", r_version)
  
  # Generate Dockerfile from template
  template_path <- system.file("docker", "Dockerfile.template", package = "OmopStudyBuilder")
  if (!file.exists(template_path)) {
    stop("Dockerfile.template not found in package", call. = FALSE)
  }
  dockerfile_content <- paste(readLines(template_path), collapse = "\n")
  substitutions <- list(
    BASE_IMAGE = base_image,
    EXPOSE_PORT = if (useRStudio) "\n\n# Expose RStudio Server port\nEXPOSE 8787" else ""
  )
  for (name in names(substitutions)) {
    placeholder <- paste0("{{", name, "}}")
    dockerfile_content <- gsub(placeholder, substitutions[[name]], dockerfile_content, fixed = TRUE)
  }
  
  dockerfile_path <- file.path(path, "Dockerfile")
  
  # Write Dockerfile
  writeLines(dockerfile_content, dockerfile_path)
  message("Generated Dockerfile with base: ", base_image)
  
  # Build the image with streaming output
  message("Building Docker image: ", image_name, ":latest")
  message("This may take 5-10 minutes on first build...\n")
  
  # Use system() for real-time streaming output
  build_args <- ""
  if (!is.null(github_token) && nzchar(github_token)) {
    build_args <- paste0("--build-arg GITHUB_PAT=", shQuote(github_token))
  }
  exit_code <- system(
    sprintf("docker build --progress=plain -t %s %s %s", 
            paste0(image_name, ":latest"), 
            build_args,
            shQuote(path))
  )
  
  if (exit_code != 0) {
    stop("Failed to build image: ", image_name, call. = FALSE)
  }
  
  message("\nSuccessfully built: ", image_name, ":latest")
  
  # Return image name invisibly (already shown in messages)
  return(invisible(image_name))
}

#' Find next available port starting from a given port
#' @param start_port Starting port number to check
#' @param max_tries Maximum number of ports to try
#' @return Available port number
#' @keywords internal
findAvailablePort <- function(start_port = 8787, max_tries = 10) {
  for (i in 0:(max_tries - 1)) {
    port <- start_port + i
    result <- system2(
      "docker",
      c("ps", "-q", "--filter", paste0("publish=", port)),
      stdout = TRUE,
      stderr = TRUE
    )
    result_status <- attr(result, "status")
    if (!is.null(result_status) && !identical(as.integer(result_status), 0L)) result <- character(0)
    
    if (length(result) == 0) {
      con <- suppressWarnings(try(
        socketConnection(host = "127.0.0.1", port = port, open = "r+", timeout = 0.25),
        silent = TRUE
      ))
      in_use_local <- !inherits(con, "try-error")
      if (isTRUE(in_use_local)) close(con)
      
      if (!isTRUE(in_use_local)) return(port)
    }
  }
  stop("No available ports found between ", start_port, " and ", start_port + max_tries - 1, call. = FALSE)
}

#' Auto-detect image name from current directory
#' @param image_name Existing image name (if provided)
#' @return Image name (lowercased directory basename)
#' @keywords internal
autoDetectImageName <- function(image_name = NULL) {
  if (!is.null(image_name)) {
    return(image_name)
  }
  detected <- tolower(basename(normalizePath(".")))
  message("Auto-detected image name: ", detected)
  return(detected)
}

#' Generate unique Docker container name
#' @param base_name Base name for container
#' @param suffix Optional suffix (e.g., "-run")
#' @param max_tries Maximum attempts to find unique name
#' @return Unique container name
#' @keywords internal
generateContainerName <- function(base_name, suffix = NULL, max_tries = 100) {
  container_name <- if (is.null(suffix)) base_name else paste0(base_name, suffix)
  for (i in 0:(max_tries - 1)) {
    check <- system2(
      "docker",
      c("ps", "-aq", "-f", paste0("name=^", container_name, "$")),
      stdout = TRUE,
      stderr = TRUE
    )
    check_status <- attr(check, "status")
    if (!is.null(check_status) && !identical(as.integer(check_status), 0L)) check <- character(0)
    if (length(check) == 0) return(container_name)
    if (i < max_tries - 1) {
      suffix_str <- if (is.null(suffix)) "" else suffix
      container_name <- paste0(base_name, suffix_str, "-", sprintf("%02d", i + 1))
    }
  }
  stop("Could not find available container name after ", max_tries, " attempts", call. = FALSE)
}

#' Process environment file argument
#' @param env_file Path to .env file or NULL
#' @param default_env_file Default env file path to use when env_file is NULL
#' @return Character vector of Docker env-file arguments (empty if no file)
#' @keywords internal
processEnvFile <- function(env_file, default_env_file = ".env") {
  if (is.null(env_file)) {
    if (is.null(default_env_file) || !nzchar(default_env_file) || !file.exists(default_env_file)) {
      return(character(0))
    }
    env_file <- default_env_file
  }
  if (!file.exists(env_file)) stop(".env file not found: ", env_file, call. = FALSE)
  message("Using env file: ", env_file)
  return(c("--env-file", normalizePath(env_file, winslash = "/", mustWork = TRUE)))
}

#' Build Docker labels for OmopStudyBuilder containers
#' @param image_name Docker image name
#' @param mode Container mode ("rstudio" or "run")
#' @return Character vector of Docker label arguments
#' @keywords internal
buildDockerLabels <- function(image_name, mode) {
  return(c(
    "--label", "omopstudybuilder=true",
    "--label", paste0("omopstudybuilder.image=", image_name),
    "--label", paste0("omopstudybuilder.mode=", mode)
  ))
}

#' Verify Docker image exists and provide helpful error
#' @param image_name Name of Docker image
#' @param check_rstudio If TRUE, also verify RStudio Server is present
#' @return TRUE (invisibly) if checks pass, otherwise throws error
#' @keywords internal
verifyImageExists <- function(image_name, check_rstudio = FALSE) {
  inspect_status <- suppressWarnings(try(system2(
    "docker",
    c("image", "inspect", image_name),
    stdout = FALSE,
    stderr = FALSE
  ), silent = TRUE))
  if (inherits(inspect_status, "try-error")) inspect_status <- 1L
  if (!isTRUE(inspect_status == 0)) {
    stop("Image '", image_name, "' not found.\nBuild it first with: buildStudy()",
         if (check_rstudio) "\nFor RStudio mode, build with: buildStudy(useRStudio = TRUE)" else "",
         call. = FALSE)
  }
  if (check_rstudio) {
    rserver_status <- suppressWarnings(try(system2(
      "docker",
      c("run", "--rm", "--entrypoint", "/bin/sh", image_name,
        "-c", "command -v rserver >/dev/null 2>&1"),
      stdout = FALSE,
      stderr = FALSE
    ), silent = TRUE))
    if (inherits(rserver_status, "try-error")) rserver_status <- 1L

    if (!isTRUE(rserver_status == 0)) {
      warning(
        "RStudio Server not available in image '", image_name, "'.\n",
        "Rebuild with: buildStudy(useRStudio = TRUE)",
        call. = FALSE
      )
      stop("Cannot start RStudio Server from an image without RStudio Server.", call. = FALSE)
    }
  }
  return(invisible(TRUE))
}

#' Run RStudio Server for interactive study execution
#'
#' Note: The Docker image must include RStudio Server (i.e., be built with
#' `buildStudy(useRStudio = TRUE)` which uses a `rocker/rstudio` base image).
#' @param image_name Name of Docker image to run (default: auto-detected from directory)
#' @param results_path Path to save results (default: "./results")
#' @param env_file Optional path to a .env file (passed to Docker via --env-file).
#'   If NULL and a `.env` file exists in the current working directory, it will be
#'   used automatically.
#' @param port Port for RStudio Server (default: 8787, auto-finds next available if busy)
#' @param password RStudio password (default: auto-generated and displayed)
#' @return Container ID (invisibly)
#' @importFrom utils browseURL
#' @export
runRStudio <- function(image_name = NULL, results_path = "./results", env_file = NULL,
                       port = 8787, password = NULL) {
  ensureDocker()
  image_name <- autoDetectImageName(image_name)
  verifyImageExists(image_name, check_rstudio = TRUE)
  
  original_port <- port
  port <- findAvailablePort(start_port = port)
  if (port != original_port) message("Port ", original_port, " in use, using port ", port, " instead")
  
  if (is.null(password)) password <- paste0("study", sample(1000:9999, 1))
  if (!dir.exists(results_path)) dir.create(results_path, recursive = TRUE)
  
  env_args <- processEnvFile(env_file)
  container_name <- generateContainerName(image_name)
  labels <- buildDockerLabels(image_name, "rstudio")
  
  args <- c("run", "-d", "--rm", "--name", container_name, "-p", paste0(port, ":8787"),
            labels, env_args, "-e", paste0("PASSWORD=", password),
            "-v", paste0(normalizePath(results_path), ":/home/rstudio/study/results"), image_name)
  
  message("Starting RStudio Server...")
  container_id <- dockerExec(args, "Failed to start RStudio Server")[1]
  Sys.sleep(2)
  
  check <- system2(
    "docker",
    c("ps", "-q", "-f", paste0("id=", container_id)),
    stdout = TRUE,
    stderr = TRUE
  )
  check_status <- attr(check, "status")
  if (!is.null(check_status) && !identical(as.integer(check_status), 0L)) check <- character(0)
  if (length(check) == 0) {
    stop("Container exited unexpectedly.\nCheck logs with: docker logs ", container_id, call. = FALSE)
  }
  
  hosts_to_try <- c("127.0.0.1", "localhost", "::1")
  reachable_host <- NULL
  start <- Sys.time()
  while (is.null(reachable_host) && as.numeric(difftime(Sys.time(), start, units = "secs")) < 30) {
    for (host in hosts_to_try) {
      con <- suppressWarnings(try(socketConnection(host = host, port = port, open = "r+", timeout = 0.5), silent = TRUE))
      ok <- !inherits(con, "try-error")
      if (isTRUE(ok)) close(con)
      if (isTRUE(ok)) {
        reachable_host <- host
        break
      }
    }
    if (is.null(reachable_host)) Sys.sleep(0.5)
  }
  if (is.null(reachable_host)) {
    logs <- try(system2("docker", c("logs", "--tail", "200", container_id), stdout = TRUE, stderr = TRUE), silent = TRUE)
    if (inherits(logs, "try-error")) {
      cond <- attr(logs, "condition")
      msg <- if (!is.null(cond) && nzchar(cond$message)) cond$message else as.character(logs)
      logs <- paste0("(Failed to read logs: ", msg, ")")
    }
    stop("RStudio container started but not listening on port ", port, " after 30s.\n\n",
         "Last 200 log lines:\n", paste(logs, collapse = "\n"), call. = FALSE)
  }
  
  url_host <- if (grepl(":", reachable_host, fixed = TRUE)) paste0("[", reachable_host, "]") else reachable_host
  url <- paste0("http://", url_host, ":", port)
  message("\nRStudio Server started successfully!\n\n  URL:      ", url, "\n  Username: rstudio",
          "\n  Password: ", password, "\n\nResults will be saved to: ", results_path,
          "\nContainer will auto-remove when stopped.\n\nTo stop: docker stop ", substr(container_id, 1, 12))

  browse_res <- try(browseURL(url), silent = TRUE)
  if (inherits(browse_res, "try-error")) {
    message("\nCould not open browser automatically.\nPlease open manually: ", url)
  } else {
    message("\nOpening browser...")
  }
  
  return(invisible(container_id))
}

#' Run study in automated mode with real-time log streaming
#' @param image_name Name of Docker image to run (default: auto-detected from directory)
#' @param results_path Path to save results (default: "./results")
#' @param env_file Optional path to a .env file (passed to Docker via --env-file).
#'   If NULL and a `.env` file exists in the current working directory, it will be
#'   used automatically.
#' @param data_path Optional path to data directory (mounted at /data)
#' @param script_path Path to R script to execute (default: "code_to_run.R")
#' @return Exit status (0 = success, non-zero = failure)
#' @export
runStudy <- function(image_name = NULL, results_path = "./results", env_file = NULL,
                     data_path = NULL, script_path = "code_to_run.R") {
  ensureDocker()
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required for runStudy().\nInstall it with: install.packages('processx')", call. = FALSE)
  }
  
  image_name <- autoDetectImageName(image_name)
  verifyImageExists(image_name)
  
  if (!dir.exists(results_path)) dir.create(results_path, recursive = TRUE)
  env_args <- processEnvFile(env_file)
  
  mounts <- c("-v", paste0(normalizePath(results_path), ":/workspace/results"))
  if (!is.null(data_path)) {
    if (!dir.exists(data_path)) stop("Data directory not found: ", data_path, call. = FALSE)
    mounts <- c(mounts, "-v", paste0(normalizePath(data_path), ":/data"))
    message("Mounting data: ", data_path)
  }
  
  container_name <- generateContainerName(image_name, suffix = "-run")
  labels <- buildDockerLabels(image_name, "run")
  args <- c("run", "--rm", "--name", container_name, labels, env_args, mounts, 
            image_name, "Rscript", script_path)
  
  message("\n========================================\nStarting automated study execution...\nContainer: ", 
          container_name, "\nScript: ", script_path, "\nResults: ", results_path, 
          "\n========================================\n")
  
  proc <- processx::process$new("docker", args, stdout = "|", stderr = "|", 
                                echo_cmd = FALSE, cleanup_tree = TRUE)
  
  while (proc$is_alive()) {
    if (proc$is_incomplete_output()) {
      output <- proc$read_output(n = 1000)
      if (length(output) > 0 && output != "") cat(output)
    }
    if (proc$is_incomplete_error()) {
      errors <- proc$read_error(n = 1000)
      if (length(errors) > 0 && errors != "") cat(errors, file = stderr())
    }
    proc$poll_io(100)
    Sys.sleep(0.05)
  }
  
  remaining_out <- proc$read_all_output()
  if (length(remaining_out) > 0 && remaining_out != "") cat(remaining_out)
  remaining_err <- proc$read_all_error()
  if (length(remaining_err) > 0 && remaining_err != "") cat(remaining_err, file = stderr())
  
  exit_status <- proc$get_exit_status()
  message("\n========================================")
  if (exit_status == 0) {
    message("Study completed successfully!\nResults saved to: ", results_path)
  } else {
    message("Study failed with exit code: ", exit_status, "\nCheck logs above for errors")
  }
  message("========================================\n")
  return(invisible(exit_status))
}
