# Docker Configuration - Single Source of Truth
# All Docker-related constants should be defined here

# R version for Docker images
DOCKER_R_VERSION <- "4.5.2"

# Base Docker image
DOCKER_BASE_IMAGE <- paste0("rocker/r-ver:", DOCKER_R_VERSION)

# Image name prefix for OMOP studies
DOCKER_IMAGE_PREFIX <- "omop-study-" 

# Default packages to install if no renv.lock exists
DOCKER_DEFAULT_PACKAGES <- c(
  "dplyr",
  "DBI",
  "DatabaseConnector",
  "yaml",
  "readr",
  "lubridate"
)

# Format default packages for Dockerfile (comma-separated quoted strings)
DOCKER_DEFAULT_PACKAGES_STR <- paste0("'", paste(DOCKER_DEFAULT_PACKAGES, collapse = "', '"), "'")
