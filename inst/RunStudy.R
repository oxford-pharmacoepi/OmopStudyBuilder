resultsFolder <- here("Results")

# Create log file ----
loggerName <- gsub(":| |-", "", paste0("log_", Sys.Date(),".txt"))
logger <- create.logger()
logfile(logger) <- here(resultsFolder, loggerName)
level(logger) <- "INFO"
info(logger, "LOG CREATED")

# Instantiate study cohorts ----
info(logger, "Instantiating study cohorts")
source(here("Cohorts", "InstantiateCohorts.R"))
info(logger, "Study cohorts instantiated")

# Run analyses ----
info(logger, "Run study analyses")
source(here("Cohorts", "1-Analysis_1.R"))
info(logger, "Analyses finished")

# export results ----
info(logger, "EXPORTING RESULTS")
zip(
  zipfile = file.path(paste0(resultsFolder, "/Results_", cdmName(cdm), ".zip")),
  files = list.files(resultsFolder, full.names = TRUE)
)
