# create logger ----
resultsFolder <- here("Results")
if(!dir.exists(resultsFolder)){
  dir.create(resultsFolder)
}
loggerName <- gsub(":| |-", "", paste0("log_", Sys.Date(),".txt"))
logger <- create.logger()
logfile(logger) <- here(resultsFolder, loggerName)
level(logger) <- "INFO"
info(logger, "LOG CREATED")

# run ----
result <- list()
source(here("Cohorts","InstantiateCohorts.R"))
info(logger, "- Running PhenotypeDiagnostics")
diagnostics <- phenotypeDiagnostics(cdm$study_cohorts,
                          survival = FALSE,
                          cohortSample = 20000,
                          matchedSample = NULL,
                          populationSample = NULL)
exportSummarisedResult(diagnostics,
                       minCellCount = minCellCount,
                       fileName = "phenotyper_results_{cdm_name}_{date}.csv",
                       path = resultsFolder)
info(logger, "Finished")
