# create logger ----
resultsFolder <- here("results")
if(!dir.exists(resultsFolder)){
  dir.create(resultsFolder)
}

createLogFile(logFile = tempfile(pattern = "log_{date}_{time}"))
logMessage("LOG CREATED")

# run ----
source(here("cohorts","instantiate_cohorts.R"))
info(logger, "- Running PhenotypeDiagnostics")
diagnostics <- phenotypeDiagnostics(cdm$study_cohorts,
                          survival = FALSE,
                          cohortSample = 20000,
                          matchedSample = NULL,
                          populationSample = NULL)

exportSummarisedResult(diagnostics,
                       minCellCount = minCellCount,
                       fileName = "phenotyper_results_{cdm_name}_{date}.csv",
                       path = results_folder)
logMessage("Finished")
