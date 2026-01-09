# create logger ----
results_folder <- here("Results")
if (!dir.exists(results_folder)) {
  dir.create(results_folder)
}

createLogFile(logFile = tempfile(pattern = "log_{date}_{time}"))
logMessage("LOG CREATED")

# run ----
result <- list()
source(here("Cohorts", "InstantiateCohorts.R"))
logMessage("- Running PhenotypeDiagnostics")
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
