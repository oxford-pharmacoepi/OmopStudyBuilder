
# Check codeToRun inputs ----
validateCdmArgument(
  cdm = cdm,
  requiredTables = c("person", "observation_period", "condition_occurrence",
                     "drug_exposure", "concept")
)
assertNumeric(min_cell_count)

# Define analysis settings -----
results_folder <- here("results")
study_period <- c(as.Date(NA), as.Date(NA))

# Initialise list to store results as we go -----
results <- list()

# Create a log file ----
createLogFile(logFile = file.path(results_folder, "log_{date}_{time}"))
logMessage("LOG CREATED")

# CDM modifications -----

# CDM summary -----
results[["snapshot"]] <- summariseOmopSnapshot(cdm)
results[["obs_period"]] <- summariseObservationPeriod(cdm)

# Instantiate study cohorts ----
logMessage("Instantiating study cohorts")
source(here("cohorts", "instantiateCohorts.R"))
logMessage("Study cohorts instantiated")

# Cohort counts and attrition ----
# results[["counts"]] <- summariseCohortCount("...")
# results[["attrition"]] <- summariseCohortAttrition("...")

# Run analyses ----
logMessage("Run study analyses")
source(here("analyses", "cohortCharacteristics.R"))
source(here("analyses", "cohortSurvival.R"))
source(here("analyses", "drugUtilisation.R"))
source(here("analyses", "incidencePrevalence.R"))
logMessage("Analyses finished")

# Finish ----
results <- bind(results)
exportSummarisedResult(
  result = results,
  minCellCount = min_cell_count,
  fileName = "results_{cdm_name}_{date}.csv",
  path = results_folder
)

cli_inform(c(v = "Study finished"))
