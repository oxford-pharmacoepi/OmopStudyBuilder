
# Check code_to_run inputs ----
omopgenerics::validateCdmArgument(cdm,
                                  requiredTables = c("person",
                                                     "observation_period",
                                                     "condition_occurrence",
                                                     "drug_exposure",
                                                     "concept"))
omopgenerics::assertNumeric(min_cell_count)

# Create a log file ----
loggerName <- gsub(":| |-", "", paste0("log_",
                                       omopgenerics::cdmName(cdm),
                                       "_", Sys.Date(),
                                       ".txt"))
logger <- create.logger()
logfile(logger) <- here("results", loggerName)
level(logger) <- "INFO"
info(logger, "LOG CREATED")

# Define analysis settings -----
study_period <- c(as.Date(NA), as.Date(NA))

# Initialise list to store results as we go -----
results <- list()

# CDM modifications -----
# CDM summary -----
results[["snapshot"]] <- summariseOmopSnapshot(cdm)
results[["obs_period"]] <- summariseObservationPeriod(cdm$observation_period)

# Instantiate study cohorts ----
info(logger, "Instantiating study cohorts")
source(here("cohorts", "instantiate_cohorts.R"))
info(logger, "Study cohorts instantiated")

# Cohort counts and attrition ----
# results[["counts"]] <- summariseCohortCount("...")
# results[["attrition"]] <- summariseCohortAttrition("...")

# Run analyses ----
info(logger, "Run study analyses")
source(here("analyses", "cohort_characteristics.R"))
source(here("analyses", "cohort_survival.R"))
source(here("analyses", "drug_utilisation.R"))
source(here("analyses", "incidence_prevalence.R"))
info(logger, "Analyses finished")

# Finish ----
results <- results |>
  vctrs::list_drop_empty() |>
  omopgenerics::bind()
exportSummarisedResult(results,
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("results"))

cli::cli_alert_success("Study finished")
