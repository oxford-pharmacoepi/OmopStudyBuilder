# renv::activate()
# renv::restore()

library(CDMConnector)
library(DBI)
library(log4r)
library(dplyr)
library(here)
library(OmopSketch)
library(omopgenerics)
library(CohortConstructor)
library(PhenotypeR)
library(CohortCharacteristics)
library(PatientProfiles)
library(visOmopResults)
library(stringr)
library(CodelistGenerator)
library(odbc)
library(RPostgres)
library(readr)

# database metadata and connection details
# The name/ acronym for the database
dbName <- "..."

# Database connection details
# db <- dbConnect(
#   RPostgres::Postgres(),
#   dbname = server_dbi,
#   port = port,
#   host = host,
#   user = user,
#   password = password
# )
db <- dbConnect(...)

# The name of the schema that contains the OMOP CDM with patient-level data
cdmSchema <- "..."

# A prefix for all permanent tables in the database
writePrefix <- "..."

# The name of the schema where results tables will be created
writeSchema <- "..."

# minimum counts that can be displayed according to data governance
minCellCount <- 5

# Create cdm object ----
cdm <- cdmFromCon(
  con = db,
  cdmSchema = cdmSchema,
  writeSchema = writeSchema,
  writePrefix = writePrefix,
  cdmName = dbName
)

# Run the study
source(here("RunStudy.R"))

# after the study is run you should have a zip folder in your output folder to share
cli::cli_alert_success("Study finished")
