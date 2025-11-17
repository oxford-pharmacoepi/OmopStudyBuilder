
# Objective 1, 2 - Influenza vaccine ----
info(logger, "Instantiating influenza vaccine codes")
influenza_vaccine_codes <- importCodelist(path = here("Cohorts",
                                                      "InfluenzaVaccine",
                                                      "influenza_vaccine.csv"),
                                          type = "csv")
info(logger, "- Creating influenza vaccine concept cohort")
if(str_detect(tolower(cdmName(cdm)), "cprd")){
  influenza_vaccine_codes <- list(influenza_vaccine = c(influenza_vaccine_codes[[1]],
                                                        46277426L)) |> newCodelist()
  cdm$influenza_vaccines <- conceptCohort(cdm,
                                          conceptSet = influenza_vaccine_codes,
                                          name = "influenza_vaccines",
                                          useSourceFields = TRUE,
                                          overlap = "merge",
                                          inObservation = TRUE,
                                          exit = "event_start_date")
}else{
  cdm$influenza_vaccines <- conceptCohort(cdm,
                                          conceptSet = influenza_vaccine_codes,
                                          name = "influenza_vaccines",
                                          overlap = "merge",
                                          inObservation = TRUE,
                                          exit = "event_start_date")
}

# COVID-19 vaccine ----
info(logger, "Instantiating covid19 vaccine cohort")
covid19_vaccine_codes<-importCodelist(here("Cohorts","Vaccinations","covid19_vaccine.csv"), type = "csv")
cdm$covid19_vaccines <- conceptCohort(cdm,
                                      conceptSet = covid19_vaccine_codes,
                                      name = "covid19_vaccines",
                                      overlap = "merge",
                                      inObservation = TRUE,
                                      exit = "event_start_date")

# Other vaccines ----
info(logger, "Instantiating vaccines")
vaccines_codes <- importCodelist(path = ,"Cohorts/Vaccinations", type = "csv")
cdm$vaccines <- conceptCohort(cdm,
                              conceptSet = vaccines_codes,
                              name = "vaccines",
                              overlap = "merge",
                              inObservation = TRUE,
                              exit = "event_start_date")


# Objective 3, immunocompromised relative to influenza vaccine ----
info(logger, "Instantiating immunocompromised cohort")
immunocompromised_codelists  <- codesFromConceptSet(here("Cohorts","Immunocompromised"), cdm)

addImmunocompromised <-  function(cohort, immunocompromised_codelists) {
  cohort |>
    addConceptIntersectFlag(
      conceptSet = immunocompromised_codelists[c(
        "haematological_malignances", "hiv", "intrinsec_immune", "aids", "transplant", "autoimmune")],
      window = c(-365, 0),
      nameStyle = "{concept_name}"
    ) |>
    addConceptIntersectFlag(
      conceptSet = immunocompromised_codelists[c("immunos_antineo", "corticoids")],
      window = c(-183, 0),
      nameStyle = "{concept_name}"
    ) |>
    mutate(immunocompromised = case_when(
      haematological_malignances + hiv + intrinsec_immune + aids +
        immunos_antineo > 0 ~ 1,
      corticoids == 1 & (autoimmune + transplant) > 0 ~ 1,
      TRUE ~  0
    )) |>
    select(c(colnames(cohort),"immunocompromised"))
}

cdm$immunocompromised_influenza_vaccine <- copyCohorts(cdm$influenza_vaccines,
                                                       name = "immunocompromised_influenza_vaccine") |>
  renameCohort(cohortId = 1, "immunocompromised_influenza_vaccine")

cdm$immunocompromised_influenza_vaccine <- cdm$immunocompromised_influenza_vaccine |>
  addImmunocompromised(immunocompromised_codelists) |>
  filter(immunocompromised == 1) |>
  compute(temporary = FALSE, name = "immunocompromised_influenza_vaccine")


# combine to single cohort table
cdm <- bind(cdm$influenza_vaccines,
            cdm$vaccines,
            cdm$immunocompromised_influenza_vaccine,
            name = "vaccines")
cdm$vaccines <- addCohortTableIndex(cdm$vaccines)

# Objective 4,  Outcomes -----
info(logger, "Instantiating outcomes")

outcomes_codes <- importCodelist(path = here("Cohorts/Outcomes/"), type = "csv")
cdm$outcomes <- conceptCohort(cdm,
                              conceptSet = outcomes_codes,
                              name = "outcomes",
                              exit = "event_start_date",
                              overlap = "merge")
cdm$outcomes <- cdm$outcomes %>%
  padCohortEnd(days = 28)

# Objective 3, Comorbidities -----
info(logger, "Instantiating comorbidities")
comorbidities_codes <- importCodelist(here("Cohorts","Comorbidities"), type = "csv")
cdm$comorbidities <- conceptCohort(cdm,
                                   conceptSet = comorbidities_codes,
                                   name = "comorbidities",
                                   exit = "event_start_date",
                                   overlap = "merge")

info(logger, "- Define cohort end date 365 after start date")
cdm$comorbidities <- cdm$comorbidities |>
  padCohortEnd(days = 365,
               cohortId = c("anaemia", "cancer", "tuberculosis"))

info(logger, "- Define cohort end date 90 after start date")
cdm$comorbidities <- cdm$comorbidities |>
  padCohortEnd(days = 90, cohortId = "acute_renal_disease")

info(logger, "- Define cohort end date at end of observation")
cdm$comorbidities <- cdm$comorbidities |>
  exitAtObservationEnd(cohortId = c("asplenia", "chronic_liver_disease", "cardiac_conditions", "diabetes_mellitus",
                                    "hypertension", "obesity", "neuromuscular_disorders", "chronic_renal_disease",
                                    "dementia", "stroke", "rheumatologic_diseases", "lung_disease", "asthma",
                                    "copd"))

# Objective 3, Pregnancy ----
info(logger, "Instantiating pregnancy cohort")
pregnancy_codes <- importCodelist(here("Cohorts","Pregnancy"), type = "csv")
cdm$pregnancy <- conceptCohort(cdm,
                               conceptSet = pregnancy_codes,
                               name = "pregnancy",
                               exit = "event_start_date",
                               overlap = "merge")

# Objective 4, Symptoms -----
info(logger, "Instantiating symptoms")
symptoms_codes <- importCodelist(here("Cohorts","Symptoms"), type = "csv")
cdm$symptoms<- conceptCohort(cdm,
                             conceptSet = symptoms_codes,
                             name = "symptoms",
                             exit = "event_start_date",
                             overlap = "merge")
cdm$symptoms <- cdm$symptoms |>
  padCohortEnd(days = 14)
# For study code: sensitivity analysis with days = 10

# Hospitalisations ----
info(logger, "Instantiating hospitalisation cohort")
hosp_codes <- importCodelist(path = here("Cohorts", "Hospitalisation"),
                             type = "csv")
cdm$hospitalisations <- conceptCohort(cdm,
                                      conceptSet = hosp_codes,
                                      name = "hospitalisations",
                                      exit = "event_end_date",
                                      overlap = "merge")

# Death ----
info(logger, "Instantiating death cohort")
cdm$death_cohort <- CohortConstructor::deathCohort(cdm,
                                                   name = "death_cohort")

# Immunocompromised separate ----
info(logger, "Instantiating immunocompromised cohort (separate)")
immunocompromised_codelists  <- codesFromConceptSet(here("Cohorts/Immunocompromised"), cdm)
cdm$immunocompromised_separate <- conceptCohort(
  cdm,
  conceptSet = immunocompromised_codelists,
  name = "immunocompromised_separate",
  exit = "event_start_date"
)


# Combine characteristics ----
info(logger, "Binding comorbidity cohort tables")
cdm <- bind(cdm$comorbidities,
            cdm$pregnancy,
            cdm$symptoms,
            cdm$hospitalisations,
            cdm$immunocompromised_separate,
            cdm$death_cohort,
            name = "characteristics")
cdm$characteristics <- addCohortTableIndex(cdm$characteristics)

# Combine all ----
info(logger, "Combining all cohorts")
cdm <- bind(cdm$vaccines,
            cdm$outcomes,
            cdm$characteristics,
            name = "study_cohorts")
cdm$study_cohorts <- addCohortTableIndex(cdm$study_cohorts)
