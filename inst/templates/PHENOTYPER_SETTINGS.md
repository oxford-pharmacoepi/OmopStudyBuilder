# PhenotypeR Diagnostics Settings

Repository: {{REPO_NAME}}
Created: {{DATE}}

This study includes `diagnosticsCode/`, which runs `PhenotypeR::phenotypeDiagnostics()`
in `runStudy.R`. Several of its arguments are currently left at generic template
defaults and should be reviewed and confirmed (or changed) for this specific study
before diagnostics are run against a real database.

## Settings to confirm

- [ ] **`diagnostics`** — which diagnostic groups to run. *Currently: all four
  (`databaseDiagnostics`, `codelistDiagnostics`, `cohortDiagnostics`,
  `populationDiagnostics`)*. Should any be left out for this study?

- [ ] **`survival`** — whether to include survival diagnostics. *Currently: `FALSE`*.
  Does this study's cohort design require survival diagnostics?

- [ ] **`cohortSample`** — max subjects sampled for cohort diagnostics
  (`NULL` = no cap, use the full cohort). *Currently: `20000`*. Does this fit the expected cohort size(s) for this study?

- [ ] **`matchedSample`** — max subjects sampled for matched diagnostics
  (per PhenotypeR docs: `NULL` = no cap; `0` = skip matched-cohort
  diagnostics entirely). *Currently: `NULL`, i.e. matching runs on the full
  population, uncapped*. Is this the intended setting for this study?

- [ ] **`populationSample`** — max subjects sampled for population-level
  diagnostics (`NULL` = no cap, sampled within `populationDateRange` if set).
  *Currently: `NULL`*. Could you confirm whether a cap makes sense here?

- [ ] **`populationDateRange`** — date range to restrict population
  diagnostics to. *Currently: unset* — per PhenotypeR docs, when unset it
  defaults to the earliest/latest observation period dates found in the
  data. Would a narrower window suit this study better?

- [ ] **`measurementSample`** / **`drugExposureSample`** — max records
  sampled for measurement/drug-exposure codelist diagnostics (per
  PhenotypeR docs: `NULL` = no cap; `0` = skip that diagnostic entirely).
  Do these work for this study's codelists?

## Where to make changes

Update the `phenotypeDiagnostics()` call in `diagnosticsCode/runStudy.R` once
these are confirmed.

---
*This issue was opened automatically by [OmopStudyBuilder](https://github.com/oxford-pharmacoepi/OmopStudyBuilder) because this study includes diagnostics code.*
