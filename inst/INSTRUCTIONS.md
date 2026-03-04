# Instructions for Running This Study

This document provides detailed instructions for running the different components of this study.

---

{{#HAS_DIAGNOSTICS}}
## Running the Diagnostics Code

The diagnostics code is located in the `diagnostics_code/` folder and should be run before the main study analysis.

### Steps

1)  Download this entire repository (you can download as a zip folder using Code → Download ZIP, or you can use GitHub Desktop).
2)  Open the project `diagnostics_code.Rproj` from the `diagnostics_code/` directory in RStudio (when inside the project, you will see its name on the top-right of your RStudio session)
3)  Open the `code_to_run.R` file - this is the only file you should need to interact with.
4)  Install the required packages using `renv::restore()` and then load these libraries
5)  Add your database specific parameters (name of database, schema name with OMOP data, schema name to write results, table name stem for results to be saved in the result schema).
6)  Create a cdm using CDMConnector (see <https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html> for connection examples for different dbms). Achilles tables must be included in your cdm reference.
7)  Run `source(here("run_diagnostics.R"))` to run the diagnostics.

---

{{/HAS_DIAGNOSTICS}}
{{#HAS_STUDY}}
## Running the Study Code

The main study analysis code is located in the `study_code/` folder.

### Steps

1)  Download this entire repository (you can download as a zip folder using Code → Download ZIP, or you can use GitHub Desktop).
2)  Open the project `study_code.Rproj` from the `study_code/` directory in RStudio (when inside the project, you will see its name on the top-right of your RStudio session)
3)  Open the `code_to_run.R` file - this is the only file you should need to interact with.
4)  Install the required packages using `renv::restore()` and then load these libraries
5)  Add your database specific parameters (name of database, schema name with OMOP data, schema name to write results, table name stem for results to be saved in the result schema).
6)  Create a cdm using CDMConnector (see <https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html> for connection examples for different dbms).
7)  Run `source(here("run_study.R"))` to run the analysis.

---

{{/HAS_STUDY}}
{{#HAS_DIAGNOSTICS}}
## Running the Diagnostics Shiny App

The diagnostics shiny app is located in the `diagnostics_shiny/` folder and is used to explore diagnostic outputs.

### Steps

1)  Ensure your diagnostic results are placed in the `results/` folder
2)  Open the project in RStudio
3)  Restore the R environment using `renv::restore()`
4)  Run the app using `shiny::runApp()`

---

{{/HAS_DIAGNOSTICS}}
{{#HAS_STUDY}}
## Running the Study Shiny App

The study shiny app is located in the `study_shiny/` folder and is used to explore study results.

### Steps

1)  Ensure your study results are placed in the `results/` folder
2)  Open the project in RStudio
3)  Restore the R environment using `renv::restore()`
4)  Run the app using `shiny::runApp()`

---

{{/HAS_STUDY}}

## Additional Resources

- [CDMConnector Database Connection Examples](https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html)
- [OmopStudyBuilder Documentation](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/)

---

*This study was generated using [OmopStudyBuilder](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/).*
