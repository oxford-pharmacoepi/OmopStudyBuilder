# Study name:

<img src="https://img.shields.io/badge/Study%20Status-Started-blue.svg" alt="Study Status: Started"/>

Brief description of the study

## Instructions to run the study diagnostics

1)  Download this entire repository (you can download as a zip folder using Code -\> Download ZIP, or you can use GitHub Desktop).
2)  Open the project <i>StudyCode.Rproj</i> from the StudyCode directory in RStudio (when inside the project, you will see its name on the top-right of your RStudio session)
3)  Open the code_to_run.R file - this is the only file you should need to interact with.

-   Install the required packages using renv::restore() and then load these libraries
-   Add your database specific parameters (name of database, schema name with OMOP data, schema name to write results, table name stem for results to be saved in the result schema).
-   Create a cdm using CDMConnector (see <https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html> for connection examples for different dbms). Achilles tables must be included in your cdm reference.
-   Run source(here("run_study.R")) to run the analysis.

The template of this study has been generated using [OmopStudyBuilder](https://github.com/oxford-pharmacoepi/OmopStudyBuilder).
