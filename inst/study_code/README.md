# Study Code

This folder contains the main study analysis code.

## Instructions to run the study

1)  Download this entire repository (you can download as a zip folder using Code -\> Download ZIP, or you can use GitHub Desktop).
2)  Open the project <i>study_code.Rproj</i> from the study_code directory in RStudio (when inside the project, you will see its name on the top-right of your RStudio session)
3)  Open the code_to_run.R file - this is the only file you should need to interact with.
4)  Install the required packages using renv::restore() and then load these libraries
5)  Add your database specific parameters (name of database, schema name with OMOP data, schema name to write results, table name stem for results to be saved in the result schema).
6)  Create a cdm using CDMConnector (see <https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html> for connection examples for different dbms).
7)  Run source(here("run_study.R")) to run the analysis.
