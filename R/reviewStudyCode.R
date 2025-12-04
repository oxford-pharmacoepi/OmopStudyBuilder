
#' Review study code
#'
#' @param dir Path to the directory containing the study code to review.
#'
#' @returns Invisibly returns `NULL`. Called for its side effects of printing
#' summaries of R, JSON, CSV, and Excel files in the directory.
#' @export
#'
reviewStudyCode <- function(dir){

  summariseRFiles(dir)
  summariseJsonFiles(dir)
  summariseCsvFiles(dir)
  summariseExcelFiles(dir)

}

summariseRFiles <- function(dir){

  cli::cat_line()
  cli::cli_h2("R scripts")

  rFiles <- list.files(dir,
             recursive = TRUE,
             pattern = "\\.R$",
             ignore.case = TRUE,
             full.names = TRUE) |>
    stringr::str_subset(
      pattern = "renv/",
      negate = TRUE)

  rFilesShort <- list.files(dir,
                       recursive = TRUE,
                       pattern = "\\.R$",
                       ignore.case = TRUE,
                       full.names = FALSE) |>
    stringr::str_subset(
      pattern = "renv/",
      negate = TRUE)

  rFilesShortLoc <- paste0(rFilesShort,
                           " (loc: ",
                           purrr::map_int(rFiles, ~ get_loc(.x)) |>
                             scales::comma(),
                           ")")
  totalLoc <- sum(purrr::map_int(rFiles, ~ get_loc(.x))) |>
    scales::comma()

  cli::cli_inform(c("{dir} contains {length(rFiles)} R scripts (total lines of code {totalLoc})",
                    i = "{rFilesShortLoc}"))


}

summariseJsonFiles <- function(dir){
  cli::cat_line()
  cli::cli_h2("JSON files")
  jsonFiles <- list.files(dir,
                       recursive = TRUE,
                       pattern = "\\.json$",
                       ignore.case = TRUE,
                       full.names = TRUE) |>
    stringr::str_subset(
      pattern = "renv/",
      negate = TRUE)

  jsonFilesShort <- list.files(dir,
                            recursive = TRUE,
                            pattern = "\\.json$",
                            ignore.case = TRUE,
                            full.names = FALSE) |>
    stringr::str_subset(
      pattern = "renv/",
      negate = TRUE)

  cli::cli_inform(c("{dir} contains {length(jsonFiles)} JSON files",
                    i = "{jsonFilesShort}"))


}

summariseCsvFiles <- function(dir){
  cli::cat_line()
  cli::cli_h2("CSV files")
  csvFiles <- list.files(dir,
                          recursive = TRUE,
                          pattern = "\\.csv$",
                          ignore.case = TRUE,
                          full.names = TRUE) |>
    stringr::str_subset(
      pattern = "renv/",
      negate = TRUE)

  csvFilesShort <- list.files(dir,
                               recursive = TRUE,
                               pattern = "\\.csv$",
                               ignore.case = TRUE,
                               full.names = FALSE) |>
    stringr::str_subset(
      pattern = "renv/",
      negate = TRUE)


  cli::cli_inform(c("{dir} contains {length(csvFiles)} CSV files",
                    i = "{csvFilesShort}"))


}

summariseExcelFiles <- function(dir){
  cli::cat_line()
  cli::cli_h2("Excel files")
  excelFiles <- list.files(dir,
                         recursive = TRUE,
                         pattern = "\\.xlsx$|\\.xls$",
                         ignore.case = TRUE,
                         full.names = TRUE) |>
    stringr::str_subset(
      pattern = "renv/",
      negate = TRUE)

  excelFilesShort <- list.files(dir,
                              recursive = TRUE,
                              pattern = "\\.xlsx$|\\.xls$",
                              ignore.case = TRUE,
                              full.names = FALSE) |>
    stringr::str_subset(
      pattern = "renv/",
      negate = TRUE)


  cli::cli_inform(c("{dir} contains {length(excelFiles)} Excel files",
                    i = "{excelFilesShort}"))


}


get_loc <- function(file_path) {
  length(readLines(file_path, warn = FALSE))
}
