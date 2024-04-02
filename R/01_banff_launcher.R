#' @title
#' Main function to start the program
#'
#' @description
#' This function takes in input a path string of an input file. The function
#' internally adresses series of tests that assess the input dataset. If any of
#' these tests fails, the user gets information allowing them to correct the
#' file and reprocess. Once the tests do not fail, the dataset is given for all
#' the observation a diagnosis (using internally the
#' function [add_diagnosis()]). The output dataset, along with its associated
#' labels (`label:en` by defaut) are provided to the user in an Excel format
#' file accessibee in the output_folder they provided (working direct as a
#' default.) The dataset comes with a report that summarize information about
#' variable distributions and descriptive statistics.
#'
#' @param banff_file A character string identifying the name of the file to be
#' processed.
#' @param output_folder A character string identifying the folder path where
#' the bookdown report files will be saved.
#' @param language optional input allowing user to get their diagnosis in a
#' specific language. Options are "label:en" (default), "label:fr", "label:de",
#' "label:sp", "label:nl", "label:jp", "label:in".
#' @param option_filter Optional argument allowing the user to filter its
#' dataset using [dplyr::filter()] synthax. The column used to filter must exist.
#' @param detail Whether the output should include temporary variables generated
#' in the process or not. FALSE by default.
#' @param .test_TA Temporary parameter to test with 'ta' variable.
#'
#' @return
#' Nothing to be returned. The function generates a folder (if not already
#' exists) where Excel files are stored.
#'
#' @examples
#' {
#'
#' banff_file <- system.file("extdata", "example.xlsx", package = "banffIT")
#' banff_launcher(banff_file, output_folder = tempdir())
#'
#' }
#'
#' @import dplyr tidyr stringr fs fabR madshapR
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom lubridate is.POSIXt
#' @importFrom tools file_ext
#' @importFrom crayon bold
#' @export
banff_launcher <- function(
    banff_file,
    output_folder = getwd(),
    language = "label:en",
    option_filter,
    detail = FALSE,
    .test_TA = FALSE){

  # Function to read csv or xlsx file
  read_file <- function(file_path = banff_file) {
    # Check the file extension
    file_extension <- file_ext(file_path)

    # Read the file based on its extension
    if (file_extension == "csv") {
      data <- read_csv_any_formats(file_path)
    } else if (file_extension == "xlsx") {
      data <- read_excel_allsheets(file_path, keep_as_list = FALSE)
    } else {
      stop("Unsupported file format. Only CSV and XLSX formats are supported.")
    }

    return(data)
  }

  ##### 1 - CHECK ARGUMENTS ####

  # list of arguments provided by user in the function.
  # fargs <- list()
  fargs <- as.list(match.call(expand.dots = TRUE))

  # check on arguments : detail
  if(!is.logical(detail))
    stop(call. = FALSE,'`detail` must be TRUE or FALSE.')

  # message to user to begin with the process.
  message_on_prompt(paste0(
"Welcome to the banff diagnosis helper.

Press [enter] to continue or [esc] to quit.

"))

  ##### 2 - GENERATE FOLDER ####

  # check the folder (to be created if does not exists).
  if(path_ext(output_folder) != "")
    stop(call. = FALSE,"Please specify a valid directory name.")
  if(!dir.exists(output_folder)) dir_create(output_folder)

  # names of objects to be generated in the folder. construct name and check previous existence.
  banff_file_name  <- make.names(path_ext_remove(basename(banff_file)))

  timestamp <-
    str_replace_all(str_replace_all(
      toString(trunc(Sys.time(), units = "secs")),":","")," ","-")

  banff_diagnosis_path  <- paste0(output_folder,'/',banff_file_name,'-', timestamp,'-diagnosis.xlsx')
  banff_assessment_path <- paste0(output_folder,'/',banff_file_name,'-', timestamp,'-assessment.xlsx')
  banff_report_path     <- paste0(output_folder,'/',banff_file_name,'-', timestamp,'-report.xlsx')
  banff_dictionary_path <- paste0(output_folder,'/banff_dictionary.xlsx')

  if(file.exists(banff_diagnosis_path))
    stop(call. = FALSE,"

Diagnosis for this file already exists in '",basename(output_folder),"'")

  ##### 3 - CHECK INPUT DATASET ####

  message("\n[1/6] - Import data")

  # creation of the dataset
  banff_dataset <- read_file(banff_file)

  # creation of the banff dictionary
  banff_dict        <- get_banff_dictionary(which = NULL, language = language, detail = detail, .test_TA = .test_TA)
  banff_dict_input  <- get_banff_dictionary(which = "input", language = language, detail = detail, .test_TA = .test_TA)
  banff_dict_output <- get_banff_dictionary(which = "output", language = language, detail = detail, .test_TA = .test_TA)

  ## creation of adequacy calculated
  adequacy_input_copy <- calculate_adequacy(banff_dataset)
  banff_dataset <-
    banff_dataset %>%
    mutate(
      adequacy = adequacy_input_copy$`adequacy_calculated`)

  # filter the dataset if the user specified filter
  if(!is.null(fargs$`option_filter`)){

    query_expr <- substitute(option_filter)

    message("\n")
    banff_dataset <-
      banff_dataset %>%
      dplyr::filter(!! query_expr)

    }

  message("\n[2/6] - Evaluation of '",bold(banff_file_name),"'")

  ##### evaluation of banff dataset input.
  banff_assessment <-
    suppressMessages(banff_dataset_evaluate(banff_dataset))

  ##### IF EMPTY #####
  if(!is.null(attributes(banff_assessment)$empty)){
    message(attributes(banff_assessment)$empty)
    return(invisible(NULL))
  }

  ##### IF ERROR DETECTED #####

  if(!is.null(attributes(banff_assessment)$error)){

    message(attributes(banff_assessment)$error)
    message("
An assessment report has been generated in your output folder to help you correcting
your dataset. You can open it as an Excel file and check the variables that contain
errors, and correct them before reprocessing.

The data dictionary that is used in the process has been downloaded in your
output folder. You can open it as an Excel file and check the names provided.
Each of the variable listed is mandatory.

For further information please refer to documentation.")

    message("Export banff assessement Excel files in \n'",bold(banff_assessment_path),"'\n")
    message("Export banff dictionary Excel files in \n'",bold(banff_assessment_path),"'\n")

    write_excel_allsheets(banff_assessment, banff_assessment_path)
    write_excel_allsheets(banff_dict, banff_dictionary_path)

    return(invisible(NULL))

  }

  message("\n[3/6] - Add diagnosis to each observation of'",bold(banff_file_name),"'")

  banff_diagnosis <- add_diagnosis(banff_dataset,.test_TA)

  message("\n[4/6] - Generate labels for each variable of '",bold(banff_file_name),"'")

  banff_diagnosis_codeset <-
    suppressWarnings({
      data_dict_match_dataset(
        banff_diagnosis,
        banff_dict_output,
        output = 'dataset',data_dict_apply = TRUE)})

  banff_diagnosis_dataset <-
    dataset_zap_data_dict(banff_diagnosis_codeset) %>%
    dataset_cat_as_labels(data_dict = banff_dict_output)

  banff_diagnosis <- list(
    dataset = banff_diagnosis_dataset,
    codeset = banff_diagnosis_codeset)

  message("\n[5/6] - Assessment of the the annotated '",bold(banff_file_name),"'\n")

  banff_report <- list()

  banff_report <-
    dataset_summarize(
      dataset = banff_diagnosis$codeset,
      data_dict = banff_dict,
      dataset_name = banff_file_name)

  banff_report$`Dataset assessment - input`     <- banff_assessment$`Dataset assessment`
  banff_report$`Dataset assessment - diagnosis` <- banff_report$`Dataset assessment`
  banff_report$`Dataset assessment`             <- NULL
  banff_report$`Variables summary (all)`        <- banff_assessment$`Data dictionary summary`

  banff_report$`Dataset assessment - diagnosis` <-
    banff_report$`Dataset assessment - diagnosis` %>%
    select("column" = "name","condition" = "Quality assessment comment", "value") %>%
    dplyr::filter(!.data$`column` %in% banff_report$`Dataset assessment - input`$column)

  banff_report <- banff_report[unique(c(
    "Overview","Dataset assessment - input",
    "Dataset assessment - diagnosis", names(banff_report)))]

  message("\n[6/6] - Export XLSX files in '",bold(basename(output_folder)),"'")
  message("\n")

  message("

The assessment and addition of diagnosis on your dataset is now finished.
In the output folder, you will find an Excel file containing both labels and codes
of your dataset and for each participant, additional columns corresponding to your
diagnosis.

An assessment and summary report has been generated in your output folder to help you
assessing and interpret your dataset. You can open it as an Excel file.

For further information please refer to documentation.")

  message("Export banff diagnostics Excel files in \n'",bold(banff_diagnosis_path),"'\n")
  message("Export banff assessement Excel files in \n'",bold(banff_report_path),"'\n")
  message("Export banff dictionary Excel files in \n'",bold(banff_assessment_path),"'\n")

  write_excel_allsheets(banff_diagnosis , banff_diagnosis_path)
  write_excel_allsheets(banff_dict, banff_dictionary_path)
  write_excel_allsheets(banff_report, banff_report_path)

  return(invisible(NULL))
}
