#' @title
#' Get the Banff data dictionary
#'
#' @description
#' This function gets the data dictionary used to control the consistency of
#' the Banff dataset.
#'
#' @param which Indicates which Banff dictionary to get. Either "input" or
#' "output". If NULL, both Banff dictionary are provided.
#' @param language Optional input allowing the user to get the diagnoses in a
#' specific language. Options are "label:en" (default), "label:fr", "label:de",
#' "label:sp", "label:nl", "label:jp", "label:in".
#' @param detail Whether the output should include temporary variables generated
#' in the process or not. FALSE by default.
#'
#' @return
#' A list of tibbles representing meta data used in the process. The metadata
#' are the list of variables used, and their associated categories, if any.
#'
#' @examples
#' {
#'
#'   get_banff_dictionary()
#'
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export
get_banff_dictionary <- function(which = NULL, language = "label:en",detail = FALSE){

  # creation of the data dictionary
  banff_dict <- get("banff_dict", envir = asNamespace("banffIT"))

  # check if the label provided by user is among the provided labels.
  labels <- str_subset(names(banff_dict$`Categories`),'^label')
  if(! tolower(language) %in% labels)
    stop(call. = FALSE,"

The possible `language` are : ",toString(labels))

      if(!toString(which) %in% c("","input","output")){
        stop(call. = FALSE,"
`which` must be NULL or a character string, either 'input' or 'output'")
  }

  # creation of the output data dictionary (extended if detail == TRUE)
  if(detail == FALSE){
    banff_dict <-
      banff_dict %>%
      data_dict_filter(filter_var = "output !='if detail = TRUE'")}

  # rename label:xx into label
  banff_dict$`Variables` <-
    banff_dict$`Variables` %>%
    rename("label" = "label:en") %>%
    select("index","name","label","valueType",everything())

  # rename label:xx into label
  banff_dict$`Categories` <-
    banff_dict$`Categories` %>%
    select("variable","name", "labels" = "label:en", "test" = all_of(language),"description") %>%
    mutate(labels = ifelse(is.na(.data$`test`),.data$`labels`,.data$`test`)) %>%
    select("variable","name","labels","description",everything(),-"test")


  if(is.null(which)){
    return(banff_dict)
  }

  if(toString(which) == "input"){
    # creation of the input data dictionary
    banff_dict_input <-
      banff_dict %>%
      data_dict_filter(filter_var = "input == 'yes'")

    banff_dict_input$`Variables` <-
      banff_dict_input$`Variables` %>%
      select(-c("input","output"))

    return(banff_dict_input)

  }


  if(toString(which) == "input"){
    # creation of the input data dictionary
    banff_dict_input <-
      banff_dict %>%
      data_dict_filter(filter_var = "input != 'no'")

    banff_dict_input$`Variables` <-
      banff_dict_input$`Variables` %>%
      select(-c("input","output"))

    return(banff_dict_input)

  }

  if(toString(which) == "output"){
    # creation of the input data dictionary
    banff_dict_output <-
      banff_dict %>%
      data_dict_filter(filter_var = "output != 'no'")

    banff_dict_output$`Variables` <-
      banff_dict_output$`Variables` %>%
      select(-c("input","output"))

    return(banff_dict_output)

  }

}

#' @title
#' Get the Banff dataset example
#'
#' @description
#' This function gets the dataset used in the vignette as an example.
#'
#' @return
#' A tibble representing the dataset used in the the vignette as an example.
#'
#' @examples
#' {
#'
#'   get_banff_example()
#'
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export
get_banff_example <- function(){

  # creation of the data dictionary
  banff_example <- get("banff_example", envir = asNamespace("banffIT"))

  return(banff_example)

}

#' @title
#' Get the Banff dataset template
#'
#' @description
#' This function gets the empty dataset with columns that are mandatory in the
#' process.
#'
#' @return
#' A tibble representing the empty dataset.
#'
#' @examples
#' {
#'
#'   get_banff_template()
#'
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export
get_banff_template <- function(){

  # creation of the data dictionary
  banff_template <- get("banff_template", envir = asNamespace("banffIT"))

  return(banff_template)

}

#' @title
#' Call the online documentation
#'
#' @description
#' This function sends the user to the online documentation for the package,
#' which includes a description of the latest version of the package, vignettes,
#' user guides, and a reference list of functions and help pages.
#'
#' @returns
#' Nothing to be returned. The function opens a web page.
#'
#' @examples
#' {
#'
#' banffIT_website()
#'
#' }
#'
#' @importFrom utils browseURL
#'
#' @export
banffIT_website <- function(){

  browseURL("https://PersonalizedTransplantCare.github.io/banffIT-documentation/")

}





