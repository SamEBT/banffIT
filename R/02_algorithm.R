#' @title
#' Add diagnosis to a banff dataset
#'
#' @description
#' This function takes a dataset and returns a diagnosis for each
#' observation. For the function to run, the dataset must not contain any errors
#' that [banff_launcher()] would have detected. Please prefer using
#' [banff_launcher()] to run additional tests.
#'
#' @param banff_dataset A tibble object.
#' @param .test_TA Temporary parameter to test with 'ta' variable.
#'
#' @return
#' A tibble object that contains additional columns with the diagnosis results.
#'
#' @examples
#' {
#'
#' library(fabR)
#' banff_file <- system.file("extdata", "example.xlsx", package = "banffIT")
#' banff_dataset <- read_excel_allsheets(banff_file)[1,]
#' add_diagnosis(banff_dataset)
#'
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export
add_diagnosis <- function(banff_dataset, .test_TA = FALSE) {

  # check input
  banff_assessment <- suppressMessages(banff_dataset_evaluate(banff_dataset))

  if(.test_TA == TRUE) warning("TA used in the algorithm.")

  if(!is.null(attributes(banff_assessment)$error)){
    message(attributes(banff_assessment)$error)
    message("
Use `banff_dataset_evaluate(banff_dataset)` to help you correcting your file.\n")
    stop(call. = TRUE)
    }

  banff_diagnosis <- banff_dataset

  #   select(
  #     "glomeruli","arteries","adequacy",
  #     "agn","cgn","ci_score","ct_score",
  #     "c4d_if","c4d_ihc", "i_score","t_score",
  #     "v_score","bk","infec","i_ifta_score",
  #     "ti_score","monofibneoint","ptc_score",
  #     "g_score","atma","atn","dsa",
  #     "patient_id","sc_date_bx","hist_dsa",
  #     "hist_tcmr","cg_score", "ctma",
  #     "sptcbmml","leuscint","newaif",
  #     "hist_camr","hist_aamr","ptld","cni","ain"
  #
  #     "ta" ,                                 # if test_TA == TRUE,
  #     )

  ## creation of adequacy calculated
  adequacy_input_copy <- calculate_adequacy(banff_dataset)
  banff_dataset <-
    banff_dataset %>%
    mutate(
      adequacy = adequacy_input_copy$`adequacy_calculated`)

  banff_diagnosis <-
    banff_diagnosis %>%
    mutate(

      nogn =
        case_when(
          .data$`agn` + .data$`cgn` == 0 ~ 0L,
          TRUE ~ 1L),

      ifta = pmax(.data$`ci_score`,.data$`ct_score`),
      c4d =
        case_when(
          .data$`c4d_if` %in% c(0,1) & .data$`c4d_ihc` == 0 ~ 0L,
          TRUE ~ 1L)
        )

  # atcmr variables
  banff_diagnosis <-
    banff_diagnosis %>%
    mutate(
      active_atcmr_code_3.TEMP =
        case_when(
          .data$`i_score` %in% c(2,3) & .data$`t_score` == 1          ~ 1L  ,  # 'BLD_a'
          .data$`i_score` == 1        & .data$`t_score` %in% c(1,2,3) ~ 2L  ,  # 'BLD_i1_a'
          TRUE                                                        ~ 0L) ,

      active_atcmr_code_4.TEMP =
        case_when(
          .data$`v_score` == 3                                        ~ 5L  ,  # 'III_a'
          .data$`v_score` == 2                                        ~ 4L  ,  # 'IIB_a'
          .data$`v_score` == 1                                        ~ 3L  ,  # 'IIA_a'
          .data$`i_score` %in% c(2,3) & .data$`t_score` == 3          ~ 2L  ,  # 'IB_a'
          .data$`i_score` %in% c(2,3) & .data$`t_score` == 2          ~ 1L  ,  # 'IA_a'
          TRUE                                                        ~ 0L
        ))

  if(.test_TA == TRUE){

    banff_diagnosis <-
      banff_diagnosis %>%
      mutate(
        chronic_atcmr_code_4.TEMP =
          case_when(

            .data$`TA` == 1 | .data$`monofibneoint` == 1              ~ 8L  ,   # "II_c",   # TA ADDED
            (.data$`bk`            == 0        &
               .data$`infec`         == 0        &
               .data$`i_ifta_score`  %in% c(2,3) &
               .data$`ti_score`      %in% c(2,3) &
               .data$`t_score`       == 3)                             ~ 7L  ,   # "IB_c",
            (.data$`bk`            == 0        &
               .data$`infec`         == 0        &
               .data$`i_ifta_score`  %in% c(2,3) &
               .data$`ti_score`      %in% c(2,3) &
               .data$`t_score`       == 2)                             ~ 6L  ,   # "IA_c",
            TRUE                                                     ~ 0L)
      )

  }else{

    banff_diagnosis <-
      banff_diagnosis %>%
      mutate(
        chronic_atcmr_code_4.TEMP =
          case_when(
            .data$`monofibneoint` == 1                              ~ 8L  ,   # "II_c",
            (.data$`bk`            == 0        &
               .data$`infec`         == 0        &
               .data$`i_ifta_score`  %in% c(2,3) &
               .data$`ti_score`      %in% c(2,3) &
               .data$`t_score`       == 3)                             ~ 7L  ,   # "IB_c",
            (.data$`bk`            == 0        &
               .data$`infec`         == 0        &
               .data$`i_ifta_score`  %in% c(2,3) &
               .data$`ti_score`      %in% c(2,3) &
               .data$`t_score`       == 2)                             ~ 6L  ,   # "IA_c",
            TRUE                                                     ~ 0L)
      )
  }

  # aamr1
  banff_diagnosis <-
    banff_diagnosis %>%
    mutate(
      aamr11.1 = ifelse((.data$`active_atcmr_code_3.TEMP` + .data$`active_atcmr_code_4.TEMP`) == 0 &
                        .data$`ptc_score` > 0,                            1L, 0L),

      aamr11.2 = ifelse(.data$`g_score` > 0 &
                        .data$`cgn` == 0 &
                        .data$`agn` == 0,                                 1L, 0L),
      aamr12  = ifelse((.data$`active_atcmr_code_3.TEMP` + .data$`active_atcmr_code_4.TEMP`) > 0 &
                        .data$`g_score` + .data$`ptc_score` >= 2 &
                        .data$`g_score` >= 1,                             1L, 0L),
      aamr13  = ifelse( .data$`v_score` > 0,                              1L, 0L),
      aamr14  = ifelse((.data$`atma` + .data$`atn`) > 0,                    1L, 0L),
      aamr1   = ifelse( .data$`aamr11.1` == 1 |
                        .data$`aamr11.2` == 1 |
                        .data$`aamr12`   == 1 |
                        .data$`aamr13`   == 1 |
                        .data$`aamr14`   == 1,                            1L, 0L),

      )

  # The default for aamr22 is 0 (which take into consideration when both GN and (atcmr_bl or atcmr) are present)
  # G + PTC must be at least 2 when GN is absent and atcmr_bl or atcmr also absent
  # PTC must be at least 2 when GN is present and atcmr_bl or atcmr also absent
  # G + PTC must be at least 2, and G must be at least 1, when GN is absent and atcmr_bl or atcmr is present
  # activeabmr definition below because it must take camr into consideration (to verify) !!!!
  # need to be double checked

  banff_diagnosis <-
    banff_diagnosis %>%
    mutate(
      aamr21    = ifelse(.data$`c4d_if`  %in% c(2,3) |
                         .data$`c4d_ihc` %in% c(1,2,3),  1L, 0L),

      aamr22.1   = ifelse(.data$`g_score` + .data$`ptc_score` >= 2 &
                         .data$`cgn`   == 0 &
                         .data$`agn`   == 0 &
                        (.data$`active_atcmr_code_3.TEMP` + .data$`active_atcmr_code_4.TEMP`) == 0,            1L, 0L),

      aamr22.2   = ifelse(.data$`ptc_score` >= 2 &
                        (.data$`cgn` == 1 | .data$`agn` == 1) &
                        (.data$`active_atcmr_code_3.TEMP` + .data$`active_atcmr_code_4.TEMP`) == 0,             1L, 0L),

      aamr22.3   = ifelse(.data$`g_score` + .data$`ptc_score` >= 2 &
                         .data$`g_score` >=1 &
                         .data$`cgn` == 0 &
                         .data$`agn` == 0 &
                        (.data$`active_atcmr_code_3.TEMP` + .data$`active_atcmr_code_4.TEMP`) > 0,              1L, 0L),

      aamr2     = ifelse(.data$`aamr21` == 1 |
                         .data$`aamr22.1` == 1 |
                         .data$`aamr22.2` == 1 |
                         .data$`aamr22.3` == 1,                             1L, 0L))

  banff_diagnosis <-
    banff_diagnosis %>%
    mutate(

      aamr      = ifelse(.data$`aamr1` == 1 &
                         .data$`aamr2` == 1 &
                         .data$`dsa` == 1,                                 1L, 0L),

      susp_aamr = ifelse(.data$`aamr` == 0 &
                         .data$`aamr1` == 1 &
                         .data$`aamr2` == 1 &
                         .data$`dsa` != 1,                                 1L, 0L),

      c4dneg_aamr = ifelse(.data$`dsa` == 1 &
                          (.data$`aamr22.1` == 1 |
                           .data$`aamr22.2` == 1 |
                           .data$`aamr22.3` == 1) &
                           .data$`aamr21` == 0L,                           1L, 0L),

      # the previous conditon is used as .data$g_score + .data$ptc_score >= 2
      susp_c4dneg_aamr = ifelse(.data$`c4dneg_aamr` == 0 &
                               (.data$`aamr22.1` == 1 |
                                .data$`aamr22.2` == 1 |
                                .data$`aamr22.3` == 1) &
                                .data$`aamr21` == 0 &
                                .data$`dsa` != 1,                         1L, 0L),

      dsaneg_aamr = ifelse(.data$`aamr1` == 1 &
                           .data$`aamr21` == 1 &
                           .data$`dsa` == 0,                              1L, 0L),

      susp_activeaamr  = ifelse(.data$`susp_aamr` == 1 |
                                .data$`susp_c4dneg_aamr` == 1,            1L, 0L)
    )




  # define historical tcmr based any previous catcmr or atcmr record
  # tcmr historical computation
  # ordering by transplant date and biopsy date

  banff_diagnosis <-
    banff_diagnosis %>%
    arrange(.data$`patient_id`, .data$`sc_date_bx`) %>%
    group_by(.data$`patient_id`) %>%
    mutate(
      hist_dsa1 = cumsum(.data$`dsa`) - .data$`dsa`,
      hist_dsa2 = cumsum(.data$`hist_dsa`)) %>%
    ungroup %>%
    group_by(.data$`patient_id`) %>%
    mutate(
      hist_tcmr1 = cumsum(.data$`active_atcmr_code_4.TEMP`) - .data$`active_atcmr_code_4.TEMP`,
      hist_tcmr2 = cumsum(.data$`chronic_atcmr_code_4.TEMP`)- .data$`chronic_atcmr_code_4.TEMP`,
      hist_tcmr3 = cumsum(.data$`hist_tcmr`)) %>%
    ungroup %>%
    mutate(
      hist_dsa_calculated  = ifelse(.data$`hist_dsa1`  >= 1L |
                         .data$`hist_dsa2`  == 1L        , 1L, 0L),

      hist_tcmr_calculated = ifelse(.data$`hist_tcmr1` >= 1L |
                         .data$`hist_tcmr2` >= 1L |
                         .data$`hist_tcmr3` >= 1L        , 1L, 0L))

  # Check with Jan chronic vs chronic active (camr11 + camr12 vs camr1 = camr11 + camr12 + camr13)
  if(.test_TA == TRUE){

    banff_diagnosis <-
      banff_diagnosis %>%
      mutate(

        camr11 = ifelse(.data$`cg_score`    != "0" &
                          .data$`ctma`        ==  0  &
                          .data$`cgn`         ==  0  &
                          .data$`agn`         ==  0  ,    1L, 0L),

        camr12 = ifelse(.data$`sptcbmml`    == 1L,      1L, 0L),

        camr13 = case_when(
          .data$`TA`       == 1  & .data$`hist_tcmr_calculated` == 0L  ~ 1L,              # TA ADDED
          .data$`leuscint` == 1 & .data$`hist_tcmr_calculated` == 0L   ~ 1L,
          .data$`newaif`   == 1                                        ~ 1L,
          TRUE                                                         ~ 0L)
      )

  }else{

    banff_diagnosis <-
      banff_diagnosis %>%
      mutate(

        camr11 = ifelse(.data$`cg_score`    != "0" &
                          .data$`ctma`        ==  0  &
                          .data$`cgn`         ==  0  &
                          .data$`agn`         ==  0  ,    1L, 0L),

        camr12 = ifelse(.data$`sptcbmml`    == 1L,      1L, 0L),

        camr13 = case_when(
          .data$`leuscint` == 1 & .data$`hist_tcmr_calculated` == 0L  ~ 1L,
          .data$`newaif`   == 1                                       ~ 1L,
          TRUE                                                        ~ 0L)
      )

  }

  banff_diagnosis <-
    banff_diagnosis %>%
    mutate(

      camr1 = ifelse(.data$`camr11` == 1L |
                       .data$`camr12` == 1L |
                       .data$`camr13` == 1L,                          1L, 0L),

      activeabmr = ifelse(
        (.data$`aamr` == 1 |
           .data$`c4dneg_aamr` == 1 |
           .data$`dsaneg_aamr` == 1) &
          .data$`camr1` == 0,                                         1L, 0L),

      camr = ifelse(.data$`camr1` == 1 &
                      .data$`aamr2` == 1 &
                      (.data$`dsa` == 1 | .data$`aamr21` == 1),          1L, 0L),

      susp_camr = ifelse(
        .data$`camr` == 0 &
          .data$`camr1` == 1 &
          .data$`aamr2` == 1 &
          .data$`dsa` != 1 &
          .data$`aamr21` != 1,                                        1L, 0L)

    )

  ## C4d negative aamr
  # removed | banff_diagnosis$dsaneg_camr == 1 because it is captured by the camr definition
  banff_diagnosis <-
    banff_diagnosis %>%
    mutate(

      c4dneg_camr      = ifelse(.data$`camr1`      == 1  &
                                .data$`dsa`        == 1 &
                               (.data$`aamr22.1`   == 1 |
                                  .data$`aamr22.2` == 1 |
                                  .data$`aamr22.3` == 1) &
                                .data$`aamr21`     == 0L,                 1L, 0L),

      susp_c4dneg_camr = ifelse(.data$`c4dneg_camr` == 0 &
                                .data$`camr1`       == 1 &
                               (.data$`aamr22.1`    == 1 |
                                  .data$`aamr22.2`  == 1 |
                                  .data$`aamr22.3`  == 1) &
                                .data$`aamr21`      == 0L &
                                .data$`dsa`         != 1,                 1L, 0L),

      dsaneg_camr      = ifelse(.data$`camr1` == 1 &
                                .data$`aamr21` == 1 &
                                .data$`dsa` == 0,                          1L, 0L),

      chractabmr       = ifelse(.data$`camr` == 1 |
                                .data$`c4dneg_camr` == 1,                  1L, 0L),

      susp_chractabmr  = ifelse(.data$`susp_camr` == 1 |
                                .data$`susp_c4dneg_camr` == 1,             1L, 0L)

      )

  # chronic abmr variables
  # camr and aamr historical computation
  banff_diagnosis <-
    banff_diagnosis %>%
    arrange(.data$`patient_id`, .data$`sc_date_bx`) %>%
    group_by(.data$`patient_id`) %>%
    mutate(
      hist_camr1 = cumsum(.data$`chractabmr`) - .data$`chractabmr`,
      hist_camr2 = cumsum(.data$`hist_camr`)) %>%
    ungroup %>%
    group_by(.data$`patient_id`) %>%
    mutate(
      hist_aamr1 = cumsum(.data$`activeabmr`) - .data$`activeabmr`,
      hist_aamr2 = cumsum(.data$`hist_aamr`)) %>%
    ungroup %>%
    mutate(
      hist_camr_calculated = ifelse(
        .data$`hist_camr1` >= 1 | .data$`hist_camr2` >= 1, 1L, 0L),

      hist_aamr_calculated = ifelse(
        .data$`hist_aamr1` >= 1 | .data$`hist_aamr2` >= 1, 1L, 0L)
    )


  # hist.abmr variable is a future variable, which people input directly, instead of deriding data.
  banff_diagnosis <-
    banff_diagnosis %>%
    mutate(

      # ifelse((.data$hist.abmr == 1), 1L,
      chrabmr3 = ifelse(.data$`hist_dsa_calculated`  == 1 |
                        .data$`hist_camr_calculated` == 1 |
                        .data$`hist_aamr_calculated` == 1,                 1L,0L),

      chrabmr  = ifelse((.data$`camr11`   == 1 | .data$`camr12` == 1) &
                         .data$`aamr2`    == 0 &
                         .data$`chrabmr3` == 1,                            1L,0L),

      c4d_only = ifelse(.data$`aamr21` == 1L &
                        ( .data$`active_atcmr_code_3.TEMP` == 0 |
                          .data$`active_atcmr_code_4.TEMP` == 0 |
                          .data$`chronic_atcmr_code_4.TEMP` == 0 |
                          .data$`activeabmr` == 0 |
                          .data$`chractabmr` == 0 |
                          .data$`chrabmr`    == 0 ) ,                      1L,0L)

    )

  # final abmr variable
  # final suspicious abmr
  banff_diagnosis <-
    banff_diagnosis %>%
    mutate(

      final_abmr = case_when(
        .data$`chractabmr` == 1  ~  4    ,
        .data$`chrabmr`    == 1  ~  3    ,
        .data$`activeabmr` == 1  ~  2    ,
        .data$`c4d_only`   == 1  ~  1    ,
        TRUE                   ~  NA),

      final_susp_abmr = case_when(
        .data$`susp_camr`         == 1  ~  2 ,
        .data$`susp_c4dneg_camr`  == 1  ~  4 ,
        .data$`susp_aamr`         == 1  ~  1 ,
        .data$`susp_c4dneg_aamr`  == 1  ~  3 ,
        TRUE                          ~  NA),

      final_abmr_verified = case_when(
        .data$`chractabmr`       == 1 ~ 6,
        .data$`chrabmr`          == 1 ~ 4,
        .data$`activeabmr`       == 1 ~ 3,
        .data$`c4d_only`         == 1 ~ 1,
        .data$`susp_camr`        == 1 ~ 5,
        .data$`susp_c4dneg_camr` == 1 ~ 8,
        .data$`susp_aamr`        == 1 ~ 2,
        .data$`susp_c4dneg_aamr` == 1 ~ 7,
        TRUE                        ~  NA)
    )

  banff_diagnosis <-
    banff_diagnosis %>%
    mutate(

      diag_code_2 = case_when(
        .data$`chractabmr`      == 1 ~ 5 ,
        .data$`susp_chractabmr` == 1 ~ 4 ,
        .data$`chrabmr`         == 1 ~ 6 ,
        .data$`activeabmr`      == 1 ~ 3 ,
        .data$`susp_activeaamr` == 1 ~ 2 ,
        .data$`c4d_only`        == 1 ~ 1 ,
        TRUE                 ~ NA),
      diag_code_2_final_abmr      = ifelse(.data$`diag_code_2` %in% c(1,3,5,6), .data$`diag_code_2`,NA),
      diag_code_2_final_susp_abmr = ifelse(.data$`diag_code_2` %in% c(2,4), .data$`diag_code_2`,NA)

    ) %>%

    mutate(

      diag_code_4_active           = na_if(.data$`active_atcmr_code_4.TEMP` , 0),
      diag_code_3                  =
        case_when(

          !is.na(.data$`diag_code_4_active`) ~ NA ,
          TRUE ~ ifelse(.data$`active_atcmr_code_3.TEMP`  == 0,NA,1)),

      diag_code_4_chronic          = na_if(.data$`chronic_atcmr_code_4.TEMP`, 0),
      diag_code_5                  = .data$`ifta`,
      diag_code_bk                 = ifelse(.data$`bk`    == 0,NA,1),
      diag_code_ptld               = ifelse(.data$`ptld`  == 0,NA,2),
      diag_code_cni                = ifelse(.data$`cni`   == 0,NA,3),
      diag_code_atn                = ifelse(.data$`atn`   == 0,NA,4),
      diag_code_cgn                = ifelse(.data$`cgn`   == 0,NA,5),
      diag_code_agn                = ifelse(.data$`agn`   == 0,NA,6),
      diag_code_infec              = ifelse(.data$`infec` == 0,NA,7),
      diag_code_ain                = ifelse(.data$`ain`   == 0,NA,8))

  banff_diagnosis <-
    banff_diagnosis %>%
    mutate(
      diag_code_1 = ifelse(rowSums(is.na(select(
        banff_diagnosis, starts_with("diag_code"),-"diag_code_5"))) ==
          ncol(select(banff_diagnosis, starts_with("diag_code"),-"diag_code_5")), 1, NA))

  banff_diagnosis <-
    banff_diagnosis %>%
    select(-contains('.TEMP'))

  # replace adequacy by what it was originally
  banff_diagnosis <-
    banff_diagnosis %>%
    bind_cols(adequacy_input_copy) %>%
    mutate(adequacy = .data$`adequacy_input`) %>%
    select(-"adequacy_input")

  return(banff_diagnosis)

}

#' @title
#' Assess a banff dataset
#'
#' @description
#' This function takes a dataset and evaluates its format and content based
#' on the accepted format specified in the data dictionary.
#'
#' @param banff_dataset A tibble object.
#'
#' @return
#' A list of tibble objects giving information on the assessment of the dataset.
#'
#' @examples
#' {
#'
#' library(fabR)
#' banff_file <- system.file("extdata", "example.xlsx", package = "banffIT")
#' banff_dataset <- read_excel_allsheets(banff_file)[1,]
#' banff_dataset_evaluate(banff_dataset)
#'
#' }
#'
#' @import dplyr tidyr madshapR
#' @importFrom rlang .data
#' @export
banff_dataset_evaluate <- function(banff_dataset) {

  banff_dict_input <- get_banff_dictionary("input")

  banff_assessment <- list(
    `Data dictionary summary` =
      data_dict_collapse(banff_dict_input)$`Variables`,

    `Dataset assessment` =
      tibble(
        column = as.character(),
        `Assessment comment` = as.character()))

  ##### Test : if is a dataset #####

  if(!silently_run(is_dataset(banff_dataset))){

    attributes(banff_assessment)$`error` <- "

The dataset you provided is not a banff dataset. It must be a data frame object
that must contain a minimal list of variables used in the process."

    banff_assessment$`Dataset assessment` <-
      tibble(
        column = '(all)',
        "Assessment comment" =
          "[ERR] - The dataset you provided is not a banff dataset")

    message(attributes(banff_assessment)$`error`)
    return(banff_assessment)

  }

  ##### Test : if is all mandatory columns are present ####

  test_missing_cols <-
    banff_dict_input$`Variables`$`name`[!
    banff_dict_input$`Variables`$`name` %in% names(banff_dataset)]

  # if no col at all
  if(length(test_missing_cols) == length(banff_dict_input$`Variables`$`name`)){

    attributes(banff_assessment)$`error` <- "

Your dataset contains no variables that matches the data dictionary."

    banff_assessment$`Dataset assessment` <-
      tibble(
        "column" = '(all)',
        "Assessment comment" =
          "[ERR] - No variables that matches the data dictionary found")

    message(attributes(banff_assessment)$`error`)
    return(banff_assessment)

  }

  # if some cols are missing
  if(length(test_missing_cols) > 0){

    attributes(banff_assessment)$`error` <- message("

Some variables in your dataset are missing.
Missing variables in dataset : \n\n",
toString(test_missing_cols) %>% str_replace_all(", ","\n"))


    banff_assessment$`Dataset assessment` <-
      tibble(
        "column" = test_missing_cols,
        "Assessment comment" = "[ERR] - Missing variable")

    message(attributes(banff_assessment)$`error`)
    return(banff_assessment)

  }

  if(nrow(banff_dataset) == 0){

    attributes(banff_assessment)$`empty` <- "

Your banff dataset is empty."

    banff_assessment$`Dataset assessment` <-
      tibble(
        "column" = test_missing_cols,
        "Assessment comment" = "[INFO] - Empty banff dataset.")

    message(attributes(banff_assessment)$`error`)
    return(banff_assessment)

  }

  test_dataset <-
    data_dict_match_dataset(banff_dataset,banff_dict_input,output = 'dataset')

  # permute adequacy calculated and adequacy for testing
  adequacy_input_copy <- calculate_adequacy(test_dataset)

  test_dataset <-
    test_dataset %>%
    mutate(
      adequacy = adequacy_input_copy$`adequacy_calculated`)

  ##### Test : if NA are present where it is not allowed. #####

  no_na_accepted <-
    banff_dict_input$`Variables` %>%
    dplyr::filter(.data$`NA accepted or not` == "NO") %>%
    pull("name")

  contains_na <-
    test_dataset %>%
    summarise(across(all_of(no_na_accepted),~ any(is.na(.)))) %>%
    pivot_longer(everything()) %>%
    dplyr::filter(.data$`value` == TRUE)  %>%
    pull("name")

  if(length(contains_na) > 0){

    attributes(banff_assessment)$`error` = paste0("

Some variables in your dataset contain missing values (NA).
The definition in the banff dictionary require that all the observations for
these variable must be filled with proper categories, and cannot contain NA.
Usually, you can replace all these NA by '0'.

Variables in the dataset that contain NA : \n\n",
toString(contains_na) %>% str_replace_all(", ","\n"))


    banff_assessment$`Dataset assessment` <-
      tibble("column" = contains_na,
             "Assessment comment" = "[ERR] - Contain some missing values (NA)")

    message(attributes(banff_assessment)$`error`)
    return(banff_assessment)

  }

  ##### Test : general tests #####

  test_unique_id <-
    test_valueType <-
    test_unique_value <-
    test_duplicate_col <-
    test_all_na <-
    test_any_na <-
    test_valid_dates <-
    test_adequacy <-
    test_gs <-
    test_sptcbmml <-
    test_categories <-
    banff_assessment$`Dataset assessment`


  #### * test_valueType ####
  # non unique (patient_id, centre, biopsy_id,sc_date_bx)

  test_valueType <-
    check_dataset_valueType(
      test_dataset %>%

        ## patch, fix error later
        add_row(test_dataset[1,] %>%
                mutate(across(c("sc_date_bx","date_tx"), ~ as_any_date("1983-07-19")))),

      banff_dict_input,valueType_guess = TRUE) %>%
    dplyr::filter(!str_detect(.data$`condition`,'\\[INFO\\]')) %>%
    mutate(
      column = .data$`name_var`,
      `Assessment comment` = "[ERR] - The variable type is not compatible with the banff dictionary") %>%
    select("column","Assessment comment")

  #### * test_unique_id ####
  # non unique (patient_id, centre, biopsy_id,sc_date_bx)

  test_unique_id <-
    test_dataset %>%
    group_by(.data$`patient_id`, .data$`centre`, .data$`biopsy_id`, .data$`sc_date_bx`) %>%
    count() %>% dplyr::filter(.data$`n`  > 2) %>%
    ungroup %>%
    mutate(
      column = "Unique ID",
      `Assessment comment` = paste0(
        "[ERR] - Multiple observations for : patient_id(",
        .data$`patient_id`,") - centre(",
        .data$`centre`, ") - biopsy_id(",
        .data$`biopsy_id`,") - sc_date_bx(",
        .data$`sc_date_bx`,")")) %>%
    select("column","Assessment comment")

  #### * test_unique_value ####
  # column with unique value          WARNING

  if(nrow(test_dataset) > 1){
    test_unique_value <-
      get_unique_value_cols(test_dataset) %>%
      ungroup() %>%
      mutate(
        column = .data$`col_name`,
        `Assessment comment` = "[INFO] - All the values are identical") %>%
      select("column","Assessment comment")}


  #### * test_duplicate_col ####
  # possible duplicated columns       WARNING
  # exclude already adressed unique values.

  test_duplicate_col <-
    test_dataset %>%
    select(-all_of(test_unique_value$`column`)) %>%
    # mutate(adequacy = i_score) %>%
    get_duplicated_cols() %>%
    mutate(
      column = .data$`col_name`,
      `Assessment comment` = paste0(
        "[INFO] - ",.data$`condition`)) %>%
    select("column","Assessment comment")

  #### * test_all_na ####
  # all NA column                     WARNING

  test_all_na <-
    test_dataset %>%
    summarize(across(-(c(all_of(no_na_accepted))), (~ all(is.na(.))))) %>%
    mutate(glomeruli = ifelse(.data$`adequacy` == FALSE, FALSE, .data$`glomeruli`)) %>%
    mutate(arteries = ifelse(.data$`adequacy` == FALSE, FALSE, .data$`arteries`)) %>%
    pivot_longer(everything(),names_to = "column",values_to = "Assessment comment") %>%
    dplyr::filter(.data$`Assessment comment` == TRUE) %>%
    mutate(`Assessment comment` = "[INFO] - All the values are missing (NA)")

  #### * test_any_na ####
  # all NA column                     WARNING

  test_any_na <-
    test_dataset %>%
    select(- all_of(test_all_na$`column`)) %>%
    summarize(across(-(all_of(no_na_accepted)), (~ any(is.na(.))))) %>%
    mutate(glomeruli = ifelse(.data$`adequacy` == FALSE, FALSE, .data$`glomeruli`)) %>%
    mutate(arteries = ifelse(.data$`adequacy` == FALSE, FALSE, .data$`arteries`)) %>%
    pivot_longer(everything(),names_to = "column",values_to = "Assessment comment") %>%
    dplyr::filter(.data$`Assessment comment` == TRUE) %>%
    mutate(`Assessment comment` = "[INFO] - The column has missing values")

  #### * test_valid_dates ####
  # sc_date_bx date_tx: must be between 1900 and today ERR

  test_valid_dates <-
    test_dataset %>%
    dplyr::filter(.data$`date_tx` > as_any_date(Sys.time()) | .data$`date_tx` < as.Date("1900-01-01")) %>%
    mutate(
      column = "date_tx",
      `Assessment comment` = "[ERR] - Some dates in 'date_tx' are not be between 1900-01-01 and today") %>%
    select("column","Assessment comment") %>%
    bind_rows(
      test_dataset %>%
        dplyr::filter(.data$`sc_date_bx` > as_any_date(Sys.time()) | .data$`sc_date_bx` < as.Date("1900-01-01")) %>%
        mutate(
          column = "sc_date_bx",
          `Assessment comment` = "[ERR] - Some dates in 'sc_date_bx' are not be between 1900-01-01 and today")) %>%
    select("column","Assessment comment") %>%
    distinct()

  #### * test_adequacy ####
  # test adequacy

  test_adequacy <-
    adequacy_input_copy %>%
    dplyr::filter(is.na(.data$`adequacy_calculated`)) %>%
    mutate(
      column = "adequacy",
      `Assessment comment` = "[ERR] - Some information in adequacy, glomeruli or arteries are missing") %>%
    bind_rows(
      adequacy_input_copy %>%
        dplyr::filter(
          !is.na(.data$`adequacy_input`) &
          .data$`adequacy_calculated` != .data$`adequacy_input`) %>%
        mutate(
          column = "adequacy",
          `Assessment comment` = "[INFO] - For some values, adequacy differ from adequacy_calculated")) %>%
    select("column","Assessment comment") %>%
    distinct()

  #### * test_gs ####
  # gs entre 0 et 100

  test_gs <-
    test_dataset %>%
    dplyr::filter(.data$`gs` < 0 | .data$`gs` > 100) %>%
    mutate(
      column = "gs",
      `Assessment comment` = "[ERR] - Some values in 'gs' are not be between 0 and 100")%>%
    select("column","Assessment comment") %>%
    distinct()

  #### * test_sptcbmml ####
  # is.na(sptcbmml) == TRUE only if microscopy == 0 ERROR

  test_sptcbmml <-
    test_dataset %>%
    select("microscopy","sptcbmml") %>%
    dplyr::filter(is.na(.data$`sptcbmml`) & .data$`microscopy` != 0) %>%
    mutate(
      column = "sptcbmml",
      `Assessment comment` = "[ERR] - sptcbmml has some missing values when microscopy is 0") %>%
    select("column","Assessment comment") %>%
    distinct()

  #### * test_categories ####
  # category in dd but not in dataset WARNING
  # category in dataset but not in dd ERROR
  categorical_variables <- unique(banff_dict_input$`Categorie`$`variable`)

  # exclude already adressed all_na
  categorical_variables <-
    categorical_variables[!categorical_variables %in% test_all_na$`column`]

  # exclude glomerulies and arteries if adequacy OK
  if(sum(is.na(test_dataset$`adequacy`)) == 0){
    categorical_variables <-
      categorical_variables[!categorical_variables %in% c("arteries","glomeruli")]
  }

  if(nrow(test_dataset) == 1){
    test_categories <- tibble("column" = as.character(),"Assessment comment" = as.character())
  }else{
    for(i in categorical_variables){
      # stop()}

      dd_cat <- banff_dict_input$`Categories`[banff_dict_input$`Categorie`$`variable` == i,]$`name`
      ds_cat <- as.character(unique(test_dataset[!is.na(test_dataset[[i]]),i][[1]]))

      cat_in_dd_only <- dd_cat[!dd_cat %in% ds_cat]
      cat_in_ds_only <- ds_cat[!ds_cat %in% dd_cat]

      if(length(cat_in_dd_only) > 0){
        test_categories <-
          test_categories %>%
          bind_rows(
            tibble(
              column = i,
              `Assessment comment` = "[INFO] - Some values are only present in the data dictionary"))}

      if(length(cat_in_ds_only) > 0){
        test_categories <-
          test_categories %>%
          bind_rows(
            tibble(
              column = i,
              `Assessment comment` = "[ERR] - Some values found in the dataset are not in the data dictionary"))}
    }
  }

  # exclude already adressed unique values
  test_categories <-
    test_categories %>%
    mutate(`Assessment comment` =
      if_else(
        (.data$`column` %in% test_unique_value$`column` &
        str_detect(.data$`Assessment comment`,"\\[INFO\\]")),NA_character_,.data$`Assessment comment`)) %>%
    dplyr::filter(!is.na(.data$`Assessment comment`))

  ##### gater all information #####

  all_tests <-
    bind_rows(
      test_valueType,
      test_unique_id,
      test_unique_value,
      test_duplicate_col,
      test_all_na,
      test_any_na,
      test_valid_dates,
      test_adequacy,
      test_gs,
      test_sptcbmml,
      test_categories)

  test_no_fail <-
    tibble(column = names(test_dataset)) %>%
    dplyr::filter(!.data$`column` %in% all_tests$`column`) %>%
    mutate(`Assessment comment` = "No problem detected")

  if("Unique ID" %in% all_tests$`column`){
    test_no_fail <-
      test_no_fail  %>%
      dplyr::filter(!.data$`column` %in%
      c("patient_id","centre","biopsy_id","sc_date_bx"))}

  banff_assessment$`Dataset assessment` <-
    bind_rows(test_no_fail,all_tests) %>%
    left_join(banff_dict_input$`Variables` %>%
              select("index",column = "name"),join_by("column")) %>%
    mutate(index = as.integer(.data$`index`),
           index = replace_na(.data$`index`,0L)) %>%
    arrange(.data$`index`) %>%
    select("column","Assessment comment")

  if(sum(str_detect(
    banff_assessment$`Dataset assessment`$`Assessment comment`,"\\[ERR\\]")) > 0){

    attributes(banff_assessment)$`error` <- "

Some variables in your dataset contain errors. The definition of these variables in
the data dictionary require that all the observations for these variable must be
filled with proper values or categories, and some of them cannot be missing (NA)."

    message(attributes(banff_assessment)$`error`)
    return(banff_assessment)}

  if(sum(str_detect(
    banff_assessment$`Dataset assessment`$`Assessment comment`,"\\[INFO\\]")) > 1){

    attributes(banff_assessment)$`warn` <- "

Your dataset contains no error. Although, some variables may require your
attention because they have some information associated (such as all NA column,
or categories not present in the dataset, or possible duplicated columns.
This message can be ignored if you think your dataset is correct."

    message(attributes(banff_assessment)$`warn`)
    return(banff_assessment)}

  if(toString(unique(
    banff_assessment$`Dataset assessment`$`Assessment comment`)) ==
    "No problem detected"){

    attributes(banff_assessment)$`warn` <- "

Your dataset contains no error."

    banff_assessment$`Dataset assessment` <-
      tibble("column" = '(all)', "Assessment comment" = "No problem detected")

    message(attributes(banff_assessment)$`warn`)
    return(banff_assessment)}

}

#' @title
#' Calculate adequacy of each biopsy from glomeruli and arteries
#'
#' @description
#' This function calculates adequacy of each biopsy (i.e., each observation) based on glomeruli and
#' arteries variables (if provided).
#'
#' @param banff_dataset A tibble object.
#'
#' @return
#' A tibble object with two columns: the calculated adequacy (adequacy_calculated)
#' and the adequacy specified in input (adequacy_input).
#'
#' @examples
#' {
#'
#' library(fabR)
#' banff_file <- system.file("extdata", "example.xlsx", package = "banffIT")
#' banff_dataset <- read_excel_allsheets(banff_file)
#' calculate_adequacy(banff_dataset)
#'
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export
calculate_adequacy <- function(banff_dataset) {

  banff_dataset <-
    banff_dataset %>%
    mutate(
      adequacy_calculated = case_when(
        !is.na(.data$`glomeruli`) &  is.na(.data$`arteries`)  ~ .data$`adequacy`,
        is.na(.data$`glomeruli`) & !is.na(.data$`arteries`)  ~ .data$`adequacy`,
        !is.na(.data$`glomeruli`) & !is.na(.data$`arteries`)  ~
          ifelse((.data$`glomeruli` == 2 & .data$`arteries` == 2), 1 ,
                 ifelse((.data$`glomeruli` == 0 & .data$`arteries` == 1), 2 , 3)),
        TRUE                                  ~ .data$`adequacy`))

  adequacy_input_copy <-
    banff_dataset %>%
    select("adequacy","adequacy_calculated") %>%
    mutate(
      adequacy_input = .data$`adequacy`,
      adequacy = .data$`adequacy_calculated`) %>%
    select(-"adequacy")

  return(adequacy_input_copy)

}

#' @importFrom madshapR dataset_cat_as_labels
#' @name dataset_cat_as_labels
#' @rdname dataset_cat_as_labels
#' @keywords imported
#' @export
madshapR::dataset_cat_as_labels

#' @importFrom madshapR dataset_summarize
#' @name dataset_summarize
#' @rdname dataset_summarize
#' @keywords imported
#' @export
madshapR::dataset_summarize
