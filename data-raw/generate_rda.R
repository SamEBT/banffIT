## code to prepare `valueType_list` dataset goes here

library(fabR)
library(usethis)

banff_dict <- read_excel_allsheets("inst/extdata/data_dictionary_Banff_2024_05_03.xlsx")
banff_dict <- banff_dict[c('read me','Variables','Categories')]

banff_example <- read_excel_allsheets("inst/extdata/example.xlsx")
banff_template <- read_excel_allsheets("inst/extdata/template.xlsx")

use_data(banff_dict,banff_template,banff_example, overwrite = TRUE, internal = TRUE)
