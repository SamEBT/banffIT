## code to prepare `valueType_list` dataset goes here

library(fabR)
library(usethis)

banff_dict <- read_excel_allsheets("inst/extdata/data_dictionary_Banff_2024_02_23.xlsx")
banff_dict <- banff_dict[c('read me','Variables','Categories')]

use_data(banff_dict, overwrite = TRUE, internal = TRUE)
