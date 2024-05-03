## code to prepare `valueType_list` dataset goes here

library(fabR)
library(usethis)

banff_dict <- read_excel_allsheets("inst/extdata/banff_dictionary.xlsx")
banff_dict <- banff_dict[c('read me','Variables','Categories')]

banff_example  <- read_excel_allsheets("inst/extdata/banff_example.xlsx")
banff_template <- read_excel_allsheets("inst/extdata/banff_template.xlsx")

use_data(banff_dict,banff_template,banff_example, overwrite = TRUE, internal = TRUE)
