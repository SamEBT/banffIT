
<!-- README.md is generated from README.Rmd. Please edit that file -->

# banffIT

<!-- badges: start -->

[![R-CMD-check](https://github.com/maelstrom-research/banffIT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/maelstrom-research/banffIT/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The banffIT package provides provides functions to assign standardized
diagnoses using the Banff Classification (Category 1 to 6 diagnoses,
including Acute and Chronic active T-cell mediated rejection as well as
Active, Chronic active, and Chronic antibody mediated rejection). The
main function `banff_launcher()`considers a minimal dataset containing
biopsies information in a specific format (described by a data
dictionary), verifies its content and format (based on the data
dictionary), assign diagnoses, and create a summary report.

# Get started

## Install the package and use the example file

``` r
# To install madshapR:
install.packages('banffIT')

library(banffIT)
# If you need help with the package, please use:
banffIT_website()

# use the example file provided. remplace tempdir by a directory name.
input_dataset <- system.file("extdata", "example.xlsx", package = "banffIT")
banff_launcher(input_dataset, output_folder = tempdir())
```
