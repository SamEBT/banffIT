
# banffIT 0.1.0

The banffIT package provides functions to assign standardized diagnoses
using the Banff Classification (Category 1 to 6 diagnoses, including
Acute and Chronic active T-cell mediated rejection as well as Active,
Chronic active, and Chronic antibody mediated rejection). The main
function `banff_launcher()`considers a minimal dataset containing
biopsies information in a specific format (described by a data
dictionary), verifies its content and format (based on the data
dictionary), assigns diagnoses, and creates a summary report.

## Main functionality

- `banff_launcher()` This function takes a path string identifying the
  input file path. The function internally runs a series of tests that
  assess the input dataset. If any of these tests fails, the user gets
  information allowing them to correct the input dataset and rerun the
  process. Once all tests pass, the dataset is given as an output with a
  diagnosis for each observation (using the function `add_diagnoses()`
  internally). The output dataset, along with its associated labels
  (“label:en” by default) are provided to the user in an Excel format
  file accessible in the output_folder specified (the working directory
  by default). The output dataset comes with a report that summarizes
  information about variable distributions and descriptive statistics.

## Additional functions (used in the main function, but can be used separately)

- `banff_dataset_evaluate()` This function takes a dataset and evaluates
  its format and content based on the accepted format specified in the
  data dictionary.

- `calculate_adequacy()` A tibble object with two variables: the
  calculated adequacy (adequacy_calculated) and the adequacy specified
  in input (adequacy_input).

- `add_diagnoses()` This function takes a dataset and returns a
  diagnosis for each observation. For the function to run, the dataset
  must not contain any errors that `banff_launcher()`would have
  detected. Please prefer using `banff_launcher()` to run additional
  tests.

## Helper functions

- `get_banff_dictionary()`, `get_banff_example()`,
  `get_banff_template()` This function gets the data dictionary used to
  control the consistency of the input dataset, a example dataset and a
  template.

- function `banffIT_website()` This function sends the user to the
  online documentation for the package, which includes a description of
  the latest version of the package, vignettes, user guides, and a
  reference list of functions and help pages.

## Imported functions from madshapR

- [dataset_cat_as_labels()](https://maelstrom-research.github.io/madshapR-documentation/reference/dataset_cat_as_labels.html)

- [dataset_summarize()](https://maelstrom-research.github.io/madshapR-documentation/reference/dataset_summarize.html)
