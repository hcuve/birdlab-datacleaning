# Builds helper functions to load questionnaire data ----
# v2.0 - 11 Nov 2019 - pls message Xavier if you spot any bugs
# Dependencies: "tidyverse" packages

# Function that reads multiple files stored in a single path/input slot
load_data <- function(path) {
  all_data <- lapply(path, read_csv)
  return(as.list(all_data))
}
