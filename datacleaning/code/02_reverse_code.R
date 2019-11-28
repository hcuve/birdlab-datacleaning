# Builds helper functions for reverse coding and scoring ----
# v2.0 - 11 Nov 2019 - pls message Xavier if you spot any bugs
# Dependencies: tidyverse, psych

# Helper function that reverse scores items
reverse <- function(df, items, scalemax) {
  df[items] <- (scalemax + 1) - df[items]
  return(df) # Return the whole data frame, not just the reversed items
}

# Items to reverse for each questionnaire
items_to_reverse_AQ <- c(
  "AQ_03",
  "AQ_06",
  "AQ_08",
  "AQ_09",
  "AQ_11",
  "AQ_12",
  "AQ_17",
  "AQ_18",
  "AQ_19",
  "AQ_20",
  "AQ_21",
  "AQ_24",
  "AQ_27",
  "AQ_28"
)

items_to_reverse_TAS <- c("TAS_04",
                          "TAS_05",
                          "TAS_10",
                          "TAS_18",
                          "TAS_19")

items_to_reverse_STAI <- c(
  # STAI-T questions to reverse
  "STAI_01",
  "STAI_03",
  "STAI_06",
  "STAI_07",
  "STAI_10",
  "STAI_13",
  "STAI_14",
  "STAI_16",
  "STAI_19",
  # STAI-S questions to reverse
  "STAIS_01",
  "STAIS_02",
  "STAIS_05",
  "STAIS_08",
  "STAIS_10",
  "STAIS_11",
  "STAIS_15",
  "STAIS_16",
  "STAIS_19",
  "STAIS_20"
)

# Helper function that reverse codes input data using the  procedure for one
# questionnaire. Choose which reversing proc to apply based on a string input.
reverse_one_questionnaire <- function(which, df) {
  # "which" has to be the first argument for the function to work properly...
  # this still needs to be tested more thoroughly
  switch(
    which,
    AQ = reverse(df = df, items = items_to_reverse_AQ, scalemax = 4),
    TAS = reverse(df = df, items = items_to_reverse_TAS, scalemax = 5),
    STAI = reverse(df = df, items = items_to_reverse_STAI, scalemax = 4),
    IAS = return(df),
    DASS = return(df)
  )
}

# Vectorized function that repeatedly applies the single-questionnaire scoring
# function to an input data frame, based on a vector indicating which
# questionnaires to score
reverse_code <- function(data, what_to_reverse) {
  map(.x = what_to_reverse, .f = reverse_one_questionnaire, df = data) %>%
    reduce(merge)
  
}