# Builds functions to help score data
# v2.0 - 11 Nov 2019 - please message Xavier if you find any bugs

# Helper function to sum an arbitrary list of columns of a df
sumvars <- function(.df, ...) {
  .df %>% # Pipe in the data
    select(...) %>% # Select the columns you asked for - supports tidy eval
    reduce(`+`) # Add them all together
}

# Helper function that scores input data using the scoring procedure for one
# questionnaire. Choose which scoring function to apply based on a string input.
score_one_questionnaire <- function(which, df) {
  # "which" has to be the first argument for the function to work properly...
  # this still needs to be tested more thoroughly
  switch(
    which,
    AQ = score_AQ(df, method = "sum"),
    # Change "method" to set the default
    TAS = score_TAS(df),
    IAS = score_IAS(df),
    DASS = score_DASS(df),
    STAI = score_STAI(df),
    BPQ = score_BPQ(df)
  )
}

# Vectorized function that repeatedly applies the single-questionnaire scoring
# function to an input data frame, based on a vector indicating which
# questionnaires to score
score_questionnaires <- function(data, what_to_score) {
  map(.x = what_to_score, .f = score_one_questionnaire, df = data) %>%
    reduce(merge)
  
}
