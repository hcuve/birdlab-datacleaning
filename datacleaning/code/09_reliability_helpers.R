# Function that takes a data frame and specified columns, and returns the raw
# (unstandardised) Cronbach's alpha for the columns of that data frame
# Dependencies: psych
calculate_raw_alpha <- function(df) {
  # Since psych::alpha() outputs a list of list of lists, we need to index into
  # the first element of the output (which is a list of reliability stats), then
  # return only the first element of that list (which is the raw alpha).
  suppressWarnings(psych::alpha(df)[[1]][[1]])
}

# Helper function to apply the calculate_raw_alpha function depending on what
# questionnaire we want to score
calculate_one_alpha <- function(which, df) {
  # "which" has to be the first argument for the function to work properly...
  # this still needs to be tested more thoroughly
  switch(
    which,
    AQ = select(df, AQ_01:AQ_28) %>% calculate_raw_alpha(),
    TAS = select(df, TAS_01:TAS_20) %>% calculate_raw_alpha(),
    IAS = select(df, IAS_01:IAS_21) %>% calculate_raw_alpha(),
    DASS = select(df, DASS_01:DASS_21) %>% calculate_raw_alpha(),
    STAI = select(df, STAI_01:STAI_20, STAIS_01:STAIS_20) %>%
      calculate_raw_alpha()
  )
}

# Function to vectorize calculate_one_alpha
calculate_alphas <- function(data, what_to_score) {
  alphas <- map(.x = what_to_score, .f = calculate_one_alpha, data)
  names(alphas) <- what_to_score
  return(alphas)
}
