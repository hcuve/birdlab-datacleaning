# Scores WASI-2 FSIQ-2 scales, incl. PRI and VCI
# This is just subsetting the CSV output from the scoring google sheets at the
# moment... we will want to build the questionnaire scoring into here eventually
score_IQ <- function(df) {
  IQ_data <- df
  
  IQ_score <- IQ_data %>%
    select(starts_with('Participant'), Years, Months, 
           `FSIQ-2 Composite`, `FSIQ-2 Percentile`)
    
  return(IQ_score)
}