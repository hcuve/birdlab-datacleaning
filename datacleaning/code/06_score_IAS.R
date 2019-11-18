# Function that scores the Interoceptive Accuracy Scale (IAS). The scale has
# 21 x 5-point Likert-style items, yielding scores between 21-105
# Dependencies: tidyverse, 03_scoring_helpers.R

score_IAS <- function(df) {
  IAS_data <-
    as_tibble(df) %>% # Building this df to make debugging easier
    select("Participant Private ID", IAS_01:IAS_21)
  
  IAS_score <-
    tibble(
      # IAS score is just the sum of the IAS answers
      "Participant Private ID" = IAS_data$`Participant Private ID`,
      IAS_score = sumvars(.df = IAS_data,
                          IAS_01:IAS_21)
    )
  return(IAS_score)
}