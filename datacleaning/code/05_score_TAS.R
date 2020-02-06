# Function that scores TAS and all subscales. Accepts a data frame as input;
# returns a data frame containing total TAS score (5-pt likert, range 20-100), 
# as well as 3 subscale factors: Difficulty Identifying Feelings (DIF), 
# Difficulty Describing Feelings (DDF), and Externally Oriented Thinking (EOT).
# Dependencies: 02_reverse_code.R, 03_scoring_helpers.R, tidyverse
# NOTE: ASSUMES REVERSE CODING HAS ALREADY BEEN DONE

score_TAS <- function(df) {
  # Creates a new df to store the TAS data
  TAS_data <- as_tibble(df) %>%
    select("Participant Private ID", "Participant Completion Code", TAS_01:TAS_20)
  
  # Return a tibble containing calculated score and subscales (simple sums)
  TAS_score <- tibble(
    "Participant Private ID" = TAS_data$`Participant Private ID`,
    #completion code
    "Participant Completion Code" = TAS_data$`Participant Completion Code`,
    TAS_score_total =  sumvars(.df = TAS_data, 
                               TAS_01:TAS_20),
    
    # Difficulty Identifying Feelings subscale is items 1, 3, 6, 7, 9, 13, & 14
    TAS_subscale_DIF = sumvars(.df = TAS_data,
                               TAS_01, TAS_03, TAS_06, TAS_07, TAS_09, 
                               TAS_13, TAS_14),
    
    # Difficulty Describing Feelings subscale is items 2, 4, 11, 12, and 17
    TAS_subscale_DDF = sumvars(.df = TAS_data,
                               TAS_02, TAS_04, TAS_11, TAS_12, TAS_17),
    
    # Externally Oriented Thoughts subscale is items 5, 8, 10, 15, 
    # 16, 18, 19, and 20
    TAS_subscale_EOT = sumvars(.df = TAS_data,
                               TAS_05, TAS_08, TAS_10, TAS_15, TAS_16, 
                               TAS_18, TAS_19, TAS_20)
  ) 
  return(TAS_score)
}
