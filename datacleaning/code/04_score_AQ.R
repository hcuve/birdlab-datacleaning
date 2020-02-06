# Function that scores AQ and all subscales. Accepts two scoring methods.
# - collapse: sum of 1 if "agree" or "slightly agree", after reverse-scoring,
#   0 if not. Ranges between 0 and 28.
# - sum: adds up all values after reverse scoring. Ranges between 28 and 112.
# Dependencies: 02_reverse_code.R, 03_scoring_helpers.R, tidyverse
# NOTE: ASSUMES REVERSE CODING HAS ALREADY BEEN DONE

score_AQ <- function(df, method = "collapse") {
  stopifnot(method == "collapse" | method == "sum")
  
  # Create a new df to store the AQ data
  # to do: include "Participant Completion Code" in all the scoring
  AQ_data <- as_tibble(df) %>%
    select("Participant Private ID","Participant Completion Code", AQ_01:AQ_28)
  
  # Collapse-summing method of scoring from the original paper. Record score = 1
  # if respondent selected "agree"/"slightly agree", else record score = 0.
  if (method == "collapse") {
    AQ_transform <- AQ_data %>%
      mutate_at(vars(AQ_01:AQ_28), list( ~ ifelse(. <= 2, 1, 0)))
    
    AQ_score <- tibble(
      "Participant Private ID" = AQ_data$`Participant Private ID`,
      #adding completion code
      "Participant Completion Code" = AQ_data$`Participant Completion Code`,
      AQ_score_total =          sumvars(.df = AQ_transform,
                                        AQ_01:AQ_28),
      
      # The Social skills subscale is items 1, 11, 13, 15, 22, 44, 46, and 47
      # in the AQ-50; so items 1, 9, 10, 12, 15, 24, 26, and 27 in the AQ-28
      AQ_subscale_social =      sumvars(.df = AQ_transform,
                                        AQ_01, AQ_09, AQ_10, AQ_12, AQ_15,
                                        AQ_24, AQ_26, AQ_27),
      
      # The Routine subscale is items 2, 25, and 34 in the AQ-50; so it is
      # items 2, 17, and 19 in the AQ-28
      AQ_subscale_routine =     sumvars(.df = AQ_transform,
                                        AQ_02, AQ_17, AQ_19),
      
      # The Switching subscale is items 4, 10, 32, and 37 in the AQ-50; so
      # it is items 4, 8, 18, and 21 in the AQ-28
      AQ_subscale_switching =   sumvars(.df = AQ_transform,
                                        AQ_04, AQ_08, AQ_18, AQ_21),
      
      # The Imagination subscale is items 3, 8, 14, 20, 36, 42, 45, 50 in the
      # AQ-50; so it is items 3, 6, 11, 14, 20, 23, 25, and 28 in the AQ-28
      AQ_subscale_imagination = sumvars(.df = AQ_transform,
                                        AQ_03, AQ_06, AQ_11, AQ_14, AQ_20,
                                        AQ_23, AQ_25, AQ_28),
      
      # The Numbers and patterns subscale / factor is items 6, 9, 19, 23, and
      # 41 in the AQ-50; so it is items 5, 7, 13, 16, and 22 in the AQ-28
      AQ_subscale_numbers =     sumvars(.df = AQ_transform,
                                        AQ_05, AQ_07, AQ_13, AQ_16, AQ_22)
    )
    return(AQ_score)
  }
  
  # Expanded sum method of scoring - ranges from 28-112
  if (method == "sum") {
    AQ_score <-
      tibble(
        "Participant Private ID" = AQ_data$`Participant Private ID`,
        "Participant Completion Code" = AQ_data$`Participant Completion Code`,
        AQ_score_total =          sumvars(.df = AQ_data,
                                          AQ_01:AQ_28),
        
        # The Social skills subscale is items 1, 11, 13, 15, 22, 44, 46, and 47
        # in the AQ-50; so items 1, 9, 10, 12, 15, 24, 26, and 27 in the AQ-28
        AQ_subscale_social =      sumvars(.df = AQ_data,
                                          AQ_01, AQ_09, AQ_10, AQ_12, AQ_15,
                                          AQ_24, AQ_26, AQ_27),
        
        # The Routine subscale is items 2, 25, and 34 in the AQ-50; so it is
        # items 2, 17, and 19 in the AQ-28
        AQ_subscale_routine =     sumvars(.df = AQ_data,
                                          AQ_02, AQ_17, AQ_19),
        
        # The Switching subscale is items 4, 10, 32, and 37 in the AQ-50; so
        # it is items 4, 8, 18, and 21 in the AQ-28
        AQ_subscale_switching =   sumvars(.df = AQ_data,
                                          AQ_04, AQ_08, AQ_18, AQ_21),
        
        # The Imagination subscale is items 3, 8, 14, 20, 36, 42, 45, 50 in the
        # AQ-50; so it is items 3, 6, 11, 14, 20, 23, 25, and 28 in the AQ-28
        AQ_subscale_imagination = sumvars(.df = AQ_data,
                                          AQ_03, AQ_06, AQ_11, AQ_14, AQ_20,
                                          AQ_23, AQ_25, AQ_28),
        
        # The Numbers and patterns subscale / factor is items 6, 9, 19, 23, and
        # 41 in the AQ-50; so it is items 5, 7, 13, 16, and 22 in the AQ-28
        AQ_subscale_numbers =     sumvars(.df = AQ_data,
                                          AQ_05, AQ_07, AQ_13, AQ_16, AQ_22)
      )
    return(AQ_score)
  }
}
