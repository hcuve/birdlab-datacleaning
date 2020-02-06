# Function that scores DASS-21 and all subscales. DASS-21 is a 21-item scale
# (0-3 likert) measuring the participant's levels of depression, anxiety, and 
# stress. DASS is based on a dimensional understanding of these factors - 
# each of the 21 items is mapped to one of the three factors.
# Dependencies: tidyverse, 03_scoring_helpers.R

score_DASS <- function(df) {
  DASS_data <- as_tibble(df) %>% # Building this df to make debugging easier
    select("Participant Private ID","Participant Completion Code", DASS_01:DASS_21)
  
  DASS_score <- tibble(
    "Participant Private ID" = DASS_data$`Participant Private ID`,
    "Participant Completion Code" = DASS_data$`Participant Completion Code`,
    DASS_score_total =         sumvars(.df = DASS_data,
                                       DASS_01:DASS_21),
    
    # Depression subscale is items 3, 5, 10, 13, 16, 17, and 21
    DASS_subscale_depression = sumvars(.df = DASS_data,
                                       DASS_03, DASS_05, DASS_10, DASS_13,
                                       DASS_16, DASS_17, DASS_21),
    
    # Anxiety subscale is items 2, 4, 7, 9, 15, 19, and 20
    DASS_subscale_anxiety =    sumvars(.df = DASS_data,
                                       DASS_02, DASS_04, DASS_07, DASS_09,
                                       DASS_15, DASS_19, DASS_20),
    
    # Stress subsccale is items 1, 6, 8, 11, 12, 14, and 18
    DASS_subscale_stress =     sumvars(.df = DASS_data,
                                       DASS_01, DASS_06, DASS_08, DASS_11,
                                       DASS_12, DASS_14, DASS_18)
  )
  return(DASS_score)
}
