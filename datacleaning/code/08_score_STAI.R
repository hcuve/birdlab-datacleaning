# Function that scores the State-Trait Anxiety Inventory (STAI) scale. The STAI
# is a 40-item (4-point Likert) scale, with scores ranging 20-80. It has two
# subscales (STAI-T and STAI-S) corresponding to Trait anxiety and State
# anxiety, respectively.
# Dependencies: 02_reverse_code.R, 03_scoring_helpers.R, tidyverse
# NOTE: ASSUMES REVERSE CODING HAS ALREADY BEEN DONE

score_STAI <-
  function(df) {
    # "_" bc R doesn't let us define vars w/ hyphens
    
    STAI_data <- as_tibble(df) %>%
      select("Participant Private ID", "Participant Completion Code", STAI_01:STAI_20, STAIS_01:STAIS_20)
    
    STAI_score <- tibble(
      "Participant Private ID" = STAI_data$`Participant Private ID`,
      "Participant Completion Code" = STAI_data$`Participant Completion Code`,
      
      # Trait anxiety is a simple sum of the "STAI" columns in the Gorilla output
      STAI_Trait_score = sumvars(.df = STAI_data,
                                 STAI_01:STAI_20),
      
      # State anxiety is a simple sum of the "STAIS" columns in the Gorilla output
      STAI_State_score = sumvars(.df = STAI_data,
                                 STAIS_01:STAIS_20)
    )
    return(STAI_score)
  }
