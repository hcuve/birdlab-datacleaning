# Function that scores the Body Perception Questionnaire (BPQ) 
# (Cabrera et al., 2018) and all subscales
# Dependencies: 03_scoring_helpers.R, tidyverse



score_BPQ <- function(df){
  
  # Create a new temporary df to store the BPQ data 
  BPQ_data <- as_tibble(df) %>%
    select("Participant Private ID", 
           BAI_01:BAI_26, ANSR_01:ANSR_20, HHI_01:HHI_28) %>%
    # Recode factors into numerics
    mutate_at(.vars = vars(BAI_01:HHI_28), 
              .funs = recode(Never = 1, 
                             Mild = 2, 
                             Moderate = 3, 
                             Severe = 4, 
                             Debilitating = 5))


  print(BPQ_data)
  
  # Construct final scores
  BPQ_score <- tibble(
    "Participant Private ID" = BPQ_data$`Participant Private ID`,
    
    # Body awareness image
    BAI_score = sumvars(.df = BPQ_data, 
                        BAI_01:BAI_26) / 26,
    
    # Autonomic nervous system response
    ANSR_score = sumvars(.df = BPQ_data,
                         ANSR_01:ANSR_20) / 20,
    
    # Health history inventory
    HHI_score = sumvars(.df = BPQ_data,
                        HHI_01:HHI_28) / 28
  )
  
  return(BPQ_score)
  
}
