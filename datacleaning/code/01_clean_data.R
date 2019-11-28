# Builds helper functions to clean the questionnaire data ----

# Function that removes the bottom row of Gorilla output (since Gorilla adds
# "END OF FILE" to the last row of any data it outputs)
remove_bottom_row <- function(df) {
  df <- df[-nrow(df), ]
  return(df)
}

# Function that drops all Gorilla columns that end with "quantised"
drop_quantised <- function(df) {
  df <-
    select(df, -matches("quantised$"))
  return(df)
}

drop_checkpoint <- function(df) {
  df <- 
    select(df, -matches("^checkpoint"))
  return(df)
}

drop_branch <- function(df) {
  df <- 
    select(df, -matches("^branch"))
  return(df)
}
# Function that drops all unnecessary Gorilla columns (but keep data & IDs)
drop_gorilla_cols <- function(df) {
  extracols <- c(
    "Event Index",
    "UTC Timestamp",
    "UTC Date",
    "Local Timestamp",
    "Local Timezone",
    "Local Date",
    "Experiment ID",
    "Experiment Version",
    "Tree Node Key",
    "Repeat Key",
    "Schedule ID",
    "Participant Public ID",
    "Participant Starting Group",
    "Participant Status",
    "Participant External Session ID",
    "Participant Device Type",
    "Participant Device",
    "Participant OS",
    "Participant Browser",
    "Participant Monitor Size",
    "Participant Viewport Size",
    "Task Version",
    "Task Name",
    "END QUESTIONNAIRE"
  )
  # Note that "kept" columns are: Private ID, completion code, and all data
  
  df <- select(df, -extracols) # Delete the extra columns
  return(df)
}

# Function that cleans each individual element in a list
clean_at_once <- function(df) {
  df <- df %>%
    remove_bottom_row() %>%
    drop_quantised() %>%
    drop_checkpoint() %>%
    drop_branch() %>%
    drop_gorilla_cols()
  return(df)
}

# Function that clean all data frames in a list using clean_questionnaire_data
clean_data <- function(list) {
  # Clean all elements of the list of data frames (i.e. clean all data at once)
  df <- lapply(list, clean_at_once) %>%
    reduce(merge)
}