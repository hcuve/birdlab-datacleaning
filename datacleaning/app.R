# App to dynamically clean and score questionnaires used by Bird Lab ----
# v2.0 - 17 Nov 2019 - pls tell Xavier if you find any bugs
#
# Questionnaires supported:
# - Short form Autism Quotient (AQ-28)
# - Toronto Alexithymia Scale (TAS)
# - Interoceptive Accuracy Scale (IAS)
# - Depression, Anxiety, and Stress Scale (DASS)
# - State-Trait Anxiety Inventory (STAI-T and STAI-S)
#
# To update:
# - Body Perception Questionnaire (BPQ)
# - Automated way to add new questionnaire scoring logic
#
# Inputs:
# - .csv of data table
# - buttons indicating what kind of cleaning is to be performed
#
# Outputs:
# - interactive table of cleaned values
# - .csv of cleaned data
# - interactive table of reliability statistics
# - .csv of reliability statistics
# - exploratory data viz??

# Load required packages and scripts
library(shiny)
library(tidyverse)
library(psych)
source("code/00_load_data.R")
source("code/01_clean_data.R")
source("code/02_reverse_code.R")
source("code/03_scoring_helpers.R")
source("code/04_score_AQ.R")
source("code/05_score_TAS.R")
source("code/06_score_IAS.R")
source("code/07_score_DASS.R")
source("code/08_score_STAI.R")
source("code/09_reliability_helpers.R")

# Useful for debugging
# verbatimTextOutput("debug) # paste this into the UI
# output$debug <- renderPrint({    }) # paste this into the server

# UI ----
ui <- fluidPage(
  titlePanel("Bird Lab Questionnaire Data Processing"),
  
  sidebarLayout(
    # Inputs and outputs
    sidebarPanel(
      textInput(inputId = "exp_id",
                label = "What's the name/ID of your experiment?"),
      
      fileInput(
        inputId = "files",
        label = "Upload all data files:",
        multiple = TRUE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      
      checkboxGroupInput(
        inputId = "questionnaires",
        label = "Which questionnaire(s) are you analyzing?",
        choices = c(
          "Autism Quotient" = "AQ",
          "Toronto Alexithymia Scale" = "TAS",
          "State-Trait Anxiety Inventory" = "STAI",
          "Interoceptive Accuracy Scale" = "IAS",
          "Depression, Anxiety, and Stress Scale" = "DASS"
        )
      ),
      
      actionButton(inputId = "go",
                   label = "Analyze!"),
      
      p(),
      
      p(strong("Downloads:")),
      
      downloadButton(outputId = "download_cleaned",
                     label = "Download all cleaned data"),
      
      downloadButton(outputId = "download_reversed",
                     label = "Download reverse-coded data"),
      
      downloadButton(outputId = "download_scores",
                     label = "Download questionnaire scores"),
      
      downloadButton(outputId = "download_reliability",
                     label = "Download reliability statistics")
    ),
    
    # Data display
    mainPanel(tabsetPanel(
      tabPanel("Data Cleaning",
               p(),
               p(strong("Cleaned Data:")),
               dataTableOutput("cleaned")),
      tabPanel(
        "Scoring Questionnaires",
        p(),
        textOutput("reversed"),
        p(strong("Scores (including all subscales):")),
        dataTableOutput("scores")
      ),
      tabPanel(
        "Reliability Statistics",
        p(),
        p(strong("Cronbach's Alpha:")),
        tableOutput("reliability")
      )
    ))
  )
)

# Server ----
server <- function(input, output) {
  # Intermediate values
  
  # Store the files the user uploads in a reactive expression, once the
  # user hits "Analyze!"
  uploaded_files <- eventReactive(input$go, {
    req(input$files)
    
    input$files$datapath
  })
  
  # Load the uploaded files into a reactive expression called "all_data"
  all_data <- reactive({
    req(input$files)
    
    uploaded_files() %>% load_data()
  })
  
  # Clean all the uploaded data and store it in a new reactive expression
  all_data_cleaned <- reactive({
    req(input$files)
    
    all_data() %>% clean_data()
  })
  
  # Store which questionnaires the user wants to analyze (but add a dependency
  # on uploaded files - it makes no sense to score without uploaded files)
  questionnaires_to_score <- reactive({
    req(input$files)
    
    input$questionnaires
  })
  
  # Reverse all the cleaned data... to enable accurate scoring!
  reversed_data <- reactive({
    isolate(all_data_cleaned() %>% reverse_code())
  })
  
  # Score all the questionnaires the user wants to analyze
  all_scores <- reactive({
    req(reversed_data(), questionnaires_to_score())
    
    score_questionnaires(data = reversed_data(),
                         what_to_score = questionnaires_to_score())
  })
  
  # Calculate Cronbach's alpha reliability for the selected questionnaires
  reliability_stats <- reactive({
    req(all_data(), questionnaires_to_score())
    
    calculate_alphas(data = reversed_data(),
                     what_to_score = questionnaires_to_score())
  })
  
  # Outputs
  
  # Display all the cleaned data in a table
  # TO DO... decide whether to add a button to switch displaying CLEANED vs.
  # REVERSED data and change the program logic if so
  output$cleaned <- renderDataTable({
    all_data_cleaned()
  },
  options = list(scrollX = TRUE))
  
  # Allow the user to download cleaned data in a .csv
  output$download_cleaned <- downloadHandler(
    filename = function() {
      paste0("All_data_cleaned_", input$exp_id, "_",
             Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(all_data_cleaned(), file)
    },
    contentType = "text/csv"
  )
  
  # Display a visual check to assure the user that reverse-coding has been done
  output$reversed <- renderText({
    paste0(
      "Check: has reverse-coding been done?",
      " ",
      ifelse(!is_null(reversed_data()), "Yes...", "NO! ERROR")
    )
  })
  
  # Allow the user to download reverse-coded data in a .csv
  output$download_reversed <- downloadHandler(
    filename = function() {
      paste0("Reverse_coded_data_",
             input$exp_id,
             "_",
             Sys.Date(),
             ".csv")
    },
    content = function(file) {
      write_csv(reversed_data(), file)
    },
    contentType = "text/csv"
  )
  
  # Display the scores/subscales for the selected questionnaires in a table
  output$scores <- renderDataTable({
    all_scores()
  },
  options = list(scrollX = TRUE))
  
  # Allow the user to download scores/subscales in a .csv
  output$download_scores <- downloadHandler(
    filename = function() {
      paste0("All_scores_", input$exp_id, "_",
             Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(all_scores(), file)
    }
  )
  
  # Display the reliability statistics (Cronbach's alpha) in a simple table
  output$reliability <-
    renderTable({
      reliability_stats()
    }, digits = 3)
  
  # Allow the user to download reliability statistics in a .csv
  output$download_reliability <- downloadHandler(
    filename = function() {
      paste0("Reliability_Cronbachs_alpha",
             input$exp_id,
             "_",
             Sys.Date(),
             ".csv")
    },
    content = function(file) {
      write_csv(as_tibble(reliability_stats()), file)
    },
    contentType = "text/csv"
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
