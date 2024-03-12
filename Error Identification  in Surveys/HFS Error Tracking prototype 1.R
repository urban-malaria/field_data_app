# SHINY DASHBOARD APP 
#
# ERROR IDENTIFICATION MODULE FOR HEALTH FACITLITY SURVEY APP V1.0
#
# THE OUTPUTS IS IN TABLE FORMAT AND THE FILTERS AS FOLLOWING 
#
# Identify discrepancies between the reported age and the calculated age based on the date of birth
# Identify number of pregnancies carried to term is greater than the number of pregnancies
# Identify records where the number of pregnancies reported exceeds five
# Highlight records where the date of data collection is missing
# Identify records where consent was not reported for RDT(Rapid Diagnostic Test) or DBS (Dried Blood Spot) samples
# Identifying incomplete records
# 
# This module reads data from a static CSV file and visualize it in a table considering the filter selected
#
#
# THIS MODULE CAN BE USE TO VIEW ERROR IS HOUSEHOLD SURVEY AND LONGITUDINAL SURVEYS 
#
#


library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(lubridate)
library(DT)

# Define UI with shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "NU Urban Dashboard"),
  dashboardSidebar(
    selectInput("filter", "Select Filter:",
                choices = c("Pregnancies > 5",
                            "Pregnancies carried Term",
                            "DOB vs AGE",
                            "Incomplete Records",
                            "Date Missing",
                            "No RDT & DBS Consent"))
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
    .custom-table-container {
      border: 2px solid black; /* Border around the table */
      background-color: white; /* Light grey background for the table */
      padding: 10px; /* Adds some spacing around the table */
    }
  "))),
    h3("Error Identification in Health Facility Survey", style = "text-align: left;"), # Title for the table
    div(class = "custom-table-container", DT::dataTableOutput("filteredTable"))
  )
)

# Define server logic
server <- function(input, output) {
  # Read dataset
  data <- read_excel("Dummy Data HFS.xlsx")
  
  # Filtered data based on selection
  filteredData <- reactive({
    df <- data
    
    if(input$filter == "Pregnancies > 5") {
      df <- df %>% filter(`q124b: How many pregnancies have you had` > 5)
    } else if(input$filter == "Pregnancies carried Term") {
      df <- df %>% filter(`q124c: How many pregnancies were carried to term?` > `q124b: How many pregnancies have you had`)
    } else if(input$filter == "DOB vs AGE") {
      df <- df %>% mutate(age_calculated = as.numeric(format(Sys.Date(), "%Y")) - year(mdy(`q102: Can you tell us your date of birth?`))) %>%
        filter(age_calculated != `q101: How old were you on your last birthday? AGE AT LAST BIRTHDAY (IN YEARS)`)
    } 
    # else if(input$filter == "Incomplete Records") 
    #   {
    #   # Replace empty strings with NA only in character columns
    #   df <- df %>%
    #     mutate(across(where(is.character), ~na_if(.x, ""))) %>%
    #     mutate(AnyNA = apply(., 1, function(row) any(is.na(row)))) %>%
    #     filter(`Complete?` == "Complete" & AnyNA) %>%                               #### Change "Complete" or "Yes" as needed
    #     select(-AnyNA)
    # } 
    else if(input$filter == "Incomplete Records") {
      # Create a logical column 'AnyEmptyOrNA' to mark rows with any empty or NA values
      df <- df %>%
        mutate(AnyEmptyOrNA = apply(., 1, function(row) {
          any(is.na(row) | row == "")
        }))
      
      # Filter rows where 'Complete?' is "Yes" but there are empty or NA values in any column
      df <- df %>%
        filter(`Complete?` == "Complete" & AnyEmptyOrNA)
      
      # Optionally, remove the helper column before displaying the data
      df <- df %>%
        select(-AnyEmptyOrNA)
    }
    
    
    
    
    
    
    else if(input$filter == "Date Missing") {
      df <- df %>% filter(is.na(Date))
    } else if(input$filter == "No RDT & DBS Consent") {
      df <- df %>% filter(`q501: CONSENT FOR RDT` != "Yes" | `q504: DRIED BLOOD SAMPLE COLLECTED` != "Yes")
    }
    
    # Selecting only required columns to display
    df %>% select(`Serial no`, State, `RESPONDENTS ADDRESS`, `NAME OF HEALTH FACILITY`, Date, `INTERVIEWER'S NAME`, `FIELD EDITOR`)
  })
  
  # Render the filtered table with DT
  output$filteredTable <- DT::renderDataTable({
    filteredData()
  }, options = list(pageLength = 8, autoWidth = TRUE))
}

# Run the application
shinyApp(ui = ui, server = server)
