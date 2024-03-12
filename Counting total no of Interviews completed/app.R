


############################   VALUE BOX DATA READ   #####################################

# SHINY DASHBOARD TO DISPLAYS TOTAL INTERVIEWS DONE IN A valueBoxOutput()

########################################################################################

library(shiny)
library(shinydashboard)
library(readr)
library(readxl)

# Simulated data frames representing your local files
# df1 <- data.frame("Serial no" = 1:10)  # Simulating a CSV/XLSX file with 10 interviews
# df2 <- data.frame("Serial no" = 1:15)  # Simulating another file with 15 interviews
# df3 <- data.frame("Serial no" = 1:20)  # And so on...
# df4 <- data.frame("Serial no" = 1:25)
# df5 <- data.frame("Serial no" = 1:30)

# Df1 <- read_excel("Dummy Data HFS.xlsx")
# data <- readxl::read_xlsx(inFile$datapath, col_names = input$header)
# data <- read.csv(inFile$datapath, header = input$header)

interviews1 <- read_csv("E:/Shiny Projects/Dummy Data 2 Feb/dummy_longitudinal.csv")
interviews2 <- read_csv("E:/Shiny Projects/Dummy Data 2 Feb/dummy_men.csv")
interviews3 <- read_excel("E:/Shiny Projects/Dummy Data 2 Feb/Dummy Data - Cross Sectional.xlsx")
interviews4 <- read_excel("E:/Shiny Projects/Dummy Data 2 Feb/Dummy Data HFS.xlsx")
interviews5 <- read_excel("E:/Shiny Projects/Dummy Data 2 Feb/Dummy Data Womens Questionnaire.xlsx")


# Define UI


ui<- dashboardPage(
  dashboardHeader(),  
  dashboardSidebar(),  
  dashboardBody(
    tags$style(".small-box.bg-yellow { background-color: #FFFF00 !important; color: #000000 !important; }"),
    fluidRow(
      valueBoxOutput("box_1"),
      valueBoxOutput("box_2"),
      valueBoxOutput("box_3"),
      valueBoxOutput("box_4"),
      valueBoxOutput("box_5")
    )
  )
)



server <- function(input, output) {
  

  
  # Render the value box with the total number of interviews
  output$box_1 <- renderValueBox({
    totalInterviews <- nrow(interviews1)
    valueBox(
      totalInterviews, "Total Longitudinal Interviews -1 ", icon = icon("users"),
      color = "blue"
    )
  })
  output$box_2 <- renderValueBox({
    totalInterviews <- nrow(interviews2)
    valueBox(
      totalInterviews, "Total Men Interviews 2", icon = icon("users"),
      color = "blue"
    )
  })
  output$box_3 <- renderValueBox({
    totalInterviews <- nrow(interviews3)
    valueBox(
      totalInterviews, "Total Cross Sectional Interviews 3 ", icon = icon("users"),
      color = "blue"
    )
  })
  output$box_4 <- renderValueBox({
    totalInterviews <- nrow(interviews4)
    valueBox(
      totalInterviews, "Total HFS Interviews 4 ", icon = icon("users"),
      color = "blue"
    )
  })
  output$box_5 <- renderValueBox({
    totalInterviews <- nrow(interviews5)
    valueBox(
      totalInterviews, "Total Womens Interviews 5 ", icon = icon("users"),
      color = "blue"
    )
  })
  
  
  
}
# Run the application
shinyApp(ui = ui, server = server)
