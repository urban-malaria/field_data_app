library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

# Load and preprocess the data
data <- read.csv("dummy_longitudinal.csv")
data$DATE <- mdy(data$DATE)  # Assuming the DATE format is MM/DD/YYYY

# Define UI
ui <- fluidPage(
  titlePanel("Interview Trend Analysis"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("state", "Select State", choices = c("Kano", "Oyo")),
      radioButtons("filterType", "Filter By", choices = c("SettlementType" = "SettlementType", "Ward" = "Ward", "Local Government Area" = "LOCAL_GOVT_AREA")),
      selectInput("timePeriod", "Select Time Period", choices = c("Daily", "Weekly", "Monthly"))
    ),
    mainPanel(
      plotOutput("trendGraph")
    )
  )
)

server <- function(input, output) {
  output$trendGraph <- renderPlot({
    # Filter data based on state
    stateData <- data %>% filter(STATE == input$state)
    
    # Determine the aggregation period and add a new column for the floored date
    timePeriod <- reactive({
      switch(input$timePeriod,
             "Daily" = "day",
             "Weekly" = "week",
             "Monthly" = "month")
    })
    
    # Use reactive to prepare the data based on the input
    preparedData <- reactive({
      stateData %>%
        mutate(FlooredDate = floor_date(DATE, unit = timePeriod())) %>%
        group_by_at(vars(input$filterType, FlooredDate)) %>%
        summarize(Interviews = n(), .groups = 'drop')
    })
    
    # Generate the plot
    ggplot(preparedData(), aes_string(x = "FlooredDate", y = "Interviews", color = input$filterType)) +
      geom_line() +
      labs(title = paste("Interview Trends -", input$timePeriod),
           x = "Date",
           y = "Number of Interviews",
           color = input$filterType) +
      theme_minimal()
  })
}






# Run the application
shinyApp(ui = ui, server = server)
