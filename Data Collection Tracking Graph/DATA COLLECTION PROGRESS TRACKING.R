# 
# 
# DATA COLLECTION PROGRESS TRACKING WITH PLOTLY.JS GRAPHS WITH FILTERS INCLUDING 
# 
# CONTROL FILTERS ARE AS PER FOLLOWING 
#
# STATE Selection       KANO & OYO                
# FILTER BY             Settlement_Type, Ward, Local Government Area
# TIME PERIOD           Daily, Weekly, Monthly, TOTAL
#



library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)  # Add plotly for interactive plots

data <- read.csv("dummy_longitudinal.csv")
data$DATE <- mdy(data$DATE)  # Assuming the DATE format is MM/DD/YYYY

# Define UI
ui <- fluidPage(
  titlePanel("Interview Trend Analysis"),
  sidebarLayout(
    sidebarPanel( width = 3,
      radioButtons("state", "Select State", choices = c("Kano", "Oyo")),
      radioButtons("filterType", "Filter By", choices = c("SettlementType" = "SettlementType", "Ward" = "Ward", "Local Government Area" = "LOCAL_GOVT_AREA")),
      selectInput("timePeriod", "Select Time Period", choices = c("Daily", "Weekly", "Monthly", "Total"))
    ),
    mainPanel(
      plotlyOutput("trendGraph", width = "700px", height = "500px")  # Change to plotlyOutput for interactive plots
    )
  )
)

server <- function(input, output) {
  output$trendGraph <- renderPlotly({
    stateData <- data %>% filter(STATE == input$state)
    
    if (input$timePeriod == "Total") {
      aggregatedData <- stateData %>%
        group_by_at(vars(input$filterType)) %>%
        summarize(Interviews = n(), .groups = 'drop')
      
      p <- ggplot(aggregatedData, aes_string(x = input$filterType, y = "Interviews", fill = input$filterType)) +
        geom_col() +
        labs(title = "Total Interviews", x = input$filterType, y = "Number of Interviews") +
        theme_minimal()
      
      ggplotly(p)
    } else {
      timePeriod <- switch(input$timePeriod, "Daily" = "day", "Weekly" = "week", "Monthly" = "month")
      
      preparedData <- reactive({
        stateData %>%
          mutate(FlooredDate = floor_date(DATE, unit = timePeriod)) %>%
          group_by_at(vars(input$filterType, FlooredDate)) %>%
          summarize(Interviews = n(), .groups = 'drop')
      })
     
      
      # Use preparedData() to access the data within the reactive expression
      p <- ggplot(preparedData(), aes_string(x = "FlooredDate", y = "Interviews", color = input$filterType)) +
        geom_line() +
        labs(title = paste("Interview Trends -", input$timePeriod), x = "Time Period", y = "Number of Interviews")
      
      # Adjust x-axis labels based on the selected time period
      if(input$timePeriod == "Daily") {
        p <- p + scale_x_date(date_labels = "%m-%d")
      } else if(input$timePeriod == "Weekly") {
        p <- p + scale_x_date(date_labels = "%Y-%U")
      } else if(input$timePeriod == "Monthly") {
        p <- p + scale_x_date(date_labels = "%Y-%m")
      }
      
      p <- p + theme_minimal()
      
      ggplotly(p)
      
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)
