#
# 
#
#
# NU URBAN DATA COLLECTION PROGESS TRACKING DATA FROM DROPBOX
#
# This Shiny Dashboard track's progress made in 5 survey 
# 
#
#

library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)  # Add plotly for interactive plots
library(readr)
library(readxl)
library(shinyjqui)

library(httr)
library(readr)

################################### Data for Graphs #####################################################

# Define UI for application that draws a histogram
 ui <- dashboardPage(
   dashboardHeader(title = "NU URBAN STATUS TRACKING"),
   dashboardSidebar(
     sidebarMenu(
       #   menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
       #   menuItem("Error Tracking", tabName = "second", icon = icon("th")),
       #   menuItem("Performance Matrix", tabName = "third", icon = icon("th")),
       #  menuItem("MAP", tabName = "forth", icon = icon("th")),

       ############ ############ ############ ############ Side BAR for GRAPH############ ############ ############

     radioButtons("state", "Select State", choices = c("Kano", "Ibadan")),
      radioButtons("filterType", "Filter By", choices = c("SettlementType" = "SETTLEMENT.TYPE", "Ward" = "WARD", "Local Government Area" = "LOCAL.GOVT..AREA")),
       selectInput("timePeriod", "Select Time Period", choices = c("Daily", "Weekly", "Monthly", "Total"))

     )
   ),
   dashboardBody(
     # Boxes need to be put in a row (or column)
     fluidRow(
       box(
      #   width = 6,
         title = "Men Survey",
         status = "primary",
         solidHeader = TRUE,
         collapsible = TRUE,
         jqui_resizable(
         plotlyOutput("trendGraph_1", width = "500", height = "400px")  # Change to plotlyOutput for interactive plots
         )

       ),

       box(
     #    width = 6,
         title = "Women Survey",
         status = "primary",
         solidHeader = TRUE,
         collapsible = TRUE,
         jqui_resizable(
         plotlyOutput("trendGraph_2", width = "500", height = "400px")  # Change to plotlyOutput for interactive plots
       ))
     ),
     fluidRow(
       box(
       #  width = 6,
         title = " House Hold Survey",
         status = "primary",
         solidHeader = TRUE,
         collapsible = TRUE,
         jqui_resizable(
         plotlyOutput("trendGraph_3", width = "500", height = "400px")  # Change to plotlyOutput for interactive plots
         )
       ),

       box(
         #width = 6,
         title = " Health Facility Survey",
         status = "primary",
         solidHeader = TRUE,
         collapsible = TRUE,
         jqui_resizable(
         plotlyOutput("trendGraph_4", width = "500", height = "400px")  # Change to plotlyOutput for interactive plots
         )

       )
     ),
     fluidRow(
       box(

         title = "Longitudinal Survey",
         status = "primary",
         solidHeader = TRUE,
         collapsible = TRUE,
         jqui_resizable(
         plotlyOutput("trendGraph_5", width = "500", height = "400px")  # Change to plotlyOutput for interactive plots
         )
       )

     )
   )
 )


server <- function(input, output) {
  
  ##############################################  ACCESSING DROP BOX DATA ##############################################################
  
  
  ####################### # Longitude data ####################### 
  
  dataFrame1 <- reactive({
    url <- "https://www.dropbox.com/scl/fi/dh80t1qsfxpkciljyn4w5/shinny_long.csv?rlkey=2lhpe7opszvb9c587eza4unma&dl=1"  ### Eniola DROPBOX URL HERE
  
      resp <- httr::GET(url)
    if(httr::status_code(resp) == 200) {
      df <- read_csv(httr::content(resp, "text", encoding = "UTF-8"),na = c("", "NA", "na"))
      
      df$DATE <- lubridate::ymd(df$DATE)  # Correctly use lubridate within the reactive expression
     # df$DATE <- lubridate::mdy(df$DATE)
      
      return(df)
    } else {
      return(data.frame(Error = "Failed to download"))
    }
  })
  
  ####################### # Health Facility Survey data####################### 
  
  dataFrame2 <- reactive({
 #   url <- "https://www.dropbox.com/scl/fi/3ow96hvtp9s05li316yfa/shinny_hf.csv?rlkey=wlfhjfg3vhkgax7ccegs3b5c0&dl=0" # new data from Eniola
 
    url <- "https://www.dropbox.com/scl/fi/d06vnku1q2vp1py4l01q1/shinny_hf_mod.csv?rlkey=b3ghv04mftivni6qx4icg2kwc&dl=1"    # Eniola Link modified
    
    resp <- httr::GET(url)
    if(httr::status_code(resp) == 200) {
      df <- read_csv(httr::content(resp, "text", encoding = "UTF-8"),na = c("", "NA", "na"))
  #    df$DATE <- lubridate::ymd(df$DATE)       # Correctly use lubridate within the reactive expression
       df$DATE <- lubridate::mdy(df$DATE)
      
   
      return(df)
    } else {
      return(data.frame(Error = "Failed to download"))
    }
  })
  
  ####################### #House Hold Survey #######################         
  
  dataFrame3 <- reactive({
    
   url <- "https://www.dropbox.com/scl/fi/wv1t7iy3mwq86seby3iml/shiny_hh_1203_rushi.csv?rlkey=1cp0vb6pbvvsym9jqjxsb2mnh&dl=1"   ### Modified dataset from Eniola
 #   url <- "https://www.dropbox.com/scl/fi/shbnds84f12c976igtfcs/shiny_hh_1203.csv?rlkey=x19s5x7nybinfv3yktih9mqr4&dl=1" # Link from Eniola
   
    
   
    # GET request
    resp <- httr::GET(url)
    # Ensure that resp is not NULL and a status code is available
    if (!is.null(resp) && !is.na(httr::status_code(resp)) && httr::status_code(resp) == 200) {
      # Read CSV content
      df <- read_csv(httr::content(resp, "text", encoding = "UTF-8"))
      
      # Convert DATE column to Date format
      df$DATE <- lubridate::mdy(df$DATE)     # set the data format as per the dataset
      
      # Filter to keep only the first row for each unique serial number
      df_filtered <- df %>%
        group_by(Serial.Number) %>%   # Serial.Number column Name 
        slice(1) %>%
        ungroup()
      
      return(df_filtered)
    } else {
      # Provide a more informative error or fallback
      return(data.frame(Error = "Failed to download or invalid response"))
    }
  })
  
  
  ####################### ##Men Survey ####################### 
  
  dataFrame4 <- reactive({
  #  url <- "https://www.dropbox.com/scl/fi/y4dmunwwrp38itw5r5pwr/dummy_men.csv?rlkey=6rd4rekzw5kn9t79mwtayauam&dl=1" ### DROPBOX URL HERE
    url <- "https://www.dropbox.com/scl/fi/qh1co36bfnb8n5yp3dcjs/shinny_men.csv?rlkey=semiy8ureuc8yqlkm1x3sjlgu&dl=1"  # Link for Eniola
    resp <- httr::GET(url)
    if(httr::status_code(resp) == 200) {
      df <- read_csv(httr::content(resp, "text", encoding = "UTF-8"))
      df$DATE <- lubridate::ymd(df$DATE)  # Correctly use lubridate within the reactive expression
      return(df)
    } else {
      return(data.frame(Error = "Failed to download"))
    }
  })
  
  ####################### ##Women Survey ####################### 
  
  dataFrame5 <- reactive({
   # url <- "https://www.dropbox.com/scl/fi/1w4lw8ae58n0ow988y0z0/dummy_women.csv?rlkey=nbobek3yyk9f1sugof1ikhyf6&dl=1"  ### DROPBOX URL HERE
    url <- "https://www.dropbox.com/scl/fi/a644mf60dulkpah21o984/shinny_women.csv?rlkey=1e1yuy6r2meje9248ozs6hafy&dl=1"
   
    resp <- httr::GET(url)
    if(httr::status_code(resp) == 200) {
      df <- read_csv(httr::content(resp, "text", encoding = "UTF-8"))
      df$DATE <- lubridate::ymd(df$DATE)  # Correctly use lubridate within the reactive expression
      
      # Filter to keep only the first row for each unique serial number
      df_filtered <- df %>%
        group_by(Serial.Number) %>%     # Serial.Number column Name 
        slice(1) %>%
        ungroup()
      
      return(df_filtered)
    } else {
      return(data.frame(Error = "Failed to download"))
    }
    
    head(data)
  })
  
  
   
  ######################################  MEN Survey Graph ############################################################
  
  
  
  output$trendGraph_1 <- renderPlotly({
    stateData <- dataFrame4() %>% filter(State == input$state)
    
    if (input$timePeriod == "Total") {
      aggregatedData <- stateData %>%
        group_by_at(vars(input$filterType)) %>%
        summarize(Interviews = n(), .groups = 'drop')
      
      p <- ggplot(aggregatedData, aes_string(x = input$filterType, y = "Interviews", fill = input$filterType)) +
        geom_col() +
        labs(title = " ", x = input$filterType, y = "Number of Interviews") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))  # Rotate x-axis labels
      
      ggplotly(p)
    } else {
      timePeriod <- switch(input$timePeriod, "Daily" = "day", "Weekly" = "week", "Monthly" = "month")
      
      preparedData <- reactive({
        stateData %>%
          mutate(FlooredDate = floor_date(DATE, unit = timePeriod)) %>%     # DATE column Name 
          group_by_at(vars(input$filterType, FlooredDate)) %>%
          summarize(Interviews = n(), .groups = 'drop')
      })
      
      p <- ggplot(preparedData(), aes_string(x = "FlooredDate", y = "Interviews", color = input$filterType)) +
        geom_line() +
        labs(title = paste("  ", input$timePeriod), x = "Time Period", y = "Number of Interviews") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))  # Rotate x-axis labels for line plot as well
      
      # Adjust x-axis labels based on the selected time period
      if(input$timePeriod == "Daily") {
        p <- p + scale_x_date(date_labels = "%m-%d")
      } else if(input$timePeriod == "Weekly") {
        p <- p + scale_x_date(date_labels = "%Y-%U")
      } else if(input$timePeriod == "Monthly") {
        p <- p + scale_x_date(date_labels = "%Y-%m")
      }
      
      ggplotly(p)
    }
  })
  
  
    ###################################### Women Survey Graph ############################################################
  
  
  output$trendGraph_2 <- renderPlotly({
    stateData <- dataFrame5() %>% filter(State == input$state)
    
    if (input$timePeriod == "Total") {
      aggregatedData <- stateData %>%
        group_by_at(vars(input$filterType)) %>%
        summarize(Interviews = n(), .groups = 'drop')
      
      p <- ggplot(aggregatedData, aes_string(x = input$filterType, y = "Interviews", fill = input$filterType)) +
        geom_col() +
        labs(title = " ", x = input$filterType, y = "Number of Interviews") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))  # Rotate x-axis labels
      
      ggplotly(p)
    } else {
      timePeriod <- switch(input$timePeriod, "Daily" = "day", "Weekly" = "week", "Monthly" = "month")
      
      preparedData <- reactive({
        stateData %>%
          mutate(FlooredDate = floor_date(DATE, unit = timePeriod)) %>%
          group_by_at(vars(input$filterType, FlooredDate)) %>%
          summarize(Interviews = n(), .groups = 'drop')
      })
      
      p <- ggplot(preparedData(), aes_string(x = "FlooredDate", y = "Interviews", color = input$filterType)) +
        geom_line() +
        labs(title = paste("  ", input$timePeriod), x = "Time Period", y = "Number of Interviews") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))  # Rotate x-axis labels
      
      # Adjust x-axis labels based on the selected time period
      if(input$timePeriod == "Daily") {
        p <- p + scale_x_date(date_labels = "%m-%d")
      } else if(input$timePeriod == "Weekly") {
        p <- p + scale_x_date(date_labels = "%Y-%U")
      } else if(input$timePeriod == "Monthly") {
        p <- p + scale_x_date(date_labels = "%Y-%m")
      }
      
      ggplotly(p)
    }
  })
  
  
  
  ###################################### House Hold Survey Graph ############################################################  
  
  
  output$trendGraph_3 <- renderPlotly({
    
 #   stateData <- dataFrame5() %>% filter(State == input$state)  #women dataframe
    stateData <-  dataFrame3() %>% filter(STATE == input$state)
      
      
      if (input$timePeriod == "Total") {
        aggregatedData <- stateData %>%
          group_by_at(vars(input$filterType)) %>%
          summarize(Interviews = n(), .groups = 'drop')
        
        p <- ggplot(aggregatedData, aes_string(x = input$filterType, y = "Interviews", fill = input$filterType)) +
          geom_col() +
          labs(title = "  ", x = input$filterType, y = "Number of Interviews") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))  # Rotate x-axis labels
        
        ggplotly(p)
      } else {
        timePeriod <- switch(input$timePeriod, "Daily" = "day", "Weekly" = "week", "Monthly" = "month")
        
        preparedData <- reactive({
          stateData %>%
            mutate(FlooredDate = floor_date(DATE, unit = timePeriod)) %>%         #### DATE name should be as per the coloum name in Dataset
            group_by_at(vars(input$filterType, FlooredDate)) %>%
            summarize(Interviews = n(), .groups = 'drop')
        })
        
        p <- ggplot(preparedData(), aes_string(x = "FlooredDate", y = "Interviews", color = input$filterType)) +
          geom_line() +
          labs(title = paste(" ", input$timePeriod), x = "Time Period", y = "Number of Interviews") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))  # Rotate x-axis labels for line plot as well
        
        # Adjust x-axis labels based on the selected time period
        if(input$timePeriod == "Daily") {
          p <- p + scale_x_date(date_labels = "%m-%d")
        } else if(input$timePeriod == "Weekly") {
          p <- p + scale_x_date(date_labels = "%Y-%U")
        } else if(input$timePeriod == "Monthly") {
          p <- p + scale_x_date(date_labels = "%Y-%m")
        }
        
        ggplotly(p)
      }
    })
  
  ###################################### HEALTH FACILITY GRAPH  ############################################################  
  
  
  
  output$trendGraph_4 <- renderPlotly({
    stateData <- dataFrame2() %>% filter(State == input$state)
    
    aggregatedData <- reactive({
      # Check if the time period is "Total"
      if (input$timePeriod == "Total") {
        # Aggregate data by "NAME.OF.HEALTH.FACILITY"
        stateData %>%
          group_by(`NAME.OF.HEALTH.FACILITY`) %>%
          summarize(Interviews = n(), .groups = 'drop')
      } else {
        # Handle other time periods
        timePeriod <- switch(input$timePeriod, "Daily" = "day", "Weekly" = "week", "Monthly" = "month")
        stateData %>%
          mutate(FlooredDate = floor_date(as.Date(DATE), unit = timePeriod)) %>%   #### DATE name should be as per the coloum name in Dataset (as.Date(DATE)
          group_by(`NAME.OF.HEALTH.FACILITY`, FlooredDate) %>%
          summarize(Interviews = n(), .groups = 'drop')
      }
    })
    
    # Conditional plot generation
    if (input$timePeriod == "Total") {
      # Use the reactive 'aggregatedData' for bar plot when time period is "Total"
      p <- ggplot(aggregatedData(), aes(x = `NAME.OF.HEALTH.FACILITY`, y = Interviews, fill = `NAME.OF.HEALTH.FACILITY`)) +
        geom_col() +
        labs(title = " ", x = "Name of Health Facility", y = "Number of Interviews")  +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) # Rotate x-axis labels
    } else {
      # Use the reactive 'aggregatedData' for line plot for other time periods
      p <- ggplot(aggregatedData(), aes(x = FlooredDate, y = Interviews, color = `NAME.OF.HEALTH.FACILITY`)) +
        geom_line() +
        labs(title = paste(" ", input$timePeriod), x = "Date", y = "Number of Interviews")  +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) # Rotate x-axis labels
      
        scale_x_date(date_labels = switch(input$timePeriod,
                                          "Daily" = "%m-%d",
                                          "Weekly" = "%Y-%U",
                                          "Monthly" = "%Y-%m"))
    }
    
    ggplotly(p)
  })
  
  
  
  ###################################### TREND GRAPH 5  Longitudinal Survey Data ############################################################  ###################################### ############################################################


  output$trendGraph_5 <- renderPlotly({
    stateData <- dataFrame1() %>% filter(State == input$state)
    
    if (input$timePeriod == "Total") {
      aggregatedData <- stateData %>%
        group_by(!!sym(input$filterType)) %>%
        summarize(Interviews = n(), .groups = 'drop')
      
      p <- ggplot(aggregatedData, aes(x = !!sym(input$filterType), y = Interviews, fill = !!sym(input$filterType))) +
        geom_col() +
        labs(title = " ", x = input$filterType, y = "Number of Interviews") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) # Rotate x-axis labels
      
      ggplotly(p)
    } else {
      timePeriod <- switch(input$timePeriod, "Daily" = "day", "Weekly" = "week", "Monthly" = "month")
      
      preparedData <- reactive({
        stateData %>%
          mutate(FlooredDate = floor_date(DATE, unit = timePeriod)) %>%
          group_by(!!sym(input$filterType), FlooredDate) %>%
          summarize(Interviews = n(), .groups = 'drop')
      })
      
      # Use preparedData() to access the data within the reactive expression
      p <- ggplot(preparedData(), aes(x = FlooredDate, y = Interviews, color = !!sym(input$filterType))) +
        geom_line() +
        labs(title = paste(" ", input$timePeriod), x = "Time Period", y = "Number of Interviews") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) # Rotate x-axis labels
      
      # Adjust x-axis labels based on the selected time period
      if(input$timePeriod == "Daily") {
        p <- p + scale_x_date(date_labels = "%m-%d")
      } else if(input$timePeriod == "Weekly") {
        p <- p + scale_x_date(date_labels = "%Y-%U")
      } else if(input$timePeriod == "Monthly") {
        p <- p + scale_x_date(date_labels = "%Y-%m")
      }
      
      ggplotly(p)
    }
  })
  
  # SERVER END  
}

# Run the application 
shinyApp(ui = ui, server = server)
