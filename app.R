######## Dashboard and Login Librarys
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)

library(readxl)

########### Graphs librarys
#library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)  # Add plotly for interactive plots


################ Map Libraries and dataloading #################################################

#library(shiny)
#library(shinydashboard)
library(ggmap)
library(leaflet)
library(readr)

# Set your Google API key here
register_google(key = " API KEY ")

# Load required columns from the CSV file
data <- read_csv("E:/Shiny Projects/NU Urban Dashboard2/data/dummy_longitudinal.csv", col_types = cols(
  HouseAddress = col_character(),
  Serial_Number = col_character(),
  Child_Name = col_character(),
  Tested = col_character(),
  Result = col_character(),
  AgeAtLastBirthday = col_character(),
  SettlementType = col_character()
))
################################### Data for Graphs #####################################################



data <- read.csv("E:/Shiny Projects/1) Data Collection Tracking Graph/dummy_longitudinal.csv")
data$DATE <- mdy(data$DATE)  # Assuming the DATE format is MM/DD/YYYY

################################### SAMPLE DATA FOR ERRORs in INterviews (performance matrix) #####################################################


# Sample data
interviewer_data <- data.frame(
  Interviewer = c("Interviewer 1", "Interviewer 2", "Interviewer 3", "Interviewer 4", "Interviewer 5"),
  Percentage = c(20, 15, 70, 55, 40)
)




############################   VALUE BOX DATA READ  #########################################


interviews1 <- read_csv("E:/Shiny Projects/Dummy Data 2 Feb/dummy_longitudinal.csv")
interviews2 <- read_csv("E:/Shiny Projects/Dummy Data 2 Feb/dummy_men.csv")
interviews3 <- readxl::read_excel("E:/Shiny Projects/Dummy Data 2 Feb/Dummy Data - Cross Sectional.xlsx")
interviews4 <- read_excel("E:/Shiny Projects/Dummy Data 2 Feb/Dummy Data HFS.xlsx")
interviews5 <- read_excel("E:/Shiny Projects/Dummy Data 2 Feb/Dummy Data Womens Questionnaire.xlsx")



####################################################



##############################################################################################################

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     #tags$code("Username: myuser  Password: mypass"),
                     br(),
                     #    tags$code("Username: myuser1  Password: mypass1")
                   ))
)

############## USERNAME AND PASSWORD FOR LOGIN MODULE ##############################

credentials = data.frame(
  username_id = c("myuser", "k"),
  passod   = sapply(c("mypass", "k"),password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)
###############################################
##############################################################################################################

header <- dashboardHeader( title = "NU Urban Dashboard", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel"), collapsed = TRUE )                      


body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))

ui<-dashboardPage(header, sidebar, body, skin = "blue")





################ SERVER##############################
server <- function(input, output, session) {
  
  
  ################################ tOTAL NO OF INTERVIEWS COMPLETED  ######################       ########################## 
 ## NEW MODE FOLLOWING
  
  # Box 1
  output$box_1 <- shinydashboard::renderValueBox({
    totalInterviews <- nrow(interviews3)
    valueBox(
      totalInterviews, "Completed Cross Sectional Survey", icon = icon("users"),
      color = "light-blue"
    )
  })
  
  # Box 2
  output$box_2 <- renderValueBox({
    
    totalInterviews <- nrow(interviews2)
    valueBox(
      totalInterviews, "Completed Men Survey", icon = icon("users"),
      color = "light-blue"
    )
  })
  
  # Box 3
  output$box_3 <- renderValueBox({
   
    totalInterviews <- nrow(interviews5)
    valueBox(
      totalInterviews, "Completed Women Survey", icon = icon("users"),
      color = "light-blue"
    )
  })
  
  # Box 4
  output$box_4 <- renderValueBox({
    totalInterviews <- nrow(interviews4)
    valueBox(
      totalInterviews, "Completed Health Facility Survey", icon = icon("users"),
      color = "light-blue"
    )
  })
  
  # Box 5
  output$box_5 <- renderValueBox({
    
    totalInterviews <- nrow(interviews1)
    valueBox(
      totalInterviews, "Completed Longitudinal  Survey", icon = icon("users"),
      color = "light-blue"
    )
    
    
  })
  ############################################################################################################
  # Box 6   ## in Error Tracking Menu
  output$box_6 <- renderValueBox({
    valueBox(22, "Error Tracking Readout -1 ", color = "light-blue"
    )
  })
  
  # Box 7  ## in Error Tracking Menu
  output$box_7 <- renderValueBox({
    valueBox(16, "Error Tracking Readout -2", color = "light-blue"
    )
  })
  ################################################################################ Performance matrix   ######
  # Box 8  ## in Performance Matrix
  output$box_8 <- renderValueBox({
    valueBox(50, "Percentage of Errors by Interviewer 1", color = "light-blue"
    )
  })
  
  # Box 9  ## in Performance Matrix
  output$box_9 <- renderValueBox({
    valueBox(69, "Percentage of Errors by Interviewer 2", color = "light-blue"
    )
  })
  
  # Box 10  ## in Performance Matrix
  output$box_10 <- renderValueBox({
    valueBox(75, "Percentage of Errors by Interviewer 3", color = "light-blue"
    )
  })
  
  # Box 11  ## in Performance Matrix
  output$box_11 <- renderValueBox({
    valueBox(40, "Percentage of Errors by Interviewer 4", color = "light-blue"
    )
  })
  
  # Box 12  ## in Performance Matrix
  output$box_12 <- renderValueBox({
    valueBox(71, "Percentage of Errors by Interviewer 5", color = "light-blue"
    )
  })
  
  #####################################################################################
  
  
  ##################################LOGIN LOGIC######################################################
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({                                            ############ Side BAR############  
    if (USER$login == TRUE ){     
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Error Tracking", tabName = "second", icon = icon("th")),
        menuItem("Performance Matrix", tabName = "third", icon = icon("th")),
        menuItem("MAP", tabName = "forth", icon = icon("th")),
        
        ############ ############ ############ ############ Side BAR for GRAPH############ ############ ############ 
        
        radioButtons("state", "Select State", choices = c("Kano", "Oyo")),
        radioButtons("filterType", "Filter By", choices = c("SettlementType" = "SettlementType", "Ward" = "Ward", "Local Government Area" = "LOCAL_GOVT_AREA")),
        selectInput("timePeriod", "Select Time Period", choices = c("Daily", "Weekly", "Monthly", "Total"))
        
        
        
      )
    }
      })
  
  
  ########################################## MAP MODULE server logic ##########################################################
  
  
  # Reactive expression for processing and geocoding data
  locations <- reactive({
    geocoded <- lapply(data$HouseAddress, function(address) {
      Sys.sleep(0.1)  # To prevent hitting the rate limit
      tryCatch({
        geocode(address, output = "latlon")
      }, error = function(e) {
        return(data.frame(lat = NA, lon = NA))  # Return NA coordinates on error
      })
    })
    coords <- do.call(rbind, geocoded)  # Combine all geocoded locations into one data frame
    combined_data <- cbind(data, coords)  # Add latitude and longitude to the original data
    combined_data  # Return the combined data
  })
  
  # Render the Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)  # Initialize the map with a base layer
  })
  
  # Observe changes in the 'locations' reactive expression and update the map
  observe({
    locs <- locations()
    if (!is.null(locs) && nrow(locs) > 0) {
      leafletProxy("map") %>%
        clearMarkers() %>%
        
        addCircleMarkers(data = locs[locs$Result == "Positive", ],
                         ~lon, ~lat, color = "red", group = "Positive", stroke = FALSE, radius = 7,
                         popup = ~paste("<b>", "Serial Number: ","</b>", Serial_Number, "<br>",
                                        "<b>", "Child Name: ","</b>", Child_Name, "<br>",
                                        "<b>","Tested: ","</b>", Tested, "<br>",
                                        "<b>","Result: ","</b>", Result, "<br>",
                                        "<b>","Age At Last Birthday: ","</b>", AgeAtLastBirthday, "<br>",
                                        "<b>", "Settlement Type: ","</b>", SettlementType,
                                        sep = "")) %>%
        
        addCircleMarkers(data = locs[locs$Result == "Negative", ],
                         ~lon, ~lat, color = "blue", group = "Negative", stroke = FALSE, radius = 7,
                         popup = ~paste("<b>", "Serial Number: ","</b>", Serial_Number, "<br>",
                                        "<b>", "Child Name: ","</b>", Child_Name, "<br>",
                                        "<b>","Tested: ","</b>", Tested, "<br>",
                                        "<b>","Result: ","</b>", Result, "<br>",
                                        "<b>","Age At Last Birthday: ","</b>", AgeAtLastBirthday, "<br>",
                                        "<b>", "Settlement Type: ","</b>", SettlementType,
                                        sep = "")) %>%
        
        #   popup = ~paste(Serial_Number, Child_Name, Tested, Result, AgeAtLastBirthday, SettlementType, sep = "<br>")) %>%
        
        
        addLayersControl(overlayGroups = c("Positive", "Negative"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        
        addLegend(position = "bottomleft", colors = c("red", "blue"), labels = c("Positive", "Negative"), title = "Test Result") %>%
        
        
        addControl(html = '<div style="background-color: white; padding: 5px;">
                          <strong>City :</strong><br>
                           <select id="cityName" onchange="changeCity()">
                            <option value="ibadan">Ibadan</option>
                           <option value="kano">Kano</option>
                            </select></div>', position = "topright")
    }
  })
  
  
  
  # React to city name changes and update map view
  observeEvent(input$cityName, {
    if (input$cityName == "ibadan") {
      leafletProxy("map") %>%
        setView(lng = 3.92, lat = 7.38, zoom = 12)
    } else if (input$cityName == "kano") {
      leafletProxy("map") %>%
        setView(lng = 8.53, lat = 11.98, zoom = 12)
    }
  })
  
  
  
  ####################################END OF MAP MODULE  ############################################################
  
  
  
  
  
  
  
  #####################Side Menu bar and BODY CONT####################################### TO BE DISPLAYED 
  
  # Actions after clicking menuItem on sidebar
  output$body <- renderUI({
    if (USER$login == TRUE ) 
      {
      tabItems(
        
                                                                      # First tab
        tabItem(tabName ="dashboard", class = "active",
                
              ############################ ########################     # NEW MODE FOLLOWING UI ###########################
                fluidRow(
                 
                  
                  # Value Box 1
                   valueBoxOutput(outputId = "box_1", width = 2),
                  
                  # Value Box 2
                  valueBoxOutput(outputId = "box_2", width = 2),
                  
                  # Value Box 3
                  valueBoxOutput(outputId = "box_3", width = 2),
                  
                  # Value Box 4
                  valueBoxOutput(outputId = "box_4", width = 2),
                  
                  # Value Box 5
                  valueBoxOutput(outputId = "box_5", width = 2)
                  
                  ########################################################################################################
                ),
                  
              fluidRow(
                column(3, 
                plotlyOutput("trendGraph_1", width = "500px", height = "400px"),  # Change to plotlyOutput for interactive plots
                plotlyOutput("trendGraph_2", width = "500px", height = "400px"),  # Change to plotlyOutput for interactive plots 
                
                plotlyOutput("trendGraph_3", width = "500px", height = "400px"),  # Change to plotlyOutput for interactive plots
                ),
                br(),
                hr(),
                
                fluidRow(
                column(2, 
                plotlyOutput("trendGraph_4", width = "500px", height = "400px"),  # Change to plotlyOutput for interactive plots  
              plotlyOutput("trendGraph_5", width = "500px", height = "400px")  # Change to plotlyOutput for interactive plots
                )
                  )
              ),
                  
                  
                  
                
              br(),
              hr(),
              
              
 ############# TABLE ###############################################
                 fluidRow(
                  box(width = 8, dataTableOutput('results'))
                )),
        
 ####################################### Error Tracking UI  ########################### 
                                                                                    # Second tab                    
        tabItem(tabName = "second",
                
                fluidRow(
                  
                  # Value Box 6 
                  valueBoxOutput(outputId = "box_6", width = 2),
                  
                  # Value Box 7
                  valueBoxOutput(outputId = "box_7", width = 2)
                  
      ##################################################################################
                  
                ),
                br(),
                hr(),
                
                
                fluidRow(
                  box(width = 12, dataTableOutput('results2'))
                )
        ),
        tabItem(tabName = "third",
                
                fluidRow(
                  
                  
                  # Value Box 8       ####################################### Performance Matrix UI   ###########################  
                  valueBoxOutput(outputId = "box_8", width = 2),
                  
                  # Value Box 9
                  valueBoxOutput(outputId = "box_9", width = 2),
                  
                  # Value Box 10
                  valueBoxOutput(outputId = "box_10", width = 2),
                  
                  # Value Box 11
                  valueBoxOutput(outputId = "box_11", width = 2),
                  
                  # Value Box 12
                  valueBoxOutput(outputId = "box_12", width = 2)
                  
                  #######################################################################################################
                  
                ),
                br(),
                hr(),
                
 ##################################################### # Performance Matix Graphs UI ###########################
                fluidRow(
                  
                  
                             column(4, 
                                    plotlyOutput("errorBarPlot_1", width = "550px", height = "400px")
                             ),
                             
                             column(4,
                                    plotlyOutput("errorBarPlot_2", width = "550px", height = "400px")
                             ),
                             
                             column(4,
                                    plotlyOutput("errorBarPlot_3", width = "550px", height = "400px")
                             )
                        
                  ) 
 #######################################################################################################
 
 ),
        
        # MAP TAB                            ###################### MAP TAb     ###########################             
        tabItem(tabName = "forth",
                
                fluidRow(
                  
                  leafletOutput("map")
                ),
                br(),
                hr(),
                
                
                fluidRow(
                  
                  ## Add ITEm
                  
                )
        )###########################################################################
        
        )}
    else {
      loginpage
    }
  })
  
  ###################################### BODY TEMP#########################
  
 #   output$results <-  DT::renderDataTable({
 #     datatable(iris, options = list(autoWidth = TRUE,
 #                                   searching = FALSE))
 # })
 #  
 #  output$results2 <-  DT::renderDataTable({
 #    datatable(mtcars, options = list(autoWidth = TRUE,
 #                                     searching = FALSE))
 #  })
  
  
  
  
  ###################################### Performance Matix Graphs Server Part ###############################################################
  
  output$errorBarPlot_1 <- renderPlotly({
    p <- ggplot(interviewer_data, aes(x=Interviewer, y=Percentage, fill=Interviewer, text=paste("Percentage: ", Percentage, "%"))) +
      geom_bar(stat="identity") +
      scale_fill_brewer(palette="Spectral") + # Colorful bars
      scale_x_discrete(breaks=c( )) + # Only show labels for Interviewer 1 and 2
      theme_minimal() +
      labs(y="Total errors by an Interviewer", x="", title=" ") + # Remove x-axis label
      coord_flip() # Flips the axes for a horizontal bar graph
    
    ggplotly(p, tooltip="text") # Convert to plotly and set tooltip
  })
  
  output$errorBarPlot_2 <- renderPlotly({
    p <- ggplot(interviewer_data, aes(x=Interviewer, y=Percentage, fill=Interviewer, text=paste("Percentage: ", Percentage, "%"))) +
      geom_bar(stat="identity") +
      scale_fill_brewer(palette="Spectral") + # Colorful bars
      scale_x_discrete(breaks=c( )) + # Only show labels for Interviewer 1 and 2
      theme_minimal() +
      labs(y="Total interviews conducted by interviewer", x="", title=" ") + # Remove x-axis label
      coord_flip() # Flips the axes for a horizontal bar graph
    
    ggplotly(p, tooltip="text") # Convert to plotly and set tooltip
  })
  
  output$errorBarPlot_3 <- renderPlotly({
    p <- ggplot(interviewer_data, aes(x=Interviewer, y=Percentage, fill=Interviewer, text=paste("Percentage: ", Percentage, "%"))) +
      geom_bar(stat="identity") +
      scale_fill_brewer(palette="Spectral") + # Colorful bars
      scale_x_discrete(breaks=c( )) + # Only show labels for Interviewer 1 and 2
      theme_minimal() +
      labs(y="Percentage of Errors by Interviewers", x="", title=" ") + # Remove x-axis label
      coord_flip() # Flips the axes for a horizontal bar graph
    
    ggplotly(p, tooltip="text") # Convert to plotly and set tooltip
  })
  
  
  ###################################### TREND GRAPH 1 SERVER LOGIC ############################################################  ###################################### ############################################################

  
  
  output$trendGraph_1 <- renderPlotly({
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
        labs(title = paste("Men Survey Data   -", input$timePeriod), x = "Time Period", y = "Number of Interviews")
      
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
  
  
  ###################################### TREND GRAPH 2 ############################################################  ###################################### ############################################################
  
  
  output$trendGraph_2 <- renderPlotly({
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
        labs(title = paste("Women Survey Data  -", input$timePeriod), x = "Time Period", y = "Number of Interviews")
      
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
  
  
  ###################################### TREND GRAPH 3  ############################################################  ###################################### ############################################################
  
  
  output$trendGraph_3 <- renderPlotly({
    stateData <- data %>% filter(STATE == input$state)
    
    if (input$timePeriod == "Total") {
      aggregatedData <- stateData %>%
        group_by_at(vars(input$filterType)) %>%
        summarize(Interviews = n(), .groups = 'drop')
      
      p <- ggplot(aggregatedData, aes_string(x = input$filterType, y = "Interviews", fill = input$filterType)) +
        geom_col() +
        labs(title = "Household Survey Data ", x = input$filterType, y = "Number of Interviews") +
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
  
  
  ###################################### TREND GRAPH 4 ############################################################  ###################################### ############################################################
  
  output$trendGraph_4 <- renderPlotly({
    stateData <- data %>% filter(STATE == input$state)
    
    if (input$timePeriod == "Total") {
      aggregatedData <- stateData %>%
        group_by_at(vars(input$filterType)) %>%
        summarize(Interviews = n(), .groups = 'drop')
      
      p <- ggplot(aggregatedData, aes_string(x = input$filterType, y = "Interviews", fill = input$filterType)) +
        geom_col() +
        labs(title = "Health Facility Survey Data ", x = input$filterType, y = "Number of Interviews") +
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
  
  
  ###################################### TREND GRAPH 5 ############################################################  ###################################### ############################################################
  
  
  output$trendGraph_5 <- renderPlotly({
    stateData <- data %>% filter(STATE == input$state)
    
    if (input$timePeriod == "Total") {
      aggregatedData <- stateData %>%
        group_by_at(vars(input$filterType)) %>%
        summarize(Interviews = n(), .groups = 'drop')
      
      p <- ggplot(aggregatedData, aes_string(x = input$filterType, y = "Interviews", fill = input$filterType)) +
        geom_col() +
        labs(title = "Longitudinal Survey Data ", x = input$filterType, y = "Number of Interviews") +
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
  
  
  ###################################### TREND GRAPH  ############################################################  ###################################### ############################################################
  
  
  
  
  
  
  
  
  
  ###################################### ############################################################
  
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)