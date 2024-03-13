###########################################

# MAP MODULE FOR LOGITUDINAL SURVEY,
#
#  IT PLOTS POSITIVE AND NEGATIVE RESULTS ON MAP
#
# THIS MODULES READS STREET ADDRESS FROM CSV FILE AND PLOT IT ON OPENSTREAT MAP
#
# USER CAN USE FILTER TO SELECT CITY/STATE AND SHOW POSITIVE AND NEGATIVE CASES

# MARKER ON MAP SHOWS DETAILS RELATED TO THE CASE 

#############################################

library(shiny)
library(shinydashboard)
library(ggmap)
library(leaflet)
library(readr)

# Set your Google API key here
#
#

register_google(key = "  ")

# Load required columns from the CSV file
#
#
#
data <- read_csv("dummy_longitudinal.csv", col_types = cols(
  HouseAddress = col_character(),
  Serial_Number = col_character(),
  Child_Name = col_character(),
  Tested = col_character(),
  Result = col_character(),
  AgeAtLastBirthday = col_character(),
  SettlementType = col_character()
))


# UI
ui <- dashboardPage(
  dashboardHeader(title = "Interactive Bubble Map"),
  dashboardSidebar(
    # Sidebar can be left empty or include other UI elements if needed
  ),
  dashboardBody(
    
    leafletOutput("map")
  )
)

# Server logic
server <- function(input, output, session) {
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
}

# Run the dashboard
shinyApp(ui = ui, server = server)
