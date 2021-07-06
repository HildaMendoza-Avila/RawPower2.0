# Hilda Mendoza-Avila
# July 3, 2021

library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(leaflet)
options(scipen = 100)

# Define UI for app
ui <- dashboardPage(
  dashboardHeader(
    # App title
    title = "Raw Power 2.0"
  ),
  dashboardSidebar(
    # Input: Selector for choosing stateDataset
    selectInput(inputId = "stateDataset",
                label = "Choose a state:",
                choices = c(
                  "Alabama",
                  "Alaska",
                  "Arizona",
                  "Arkansas",
                  "California",
                  "Colorado",
                  "Connecticut",
                  "Delaware",
                  "Florida",
                  "Georgia",
                  "Hawaii",
                  "Idaho",
                  "Illinois",
                  "Indiana",
                  "Iowa",
                  "Kansas",
                  "Kentucky",
                  "Louisiana",
                  "Maine",
                  "Maryland",
                  "Massachusetts",
                  "Michigan",
                  "Minnesota",
                  "Mississippi",
                  "Missouri",
                  "Montana",
                  "Nebraska",
                  "Nevada",
                  "New Hampshire",
                  "New Jersey",
                  "New Mexico",
                  "New York",
                  "North Carolina",
                  "North Dakota",
                  "Ohio",
                  "Oklahoma",
                  "Oregon",
                  "Pennsylvania",
                  "Rhode Island",
                  "South Carolina",
                  "South Dakota",
                  "Tennessee",
                  "Texas",
                  "Utah",
                  "Vermont",
                  "Virginia",
                  "Washington",
                  "West Virginia",
                  "Wisconsin",
                  "Wyoming"
                )
    )
  ),
  dashboardBody(
    tags$style(type = "text/css", "#selectedStateMap {height: calc(100vh - 90px) !important;}"),
    leafletOutput("selectedStateMap")
  )
)

# Define server logic required to draw a leaflet Map
server <- function(input, output) {
  energyData <- read_excel("cleanedEnergyData.xlsx")
  dataset <- energyData
  
  # CODE TO BE IMPROVED FOR FASTER RUNTIME -----------
  
  sourceURLs <- list(COAL = "COAL.png", OIL = "OIL.png", GAS = "GAS.png", NUCLEAR = "NUCLEAR.png",
                     HYDRO = "HYDRO.png", BIOMASS = "BIOMASS.png", WIND = "WIND.png",
                     SOLAR = "SOLAR.png", GEOTHERMAL = "GEOTHERMAL.png", OTHER = "OTHER.png")
  
  universalTotalGraphHeight = 5;
  universalAnchorY = 3;
  
  getIconWidth <- function(sourceIndex){
    sourceWidth <- switch(sourceIndex, dataset$COAL_PERCENTAGE, dataset$OIL_PERCENTAGE,
                          dataset$GAS_PERCENTAGE, dataset$NUCLEAR_PERCENTAGE,
                          dataset$HYDRO_PERCENTAGE, dataset$BIOMASS_PERCENTAGE,
                          dataset$WIND_PERCENTAGE, dataset$SOLAR_PERCENTAGE,
                          dataset$GEOTHERMAL_PERCENTAGE, dataset$OTHER_PERCENTAGE)
    sourceWidth <- sourceWidth/7
    sourceWidth <- ifelse(sourceWidth < 0.00000001, 0.00000001, sourceWidth)
    sourceWidth
  }
  
  getIconAnchorX <- function(index, anchorSum){
    inconWidth = getIconWidth(index)
    
    ifelse(index == 1, inconWidth + anchorSum, getIconAnchorX(index - 1, anchorSum = anchorSum + inconWidth))
  }
  
  sourceGraphIcon <- function(index){
    makeIcon(
      iconUrl = sourceURLs[index],
      iconWidth = getIconWidth(index), iconHeight = universalTotalGraphHeight,
      iconAnchorX = getIconAnchorX(index, 0),
      iconAnchorY = universalAnchorY
    )
  }
  
  #-------------------------------------------------------
  
  getStateDataset <- function(selectedState) {
    subset(energyData, PlantState == selectedState)
  }
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$stateDataset, 
           "Alabama" = getStateDataset('AL'),
           "Alaska" = getStateDataset('AK'),
           "Arizona" = getStateDataset('AZ'),
           "Arkansas" = getStateDataset('AR'),
           "California" = getStateDataset('CA'),
           "Colorado" = getStateDataset('CO'),
           "Connecticut" = getStateDataset('CT'),
           "Delaware" = getStateDataset('DE'),
           "Florida" = getStateDataset('FL'),
           "Georgia" = getStateDataset('GA'),
           "Hawaii" = getStateDataset('HI'),
           "Idaho" = getStateDataset('ID'),
           "Illinois" = getStateDataset('IL'),
           "Indiana" = getStateDataset('IN'),
           "Iowa" = getStateDataset('IA'),
           "Kansas" = getStateDataset('KS'),
           "Kentucky" = getStateDataset('KY'),
           "Louisiana" = getStateDataset('LA'),
           "Maine" = getStateDataset('ME'),
           "Maryland" = getStateDataset('MD'),
           "Massachusetts" = getStateDataset('MA'),
           "Michigan" = getStateDataset('MI'),
           "Minnesota" = getStateDataset('MN'),
           "Mississippi" = getStateDataset('MS'),
           "Missouri" = getStateDataset('MO'),
           "Montana" = getStateDataset('MT'),
           "Nebraska" = getStateDataset('NE'),
           "Nevada" = getStateDataset('NV'),
           "New Hampshire" = getStateDataset('NH'),
           "New Jersey" = getStateDataset('NJ'),
           "New Mexico" = getStateDataset('NM'),
           "New York" = getStateDataset('NY'),
           "North Carolina" = getStateDataset('NC'),
           "North Dakota" = getStateDataset('ND'),
           "Ohio" = getStateDataset('OH'),
           "Oklahoma" = getStateDataset('OK'),
           "Oregon" = getStateDataset('OR'),
           "Pennsylvania" = getStateDataset('PA'),
           "Rhode Island" = getStateDataset('RI'),
           "South Carolina" = getStateDataset('SC'),
           "South Dakota" = getStateDataset('SD'),
           "Tennessee" = getStateDataset('TN'),
           "Texas" = getStateDataset('TX'),
           "Utah" = getStateDataset('UT'),
           "Vermont" = getStateDataset('VT'),
           "Virginia" = getStateDataset('VA'),
           "Washington" = getStateDataset('WA'),
           "West Virginia" = getStateDataset('WV'),
           "Wisconsin" = getStateDataset('WI'),
           "Wyoming" = getStateDataset('WY')
    )
  })
  
  output$selectedStateMap <- renderLeaflet({
    dataset <- datasetInput()
    leaflet(dataset) %>%
      addTiles() %>% # Add default OpenStreetMap map titles
      addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                 icon = sourceGraphIcon(10)) %>%
      addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                 icon = sourceGraphIcon(9)) %>%
      addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                 icon = sourceGraphIcon(8)) %>%
      addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                 icon = sourceGraphIcon(7)) %>%
      addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                 icon = sourceGraphIcon(6)) %>%
      addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                 icon = sourceGraphIcon(5)) %>%
      addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                 icon = sourceGraphIcon(4)) %>%
      addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                 icon = sourceGraphIcon(3)) %>%
      addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                 icon = sourceGraphIcon(2)) %>%
      addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                 icon = sourceGraphIcon(1)) 
  })
}

# Create Shiny app
runApp(shinyApp(ui, server), launch.browser = TRUE)




















































