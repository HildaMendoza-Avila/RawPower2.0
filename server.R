# Hilda Mendoza-Avila
# July 3, 2021

library(shinydashboard)
library(leaflet)
library(dplyr)
library(readxl)
library(curl) # make the jsonlite suggested dependency explicit
library(leaflet.extras)

# Define server logic required to draw a leaflet Map
function(input, output, session) {
  energyData <- read_excel("cleanedEnergyData.xlsx")
  
  sourcesList <- list(COAL = "COAL",  OIL = "OIL", GAS = "GAS", NUCLEAR = "NUCLEAR", 
                        HYDRO = "HYDRO", BIOMASS = "BIOMASS", WIND = "WIND", 
                        SOLAR = "SOLAR", GEOTHERMAL = "GEOTHERMAL", OTHER = "OTHER", NO_ENERGY_PRODUCED = "NO_ENERGY_PRODUCED")
  
  sourceURLs <- list(COAL = "COAL.png", OIL = "OIL.png", GAS = "GAS.png", NUCLEAR = "NUCLEAR.png",
                     HYDRO = "HYDRO.png", BIOMASS = "BIOMASS.png", WIND = "WIND.png",
                     SOLAR = "SOLAR.png", GEOTHERMAL = "GEOTHERMAL.png", OTHER = "OTHER.png", NO_ENERGY_PRODUCED = "No_Energy_Production.png")
  
  universalTotalGraphWidth = 7
  universalTotalGraphHeight = 20
  
  getStateDataset <- function(selectedState) {
    selectedStateData <- subset(energyData, PlantState == selectedState)
    selectedStateData
  }
  
  getStateCenter <- function(selectedStateData){
    # return a list of two elements
    # first element is the Longitude of the center of the selected state
    # second element is the Latitude of the center of the selected state
    
    if (nrow(selectedStateData) >= 1){
      lngIndex = 6    # Longitude is column 6
      latIndex = 5    # Latitude  is column 5
      
      lngMin = selectedStateData[[1, lngIndex]] 
      lngMax = selectedStateData[[1, lngIndex]] 
      
      latMin = selectedStateData[[1, latIndex]] 
      latMax = selectedStateData[[1, latIndex]]
      
      
      # MainEnergySources <- vector("character", nrow(energyData))
      for (currPlantRow in 1:nrow(selectedStateData)){
        currLngVal = selectedStateData[[currPlantRow, lngIndex]]
        currLatVal = selectedStateData[[currPlantRow, latIndex]]
        
        if(currLngVal > lngMax){
          lngMax = currLngVal
        }
        else if(currLngVal < lngMin){
          lngMin = currLngVal
        }
        
        if(currLatVal > latMax){
          latMax = currLatVal
        }
        else if(currLatVal < latMin){
          latMin = currLatVal
        }
      }
      longitude = ((lngMax - lngMin)/2) + lngMin
      latitude  = ((latMax - latMin)/2) + latMin
      
      centerCoords <- list(Lng = longitude, Lat = latitude, 
                           lngMin = lngMin, lngMax = lngMax, 
                           latMin = latMin, latMax = latMax)
      centerCoords
    }
  }
  
  clearUnselectedSources <- function(selectedStateData){
    if(!input$allCheck){
      if(!input$coalCheck && !input$nonrenewablesCheck){
        selectedStateData <- subset(selectedStateData, MainEnergySource != sourcesList$COAL)
      }
      if(!input$oilCheck && !input$nonrenewablesCheck){
        selectedStateData <- subset(selectedStateData, MainEnergySource != sourcesList$OIL)
      }
      if(!input$gasCheck && !input$nonrenewablesCheck){
        selectedStateData <- subset(selectedStateData, MainEnergySource != sourcesList$GAS)
      }
      if(!input$nuclearCheck && !input$nonrenewablesCheck){
        selectedStateData <- subset(selectedStateData, MainEnergySource != sourcesList$NUCLEAR)
      }
      if(!input$hydroCheck && !input$renewablesCheck){
        selectedStateData <- subset(selectedStateData, MainEnergySource != sourcesList$HYDRO)
      }
      if(!input$biomassCheck && !input$renewablesCheck){
        selectedStateData <- subset(selectedStateData, MainEnergySource != sourcesList$BIOMASS)
      }
      if(!input$windCheck && !input$renewablesCheck){
        selectedStateData <- subset(selectedStateData, MainEnergySource != sourcesList$WIND)
      }
      if(!input$solarCheck && !input$renewablesCheck){
        selectedStateData <- subset(selectedStateData, MainEnergySource != sourcesList$SOLAR)
      }
      if(!input$geothermalCheck && !input$renewablesCheck){
        selectedStateData <- subset(selectedStateData, MainEnergySource != sourcesList$GEOTHERMAL)
      }
      if(!input$otherCheck && !input$nonrenewablesCheck){
        selectedStateData <- subset(selectedStateData, MainEnergySource != sourcesList$OTHER)
      }
    }
    
    if(!input$noProductionPlants){
      selectedStateData <- subset(selectedStateData, MainEnergySource != sourcesList$NO_ENERGY_PRODUCED)
    }
    
    selectedStateData
  }
  
  # TODO: finish working on the logic for this function :)
  getZoomValue <- function(stateCoords){
    # compute zoom integer value according to the provided stateCoords list information
    # stateCoords$lngMin, stateCoords$lngMax, stateCoords$latMin, stateCoords$latMax
    
    6
  }
  
  
  
  
  plantEnergyIcons <- iconList(
    COAL               = makeIcon(sourceURLs$COAL,               iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    OIL                = makeIcon(sourceURLs$OIL,                iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    GAS                = makeIcon(sourceURLs$GAS,                iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    NUCLEAR            = makeIcon(sourceURLs$NUCLEAR,            iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    HYDRO              = makeIcon(sourceURLs$HYDRO,              iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    BIOMASS            = makeIcon(sourceURLs$BIOMASS,            iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    WIND               = makeIcon(sourceURLs$WIND,               iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    SOLAR              = makeIcon(sourceURLs$SOLAR,              iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    GEOTHERMAL         = makeIcon(sourceURLs$GEOTHERMAL,         iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    OTHER              = makeIcon(sourceURLs$OTHER,              iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    NO_ENERGY_PRODUCED = makeIcon(sourceURLs$NO_ENERGY_PRODUCED, iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight)
  )
  
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
  
  # output$selectedCheck <- renderText({input$allCheck})
  
  output$selectedStateMap <- renderLeaflet({
    stateDataset <- datasetInput()
    
    stateCoords <- getStateCenter(stateDataset)
    
    stateDataset <- clearUnselectedSources(stateDataset)
    
    leaflet(stateDataset) %>%
      addTiles() %>%  # Adds default OpenStreetMap map titles
      setView(stateCoords$Lng, stateCoords$Lat, zoom = getZoomValue(stateCoords)) %>%
      # fitBounds(lng1 = stateCoords$lngMin, lng2 = stateCoords$lngMax, lat1 = stateCoords$latMin, lat2 = stateCoords$latMax) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title = "Locate me", 
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      addMarkers(lng = stateDataset$Longitude, lat = stateDataset$Latitude, popup = stateDataset$PlantName, icon = ~plantEnergyIcons[MainEnergySource]) %>%
      addResetMapButton() 
    
    
    
    # TODO: if user allows location permission, then find a way to display the selected energy sources located in the user's area
    
  })
}






















