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
  
  clearUnselectedSources <- function(selectedStateData){
    if(!input$allCheck){
      
      if(!input$nonrenewablesCheck){
        
        if(!input$coalCheck){
          selectedStateData <- subset(selectedStateData, !(COAL_PERCENTAGE > 0) )    
        }
        if(!input$oilCheck){
          selectedStateData <- subset(selectedStateData, !(OIL_PERCENTAGE > 0) )    
        }
        if(!input$gasCheck){
          selectedStateData <- subset(selectedStateData, !(GAS_PERCENTAGE > 0) )    
        }
        if(!input$nuclearCheck){
          selectedStateData <- subset(selectedStateData, !(NUCLEAR_PERCENTAGE > 0) )    
        }
        if(!input$otherCheck){
          selectedStateData <- subset(selectedStateData, !(OTHER_PERCENTAGE > 0) )    
        }
        
      }
      if (!input$renewablesCheck){
        
        if(!input$hydroCheck){
          selectedStateData <- subset(selectedStateData, !(HYDRO_PERCENTAGE > 0) )    
        }
        if(!input$biomassCheck){
          selectedStateData <- subset(selectedStateData, !(BIOMASS_PERCENTAGE > 0) )   
        }
        if(!input$windCheck){
          selectedStateData <- subset(selectedStateData, !(WIND_PERCENTAGE > 0) )    
        }
        if(!input$solarCheck){
          selectedStateData <- subset(selectedStateData, !(SOLAR_PERCENTAGE > 0) )    
        }
        if(!input$geothermalCheck){
          selectedStateData <- subset(selectedStateData, !(GEOTHERMAL_PERCENTAGE > 0) )    
        }
        
      }
    }
    
    if(!input$noProductionPlants){
      selectedStateData <- subset(selectedStateData, ((NonRenewableGenerationPercentage + RenewableGenerationPercentage) > 0) )   
    }
    
    selectedStateData
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
  
  output$sourceCheckPrompt <- renderText("\nSelect the power sources to be mapped:")
  
  output$selectedStateMap <- renderLeaflet({
    stateDataset <- datasetInput()
    
    stateDataset <- clearUnselectedSources(stateDataset)
    
    leaflet(stateDataset) %>%
      addTiles() %>%  # Adds default OpenStreetMap map titles
      addEasyButton(easyButton(
        icon="fa-crosshairs", title = "Locate me", 
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      addMarkers(lng = stateDataset$Longitude, lat = stateDataset$Latitude, popup = stateDataset$PlantName, icon = ~plantEnergyIcons[MainEnergySource]) %>%
      addResetMapButton() 
    
    # TODO: if user allows location permission, then find a way to display the selected energy sources located in the user's area
    
  })
}