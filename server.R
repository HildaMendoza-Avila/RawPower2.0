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
  
  clearUnselectedSources <- function(selectedStateData, allCheck, coalCheck, oilCheck, gasCheck, nuclearCheck, otherCheck, nonrenewablesCheck, 
                                     hydroCheck, biomassCheck, windCheck, solarCheck, geothermalCheck, renewablesCheck, noProductionPlants){
    if(!allCheck){
      
      if(!nonrenewablesCheck){
        
        if(!coalCheck){
          selectedStateData <- subset(selectedStateData, !(COAL_PERCENTAGE > 0) )    
        }
        if(!oilCheck){
          selectedStateData <- subset(selectedStateData, !(OIL_PERCENTAGE > 0) )    
        }
        if(!gasCheck){
          selectedStateData <- subset(selectedStateData, !(GAS_PERCENTAGE > 0) )    
        }
        if(!nuclearCheck){
          selectedStateData <- subset(selectedStateData, !(NUCLEAR_PERCENTAGE > 0) )    
        }
        if(!otherCheck){
          selectedStateData <- subset(selectedStateData, !(OTHER_PERCENTAGE > 0) )    
        }
        
      }
      if (!renewablesCheck){
        
        if(!hydroCheck){
          selectedStateData <- subset(selectedStateData, !(HYDRO_PERCENTAGE > 0) )    
        }
        if(!biomassCheck){
          selectedStateData <- subset(selectedStateData, !(BIOMASS_PERCENTAGE > 0) )   
        }
        if(!windCheck){
          selectedStateData <- subset(selectedStateData, !(WIND_PERCENTAGE > 0) )    
        }
        if(!solarCheck){
          selectedStateData <- subset(selectedStateData, !(SOLAR_PERCENTAGE > 0) )    
        }
        if(!geothermalCheck){
          selectedStateData <- subset(selectedStateData, !(GEOTHERMAL_PERCENTAGE > 0) )    
        }
        
      }
    }
    
    if(!noProductionPlants){
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
  left_datasetInput <- reactive({
    switch(input$left_stateDataset,
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
  
  right_datasetInput <- reactive({
    switch(input$right_stateDataset,
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
  
  output$left_sourceCheckPrompt  <- renderText("\nSelect the electrical power generation sources to be mapped on left map:")
  output$right_sourceCheckPrompt <- renderText("\nSelect the electrical power generation sources to be mapped on right map:")
  
  output$leftMap <- renderLeaflet({
    stateDataset <- left_datasetInput()
    
    stateDataset <- clearUnselectedSources(stateDataset, input$left_allCheck, 
                                          input$left_coalCheck, input$left_oilCheck, input$left_gasCheck, input$left_nuclearCheck, 
                                          input$left_otherCheck, input$left_nonrenewablesCheck, input$left_hydroCheck, input$left_biomassCheck, 
                                          input$left_windCheck, input$left_solarCheck, input$left_geothermalCheck, input$left_renewablesCheck, 
                                          input$left_noProductionPlants)
    
    leaflet(stateDataset) %>%
      addTiles() %>%  # Adds default OpenStreetMap map titles
      addEasyButton(easyButton(
        icon="fa-crosshairs", title = "Locate me", 
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      addMarkers(lng = stateDataset$Longitude, lat = stateDataset$Latitude, 
                 popup = paste("Plant Name: ", stateDataset$PlantName, 
                               "<br>Main Electricity Production Source: ", stateDataset$MainEnergySource, 
                               "<br>Annual Electricity Generation Total: ", stateDataset$AnnualGenerationTotal, 
                               "(MWh)<br>Coal Energy Generation Percentage: ", stateDataset$COAL_PERCENTAGE, 
                               "%<br>Oil Energy Generation Percentage: ", stateDataset$OIL_PERCENTAGE, 
                               "%<br>Gas Energy Generation Percentage: ", stateDataset$GAS_PERCENTAGE, 
                               "%<br>Nuclear Energy Generation Percentage: ", stateDataset$NUCLEAR_PERCENTAGE, 
                               "%<br>Hydro Energy Generation Percentage: ", stateDataset$HYDRO_PERCENTAGE, 
                               "%<br>Biomass Energy Generation Percentage: ", stateDataset$BIOMASS_PERCENTAGE, 
                               "%<br>Wind Energy Generation Percentage: ", stateDataset$WIND_PERCENTAGE, 
                               "%<br>Solar Energy Generation Percentage: ", stateDataset$SOLAR_PERCENTAGE, 
                               "%<br>Geothermal Energy Generation Percentage: ", stateDataset$GEOTHERMAL_PERCENTAGE, 
                               "%<br>Non-Renewable Energy Production Percentage: ", stateDataset$NonRenewableGenerationPercentage, 
                               "%<br>Renewable Energy Production Percentage: ", stateDataset$RenewableGenerationPercentage, 
                               "%<br>Other Energy Generation Percentage: ", stateDataset$OTHER_PERCENTAGE, "%"), 
                 icon = ~plantEnergyIcons[MainEnergySource]) %>%
      addResetMapButton() 
    
    # TODO: if user allows location permission, then find a way to display the selected energy sources located in the user's area
    
  })
  
  
  output$rightMap <- renderLeaflet({
    stateDataset <- right_datasetInput()
    
    stateDataset <- clearUnselectedSources(stateDataset, input$right_allCheck, 
                                           input$right_coalCheck, input$right_oilCheck, input$right_gasCheck, input$right_nuclearCheck, 
                                           input$right_otherCheck, input$right_nonrenewablesCheck, input$right_hydroCheck, input$right_biomassCheck, 
                                           input$right_windCheck, input$right_solarCheck, input$right_geothermalCheck, input$right_renewablesCheck, 
                                           input$right_noProductionPlants)
    
    leaflet(stateDataset) %>%
      addTiles() %>%  # Adds default OpenStreetMap map titles
      addEasyButton(easyButton(
        icon="fa-crosshairs", title = "Locate me", 
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      addMarkers(lng = stateDataset$Longitude, lat = stateDataset$Latitude, 
                 popup = paste("Plant Name: ", stateDataset$PlantName, 
                               "<br>Main Electricity Production Source: ", stateDataset$MainEnergySource, 
                               "<br>Annual Electricity Generation Total: ", stateDataset$AnnualGenerationTotal, 
                               "(MWh)<br>Coal Energy Generation Percentage: ", stateDataset$COAL_PERCENTAGE, 
                               "%<br>Oil Energy Generation Percentage: ", stateDataset$OIL_PERCENTAGE, 
                               "%<br>Gas Energy Generation Percentage: ", stateDataset$GAS_PERCENTAGE, 
                               "%<br>Nuclear Energy Generation Percentage: ", stateDataset$NUCLEAR_PERCENTAGE, 
                               "%<br>Hydro Energy Generation Percentage: ", stateDataset$HYDRO_PERCENTAGE, 
                               "%<br>Biomass Energy Generation Percentage: ", stateDataset$BIOMASS_PERCENTAGE, 
                               "%<br>Wind Energy Generation Percentage: ", stateDataset$WIND_PERCENTAGE, 
                               "%<br>Solar Energy Generation Percentage: ", stateDataset$SOLAR_PERCENTAGE, 
                               "%<br>Geothermal Energy Generation Percentage: ", stateDataset$GEOTHERMAL_PERCENTAGE, 
                               "%<br>Non-Renewable Energy Production Percentage: ", stateDataset$NonRenewableGenerationPercentage, 
                               "%<br>Renewable Energy Production Percentage: ", stateDataset$RenewableGenerationPercentage, 
                               "%<br>Other Energy Generation Percentage: ", stateDataset$OTHER_PERCENTAGE, "%"), 
                 icon = ~plantEnergyIcons[MainEnergySource]) %>%
      addResetMapButton() 
  })
}