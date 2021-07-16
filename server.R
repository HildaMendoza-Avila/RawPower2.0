# Hilda Mendoza-Avila
# July 3, 2021

library(shinydashboard)
library(leaflet)
library(dplyr)
library(tidyverse)
library(curl) # make the jsonlite suggested dependency explicit

# Define server logic required to draw a leaflet Map
function(input, output, session) {
  sourceURLs <- list(COAL = "COAL.png", OIL = "OIL.png", GAS = "GAS.png", NUCLEAR = "NUCLEAR.png",
                     HYDRO = "HYDRO.png", BIOMASS = "BIOMASS.png", WIND = "WIND.png",
                     SOLAR = "SOLAR.png", GEOTHERMAL = "GEOTHERMAL.png", OTHER = "OTHER.png", NO_ENERGY_PRODUCED = "No_Energy_Production.png")
  
  energyData <- read_excel("cleanedEnergyData.xlsx")
  stateDataset <- NULL
  # dataset <- energyData
  # dataset$IconURL <- sourceURLs$GAS
  
  # CODE TO BE IMPROVED FOR FASTER RUNTIME -----------
  
  universalTotalGraphWidth = 7
  universalTotalGraphHeight = 50
  
  
  # universalAnchorY = 3;
  # getIconWidth <- function(sourceIndex){
  #   sourceWidth <- switch(sourceIndex, dataset$COAL_PERCENTAGE, dataset$OIL_PERCENTAGE,
  #                         dataset$GAS_PERCENTAGE, dataset$NUCLEAR_PERCENTAGE,
  #                         dataset$HYDRO_PERCENTAGE, dataset$BIOMASS_PERCENTAGE,
  #                         dataset$WIND_PERCENTAGE, dataset$SOLAR_PERCENTAGE,
  #                         dataset$GEOTHERMAL_PERCENTAGE, dataset$OTHER_PERCENTAGE)
  #   sourceWidth <- sourceWidth/7
  #   sourceWidth <- ifelse(sourceWidth < 0.00000001, 0.00000001, sourceWidth)
  #   sourceWidth
  # }
  # 
  # getIconAnchorX <- function(index, anchorSum){
  #   inconWidth = getIconWidth(index)
  # 
  #   ifelse(index == 1, inconWidth + anchorSum, getIconAnchorX(index - 1, anchorSum = anchorSum + inconWidth))
  # }

  # sourceIsChecked <- function(index){
  #   if(input$allCheck){
  #     TRUE
  #   }
  #   else{
  #     switch(index,
  #            input$coalCheck, 
  #            input$oilCheck, 
  #            input$gasCheck,
  #            input$nuclearCheck,
  #            input$hydroCheck,
  #            input$biomassCheck,
  #            input$windCheck,
  #            input$solarCheck,
  #            input$geothermalCheck,
  #            input$otherCheck)
  #   }
  # }
  
  # sourceGraphIcon <- function(index){
  #   # if(sourceIsChecked(index)){
  #     makeIcon(
  #       iconUrl = sourceURLs[index],
  #       iconWidth = getIconWidth(index), iconHeight = universalTotalGraphHeight,
  #       iconAnchorX = getIconAnchorX(index, 0),
  #       iconAnchorY = universalAnchorY
  #     )
  #   # }
  # }
  
  # Warning: the condition has length > 1 and only the first element will be used
  # getSourcePercentage <- function(sourceIndex){
  #   switch(sourceIndex, dataset$COAL_PERCENTAGE, dataset$OIL_PERCENTAGE,
  #          dataset$GAS_PERCENTAGE, dataset$NUCLEAR_PERCENTAGE,
  #          dataset$HYDRO_PERCENTAGE, dataset$BIOMASS_PERCENTAGE,
  #          dataset$WIND_PERCENTAGE, dataset$SOLAR_PERCENTAGE,
  #          dataset$GEOTHERMAL_PERCENTAGE, dataset$OTHER_PERCENTAGE)
  # }
  
  # getPlantIcon <- function(){
  #   highestPercentageSoFar = -1
  #   highestSourceIndex = 1
  #   for(sourceIndex in 1:10){
  #     # Warning: the condition has length > 1 and only the first element will be used
  #     sourcePercent = getSourcePercentage(sourceIndex)
  #     if(sourcePercent > highestPercentageSoFar){
  #       highestPercentageSoFar = sourcePercent
  #       highestSourceIndex = sourceIndex
  #     }
  #   }
  #   getSourcePercentage(highestSourceIndex)
  # }
  
  
  # getPlantIconURL <- function(){
  #   sourceURLs$GEOTHERMAL
  # }
  
  
  
  
  # # makeIconForNoEnergyProduction <- function(){
  # #   makeIcon(
  # #         iconUrl = sourceURLs$NoEnergy, # No energy produced png url
  # #         iconHeight = universalTotalGraphHeight,
  # #         iconWidth = universalTotalGraphWidth
  # #       )
  # # }
  # # 
  # # makeIconRegEnergyProduction <- function(){
  # #   
  # # }
  # 
  # 
  # # Note - Warning: currentDatasetEntry is the set of entries with the selected state - We might need to use a foreach loop
  # # constructSourceIcon <- function(){
  #   # print(dataset[9][])
  #   
  #   # makeIconForNoEnergyProduction()
  #   
  #   # currNonRenewTotal <- dataset$NonRenewableGenerationTotal
  #   # currRenewTotal <- dataset$RenewableGenerationTotal
  # 
  #   # ifelse(dataset[9][] < 1, makeIconForNoEnergyProduction(), makeIconRegEnergyProduction())
  #   # 
  #   
  #   # create a temporary dataframe from currentDatasetEntry to construct the source icon
  #   # EnergyPlantName <- currentDatasetEntry$PlantName
  #   # EnergySource <- c("COAL", "OIL", "GAS", "NUCLEAR", "HYDRO", "BIOMASS", "WIND", "SOLAR", "GEOTHERMAL", "OTHER")
  #   # EnergyPercentage <- c(currentDatasetEntry$COAL_TOTAL, currentDatasetEntry$OIL_TOTAL,
  #   #                       currentDatasetEntry$GAS_TOTAL, currentDatasetEntry$NUCLEAR_TOTAL,
  #   #                       currentDatasetEntry$HYDRO_TOTAL, currentDatasetEntry$BIOMASS_TOTAL,
  #   #                       currentDatasetEntry$WIND_TOTAL, currentDatasetEntry$SOLAR_TOTAL,
  #   #                       currentDatasetEntry$GEOTHERMAL_TOTAL, currentDatasetEntry$OTHER_TOTAL)
  #   # tempDataFrame <- data.frame(EnergyPlantName, EnergySource, EnergyPercentage)
  # 
  #   # # Stacked + percent
  #   # plantIcon <- ggplot(tempDataFrame, aes(fill=EnergySource, y=EnergyPercentage, x=EnergyPlantName)) +
  #   #   geom_bar(position="fill", stat="identity")
  #   #
  #   # ggsave("PlantIcon.png", plot = plantIcon)
  #   #
  #   # makeIcon(
  #   #       iconUrl = "PlantIcon.png",
  #   #       iconHeight = universalTotalGraphHeight,
  #   #       iconWidth = universalTotalGraphWidth,
  #   #       # iconAnchorX = getIconAnchorX(index, 0),
  #   #       # iconAnchorY = universalAnchorY
  #   #     )
  #   
  #   
  #   
  #   
  #   
  #   
  #   
  #   # currentDatasetEntry <- head(energyData, 1)
  #   # print(currentDatasetEntry)
  #   
  #   # if(currNonRenewTotal == 0){ #&& currRenewTotal == 0){
  #   #   makeIcon(
  #   #           iconUrl = sourceURLs[11], # No energy produced png url
  #   #           iconHeight = universalTotalGraphHeight,
  #   #           iconWidth = universalTotalGraphWidth,
  #   #           # iconAnchorX = getIconAnchorX(index, 0),
  #   #           # iconAnchorY = universalAnchorY
  #   #         )
  #   # }
  #   # else
  #   # {
  #   #   makeIcon(
  #   #     iconUrl = sourceURLs[5], # No energy produced png url
  #   #     iconHeight = universalTotalGraphHeight,
  #   #     iconWidth = universalTotalGraphWidth,
  #   #     # iconAnchorX = getIconAnchorX(index, 0),
  #   #     # iconAnchorY = universalAnchorY
  #   #   )
  #   # }
  #     
  #   # if(sourceIsChecked(1)){
  #   #   makeIcon(
  #   #     iconUrl = getPlantIcon(),
  #   #     iconHeight = universalTotalGraphHeight,
  #   #     iconWidth = universalTotalGraphWidth,
  #   #     # iconAnchorX = getIconAnchorX(index, 0),
  #   #     # iconAnchorY = universalAnchorY
  #   #   )
  #   # }
  #   
  #   
  #   # rm(tempDataFrame)
  # # }
  # 
  # #-------------------------------------------------------
  
  getStateDataset <- function(selectedState) {
    selectedStateData <- subset(energyData, PlantState == selectedState)
    
    # identify the main energy source type for all of the energy plants in the selected state
    
    if (nrow(selectedStateData) >= 1){
      output <- vector("character", nrow(selectedStateData))
      for (currPlantRow in 1:nrow(selectedStateData)){
        mainType = "NO_ENERGY_PRODUCED"
        maxProducSoFar = 0
        
        for (currSourceCol in 10:18){ # Column indexes for the TOTAL production of the 9 Energy Sources at the current plant
          currEnergyTotal = selectedStateData[[currPlantRow, currSourceCol]]
          
          if (currEnergyTotal > maxProducSoFar){
            previousMainType  = mainType
            mainType = switch( (currSourceCol-9), 
                               if_else(input$coalCheck,       "COAL",       mainType),
                               if_else(input$oilCheck,        "OIL",        mainType),
                               if_else(input$gasCheck,        "GAS",        mainType),
                               if_else(input$nuclearCheck,    "NUCLEAR",    mainType),
                               if_else(input$hydroCheck,      "HYDRO",      mainType),
                               if_else(input$biomassCheck,    "BIOMASS",    mainType),
                               if_else(input$windCheck,       "WIND",       mainType),
                               if_else(input$solarCheck,      "SOLAR",      mainType),
                               if_else(input$geothermalCheck, "GEOTHERMAL", mainType)
            )
            if (mainType != previousMainType){
              maxProducSoFar = currEnergyTotal
            }
          }
          
        }
        
        # check for OTHER_TOTAL since it was not in the column range above
        otherEnergyTotal = selectedStateData[[currPlantRow, 32]]
        if (otherEnergyTotal > maxProducSoFar){
          maxProducSoFar = otherEnergyTotal
          mainType = "OTHER"
        }
        
        output[[currPlantRow]] <- mainType
      }
    }
    selectedStateData$MainEnergySource <- output
    selectedStateData
  }
  
  
  plantEnergyIcons <- iconList(
    COAL               = makeIcon(sourceURLs$COAL, iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    OIL                = makeIcon(sourceURLs$OIL, iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    GAS                = makeIcon(sourceURLs$GAS, iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    NUCLEAR            = makeIcon(sourceURLs$NUCLEAR, iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    HYDRO              = makeIcon(sourceURLs$HYDRO, iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    BIOMASS            = makeIcon(sourceURLs$BIOMASS, iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    WIND               = makeIcon(sourceURLs$WIND, iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    SOLAR              = makeIcon(sourceURLs$SOLAR, iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    GEOTHERMAL         = makeIcon(sourceURLs$GEOTHERMAL, iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
    OTHER              = makeIcon(sourceURLs$OTHER, iconWidth = universalTotalGraphWidth, iconHeight = universalTotalGraphHeight),
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
    
    # print(stateDataset$IconURL)
    
    leaflet(stateDataset) %>%
      addTiles() %>%  # Add default OpenStreetMap map titles
      addMarkers(lng = stateDataset$Longitude, lat=stateDataset$Latitude, popup=stateDataset$PlantName, icon = ~plantEnergyIcons[MainEnergySource])
    
    
    
    # addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
    #            icon = sourceGraphIcon(10)) %>%
    # addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
    #            icon = sourceGraphIcon(9)) %>%
    # addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
    #            icon = sourceGraphIcon(8)) %>%
    # addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
    #            icon = sourceGraphIcon(7)) %>%
    # addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
    #            icon = sourceGraphIcon(6)) %>%
    # addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
    #            icon = sourceGraphIcon(5)) %>%
    # addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
    #            icon = sourceGraphIcon(4)) %>%
    # addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
    #            icon = sourceGraphIcon(3)) %>%
    # addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
    #            icon = sourceGraphIcon(2)) %>%
    # addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
    #            icon = sourceGraphIcon(1))
  })
}