# egrid2018_data_v2.xlsx File Data Cleaning

# Hilda Mendoza-Avila
# July 3, 2021

library(readxl)
library("writexl")
options(scipen = 100)

energyData <- read_excel("egrid2018_data_v2.xlsx")

# BEGINNING OF DATA CLEANING --------------------------------------

# replace all Na entries with 0
energyData[is.na(energyData)] = 0.00000000

energyData <- subset(energyData, Latitude > 0.00000)
energyData <- subset(energyData, Longitude != 0)


# replace all negatives with 0
negativeSubset <- subset(energyData, select=9:length(energyData))
negativeSubset[negativeSubset < 0] = 0.00000000
energyData[9:length(energyData)] <- negativeSubset
rm(negativeSubset)

# add total values of OTHER_FOSIL and OTHER_UNKNOWN
energyData$OTHER_TOTAL = energyData$OTHER_FOSSIL_TOTAL + energyData$OTHER_UNKNOWN_TOTAL

# add percentage values of OTHER_FOSIL and OTHER_UNKNOWN
energyData$OTHER_PERCENTAGE = energyData$OTHER_FOSSIL_PERCENTAGE + energyData$OTHER_UNKNOWN_PERCENTAGE

# remove columns OTHER_FOSSIL_TOTAL, OTHER_UNKNOWN_TOTAL, OTHER_FOSSIL_PERCENTAGE, and OTHER_UNKNOWN_PERCENTAGE
energyData = subset(energyData, select = -c(OTHER_FOSSIL_TOTAL,OTHER_UNKNOWN_TOTAL,OTHER_FOSSIL_PERCENTAGE,OTHER_UNKNOWN_PERCENTAGE))

# make sure the percentage values are correct (these values got multiplied by 10000)
divideFactor <- 10000
energyData$COAL_PERCENTAGE <- energyData$COAL_PERCENTAGE/divideFactor
energyData$OIL_PERCENTAGE <- energyData$OIL_PERCENTAGE/divideFactor
energyData$GAS_PERCENTAGE <- energyData$GAS_PERCENTAGE/divideFactor
energyData$NUCLEAR_PERCENTAGE <- energyData$NUCLEAR_PERCENTAGE/divideFactor
energyData$HYDRO_PERCENTAGE <- energyData$HYDRO_PERCENTAGE/divideFactor
energyData$BIOMASS_PERCENTAGE <- energyData$BIOMASS_PERCENTAGE/divideFactor
energyData$WIND_PERCENTAGE <- energyData$WIND_PERCENTAGE/divideFactor
energyData$SOLAR_PERCENTAGE <- energyData$SOLAR_PERCENTAGE/divideFactor
energyData$GEOTHERMAL_PERCENTAGE <- energyData$GEOTHERMAL_PERCENTAGE/divideFactor
energyData$OTHER_PERCENTAGE <- energyData$OTHER_PERCENTAGE/divideFactor
rm(divideFactor)

colnames(energyData)[which(names(energyData) == 'Non-RenewableGenerationPercentage')] <- "NonRenewableGenerationPercentage"

# identify the main energy source type for all of the energy plants

sourcesList <- list(COAL = "COAL",  OIL = "OIL", GAS = "GAS", NUCLEAR = "NUCLEAR", 
                      HYDRO = "HYDRO", BIOMASS = "BIOMASS", WIND = "WIND", 
                      SOLAR = "SOLAR", GEOTHERMAL = "GEOTHERMAL", OTHER = "OTHER", NO_ENERGY_PRODUCED = "NO_ENERGY_PRODUCED")

if (nrow(energyData) >= 1){
  MainEnergySources <- vector("character", nrow(energyData))
  for (currPlantRow in 1:nrow(energyData)){
    mainType = sourcesList$NO_ENERGY_PRODUCED
    maxProducSoFar = 0
    
    for (currSourceCol in 10:18){ # Column indexes for the TOTAL production of the 9 Energy Sources at the current plant
      currEnergyTotal = energyData[[currPlantRow, currSourceCol]]
      
      if (currEnergyTotal > maxProducSoFar){
        maxProducSoFar = currEnergyTotal
        
        mainType = switch( (currSourceCol-9), 
                           sourcesList$COAL,
                           sourcesList$OIL,
                           sourcesList$GAS, 
                           sourcesList$NUCLEAR,
                           sourcesList$HYDRO, 
                           sourcesList$BIOMASS,
                           sourcesList$WIND, 
                           sourcesList$SOLAR,
                           sourcesList$GEOTHERMAL,
        )
      }
      
    }
    
    # check for OTHER_TOTAL since it was not in the column range above
    otherEnergyTotal = energyData[[currPlantRow, 32]]
    if (otherEnergyTotal > maxProducSoFar){
      maxProducSoFar = otherEnergyTotal
      mainType = sourcesList$OTHER
    }
    
    MainEnergySources[[currPlantRow]] <- mainType
  }
}
energyData$MainEnergySource <- MainEnergySources

rm(currEnergyTotal, currSourceCol, MainEnergySources, currPlantRow, mainType, maxProducSoFar, otherEnergyTotal, sourcesList)

# END OF DATA CLEANING --------------------------------------

write_xlsx(energyData,"cleanedEnergyData.xlsx")
rm(energyData)
