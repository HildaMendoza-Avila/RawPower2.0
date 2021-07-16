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

# END OF DATA CLEANING --------------------------------------

write_xlsx(energyData,"cleanedEnergyData.xlsx")
rm(energyData)
