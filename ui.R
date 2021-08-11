# Hilda Mendoza-Avila
# July 3, 2021

library(shinydashboard)
library(leaflet)
library(readxl)
library(ggplot2)
options(scipen = 100)

# Define UI for app
dashboardPage(
  dashboardHeader(
    # App title
    title = "Raw Power 2.0"
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    
    column(width = 2,
           box(width = NULL,
               
               # Input: Selector for choosing leftStateDataset
               selectInput(inputId = "left_stateDataset",
                           label = "Select a state:",
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
                           ), 
                           selected = "Illinois"
               ), 
               textOutput("left_ourceCheckPrompt"),
               checkboxInput("left_allCheck", "All", TRUE), 
               checkboxInput("left_noProductionPlants", "Plants With No Production", FALSE),
               checkboxInput("left_coalCheck", "Coal", FALSE), 
               checkboxInput("left_oilCheck", "Oil", FALSE), 
               checkboxInput("left_gasCheck", "Gas", FALSE), 
               checkboxInput("left_nuclearCheck", "Nuclear", FALSE), 
               checkboxInput("left_hydroCheck", "Hydro", FALSE), 
               checkboxInput("left_biomassCheck", "Biomass", FALSE), 
               checkboxInput("left_windCheck", "Wind", FALSE), 
               checkboxInput("left_solarCheck", "Solar", FALSE), 
               checkboxInput("left_geothermalCheck", "Geothermal", FALSE), 
               checkboxInput("left_otherCheck", "Other", FALSE), 
               checkboxInput("left_renewablesCheck", "Renewables", FALSE), 
               checkboxInput("left_nonrenewablesCheck", "Non-renewables", FALSE)
           ) 
    ),
    
    box( height = 3, 
      box(
        height = 3,
        tags$style(type = "text/css", "#leftMap {height: calc(100vh - 110px) !important;}"),
        leafletOutput("leftMap",  width = "100%", height = "100%"),
      ),
      box(
        height = 3,
        tags$style(type = "text/css", "#rightMap {height: calc(100vh - 110px) !important;}"),
        leafletOutput("rightMap",  width = "100%", height = "100%")
      )
    ), 
    
    column(width = 2,
           box(width = NULL,
               
               # Input: Selector for choosing leftStateDataset
               selectInput(inputId = "right_stateDataset",
                           label = "Select a state:",
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
                           ), 
                           selected = "New York"
               ), 
               textOutput("right_ourceCheckPrompt"),
               checkboxInput("right_allCheck", "All", TRUE), 
               checkboxInput("right_noProductionPlants", "Plants With No Production", FALSE),
               checkboxInput("right_coalCheck", "Coal", FALSE), 
               checkboxInput("right_oilCheck", "Oil", FALSE), 
               checkboxInput("right_gasCheck", "Gas", FALSE), 
               checkboxInput("right_nuclearCheck", "Nuclear", FALSE), 
               checkboxInput("right_hydroCheck", "Hydro", FALSE), 
               checkboxInput("right_biomassCheck", "Biomass", FALSE), 
               checkboxInput("right_windCheck", "Wind", FALSE), 
               checkboxInput("right_solarCheck", "Solar", FALSE), 
               checkboxInput("right_geothermalCheck", "Geothermal", FALSE), 
               checkboxInput("right_otherCheck", "Other", FALSE), 
               checkboxInput("right_renewablesCheck", "Renewables", FALSE), 
               checkboxInput("right_nonrenewablesCheck", "Non-renewables", FALSE)
           ) 
    ),
  )
)
