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
  dashboardSidebar(),
  dashboardBody(
    column(width = 3,
           box(width = NULL,
               
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
                           ), 
                           selected = "Illinois"
               ), 
               checkboxInput("allCheck", "All", TRUE), 
               checkboxInput("noProductionPlants", "Plants With No Production", FALSE),
               checkboxInput("coalCheck", "Coal", FALSE), 
               checkboxInput("oilCheck", "Oil", FALSE), 
               checkboxInput("gasCheck", "Gas", FALSE), 
               checkboxInput("nuclearCheck", "Nuclear", FALSE), 
               checkboxInput("hydroCheck", "Hydro", FALSE), 
               checkboxInput("biomassCheck", "Biomass", FALSE), 
               checkboxInput("windCheck", "Wind", FALSE), 
               checkboxInput("solarCheck", "Solar", FALSE), 
               checkboxInput("geothermalCheck", "Geothermal", FALSE), 
               checkboxInput("otherCheck", "Other", FALSE), 
               checkboxInput("renewablesCheck", "Renewables", FALSE), 
               checkboxInput("nonrenewablesCheck", "Non-renewables", FALSE)
           ) 
    ),
    
    tags$style(type = "text/css", "#selectedStateMap {height: calc(100vh - 90px) !important;}"),
    verbatimTextOutput("stateCoordsInfo"),
    leafletOutput("selectedStateMap",  width = "75%", height = "100%")
  )
)
