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
