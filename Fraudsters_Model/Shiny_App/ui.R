library(shiny)
library(shinydashboard)
library(shinyFiles)
## UI
header <- dashboardHeader(title = "Fraudsters Model")


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Predict!", tabName = "model", icon = icon("bar-chart-o"),
             downloadButton('download',"Download the Predictions"),
             fileInput('file1', 'Choose CSV File',
                       accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv'))
    )
  )
)
body <- dashboardBody(h2("Fraudsters Model - Predictions"),
                      
                      fluidRow(uiOutput("Good_Users"),
                        img(src='The-History-of-Credit-Cards.jpg',
                                   height = 160, width = 210)),
                      fluidRow(uiOutput("Fraudsters"),
                               img(src='Fraudster.jpg',
                                   height = 160, width = 210)),
                      fluidRow(plotlyOutput("Graphic_Universes"))
                      
                      
                      )

ui <- dashboardPage(header, 
                    sidebar, 
                    body)

