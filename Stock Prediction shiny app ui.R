

library(shiny)
library(shinydashboard)
library(leaflet)


header <- dashboardHeader(
    title = "Next 5D Closing Price"
)

body <- dashboardBody(
    fluidRow(
        column(width = 9,
               box(width = NULL, solidHeader = TRUE,
                   plotOutput("distPlot")
                   ),
               
               box(width = NULL,
                   DT::dataTableOutput("predDF")
               )
              ),
        column(width = 3,
               box(width = NULL, status = "warning",
                   
                   selectInput("symb",
                               "Select stock:",
                               choices=c("MSFT","FB","AAPL","GS","GOOGL")),
                              
                   selectInput("selectmodel",
                           "Select model:",
                           choices=c("NNET","SVM","MARS"))
               
                  
                  )
               
             )
))

dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body
)
