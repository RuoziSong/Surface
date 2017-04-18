library(shinydashboard)
library(leaflet)
library(DT)

header<-dashboardHeader(title="Score Map by Zipcode")

body<-dashboardBody(
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("zipcodeMap", height=400)
               ),
           box(width=NULL,
               dataTableOutput("zipcodeTable")
           )
    ),
    column(width=3,
           box(width=NULL, 
               uiOutput("yearSelect"),
               radioButtons("measure", "Measure",c("Mean"="Mean", "Median"="Median"))
               
               )
           )
    )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
