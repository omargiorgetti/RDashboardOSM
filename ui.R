library(leaflet)
library(shinydashboard)


dashboardPage(
  dashboardHeader(title="Accessi al Pronto Soccorso del Mugello",
                  titleWidth = "400"),
  dashboardSidebar(disable=TRUE), 
  dashboardBody(
    fluidPage(
      fluidRow(
        column(4,
          fluidRow(
            valueBoxOutput("numberBox"),
            valueBoxOutput("meanageBox")
          ), 
          dateRangeInput("date_range", label="Intervallo tempolare",
                           start = "2015-01-01",
                           end = "2015-12-31",
                           format = "yyyy-mm-dd"),    
            uiOutput("comuniControl"),
            uiOutput("capitoloControl")
          ),
        column(8,
          box(width = NULL,
            leafletOutput("map",height=450),
            tabBox(width = NULL,height = 350,
              tabPanel("Comune",
                plotOutput("comunecount",height = 300)
              ),
              tabPanel("Settimane",
                plotOutput("settcount",height = 300)
              ),
              tabPanel("Esito",
                plotOutput("esitocount",height = 300)
              ),
              tabPanel("Dati",
                dataTableOutput("table"))
            )  
          )
        )
      )
    )
  )
)

