

library(shiny)
library(leaflet)

ui <- fluidPage(
    titlePanel("Trafico UY"),
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("variable", "Variables a Mostrar:",
                                c("Semaforos" = "semaforo",
                                  "Barrios" = "barrio",
                                  "Sensores" = "sensor")),
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Mapa",
                     leafletOutput('map', width = "100%", height = "60vh")),
            tabPanel("Univariado",
                     dataTableOutput('uni')),
            tabPanel("Multivariado",
                     plotOutput('multi')))
        )
    )
)