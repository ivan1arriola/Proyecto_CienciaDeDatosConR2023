

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
                     leafletOutput('map')),
            tabPanel("Univariado",
                     dataTableOutput('uni')),
            tabPanel("Multivariado",
                     plotOutput('multi')))
        )
    )
)