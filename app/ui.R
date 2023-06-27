#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

ui <- fluidPage(
    titlePanel("Trafico UY"),
    sidebarLayout(
        sidebarPanel(),
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