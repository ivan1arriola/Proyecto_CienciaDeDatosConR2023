#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DBI)
library(RPostgres)
library(sf)
library(leaflet)
library(paletteer)
library(geouy)
library(spdep)
library(leaflet)
source("utils.R")


mvd_map <- load_geouy("Barrios")
mvd_map_fixed <- st_make_valid(st_transform(mvd_map, crs = 4326))
puntos_sensores <- d_sensores %>% 
  select(latitud, longitud) %>%
  mutate(puntos_transformados = transformarCoord(latitud, longitud, mvd_map))
capaSemafotos <- st_read(here::here("v_int_semaforos", "v_int_semaforos.shp"))

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS"),
  dbname = Sys.getenv("DB_NAME")
)

velocidadxBarrioMax <- DBI::dbGetQuery(
  con,
  "
    SELECT
      d_sensores.barrio,
      MAX(fct_registros.velocidad) AS max_velocidad
    FROM fct_registros
    LEFT JOIN d_sensores ON fct_registros.id_detector = d_sensores.id_detector
    GROUP BY d_sensores.barrio
    "
)


server <- function(input, output, session) {
  
    output$map <- renderLeaflet({

      leaflet() %>%
        addTiles() %>%
        addPolygons(
          data=mvd_map_fixed,
          weight = 5,
          opacity = 0.5,
          fill = TRUE,
          fillColor = ~colores(nacol(mvd_map_fixed)),
          popup = NULL,
          popupOptions = NULL,
          label = ~stringr::str_to_title(nombbarr) 
        ) %>% 
        addMarkers(
          data = st_transform(capaSemafotos, crs = 4326), 
          icon= semaforoIcon
        ) %>% 
        addMarkers(
          data = d_sensores, 
          lat = ~latitud, 
          lng = ~longitud,
          label = ~paste(dsc_avenida, " - ", dsc_int_anterior, " - ", dsc_int_siguiente)
        )
      
      })
    
    output$uni <- renderDataTable({
      velocidadxBarrioMax 

    })
    
    output$multi <- renderPlot({})
  }