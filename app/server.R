
library(magrittr)
library(shiny)
library(leaflet)
library(geouy)
source("utils.R")


con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS"),
  dbname = Sys.getenv("DB_NAME")
)

d_sensores <- DBI::dbGetQuery(
  con,
  "SELECT * FROM d_sensores"
)

map_file <- "downloaded_data/Barrios.shp"

if (file.exists(map_file)) {
  mvd_map <- sf::st_read(map_file)
} else {
  mvd_map <- load_geouy("Barrios")
  sf::st_write(mvd_map, map_file)  # Guardar el mapa en un archivo
}

mvd_map_fixed <- sf::st_make_valid(st_transform(mvd_map, crs = 4326))


puntos_sensores <- d_sensores %>% 
  dplyr::select(barrio, latitud, longitud) %>%
  dplyr::mutate(transformarCoord(latitud, longitud, mvd_map))

capaSemafotos <- sf::st_read("v_int_semaforos/v_int_semaforos.shp")


velocidadxBarrioMax <- tryCatch( 
  { 
    readr::read_csv('downloaded_data/velocidadxBarrioMax.csv') 
  }, 
  warning = function(w) {
    return(NA) 
  }, error = function(e) { 
    message(e) 
    DBI::dbGetQuery( 
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
  }, finally = { 
    DBI::dbDisconnect(con) 
  } 
)




server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles()
  })
  
  observe({
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers()
    
    if ("semaforo" %in% input$variable) {
      leafletProxy("map") %>%
        addMarkers(
          data = st_transform(capaSemafotos, crs = 4326),
          icon = semaforoIcon,
          group = 'Semaforo'
        )
    }
    
    if ("sensor" %in% input$variable) {
      leafletProxy("map") %>%
        addMarkers(
          data = d_sensores,
          lat = ~latitud,
          lng = ~longitud,
          label = ~paste(dsc_avenida, " - ", dsc_int_anterior, " - ", dsc_int_siguiente),
          group = 'Sensores'
        )
    }
    
    if ("barrio" %in% input$variable) {
      leafletProxy("map") %>%
        addPolygons(
          data = mvd_map_fixed,
          weight = 5,
          opacity = 0.5,
          fill = TRUE,
          fillColor = ~colores(nacol(mvd_map_fixed)),
          popup = NULL,
          popupOptions = NULL,
          label = ~stringr::str_to_title(nombbarr),
          group = 'Barrios'
        )
    }
  })
  
  output$uni <- renderDataTable({
    velocidadxBarrioMax
  })
  
  output$multi <- renderPlot({})
}
