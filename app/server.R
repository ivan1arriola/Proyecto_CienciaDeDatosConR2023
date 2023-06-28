
library(magrittr)
library(shiny)
library(leaflet)
source("utils.R")


### Cargar Mapa
map_file <- "downloaded_data/Barrios.shp"
if (file.exists(map_file)) {
  mvd_map <- sf::st_read(map_file)
} else {
  mvd_map <- geouy::load_geouy("Barrios")
  sf::st_write(mvd_map, map_file) 
}
mvd_map_fixed <- sf::st_make_valid(sf::st_transform(mvd_map, crs = 4326))



### Coneccion a la Base de datos
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS"),
  dbname = Sys.getenv("DB_NAME")
)


### Datos de los semaforos
capaSemafotos <- sf::st_read("v_int_semaforos/v_int_semaforos.shp")

### Datos de los sensores
sensores_file <- "downloaded_data/sensores.csv"
if (file.exists(sensores_file)) {
  d_sensores <- readr::read_csv(sensores_file)
} else {
  d_sensores <- DBI::dbGetQuery(
    con,
    "SELECT * FROM d_sensores"
  )
  readr::write_csv(d_sensores, sensores_file)
}

puntos_sensores <- d_sensores %>% 
  dplyr::select(barrio, latitud, longitud) %>%
  dplyr::mutate(transformarCoord(latitud, longitud, mvd_map))


### Velocidades Maximas detectadas por Barrio
velocidades_file <- "downloaded_data/velocidadxBarrioMax.csv"
if (file.exists(velocidades_file)) {
  velocidadxBarrioMax <- readr::read_csv(velocidades_file)
  
} else {
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
    readr::write_csv(velocidadxBarrioMax, velocidades_file)
  
}





server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = (-56.43151  + -56.02365 ) / 2, 
              lat = (-34.93811  + -34.70182) / 2, 
              zoom = 11)
    })
  
  observe({
    if ("semaforo" %in% input$variable) {
      leafletProxy("map") %>%
        clearGroup(group = 'Semaforo') %>%
        addMarkers(
          data = sf::st_transform(capaSemafotos, crs = 4326),
          icon = semaforoIcon,
          group = 'Semaforo'
        )
    } else leafletProxy("map") %>%
      clearGroup(group = 'Semaforo')
  })
  
  observe({
    if ("sensor" %in% input$variable) {
      leafletProxy("map") %>%
        clearGroup(group = 'Sensores') %>%
        addMarkers(
          data = d_sensores,
          lat = ~latitud,
          lng = ~longitud,
          label = ~dsc_avenida,
          group = 'Sensores'
        )
    } else {
      leafletProxy("map") %>%
        clearGroup(group = 'Sensores')
    }
  })
  
  
  observe({
    if ("barrio" %in% input$variable) {
      leafletProxy("map") %>%
        clearGroup(group = 'Barrios') %>%
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
    } else leafletProxy("map") %>%
      clearGroup(group = 'Barrios')
  })
  
  
  output$uni <- renderDataTable({
    velocidadxBarrioMax
  })
  
  output$multi <- renderPlot({})
}
