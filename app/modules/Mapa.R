mapa_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    leafletOutput(ns("map"), width = "100%", height = "60vh"),
    checkboxGroupInput(ns("variable"), "Variables a Mostrar:",
                       c("Semaforos" = "semaforo",
                         "Barrios" = "barrio",
                         "Sensores" = "sensor"))
  )
}


mapa_server <- function(input, output, session) {
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
}

print("Mapa.R Loaded")

