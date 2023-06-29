# Interfaz ---------------------------------------------------------------------

mapa_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      leafletOutput(ns("map"), width = "100%", height = "60vh")
    ),
    fluidRow(
      box(
        h1("Controles"),
        checkboxInput(inputId = ns("valor_semaforo"),
                      label = "Semaforos",
                      value = FALSE),
        
        checkboxInput(inputId = ns("valor_barrio"),
                      label = "Barrios",
                      value = FALSE),
        
        checkboxInput(inputId = ns("valor_sensor"),
                      label = "Sensores",
                      value = FALSE),
        
        conditionalPanel(
          condition = paste0("input['", id, "-valor_sensor'] == 1"), #Esta es la unica forma en la que anda...
          h2("Opciones para Sensor"),
            selectInput(
              ns("sensor_color"), 
              "Colorear sensor segun:",
              c(
                "Semaforos" = "semaforo",
                "Barrios" = "barrio",
                "Sensores" = "sensor"
                )
            
            )
        )
      )
      
    )
  )
}



# Servidor ---------------------------------------------------------------------
mapa_server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = (-56.43151  + -56.02365 ) / 2, 
              lat = (-34.93811  + -34.70182) / 2, 
              zoom = 11)
    
  })
  
  observe({
    if (input$valor_semaforo) {
      leafletProxy("map") %>%
        addMarkers(
          data = sf::st_transform(capaSemafotos, crs = 4326),
          icon = semaforoIcon,
          group = 'Semaforo'
        )
    } else leafletProxy("map") %>%
      clearGroup(group = 'Semaforo')
  })
  
  observe({
    if (input$valor_sensor) {
      leafletProxy("map") %>%
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
    if (input$valor_barrio) {
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
    } else leafletProxy("map") %>%
      clearGroup(group = 'Barrios')
  })
}

print("Mapa.R Loaded")

