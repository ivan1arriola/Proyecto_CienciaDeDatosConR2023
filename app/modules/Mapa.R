# Interfaz ---------------------------------------------------------------------

mapa <- function(ns){
  box(
    title = "Mapa de Montevideo",
    status = "primary",
    width = 8, 
    solidHeader = TRUE,
    leafletOutput(
      ns("map"), 
      width = "100%", 
      height = "60vh"
    )
  )
}

checkboxes <- function(ns){
  box(
    title = "Controles Mapa",
    status = "info",
    width = 12,
    solidHeader = TRUE,
    checkboxInput(inputId = ns("valor_semaforo"),
                  label = "Semaforos",
                  value = FALSE),
    
    checkboxInput(inputId = ns("valor_barrio"),
                  label = "Barrios",
                  value = FALSE),
    
    checkboxInput(inputId = ns("valor_sensor"),
                  label = "Sensores",
                  value = FALSE),
  )
}

conditional_panel_sensor <- function(ns) {
  conditionalPanel(
    condition = paste0("input['", ns("valor_sensor"), "'] == 1"),
    box(
      title = "Opciones para Sensor",
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
}

conditional_panel_barrio <- function(ns) {
  conditionalPanel(
    condition = paste0("input['", ns("valor_barrio"), "'] == 1"),
    box(
      title = "Opciones para Barrio",
      selectInput(
        ns("barrio_color"), 
        "Colorear Barrio según:",
        c(
          "Cantidad de infracciones" = "infracciones",
          "Velocidad Maxima registrada" = "velocidadMax",
          "Cantidad de Sensores" = "sensor"
        )
      )
    )
  )
}

mapa_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    mapa(ns),
    fluidRow(
        column(
          width = 4,
          h1("Controles"),
          checkboxes(ns),
          conditional_panel_sensor(ns),
          conditional_panel_barrio(ns)
        )
      )
    )
}



# Servidor ---------------------------------------------------------------------
mapa_server <- function(input, output, session) {
  
  addPolygonsToMap <- function(data, fillColor) {
    leafletProxy("map") %>%
      clearGroup(group = 'Barrios') %>% 
      addPolygons(
        data = data,
        weight = 5,
        opacity = 0.5,
        fill = TRUE,
        fillColor = fillColor,
        popup = NULL,
        popupOptions = NULL,
        label = ~stringr::str_to_title(nombbarr),
        group = 'Barrios'
      )
  }
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(
        lng = (-56.43151  + -56.02365 ) / 2, 
        lat = (-34.93811  + -34.70182) / 2, 
        zoom = 11
      )
  })
  
  observe({
    if (input$valor_semaforo) {
      leafletProxy("map") %>%
        addMarkers(
          data = sf::st_transform(capaSemafotos, crs = 4326),
          icon = semaforoIcon,
          group = 'Semaforo'
        )
    } else {
      leafletProxy("map") %>%
        clearGroup(group = 'Semaforo')
    }
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
      if (input$barrio_color == "volumendMax") {
        map_mvd_max <- inner_join(
          mvd_map_fixed,
          registros_max_barrioxdiaxhora,
          by = c("nombbarr" = "barrio")
        )
        
        fillColor <- ~colorNumeric(
          palette = "Reds",
          domain = map_mvd_max$max_velocidad
        )(max_velocidad)
        
        addPolygonsToMap(map_mvd_max, fillColor)
      } else {
        fillColor <- ~colores(nacol(mvd_map_fixed))
        addPolygonsToMap(mvd_map_fixed, fillColor)
      }
    } else {
      leafletProxy("map") %>%
        clearGroup(group = 'Barrios')
    }
  })
  
}


print("Mapa.R Loaded")

