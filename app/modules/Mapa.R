# Interfaz ---------------------------------------------------------------------

mapa <- function(ns) {
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

checkboxes <- function(ns) {
  box(
    title = "Controles Mapa",
    status = "info",
    collapsible = TRUE,
    width = NULL,
    solidHeader = TRUE,
    checkboxInput(
      inputId = ns("valor_semaforo"),
      label = "Semaforos",
      value = FALSE
    ),
    checkboxInput(
      inputId = ns("valor_barrio"),
      label = "Barrios",
      value = FALSE
    ),
    checkboxInput(
      inputId = ns("valor_sensor"),
      label = "Sensores",
      value = FALSE
    ),
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
      width = NULL,
      selectInput(
        ns("barrio_color"),
        "Colorear Barrio según:",
        c(
          "Velocidad Maxima registrada" = "velocidadMax",
          "Volumen Max x Hora" = "volumenMax",
          "Nombre del Barrio" = "barrio"
        ),
        selected = "volumenMax"

      ),
      selectInput(
        ns("hora"),
        "Seleccione un rango de horas",
        c(
          "00:00 - 06:00",
          "06:01 - 12:00",
          "12:01 - 18:00",
          "18:01 - 23:59"
        ),
        selected = "00:00 - 06:00"
      ),
      selectInput(
        ns("dia"),
        "Seleccione un día de la semana",
        choices = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"),
        selected = "Lunes"
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


#> names(registros_max_barrioxdiaxhora)
# [1] "barrio"           "day_of_week"      "hora_rango"       "max_velocidad"    "max_volumen"      "cant_registros"   "dia_de_la_semana"




mapa_server <- function(input, output, session) {
  generar_mapa <- function(dia_semana, rango_hora) {
    registros_filtrados <- registros_max_barrioxdiaxhora %>%
      filter(dia_de_la_semana == dia_semana) %>%
      filter(hora_rango == rango_hora) %>%
      select(barrio, max_velocidad, max_volumen, cant_registros) %>%
      inner_join(mvd_map_fixed, by = c("barrio" = "nombbarr")) %>%
      select(barrio, max_velocidad, max_volumen, cant_registros, geometry)

    return(registros_filtrados)
  }


  addPolygonsToMap <- function(data, fillColor) {
    leafletProxy("map") %>%
      clearGroup(group = "Barrios") %>%
      addPolygons(
        data = data,
        weight = 5,
        opacity = 0.5,
        fill = TRUE,
        fillColor = fillColor,
        popup = NULL,
        popupOptions = NULL,
        group = "Barrios"
      )
  }

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(
        lng = (-56.43151 + -56.02365) / 2,
        lat = (-34.93811 + -34.70182) / 2,
        zoom = 11
      )
  })

  observe({
    if (input$valor_semaforo) {
      leafletProxy("map") %>%
        addMarkers(
          data = sf::st_transform(capaSemafotos, crs = 4326),
          icon = semaforoIcon,
          group = "Semaforo"
        )
    } else {
      leafletProxy("map") %>%
        clearGroup(group = "Semaforo")
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
          group = "Sensores"
        )
    } else {
      leafletProxy("map") %>%
        clearGroup(group = "Sensores")
    }
  })

  observe({
    if (input$valor_barrio) {
      dia_semana <- input$dia
      if (is.null(dia_semana)) {
        dia_semana <- "Lunes"
      }
      print(dia_semana)
    
      rango_hora <- input$hora
      if (is.null(rango_hora)) {
        rango_hora <- "00:00 - 06:00"
      }
      print(rango_hora)
      
      map_mvd_max <- generar_mapa(dia_semana, rango_hora)
      map_mvd_max <- st_as_sf(map_mvd_max)

      print("map_mvd_max")
      
      if (input$barrio_color == "volumenMax") {
      print("volumenMax")
        
        fillColor <- ~ colorNumeric(
          palette = "Reds",
          domain = map_mvd_max$max_volumen 
        )(max_volumen)
        addPolygonsToMap(map_mvd_max, fillColor)
        
      } else if (input$barrio_color == "velocidadMax") {
        print("velocidadMax")

        fillColor <- ~ colorNumeric(
          palette = "Reds",
          domain = map_mvd_max$max_velocidad
        )(max_velocidad)

        addPolygonsToMap(map_mvd_max, fillColor)
        
      } else if (input$barrio_color == "barrio"){
        print("barrio")
        fillColor <- ~ colores(nacol(mvd_map_fixed))
        addPolygonsToMap(mvd_map_fixed, fillColor)
      } else {
        print("No se selecciono nada")
      }
    } else {
      leafletProxy("map") %>%
        clearGroup(group = "Barrios")
    }
  })
}


print("Mapa.R Loaded")


# resumir la informacion de los sensores
#> names(registros_max_barrioxdiaxhora)
# [1] "barrio"           "day_of_week"      "hora_rango"       "max_velocidad"    "max_volumen"      "cant_registros"   "dia_de_la_semana"


# > names(mvd_map_fixed)
# [1] "gml_id"    "area_km"   "nombbarr"  "nrobarrio" "geometry"
