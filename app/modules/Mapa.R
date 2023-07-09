# Interfaz ---------------------------------------------------------------------
intervalos <- c(
  '00:00',
  '01:00',
  '02:00',
  '03:00',
  '04:00',
  '05:00',
  '06:00',
  '07:00',
  '08:00',
  '09:00',
  '10:00',
  '11:00',
  '12:00',
  '13:00',
  '14:00',
  '15:00',
  '16:00',
  '17:00',
  '18:00',
  '19:00',
  '20:00',
  '21:00',
  '22:00',
  '23:00'
)


mapa <- function(ns) {
  box(
    title = "Mapa de Montevideo",
    status = "primary",
    width = 8,
    solidHeader = TRUE,
    leafletOutput(
      ns("map"),
      width = "100%",
      height = "80vh"
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
          "Promedio de volumen x hora" = "avg_volumen",
          "Nombre del Barrio" = "barrio"
        ),
        selected = "barrio"

      ),
      sliderInput(
        inputId =  ns("hora"),
        label = "Seleccione un rango de horas",
        min = 0,
        max = length(intervalos) - 1,
        value = 0,
        step = 1
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

    print(registros_filtrados)

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

      rango_hora <- intervalos[input$hora + 1]

      print(rango_hora)
      

      map_mvd_max <- generar_mapa(dia_semana, rango_hora)
      map_mvd_max <- st_as_sf(map_mvd_max)

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
