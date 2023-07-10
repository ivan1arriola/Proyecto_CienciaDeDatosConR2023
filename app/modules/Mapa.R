lista_barrios <- d_sensores %>%
    select(barrio) %>%
    distinct() %>%
    arrange(barrio) %>%
    pull()

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

plot <- function(ns) {
  box(
      title = "Grafico de barras de volumen promedio por rango horario",
      status = "primary",
      solidHeader = TRUE,
      width = 8,
      plotOutput(ns("barras"))
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

opciones <- function(ns) {
  box(
    title = "Opciones",
    status = "info",
    collapsible = TRUE,
    width = NULL,
    solidHeader = TRUE,

    # Quiero poder elegir un barrio, un dia de la semana , una hora y variable a mostrar
    # - Barrio: selectInput
    # - Dia de la semana: radioButtons
    # - Una hora: sliderInput
    # - Variable a mostrar: radioButtons

    selectInput(
      ns("barrio"),
      "Seleccione un barrio",
      choices = lista_barrios,
      selected = "Aguada"
    ),

    radioButtons(
      ns("dia_de_la_semana"),
      "Seleccione un dia de la semana",
      choices = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"),
      selected = "Lunes"
    ),

    sliderInput(
      ns("hora"),
      "Seleccione una hora",
      min = 0,
      max = 23,
      value = 0,
      step = 1
    ),

    radioButtons(
      ns("variable"),
      "Seleccione una variable",
      choices = c("Volumen" = "vol", "Velocidad" = "vel"),
      selected = "vol"
    ),
  )
  
}




mapa_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    mapa(ns),
    plot(ns),
    fluidRow(
      column(
        width = 4,
        h1("Controles"),
        checkboxes(ns),
        opciones(ns)

        
      )
    )
  )
}



# Servidor ---------------------------------------------------------------------


#> names(registros_max_barrioxdiaxhora)
# [1] "barrio"           "day_of_week"      "hora_rango"       "max_velocidad"    "max_volumen"      "cant_registros"   "dia_de_la_semana"



generar_mapa <- function(dia_semana, rango_hora) {
    print("Generando mapa")
    print(dia_semana)
    print(rango_hora)
    registros_filtrados <- registros_max_barrioxdiaxhora %>%
      filter(dia_de_la_semana == dia_semana & hora_rango == strptime(rango_hora, format = "%H:%M")) %>%
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

  addMarkersToMap <- function(data, fillColor) {
    leafletProxy("map") %>%
      clearGroup(group = "Sensores") %>%
      addCircleMarkers(
        data = data,
        lng = ~longitud,
        lat = ~latitud,
        weight = 5,
        radius = ~promedio_volumen/2,
        opacity = 0.5,
        fill = TRUE,
        fillColor = fillColor,
        popup = NULL,
        popupOptions = NULL,
        group = "Sensores"
      )
  }
mapa_server <- function(input, output, session) {
  

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
      dia_semana <- input$dia_de_la_semana
      rango_hora <- intervalos[input$hora + 1]      
      variable <- input$variable

      if (variable == "vol") {

        registros_filtrados <- registros_max_barrioxdiaxhora_sensor %>%
          filter(dia_de_la_semana == dia_semana & hora_rango == strptime(rango_hora, format = "%H:%M")) %>% 
          left_join(d_sensores, by = c(latitud = "latitud", longitud = "longitud"))

        fillColor <- ~ colorNumeric(
          palette = "Reds",
          domain = registros_filtrados$max_volumen 
        )(max_volumen)

        addMarkersToMap(registros_filtrados, fillColor)


      } else if (variable == "vel"){
        registros_filtrados <- registros_max_barrioxdiaxhora_sensor %>%
          filter(dia_de_la_semana == dia_semana & hora_rango == strptime(rango_hora, format = "%H:%M"))%>% 
          left_join(d_sensores, by = c(latitud = "latitud", longitud = "longitud"))

        fillColor <- ~ colorNumeric(
          palette = "Reds",
          domain = registros_filtrados$max_velocidad 
        )(max_velocidad)
        addMarkersToMap(registros_filtrados, fillColor)
      }




    } else {
      leafletProxy("map") %>%
        clearGroup(group = "Sensores")
    }
  })

  observe({
    if (input$valor_barrio) {
      dia_semana <- input$dia_de_la_semana
      rango_hora <- intervalos[input$hora + 1]      
      print ( paste ( "dia_semana: ", dia_semana, " rango_hora: ", rango_hora, " variable: ", input$variable ) )
      map_mvd_max <- generar_mapa(dia_semana, rango_hora)
      map_mvd_max <- st_as_sf(map_mvd_max)
      if (input$variable == "vol") {
        fillColor <- ~ colorNumeric(
          palette = "Reds",
          domain = map_mvd_max$max_volumen 
        )(max_volumen)
        addPolygonsToMap(map_mvd_max, fillColor)
      } else if (input$variable == "vel") {
        fillColor <- ~ colorNumeric(
          palette = "Reds",
          domain = map_mvd_max$max_velocidad
        )(max_velocidad)
        addPolygonsToMap(map_mvd_max, fillColor)
      } else if (input$variable == "barrio"){
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


  ##------------------------------

  v <- reactiveValues(data = NULL)
  f <- reactiveValues(data = NULL)

  max_avg_volumen <- registros_max_barrioxdiaxhora %>%
    select(promedio_volumen) %>%
    pull() %>%
    max()


  observeEvent(input$variable, {
    if (input$variable == "vol") {
      f$data <- registros_max_barrioxdiaxhora %>%
        mutate(variable = promedio_volumen)
    } else if (input$variable == "vel") {
      f$data <- registros_max_barrioxdiaxhora %>%
        mutate(variable = promedio_velocidad)
    }
  })
  
  observeEvent(c(input$variable, input$barrio, input$dia_de_la_semana), {

    if (input$variable == "vol") {
      v$data <- registros_max_barrioxdiaxhora %>%
        mutate(variable = promedio_volumen) %>% 
        filter(barrio == input$barrio & dia_de_la_semana == input$dia_de_la_semana)
    } else if (input$variable == "vel") {
      v$data <- registros_max_barrioxdiaxhora %>%
        mutate(variable = promedio_velocidad) %>% 
        filter(barrio == input$barrio & dia_de_la_semana == input$dia_de_la_semana)
    }
      
  })

  output$barras <- renderPlot({
    if (is.null(v$data)) {
      return()
    }
    v$data %>% ggplot( aes(x = hora_rango, y = variable)) +
      geom_col() +
      scale_x_discrete(
        name = "Rango horario"
      ) +
      scale_y_continuous(
        name = "Promedio",
        limits = c(0, max_avg_volumen)
        ) 
  })

}


print("Mapa.R Loaded")
