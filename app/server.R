

#### SERVIDOR ####
server <- function(input, output, session) {
  #Datos por barrio

datos <- reactiveValues(
  datos = NULL,
  nombre_variable = NULL,
  maximo = NULL,
  mapa = NULL
)

lookup <- list(
    vol = list(nombre_variable = "Volumen Promedio", columna_datos = "promedio_volumen"),
    vel = list(nombre_variable = "Velocidad Promedio", columna_datos = "promedio_velocidad"),
    vel_max = list(nombre_variable = "Velocidad máxima", columna_datos = "max_velocidad"),
    vol_max = list(nombre_variable = "Volumen máximo", columna_datos = "max_volumen")
)

observeEvent(c(input$variable, input$barrioSeleccionado, input$dia_de_la_semana), {
  ## Datos para el gráfico
  if (input$variable %in% names(lookup)) {
    datos$datos <- registros_max_barrioxdiaxhora %>%
      select(barrio, dia_de_la_semana, hora_rango, !!sym(lookup[[input$variable]]$columna_datos)) %>%
      mutate(
        variable = !!sym(lookup[[input$variable]]$columna_datos),
        hora_rango = as.character(hora_rango) %>% substr(1, 5)
      ) %>% 
      filter(barrio == input$barrioSeleccionado & dia_de_la_semana == input$dia_de_la_semana)
    
    datos$nombre_variable <- lookup[[input$variable]]$nombre_variable
    
    datos$maximo <- registros_max_barrioxdiaxhora %>%
      select(!!sym(lookup[[input$variable]]$columna_datos)) %>%
      pull() %>%
      max()
  }
})

observeEvent(c(input$variable, input$dia_de_la_semana, input$hora), {
    ## Datos para el mapa
    if (input$variable %in% names(lookup)) {
    datos$mapa <- registros_max_barrioxdiaxhora %>%
    mutate(
        variable = !!sym(lookup[[input$variable]]$columna_datos),
        hora_rango = as.character(hora_rango) %>% substr(1, 5)
      ) %>% 
      select(barrio, dia_de_la_semana, hora_rango, !!sym(lookup[[input$variable]]$columna_datos), variable) %>%
      filter(hora_rango == intervalos[input$hora +1] & dia_de_la_semana == input$dia_de_la_semana) %>% 
      inner_join(mvd_map_fixed, by = c("barrio" = "nombbarr"))

    print(head(datos$mapa))
  }    
})



  #------------------------------------------------------
  # Mapa

output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(
        lng = (-56.43151 + -56.02365) / 2,
        lat = (-34.93811 + -34.70182) / 2,
        zoom = 12
      )
})
  
mostrar <- reactiveValues( 
    sensores = FALSE,
    barrios = FALSE
)

observe({
    mostrar$sensores <- "sensor" %in% input$aMostrar
    mostrar$barrios <- "barrio" %in% input$aMostrar
})

observeEvent( mostrar$sensores, {
  if (mostrar$sensores) {
    print ("Mostrando sensores")
  }
  else {
    print ("Ocultando sensores")
  }
})

observeEvent( c(mostrar$barrios , input$variable, input$barrioSeleccionado, input$dia_de_la_semana, input$hora), {
  if (mostrar$barrios) {
    print ("Mostrando barrios")
    req(datos$mapa)

    pal <- colorNumeric(
      palette = "Blues",
      domain = datos$mapa$variable
    )
    
    leafletProxy("map") %>%
      clearGroup(group = "Barrios") %>%
      addPolygons(
        data = st_as_sf(datos$mapa),
        group = "Barrios",
        fillColor = ~pal(variable),
        fillOpacity = 0.5,
        weight = 1,
        color = "black",
        label = ~barrio,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )
  }
  else {
    print ("Ocultando barrios")
    leafletProxy("map") %>%
      clearGroup(group = "Barrios")
  }
})


   
#------------------------------------------------------
# Grafico




  
output$barras <- renderPlot({
    
    print("Generando grafico")
    req(datos$datos)
    datos$datos %>% ggplot( aes(x = hora_rango, y = variable)) +
      geom_col(
        aes(
          fill = variable
        )
      ) +
      scale_x_discrete(
        name = "Hora",
        labels = intervalos
      )+
      scale_y_continuous(
        name = datos$nombre_variable,
        limits = c(0, datos$maximo)
      ) +
      geom_text(
        aes(
          label = round(variable, 2)
        ), 
        vjust = -0.5
      ) +
      theme_bw() +
      theme(
        legend.position = "none"
      )
    
})


}





