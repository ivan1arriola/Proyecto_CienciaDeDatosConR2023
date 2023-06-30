univariado_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    
    box(
      title = "Boxplot",
      status = "info",
      width = 12,
      selectInput(
        ns("boxplot_dia"),
        "Seleccione un día de la semana",
        choices = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"),
        selected = "Lunes"
      ),
      selectInput(
        ns("boxplot_hora"),
        "Seleccione un rango de horas",
        c(
          "00:00 - 06:00",
          "06:01 - 12:00",
          "12:01 - 18:00",
          "18:01 - 23:59"
        ),
        selected = "00:00 - 06:00"
      ),
      plotOutput(ns("boxplot"))
    ),
    box(
      title = "Tabla de datos",
      status = "primary",
      width = 10, 
      solidHeader = TRUE,
      collapsible = TRUE,
      dataTableOutput(ns("uni"))
    )
  )
}


univariado_server  <- function(input, output, session) {
  output$uni <- renderDataTable({
    registros_max_barrioxdiaxhora %>% select(-day_of_week)
  })

  output$boxplot <- renderPlot({
    registros_filtrados <- registros_max_barrioxdiaxhora %>%
      filter(dia_de_la_semana == input$boxplot_dia) %>%
      filter(hora_rango == input$boxplot_hora) %>%
      select(barrio, max_velocidad, max_volumen, cant_registros) %>%
      inner_join(mvd_map_fixed, by = c("barrio" = "nombbarr")) %>%
      select(barrio, max_velocidad, max_volumen, cant_registros, geometry)
    
    print(registros_filtrados)

    ggplot(registros_filtrados) +
      geom_boxplot(aes(x = barrio, y = max_velocidad)) +
      labs(
        title = "Boxplot de velocidad máxima por barrio",
        x = "Barrio",
        y = "Velocidad máxima"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
}

print("Tabla.R Loaded")