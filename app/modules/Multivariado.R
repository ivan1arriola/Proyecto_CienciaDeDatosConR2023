multivariado_ui <- function(id) {
  ns <- NS(id)
  lista_barrios <- d_sensores %>% 
    dplyr::select(barrio) %>%
    dplyr::distinct() %>%
    dplyr::pull()


  fluidRow(
    box(
      title = "Seleccione un barrio",
      status = "primary",
      solidHeader = TRUE,
      width = 4,
      selectInput(
        ns("barrio"),
        "Seleccione un barrio",
        choices = lista_barrios,
        selected = "Aguada"
      )
    ),

    box(
      title = "Seleccione un dia de la semana",
      status = "primary",
      solidHeader = TRUE,
      width = 4,
      selectInput(
        ns("dia_de_la_semana"),
        "Seleccione un dia de la semana",
        choices = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"),
        selected = "Lunes"
      )
    ),

    box(
      title = "Grafico de barras",
      status = "primary",
      solidHeader = TRUE,
      width = 8,
      plotOutput(ns("barras"))
    )
  )
    
  
}


multivariado_server  <- function(input, output, session) {

  v <- reactiveValues(data = NULL)

  max_avg_volumen <- registros_max_barrioxdiaxhora %>%
    dplyr::select(promedio_volumen) %>%
    dplyr::pull() %>%
    max()
  
  observeEvent(input$barrio, {
    v$data <- registros_max_barrioxdiaxhora %>%
      filter(barrio == input$barrio & dia_de_la_semana == input$dia_de_la_semana)
  })

  observeEvent(input$dia_de_la_semana, {
    v$data <- registros_max_barrioxdiaxhora %>%
      filter(dia_de_la_semana == input$dia_de_la_semana & barrio == input$barrio)
  })

  output$barras <- renderPlot({
    if (is.null(v$data)) {
      return()
    }
    ggplot(v$data, aes(x = hora_rango, y = promedio_volumen)) +
      geom_col() +
      scale_x_discrete(
        name = "Rango horario"
      ) +
      scale_y_continuous(
        name = "Volumen promedio",
        limits = c(0, max_avg_volumen)
        ) 
  })

  
}