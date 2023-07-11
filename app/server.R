server <- function(input, output, session) {

  moduleServer(
    id = "mapa_module",
    module = mapa_server,
    valor_barrio = reactive(input$valor_barrio),
    valor_sensor = reactive(input$valor_sensor),
  )

  moduleServer(
    id = "univariado_module",
    module = univariado_server
  )
  
  output$multi <- renderPlot({})
}
