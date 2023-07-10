server <- function(input, output, session) {
  
  callModule(mapa_server, "mapa_module")
  callModule(univariado_server, "univariado_module")
  
  output$multi <- renderPlot({})
}
