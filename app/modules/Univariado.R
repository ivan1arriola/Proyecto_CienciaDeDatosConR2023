univariado_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = "Tabla de datos",
      status = "primary",
      width = 10, 
      solidHeader = TRUE,
      collapsible = TRUE,
      dataTableOutput(ns("uni"))
    ),
    box()
  )
}


univariado_server  <- function(input, output, session) {
  output$uni <- renderDataTable({
    registros_max_barrioxdiaxhora %>% select(-day_of_week)
  })
  
}

print("Tabla.R Loaded")