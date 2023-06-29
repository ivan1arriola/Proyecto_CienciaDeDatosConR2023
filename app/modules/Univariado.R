univariado_ui  <- function(id){
  ns <- NS(id)
  fluidRow(
    dataTableOutput(ns("uni"))
  )
  
}

univariado_server  <- function(input, output, session) {
  output$uni <- renderDataTable({
    velocidadxBarrioMax
    print(velocidadxBarrioMax)
  })
  
}

print("Tabla.R Loaded")