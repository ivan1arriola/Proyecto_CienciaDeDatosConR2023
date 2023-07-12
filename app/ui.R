mapa <- function() {
    leafletOutput(
      "map",
      width = "100%"
    )
}

grafico <- function() {
    plotOutput(
      "barras"
    )
}





fluidPage(
  titlePanel("Datos de Trafico"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tab == 'Mapa'",
        checkboxGroupInput(
          "aMostrar",
          "Variables a mostrar",
          choices = c("Barrio" = "barrio", "Sensor" = "sensor")
        ),
        sliderInput(
          "hora",
          "Seleccione una hora",
          min = 0,
          max = 23,
          value = 0,
          step = 1
        )
      ),
      conditionalPanel(
        condition = "input.tab == 'Gráficos'",
        selectInput(
        "barrioSeleccionado",
        "Seleccione un barrio",
        choices = lista_barrios,
        selected = "Aguada"
      )
      )
      ,
      radioButtons(
        "variable",
        "Seleccione una variable",
        choices = c("Volumen Promedio" = "vol", "Velocidad Promedio" = "vel", "Velocidad Máxima" = "vel_max",
                    "Volumen Máximo" = "vol_max"),
        selected = "vol"
      ),
      radioButtons(
        "dia_de_la_semana",
        "Seleccione un día de la semana",
        choices = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo"),
        selected = "Lunes"
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tab",
        tabPanel("Mapa",
          h2("Mapa de Montevideo", align = "center"),
          mapa()
        ),
        tabPanel("Gráficos",
          h2("Gráfico de Barras", align = "center"),
          grafico()
        )
      )
    )
  )
)

