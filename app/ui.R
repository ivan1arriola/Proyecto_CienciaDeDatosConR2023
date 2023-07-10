

header <- dashboardHeader(
    title = "Trafico UY"
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Mapa", tabName = "mapa", icon = icon("map")),
        menuItem("Univariado", tabName = "univariado", icon = icon("chart-bar")),
        menuItem("Multivariado", tabName = "multivariado", icon = icon("chart-pie")),
        menuItem("Modelo", tabName = "modelos", icon = icon("chart-line"))
        
    )
)

body <- dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        tabItem(
            tabName = "mapa",
            fluidRow(
                mapa_ui("mapa_module")
            )
        ),
        
        tabItem(
            tabName = "univariado",
            fluidRow(
                univariado_ui("univariado_module"),
            )
        ),
        
        tabItem(
            tabName = "multivariado",
            fluidRow(
                multivariado_ui("multivariado_module")
            )
        ),

        tabItem(
            tabName = "modelo",
            fluidRow(
                plotOutput('modelo')
            )
        )
    )
)

ui <- dashboardPage(
    skin = "black",
    header,
    sidebar,
    body
)

