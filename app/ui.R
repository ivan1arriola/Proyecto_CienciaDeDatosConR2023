

header <- dashboardHeader(
    title = "Trafico UY"
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Mapa", tabName = "mapa", icon = icon("map"))
        
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

