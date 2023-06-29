

header <- dashboardHeader(
    title = "Trafico UY"
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Mapa", tabName = "mapa", icon = icon("map")),
        menuItem("Univariado", tabName = "univariado", icon = icon("chart-bar")),
        menuItem("Multivariado", tabName = "multivariado", icon = icon("chart-pie"))
        
    )
)

body <- dashboardBody(
    tags$style("#variable { margin: 20px; } .content {margin-left: 2rem"),
    
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
                plotOutput('multi')
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
