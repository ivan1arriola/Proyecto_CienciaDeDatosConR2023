#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS"),
  dbname = Sys.getenv("DB_NAME")
)
server <- function(input, output, session) {
  
    output$map <- renderPlot({
      (
        mvd_map_velocidad_media <- mvd_map %>%
          left_join(velocidadxBarrioMax,
                    by = c("NOMBBARR" = "barrio"))
      )
      
      mvd_map_velocidad_media %>%
        ggplot(aes(fill = promedio_velocidad)) + 
        geom_sf(colour = "grey75", size = 0.07) +
        scale_fill_paletteer_c("grDevices::Heat")
    })
    
    output$uni <- renderDataTable({
      promedios_velocidad_marzo23 %>% 
        select(dsc_avenida,velocidad) %>% 
        group_by(dsc_avenida) %>% 
        summarise(
          cantMayorALimite = sum(velocidad > 60)
        ) %>% arrange(desc(cantMayorALimite))

    })
    
    output$multi <- renderPlot({})
  }