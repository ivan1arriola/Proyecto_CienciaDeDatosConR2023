#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(geouy)
library(paletteer)


mvd_map <- load_geouy('Barrios')

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS"),
  dbname = Sys.getenv("DB_NAME")
)

velocidadxBarrioMax <- DBI::dbGetQuery(
  con,
  "
    SELECT
      d_sensores.barrio,
      MAX(fct_registros.velocidad) AS max_velocidad
    FROM fct_registros
    LEFT JOIN d_sensores ON fct_registros.id_detector = d_sensores.id_detector
    GROUP BY d_sensores.barrio
    "
)


server <- function(input, output, session) {
  
    output$map <- renderPlot({
      (
        mvd_map_velocidad_media <- mvd_map %>%
          left_join(velocidadxBarrioMax,
                    by = c("nombbarr" = "barrio"))
      )
      
      mvd_map_velocidad_media %>%
        ggplot(aes(fill = max_velocidad)) + 
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