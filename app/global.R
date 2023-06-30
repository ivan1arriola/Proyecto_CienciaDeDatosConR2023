print("global.R")

# Directorios -------------------------------------------------------------

dataDir <- "data"
moduleDir <- "modules"

# Paquetes ----------------------------------------------------------------
library(magrittr)
library(shiny)
library(leaflet)
library(bslib)
library(shinydashboard)


# Cargar modulos ----------------------------------------------------------

source(paste0(moduleDir, "/Mapa.R"))
source(paste0(moduleDir, "/Univariado.R"))
source("utils.R")


# Cargar Datos ------------------------------------------------------------

### Cargar Mapa
map_file <- paste0(dataDir, "/Barrios.shp")
if (file.exists(map_file)) {
  mvd_map <- sf::st_read(map_file)
} else {
  mvd_map <- geouy::load_geouy("Barrios")
  sf::st_write(mvd_map, map_file) 
}
mvd_map_fixed <- sf::st_make_valid(sf::st_transform(mvd_map, crs = 4326))

#### colores del mapa
colores <- leaflet::colorFactor(
  palette = "Set1", 
  domain = nacol(sf::st_make_valid(sf::st_transform(mvd_map, crs = 4326)))
)



### Coneccion a la Base de datos
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS"),
  dbname = Sys.getenv("DB_NAME")
)


### Datos de los semaforos
capaSemafotos <- sf::st_read(paste0(dataDir, "/v_int_semaforos/v_int_semaforos.shp"))

### Datos de los sensores
sensores_file <- paste0(dataDir, "/sensores.csv")
if (file.exists(sensores_file)) {
  d_sensores <- readr::read_csv(sensores_file)
} else {
  d_sensores <- DBI::dbGetQuery(
    con,
    "SELECT * FROM d_sensores"
  )
  readr::write_csv(d_sensores, sensores_file)
}

puntos_sensores <- d_sensores %>% 
  dplyr::select(barrio, latitud, longitud) %>%
  dplyr::mutate(transformarCoord(latitud, longitud, mvd_map))


### registros maximos por rango hora, dia de la semana y barrio
registros_max_file <- paste0(dataDir, "/registros_max_file.csv")
if (file.exists(registros_max_file)) {
  registros_max_barrioxdiaxhora <- readr::read_csv(registros_max_file)
  
} else {
  registros_max_barrioxdiaxhora <- DBI::dbGetQuery(
    con,
    "
      SELECT
        d_sensores.barrio,
        d_date.day_of_week,
        CASE
            WHEN fct_registros.id_hora >= 0 AND fct_registros.id_hora <= 600 THEN '00:00 - 06:00'
            WHEN fct_registros.id_hora > 600 AND fct_registros.id_hora <= 1200 THEN '06:01 - 12:00'
            WHEN fct_registros.id_hora > 1200 AND fct_registros.id_hora <= 1800 THEN '12:01 - 18:00'
            WHEN fct_registros.id_hora > 1800 AND fct_registros.id_hora <= 2359 THEN '18:01 - 23:59'
            ELSE 'Unknown'
        END AS hora_rango,
        MAX(fct_registros.velocidad) AS max_velocidad,
        MAX(fct_registros.volume) AS max_volumen,
        COUNT(fct_registros.velocidad) AS cant_registros
    FROM fct_registros
    INNER JOIN d_sensores ON fct_registros.id_detector = d_sensores.id_detector
    LEFT JOIN d_date ON fct_registros.id_fecha = d_date.id_fecha
    GROUP BY d_sensores.barrio, d_date.day_of_week, hora_rango
      "
  )
  readr::write_csv(registros_max_barrioxdiaxhora, registros_max_file)
  
}

registros_max_barrioxdiaxhora <- registros_max_barrioxdiaxhora %>% 
  mutate( 
    dia_de_la_semana = 
      factor(
       day_of_week,
        levels = c(1, 2, 3, 4, 5, 6, 7),
        labels = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo")
      )
  )


