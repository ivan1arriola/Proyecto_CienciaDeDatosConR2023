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
library(dplyr)
library(ggplot2)
library(sf)

# Cargar modulos ----------------------------------------------------------

source(paste0(moduleDir, "/Mapa.R"))
source(paste0(moduleDir, "/Univariado.R"))
source(paste0(moduleDir, "/Multivariado.R"))
source("utils.R")


# Variables globales ------------------------------------------------------

intervalos <- c(
  '00:00',
  '01:00',
  '02:00',
  '03:00',
  '04:00',
  '05:00',
  '06:00',
  '07:00',
  '08:00',
  '09:00',
  '10:00',
  '11:00',
  '12:00',
  '13:00',
  '14:00',
  '15:00',
  '16:00',
  '17:00',
  '18:00',
  '19:00',
  '20:00',
  '21:00',
  '22:00',
  '23:00'
)

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
registros_max_file <-  "registros_max_barrio_file.csv"

registros_max_barrioxdiaxhora <- obtener_registros_max(registros_max_file, con,
    "
      SELECT
        d_sensores.barrio,
        d_date.day_of_week,
        CASE
            WHEN fct_registros.id_hora >= 0 AND fct_registros.id_hora < 100 THEN '00:00'
            WHEN fct_registros.id_hora >= 100 AND fct_registros.id_hora < 200 THEN '01:00'
            WHEN fct_registros.id_hora >= 200 AND fct_registros.id_hora < 300 THEN '02:00'
            WHEN fct_registros.id_hora >= 300 AND fct_registros.id_hora < 400 THEN '03:00'
            WHEN fct_registros.id_hora >= 400 AND fct_registros.id_hora < 500 THEN '04:00'
            WHEN fct_registros.id_hora >= 500 AND fct_registros.id_hora < 600 THEN '05:00'
            WHEN fct_registros.id_hora >= 600 AND fct_registros.id_hora < 700 THEN '06:00'
            WHEN fct_registros.id_hora >= 700 AND fct_registros.id_hora < 800 THEN '07:00'
            WHEN fct_registros.id_hora >= 800 AND fct_registros.id_hora < 900 THEN '08:00'
            WHEN fct_registros.id_hora >= 900 AND fct_registros.id_hora < 1000 THEN '09:00'
            WHEN fct_registros.id_hora >= 1000 AND fct_registros.id_hora < 1100 THEN '10:00'
            WHEN fct_registros.id_hora >= 1100 AND fct_registros.id_hora < 1200 THEN '11:00'
            WHEN fct_registros.id_hora >= 1200 AND fct_registros.id_hora < 1300 THEN '12:00'
            WHEN fct_registros.id_hora >= 1300 AND fct_registros.id_hora < 1400 THEN '13:00'
            WHEN fct_registros.id_hora >= 1400 AND fct_registros.id_hora < 1500 THEN '14:00'
            WHEN fct_registros.id_hora >= 1500 AND fct_registros.id_hora < 1600 THEN '15:00'
            WHEN fct_registros.id_hora >= 1600 AND fct_registros.id_hora < 1700 THEN '16:00'
            WHEN fct_registros.id_hora >= 1700 AND fct_registros.id_hora < 1800 THEN '17:00'
            WHEN fct_registros.id_hora >= 1800 AND fct_registros.id_hora < 1900 THEN '18:00'
            WHEN fct_registros.id_hora >= 1900 AND fct_registros.id_hora < 2000 THEN '19:00'
            WHEN fct_registros.id_hora >= 2000 AND fct_registros.id_hora < 2100 THEN '20:00'
            WHEN fct_registros.id_hora >= 2100 AND fct_registros.id_hora < 2200 THEN '21:00'
            WHEN fct_registros.id_hora >= 2200 AND fct_registros.id_hora < 2300 THEN '22:00'
            WHEN fct_registros.id_hora >= 2300 AND fct_registros.id_hora < 2400 THEN '23:00'
            ELSE 'Unknown'
        END AS hora_rango,
        MAX(fct_registros.velocidad) AS max_velocidad,
        MAX(fct_registros.volume) AS max_volumen,
        AVG(fct_registros.velocidad) AS promedio_velocidad,
        AVG(fct_registros.volume) AS promedio_volumen,
        COUNT(fct_registros.velocidad) AS cant_registros
    FROM fct_registros
    INNER JOIN d_sensores ON fct_registros.id_detector = d_sensores.id_detector
    LEFT JOIN d_date ON fct_registros.id_fecha = d_date.id_fecha
    GROUP BY d_sensores.barrio, d_date.day_of_week, hora_rango
    "
  )



### registros maximos por rango hora, dia de la semana y sensor
registros_max_sensor_file <-"/registros_max_sensor_file.csv"
registros_max_barrioxdiaxhora_sensor <- obtener_registros_max(registros_max_sensor_file,
    con,
    "
      SELECT
        d_sensores.barrio,
        d_date.day_of_week,
        CASE
            WHEN fct_registros.id_hora >= 0 AND fct_registros.id_hora < 100 THEN '00:00'
            WHEN fct_registros.id_hora >= 100 AND fct_registros.id_hora < 200 THEN '01:00'
            WHEN fct_registros.id_hora >= 200 AND fct_registros.id_hora < 300 THEN '02:00'
            WHEN fct_registros.id_hora >= 300 AND fct_registros.id_hora < 400 THEN '03:00'
            WHEN fct_registros.id_hora >= 400 AND fct_registros.id_hora < 500 THEN '04:00'
            WHEN fct_registros.id_hora >= 500 AND fct_registros.id_hora < 600 THEN '05:00'
            WHEN fct_registros.id_hora >= 600 AND fct_registros.id_hora < 700 THEN '06:00'
            WHEN fct_registros.id_hora >= 700 AND fct_registros.id_hora < 800 THEN '07:00'
            WHEN fct_registros.id_hora >= 800 AND fct_registros.id_hora < 900 THEN '08:00'
            WHEN fct_registros.id_hora >= 900 AND fct_registros.id_hora < 1000 THEN '09:00'
            WHEN fct_registros.id_hora >= 1000 AND fct_registros.id_hora < 1100 THEN '10:00'
            WHEN fct_registros.id_hora >= 1100 AND fct_registros.id_hora < 1200 THEN '11:00'
            WHEN fct_registros.id_hora >= 1200 AND fct_registros.id_hora < 1300 THEN '12:00'
            WHEN fct_registros.id_hora >= 1300 AND fct_registros.id_hora < 1400 THEN '13:00'
            WHEN fct_registros.id_hora >= 1400 AND fct_registros.id_hora < 1500 THEN '14:00'
            WHEN fct_registros.id_hora >= 1500 AND fct_registros.id_hora < 1600 THEN '15:00'
            WHEN fct_registros.id_hora >= 1600 AND fct_registros.id_hora < 1700 THEN '16:00'
            WHEN fct_registros.id_hora >= 1700 AND fct_registros.id_hora < 1800 THEN '17:00'
            WHEN fct_registros.id_hora >= 1800 AND fct_registros.id_hora < 1900 THEN '18:00'
            WHEN fct_registros.id_hora >= 1900 AND fct_registros.id_hora < 2000 THEN '19:00'
            WHEN fct_registros.id_hora >= 2000 AND fct_registros.id_hora < 2100 THEN '20:00'
            WHEN fct_registros.id_hora >= 2100 AND fct_registros.id_hora < 2200 THEN '21:00'
            WHEN fct_registros.id_hora >= 2200 AND fct_registros.id_hora < 2300 THEN '22:00'
            WHEN fct_registros.id_hora >= 2300 AND fct_registros.id_hora < 2400 THEN '23:00'
            ELSE 'Unknown'
        END AS hora_rango,
        MAX(fct_registros.velocidad) AS max_velocidad,
        MAX(fct_registros.volume) AS max_volumen,
        AVG(fct_registros.velocidad) AS promedio_velocidad,
        AVG(fct_registros.volume) AS promedio_volumen,
        COUNT(fct_registros.velocidad) AS cant_registros
    FROM fct_registros
    INNER JOIN d_sensores ON fct_registros.id_detector = d_sensores.id_detector
    LEFT JOIN d_date ON fct_registros.id_fecha = d_date.id_fecha
    GROUP BY d_sensores.barrio, d_date.day_of_week, hora_rango, d_sensores.id_detector
    "
  )

