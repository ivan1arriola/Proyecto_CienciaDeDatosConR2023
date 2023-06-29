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


### Velocidades Maximas detectadas por Barrio
velocidades_file <- paste0(dataDir, "/velocidadxBarrioMax.csv")
if (file.exists(velocidades_file)) {
  velocidadxBarrioMax <- readr::read_csv(velocidades_file)
  
} else {
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
  readr::write_csv(velocidadxBarrioMax, velocidades_file)
  
}




