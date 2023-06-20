# Este script lo que hace es descargar la tabla d_sensores y sobrescribir los
# valores que habian de barrio con los que hay en el mapa del INE que conseguimos
# en un repositorio de GITHUB

# Este script no se encarga de crear la columna de barrio!


library(dplyr)
library(DBI)
library(RPostgres)
library(sf)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS"),
  dbname = Sys.getenv("DB_NAME")
)

d_sensores <- DBI::dbGetQuery(
  con,
  "SELECT * FROM d_sensores"
)

mvd_map <- st_read(here::here("./ine_barrios/ine_barrios_mvd_nbi85.shp"))

transformarCoord <- function(lat, lon, mvd_map){
  puntos_lat_lng <- data.frame(lng = lon, lat = lat)
  puntos_sf <- st_as_sf(puntos_lat_lng,
                        coords = c("lng", "lat"),
                        crs = 4326)
  puntos_transformados <- st_transform(puntos_sf,
                                       crs = st_crs(mvd_map))
  return(puntos_transformados)
}

encontrar_barrio <- function(lat, lon, mvd_map) {
  # Convertir las coordenadas geográficas a un objeto espacial sf
  puntos_transformados <- transformarCoord(lat, lon, mvd_map)
  
  # Encontrar el barrio que contiene el punto
  ls_contains <- st_contains(mvd_map$geometry,
                             puntos_transformados$geometry)
  indice_barrio <- which(as.logical(ls_contains))[1]
  matching_barrios <- mvd_map[indice_barrio, ]
  
  # Obtener el nombre del barrio
  barrio <- matching_barrios$NOMBBARR
  
  # Retornar el nombre del barrio
  return(barrio)
}



#Agregar columna barrio a la tabla

d_sensores <- d_sensores %>%
  select(-barrio) %>%
  mutate(barrio = sapply(1:nrow(.), function(i) {
    encontrar_barrio(d_sensores[i, "latitud"], d_sensores[i, "longitud"], mvd_map)
  }))


query <- "UPDATE d_sensores SET barrio = $1 WHERE id_detector = $2"
for (i in 1:nrow(d_sensores)) {
  DBI::dbExecute(con, query, list(d_sensores$barrio[i], d_sensores$id_detector[i]))
}
