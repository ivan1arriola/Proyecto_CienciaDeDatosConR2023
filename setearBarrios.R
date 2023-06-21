# Este script lo que hace es descargar la tabla d_sensores y sobrescribir los
# valores que habian de barrio con los que hay en el mapa del INE que conseguimos
# en un repositorio de GITHUB

# Este script no se encarga de crear la columna de barrio!


library(dplyr)
library(DBI)
library(RPostgres)
library(sf)
source(here::here("utils.R"))

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
