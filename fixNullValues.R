library(DBI)
library(RPostgres)
library(dplyr)
library(readr)
library(stringr)


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

datos_null <- DBI::dbGetQuery(
  con,
  "SELECT *
  FROM fct_registros
  WHERE id_detector IS NULL OR id_fecha is null or id_carril is null or id_registros is null or
      id_hora is null;"
)

datos_null$fecha <- as.Date(as.character(datos_null$id_fecha), format = "%Y%m%d")
datos_null$hora <- datos_null$id_hora

glimpse(datos_null)
summary(datos_null)

# Se puede ver que los datos faltantes van de id 91804143 hasta 92403719
# Todos son del carril 2
# Son todos de Julio del 2021, del 24 hasta el 31




#Me voy a traer el csv de conteo y volumen de Octubre 2021
conteo_vehicular_Julio21 <- read.csv(here::here("datos_descargados", "autoscope_07_2021_volumen.csv"))
conteo_vehicular_Julio21$fecha <- as.Date(conteo_vehicular_Julio21$fecha, format = "%Y-%m-%d")
conteo_vehicular_Julio21$hora <- as.integer(substr(conteo_vehicular_Julio21$hora, 1, 2)) * 100 + as.integer(substr(conteo_vehicular_Julio21$hora, 4, 5))


glimpse(conteo_vehicular_Julio21)
summary(conteo_vehicular_Julio21)


datos_combinados <- inner_join(datos_null, conteo_vehicular_Julio21, by = c("fecha", "hora", "id_carril", "volume", "volumen_hora"))

#NO SE PUEDE ... BYE
