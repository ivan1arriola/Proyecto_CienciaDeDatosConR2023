
transformarCoord <- function(lat, lon, mvd_map) {
  puntos_lat_lng <- data.frame(lng = lon, lat = lat)
  puntos_sf <- sf::st_as_sf(puntos_lat_lng,
                            coords = c("lng", "lat"),
                            crs = 4326)
  puntos_transformados <- sf::st_transform(puntos_sf,
                                           crs = sf::st_crs(mvd_map))
  return(puntos_transformados)
}
print("transformarCoord loaded")

encontrar_barrio <- function(lat, lon, mvd_map) {
  # Convertir las coordenadas geográficas a un objeto espacial sf
  puntos_transformados <- transformarCoord(lat, lon, mvd_map)
  
  # Encontrar el barrio que contiene el punto
  ls_contains <- sf::st_contains(mvd_map$the_geom,
                                 puntos_transformados$geometry)
  indice_barrio <- which(as.logical(ls_contains))[1]
  matching_barrios <- mvd_map[indice_barrio, ]
  
  # Obtener el nombre del barrio
  barrio <- matching_barrios$nombbarr
  
  # Retornar el nombre del barrio
  return(barrio)
}
print("encontrar_barrio loaded")

# Definir una función personalizada para colorear polígonos
nacol <- function(spdf) {
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  nunique <- function(x) {unique(x[!is.na(x)])}
  np = nrow(spdf)
  adjl = spdep::poly2nb(spdf)
  cols = rep(NA, np)
  cols[1] = 1
  nextColour = 2
  for (k in 2:np) {
    adjcolours = nunique(cols[adjl[[k]]])
    if (length(adjcolours) == 0) {
      cols[k] = resample(cols[!is.na(cols)], 1)
    } else {
      avail = setdiff(nunique(cols), nunique(adjcolours))
      if (length(avail) == 0) {
        cols[k] = nextColour
        nextColour = nextColour + 1
      } else {
        cols[k] = resample(avail, size = 1)
      }
    }
  }
  return(cols)
}
print("nacol loaded")


semaforoIcon <- leaflet::makeIcon(
  iconUrl = "media/semaforo.png",
  iconWidth = 10, iconHeight = 13
)
print("semaforoIcon loaded")

intToHour <- function(int) {
  minExtract <- substring(int, first = nchar(int) - 1)
  hourExtract <- substring(int, first = 0, last = 2)
  hora_nueva <- data.frame(hourExtract, minExtract)  %>% 
    dplyr::mutate(hora_nueva = paste(hourExtract, minExtract, sep = ":"),
                  hora_nueva = lubridate::format(strptime(hora_nueva, "%Y-%m-%d %H:%M:%S"), '%H:%M'))
  
  return(hora_nueva)
}
print("intToHour loaded")


# Directorio de la carpeta "data"
data_folder <- here::here("data")

if (!dir.exists(data_folder)) {
  dir.create(data_folder)
}

# Función para cargar datos desde archivos CSV o desde la base de datos
load_data <- function(filename, con, query) {
  if (file.exists(paste0(data_folder,"/", filename))) {
    data <- read.csv(file.path(data_folder, filename))
  } else {
    data <- DBI::dbGetQuery(con, query)
    write.csv(data, file.path(data_folder, filename), row.names = FALSE)
  }
  return(data)
}
