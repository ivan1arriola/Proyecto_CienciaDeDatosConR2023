
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


data_folder_app <- "data"
obtener_registros_max <- function(nombre_archivo, conexion, consulta) {
  registros_max_file <- paste0(data_folder_app, "/", nombre_archivo)
  if (file.exists(registros_max_file)) {
    registros_max <- readr::read_csv(registros_max_file)
  } else {
    registros_max <- DBI::dbGetQuery(conexion, consulta)
    readr::write_csv(registros_max, registros_max_file)
  }
  
  registros_max <- registros_max %>% 
    dplyr::mutate( 
      dia_de_la_semana = factor(
        day_of_week,
        levels = c(1, 2, 3, 4, 5, 6, 7),
        labels = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo")
      )
    )
  
  return(registros_max)
}

obtener_registros_max2 <- function(nombre_archivo, conexion, consulta) {
  registros_max_file <- paste0(data_folder_app, "/", nombre_archivo)
  if (file.exists(registros_max_file)) {
    registros_max <- readr::read_csv(registros_max_file)
  } else {
    registros_max <- DBI::dbGetQuery(conexion, consulta)
    readr::write_csv(registros_max, registros_max_file)
  }
  return(registros_max)
}