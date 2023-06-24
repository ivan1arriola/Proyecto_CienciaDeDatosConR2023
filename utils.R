library(sf)

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
  ls_contains <- st_contains(mvd_map$the_geom,
                             puntos_transformados$geometry)
  indice_barrio <- which(as.logical(ls_contains))[1]
  matching_barrios <- mvd_map[indice_barrio, ]
  
  # Obtener el nombre del barrio
  barrio <- matching_barrios$nombbarr
  
  # Retornar el nombre del barrio
  return(barrio)
}