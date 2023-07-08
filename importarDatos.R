# Directorio de la carpeta "data"
data_folder <- "data"

# Crear la carpeta si no existe
if (!dir.exists(data_folder)) {
  dir.create(data_folder)
}


#Coneccion a base de datos
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS"),
  dbname = Sys.getenv("DB_NAME")
)
print("Coneccion con base de datos")

avenidas <- c("18 de Julio","8 de Octubre","Agraciada", "Av Brasil", "Av Italia","Bv Artigas","Bv Batlle y Ordonez","Bv Espana","C M Ramirez","Centenario","Fernandez Crespo","Bolivia","Albo","Belloni","Benito Blanco","Burges","Garibaldi","Garzon","Gral Flores","Millan","Morquio","Ponce","Rambla","Ricaldoni","Rivera","Rodo","San Martin","Saravia", "Soca","Uruguay","Varela", "L A de Herrera")  

mvd_map <- load_geouy("Barrios")
mvd_map_fixed <- st_make_valid(st_transform(mvd_map, crs = 4326))




# Función para verificar si un archivo CSV existe en la carpeta "data"
file_exists <- function(filename) {
  file_path <- file.path(data_folder, filename)
  file.exists(file_path)
}

# Cargar los datos desde archivos CSV si existen, en caso contrario, cargarlos desde la base de datos

# d_sensores
if (file_exists("d_sensores.csv")) {
  d_sensores <- read.csv(file.path(data_folder, "d_sensores.csv"))
} else {
  d_sensores <- DBI::dbGetQuery(con, "SELECT * FROM d_sensores")
  write.csv(d_sensores, file.path(data_folder, "d_sensores.csv"), row.names = FALSE)
}
print("d_sensores")

puntos_sensores <- d_sensores %>% 
  select(barrio, latitud, longitud) %>%
  mutate(transformarCoord(latitud, longitud, mvd_map))

# velocidad_volumen
if (file_exists("velocidad_volumen.csv")) {
  velocidad_volumen <- read.csv(file.path(data_folder, "velocidad_volumen.csv"))
} else {
  velocidad_volumen <- dbGetQuery(
    conn = con,
    statement = "
  SELECT 
    fct_registros.velocidad,
    fct_registros.volume as volumen
  FROM
    fct_registros TABLESAMPLE SYSTEM (10)
  
  "
  )
  write.csv(velocidad_volumen, file.path(data_folder, "velocidad_volumen.csv"), row.names = FALSE)
}
print("velocidad_volumen")


# velocidad_calles
if (file_exists("velocidad_calles.csv")) {
  velocidad_calles <- read.csv(file.path(data_folder, "velocidad_calles.csv"))
} else {
  velocidad_calles <- DBI::dbGetQuery(
    con,
    "
  SELECT d_sensores.dsc_avenida,
    AVG(velocidad) AS velocidad_promedio
  FROM fct_registros
  INNER JOIN d_sensores ON fct_registros.id_detector = d_sensores.id_detector
  GROUP BY dsc_avenida
  "
  )
  write.csv(velocidad_calles, file.path(data_folder, "velocidad_calles.csv"), row.names = FALSE)
}
print("velocidad_calles")  



# excesos
if (file_exists("excesos.csv")) {
  excesos <- read.csv(file.path(data_folder, "excesos.csv"))
} else {
  excesos <- DBI::dbGetQuery(
    con,
    "
  SELECT d_sensores.dsc_avenida,
    fct_registros.velocidad 
  FROM fct_registros TABLESAMPLE SYSTEM (10)
  JOIN d_sensores ON fct_registros.id_detector = d_sensores.id_detector
  "
  ) %>% 
    mutate(
      Tipo = ifelse(dsc_avenida %in% avenidas,
                    "Avenida", "Calle"),
      limite = ifelse(dsc_avenida %in% c("Bv Artigas",
                                         "Rambla",
                                         "Larranaga",
                                         "Bv Batlle y Ordonez",
                                         "Garzon"), 60, 45)
    )
  write.csv(excesos, file.path(data_folder, "excesos.csv"), row.names = FALSE)
}
print("excesos")

# promedios_semanales
if (file_exists("promedios_semanales.csv")) {
  promedios_semanales <- read.csv(file.path(data_folder, "promedios_semanales.csv"))
} else {
  promedios_semanales <- DBI::dbGetQuery(
    con,
    "
  WITH tabla as (
    SELECT
      d_date.day_of_week as dia,
      fct_registros.velocidad as velocidad,
      fct_registros.volume as volumen
    FROM fct_registros
    LEFT JOIN d_date ON fct_registros.id_fecha = d_date.id_fecha
    )
    
    
    SELECT
      avg(velocidad) as avg_velocidad,
      avg(volumen) as avg_volumen,
      dia
    FROM tabla 
    GROUP BY dia
    "
  ) %>%
    mutate( 
      dia_semana = 
        factor(
          dia,
          levels = c(1, 2, 3, 4, 5, 6, 7),
          labels = c("Lunes", "Martes", "Miercoles",
                     "Jueves", "Viernes", "Sabado", "Domingo")
        ) 
    ) %>% 
    pivot_longer(
      cols = c("avg_velocidad", "avg_volumen"),
      names_to = "variable",
      values_to = "promedio"
    ) 
  write.csv(promedios_semanales, file.path(data_folder, "promedios_semanales.csv"), row.names = FALSE)
}
print("promedios_semanales")


# registros_max_barrioxdiaxhora
if (file_exists("registros_max_barrioxdiaxhora.csv")) {
  registros_max_barrioxdiaxhora <- read.csv(file.path(data_folder, "registros_max_barrioxdiaxhora.csv"))
} else {
  registros_max_barrioxdiaxhora <- DBI::dbGetQuery(
    con,
    "
      SELECT
        d_date.day_of_week,
        d_date.day_name,
        d_date.month_actual,
        d_date.month_name,
        d_date.year_actual,
        d_date.weekend_indr,
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
          WHEN fct_registros.id_hora >= 2300 AND fct_registros.id_hora <= 2359 THEN '23:00'
        ELSE 'Unknown'
      END AS hora_rango,
        MAX(fct_registros.velocidad) AS max_velocidad,
        MAX(fct_registros.volume) AS max_volumen,
        AVG(fct_registros.velocidad) AS promedio_velocidad,
        AVG(fct_registros.volume) AS promedio_volumen,
        MAX(fct_registros.volumen_hora) as max_volumen_hora,
        AVG(fct_registros.volumen_hora) as promedio_volumen_hora,
        COUNT(fct_registros.velocidad) AS cant_registros
    FROM fct_registros
    INNER JOIN d_sensores ON fct_registros.id_detector = d_sensores.id_detector
    LEFT JOIN d_date ON fct_registros.id_fecha = d_date.id_fecha
    GROUP BY d_date.day_of_week, hora_rango, d_date.month_actual, d_date.year_actual, d_date.weekend_indr, d_date.day_name, d_date.month_name
    "
  )
  write.csv(registros_max_barrioxdiaxhora, file.path(data_folder, "registros_max_barrioxdiaxhora.csv"), row.names = FALSE)
}
print("registros_max_barrioxdiaxhora")



# tabla
if (file_exists("tabla.csv")) {
  tabla <- read.csv(file.path(data_folder, "tabla.csv"))
} else {
  tabla <- DBI::dbGetQuery(
    con,
    "
  SELECT
    fct_registros.volume, 
    fct_registros.velocidad,
    d_date.day_of_week
  FROM
    fct_registros TABLESAMPLE SYSTEM (0.5)
  INNER JOIN 
    d_sensores ON fct_registros.id_detector = d_sensores.id_detector
  INNER JOIN 
    d_date ON fct_registros.id_fecha = d_date.id_fecha
  WHERE
    fct_registros.velocidad <> 0 AND fct_registros.volume <> 0

  "
  ) %>% 
    mutate( 
      dia_de_la_semana = 
        factor(
          day_of_week,
          levels = c(1, 2, 3, 4, 5, 6, 7),
          labels = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo")
        )
    )
  write.csv(tabla, file.path(data_folder, "tabla.csv"), row.names = FALSE)
}
print("tabla")

# registros_año_mes
if (file_exists("registros_año_mes.csv")) {
  registros_año_mes <- read.csv(file.path(data_folder, "registros_año_mes.csv"))
} else {
  registros_año_mes <- DBI::dbGetQuery(
    con,
    "
       SELECT
        count(*),
    d_date.month_actual, d_date.year_actual
    FROM fct_registros
    LEFT JOIN d_date ON fct_registros.id_fecha = d_date.id_fecha
    GROUP BY d_date.month_actual, d_date.year_actual

    "
  )
  write.csv(registros_año_mes, file.path(data_folder, "registros_año_mes.csv"), row.names = FALSE)
}
print("registros_año_mes")

# max_velocidad_hora_barrio
if (file_exists("max_velocidad_hora_barrio.csv")) {
  max_velocidad_hora_barrio <- read.csv(file.path(data_folder, "max_velocidad_hora_barrio.csv"))
} else {
  max_velocidad_hora_barrio <- DBI::dbGetQuery(
    con,
    "
    SELECT
      d_sensores.barrio,
      MAX(fct_registros.velocidad) AS max_velocidad,
      AVG(fct_registros.velocidad) AS avg_velocidad,
      MAX(fct_registros.volume) AS max_volumen,
      AVG(fct_registros.volume) AS avg_volumen,
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
        WHEN fct_registros.id_hora >= 2300 AND fct_registros.id_hora <= 2359 THEN '23:00'
        ELSE 'Unknown'
      END AS hora_rango
    FROM fct_registros
    INNER JOIN d_sensores ON
      fct_registros.id_detector = d_sensores.id_detector
    LEFT JOIN d_date ON
      fct_registros.id_fecha = d_date.id_fecha
    GROUP BY d_sensores.barrio, hora_rango
    "
  )
  write.csv(max_velocidad_hora_barrio, file.path(data_folder, "max_velocidad_hora_barrio.csv"), row.names = FALSE)
}
print("max_velocidad_hora_barrio")

# datos_arbol
if (file_exists("datos_arbol.csv")) {
  datos_arbol <- read.csv(file.path(data_folder, "datos_arbol.csv"))
} else {
  datos_arbol <- DBI::dbGetQuery(
    con,
    "
  SELECT
    AVG(fct_registros.volume) as avg_volumen,
    AVG(fct_registros.velocidad) as avg_velocidad,
    d_date.weekend_indr as esFinDeSemana,
    CASE
            WHEN fct_registros.id_hora >= 600 AND fct_registros.id_hora < 1900 THEN TRUE
            ELSE FALSE
    END AS esDeDia
FROM fct_registros
LEFT JOIN d_date ON fct_registros.id_fecha = d_date.id_fecha
GROUP BY
    fct_registros.id_detector,
    esFinDeSemana,
    esDeDia
  "
  )
  write.csv(datos_arbol, file.path(data_folder, "datos_arbol.csv"), row.names = FALSE)
}
print("datos_arbol")

# forest_data
if (file_exists("forest_data.csv")) {
  forest_data <- read.csv(file.path(data_folder, "forest_data.csv"))
} else {
  forest_data <- DBI::dbGetQuery(
    con,
    "
  SELECT
    AVG(fct_registros.volume) as avg_volumen,
    AVG(fct_registros.velocidad) as avg_velocidad,
    MAX(fct_registros.volume) as max_volumen,
    fct_registros.id_carril,
    fct_registros.id_hora
FROM fct_registros
LEFT JOIN d_date ON fct_registros.id_fecha = d_date.id_fecha
GROUP BY
    fct_registros.id_detector,
    fct_registros.id_carril,
    fct_registros.id_hora
  "
  )
  write.csv(forest_data, file.path(data_folder, "forest_data.csv"), row.names = FALSE)
}
print("forest_data")


