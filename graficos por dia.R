#Vamos a analizar la relacion entre el volumen de trefico, la velocidad, el dia de la semana y el barrio donde se encuentra el sensor
tabla <- DBI::dbGetQuery(
  con,
  "
  SELECT
    fct_registros.volume, 
    fct_registros.velocidad,
    d_date.day_of_week
  FROM
    fct_registros
  INNER JOIN 
    d_sensores ON fct_registros.id_detector = d_sensores.id_detector
  INNER JOIN 
    d_date ON fct_registros.id_fecha = d_date.id_fecha
  ORDER BY RANDOM()
  LIMIT 1000
  "
)

   
tabla %>%
  ggplot() + 
  geom_density(
    aes(x = velocidad)
  ) +
  facet_wrap(~day_of_week) + 
  labs(x = "Velocidad",
       y = "Cantidad")

tabla %>%
  ggplot() + 
  geom_density(
    aes(x = volume)
  ) +
  facet_wrap(~day_of_week) + 
  labs(x = "Volumen",
       y = "Cantidad")


