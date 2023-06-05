# proyecto_STAT_NT

Proyecto de Ciencia De Datos

## Posibles Datos Para Investigacion

-   [Conteo vehicular en las principales avenidas de Montevideo](https://catalogodatos.gub.uy/dataset/intendencia-montevideo-conteo-de-vehiculos-del-centro-de-gestion-de-la-movilidad)
-   [Velocidad promedio vehicular en las principales avenidas de Montevideo](https://catalogodatos.gub.uy/dataset/intendencia-montevideo-velocidad-promedio-vehicular-en-las-principales-avenidas-de-montevideo)

- [Ubicación de sensores de medición de conteo vehículos](https://catalogodatos.gub.uy/dataset/intendencia-montevideo-ubicacion-de-sensores-de-medicion-de-conteo-vehiculos)

## Observaciones
Los datos son mensuales. Van desde Enero 2021 hasta Abril 2023.

## Descripcion de variables

**Conjunto de datos: Conteo vehicular en las principales avenidas de Montevideo**

-   `cod_detector`: Entero - ID de la cámara que monitorea un carril específico para detectar vehículos.
-   `id_carril`: Entero - Número del carril monitoreado (1, 2, 3, ...).
-   `fecha`: AAAA-MM-DD - Día en que se realizó la medición.
-   `hora`: hh:mm:ss - Hora en que se realizó la medición.
-   `dsc_avenida`: Texto - Nombre de la avenida donde se mide el tráfico.
-   `dsc_int_anterior`: Texto - Nombre de la vía desde donde vienen los vehículos.
-   `dsc_int_siguiente`: Texto - Nombre de la vía hacia donde se dirigen los vehículos.
-   `latitud`: Float - Latitud del lugar de medición.
-   `longitud`: Float - Longitud del lugar de medición.
-   `volumen`: Entero - Cantidad de vehículos detectados en el carril en los últimos 5 minutos.
-   `volumen_hora`: Entero - Cantidad de vehículos detectados en el carril en la última hora.

**Conjunto de datos: Velocidad promedio vehicular en las principales avenidas de Montevideo**

-   `cod_detector`: Entero - ID de la cámara que monitorea un carril específico para detectar vehículos.
-   `id_carril`: Entero - Número del carril monitoreado (1, 2, 3, ...).
-   `fecha`: AAAA-MM-DD - Día en que se realizó la medición.
-   `hora`: hh:mm:ss - Hora en que se realizó la medición.
-   `dsc_avenida`: Texto - Nombre de la avenida donde se mide el tráfico.
-   `dsc_int_anterior`: Texto - Nombre de la vía desde donde vienen los vehículos.
-   `dsc_int_siguiente`: Texto - Nombre de la vía hacia donde se dirigen los vehículos.
-   `latitud`: Float - Latitud del lugar de medición.
-   `longitud`: Float - Longitud del lugar de medición.
-   `velocidad_promedio`: Entero - Promedio de las velocidades de los autos que circularon por el carril durante los últimos 5 minutos.


**Conjunto de datos: Ubicación de sensores de medición de conteo vehículos**

- `dsc_avenida`: Texto - Nombre de la avenida donde se encuentra el sensor o cámara y donde se mide el tránsito.
- `dsc_int_anterior`: Texto - Nombre de la vía que forma el cruce desde donde vienen los vehículos.
- `dsc_int_siguiente`: Texto - Nombre de la vía que forma el cruce donde está el sensor. En general, el sensor se encuentra un poco antes de esta vía. El sentido de circulación será desde el cruce con `dsc_int_anterior` hacia el cruce con `dsc_int_siguiente`.
- `latitud`: Float - Coordenada que indica la latitud de la ubicación del sensor.
- `longitud`: Float - Coordenada que indica la longitud de la ubicación del sensor.


## Posibles Preguntas de Investigación

-   ¿Influye el barrio con la velocidad a la que van los vehículos?
-   ¿Existe alguna correlación entre el volumen de tráfico y la velocidad promedio en las avenidas de Montevideo?
-   ¿Existen diferencias en el volumen de tráfico y la velocidad promedio entre días laborales y fines de semana?
-   ¿Cuáles son las avenidas con los mayores promedios de velocidad en Montevideo?
