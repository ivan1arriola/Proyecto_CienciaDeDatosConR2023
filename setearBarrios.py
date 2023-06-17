import requests
import json
import psycopg2

# Detalles de conexión
DB_USER = "scraping_iesta"
DB_PASS = "scraping_iesta"
DB_PORT = "5439"
DB_HOST = "164.90.254.224"
DB_NAME = "datos_transito"

# Establecer la conexión
conn = psycopg2.connect(
    host=DB_HOST,
    port=DB_PORT,
    database=DB_NAME,
    user=DB_USER,
    password=DB_PASS
)

# Crear un cursor
cursor = conn.cursor()

# Ejecutar consulta
cursor.execute("SELECT * FROM d_sensores")

# Obtener los resultados
results = cursor.fetchall()

# Función para obtener el nombre del barrio
def obtener_barrio(latitud, longitud, calle, esquina, api_key):
    # Primero intentar obtener el barrio según latitud y longitud
    url = f'https://maps.googleapis.com/maps/api/geocode/json?latlng={latitud},{longitud}&key={api_key}&result_type=neighborhood'

    response = requests.get(url)
    data = response.json()

    if 'results' in data and len(data['results']) > 0:
        address_components = data['results'][0]['address_components']

        for component in address_components:
            if 'neighborhood' in component['types']:
                return component['long_name']

    print(f"No se pudo obtener el barrio para la latitud {latitud} y longitud {longitud}.")

    # Intentar obtener el barrio según calle y esquina
    direccion = f"{calle} y {esquina}"
    url = f'https://maps.googleapis.com/maps/api/geocode/json?address={direccion}&key={api_key}&result_type=neighborhood'

    response = requests.get(url)
    data = response.json()

    if 'results' in data and len(data['results']) > 0:
        address_components = data['results'][0]['address_components']

        for component in address_components:
            if 'neighborhood' in component['types']:
                return component['long_name']

    print(f"No se pudo obtener el barrio para la calle {calle} y esquina {esquina}.")

    # Quiero guardar los datos de la latitud y longitud que no se pudieron obtener
    with open('datos_faltantes.txt', 'a') as f:
        f.write(f"{latitud},{longitud}\n")

    return None

# API Key de Google Maps
API_KEY = "AIzaSyDvgW2B2SVrvlWZqgu8Cug92dGEIibJIeg"

cambios = []

# Agregar columna "barrio" a la tabla d_sensores
#cursor.execute("ALTER TABLE d_sensores ADD COLUMN barrio VARCHAR(100)")

# Confirmar los cambios
#conn.commit()

# Obtener datos de los sensores y el nombre del barrio
for row in results:
    sensor_id, calle1, calle2, calle3, latitud, longitud , barrio= row
    barrio = obtener_barrio(latitud, longitud, calle1, calle2, API_KEY)

    if barrio is not None:
        print(f"Sensor ID: {sensor_id}")
        print(f"Calle 1: {calle1}")
        print(f"Calle 2: {calle2}")
        print(f"Calle 3: {calle3}")
        print(f"Latitud: {latitud}")
        print(f"Longitud: {longitud}")
        print(f"Barrio: {barrio}")
        print("")

        # Agregar el cambio a la lista
        cambios.append((barrio, sensor_id))

# Confirmar los cambios
confirmar_cambios = input("¿Deseas confirmar los cambios en la base de datos? (S/N): ")

if confirmar_cambios.lower() == "s":
    # Actualizar los cambios en la base de datos
    for cambio in cambios:
        barrio, sensor_id = cambio
        cursor.execute("UPDATE d_sensores SET barrio = %s WHERE id_detector = %s", (barrio, sensor_id))

    # Confirmar los cambios
    conn.commit()
    print("Los cambios han sido confirmados y guardados en la base de datos.")
else:
    print("Los cambios no han sido confirmados.")

# Cerrar la conexión
conn.close()