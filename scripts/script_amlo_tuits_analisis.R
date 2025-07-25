#######################################################
#######################################################
##################### HELLO WORLD #####################
#######################################################
#######################################################
################ Análisis exploratorio ################
############ Tuits de AMLO sobre seguridad ############
#######################################################
#######################################################


# 1. Cargar paquetes
library(tidyverse)
library(tidytext)
library(stringr)
library(readr)
library(janitor)
library(stopwords)

# 2. Leer DataFrame
tuits_amlo <- read_csv("https://raw.githubusercontent.com/jesustello/tuits_presidentes/refs/heads/main/datos/amlo_tuits_texto.csv")  # ← Cambia esta ruta por la ubicación real del archivo, Usando el raw

# 3. Limpiar nombres de columnas
tuits_amlo <- tuits_amlo |> janitor::clean_names()  # texto y no

# 4. Eliminar frases completas antes de tokenizar
frases_a_eliminar <- c(
  "Conferencia matutina",
  "Conferencia de prensa",
  "Conferencia de prensa matutina",
  "Conferencia en vivo",
  "desde Palacio Nacional",
  "Conferencia de prensa en vivo",
  "https://www.eluniversal.com.mx/deportes/la-nba-permitira-que-los-jugadores-consuman-marihuana-ya-no-habra-sanciones/",
  "https://www.pscp.tv/w/cMjKTzF4TlFhYWVySmVhUWJ8MU1ZR05QUG9iTXp4d8hbJbOiWNvgEpQ4oM0gFnSn2gXyDCQomW0h5F-9994B"
)

# Reemplazamos esas frases por vacío ("")
tuits_amlo <- tuits_amlo |> 
  mutate(Texto = str_remove_all(texto,
                                regex(paste(frases_a_eliminar, collapse = "|"),
                                      ignore_case = TRUE)))

# 5. Limpieza básica del texto
tuits_amlo <- tuits_amlo |>
  mutate(
    texto = str_squish(texto),         # quita espacios extra
    texto = str_to_lower(texto)        # pasa todo a minúsculas
  )

# 6. Sustitución de variantes por forma unificada
tuits_amlo <- tuits_amlo |>
  mutate(
    texto = str_replace_all(texto, regex("ee\\.?\\s?uu\\.?", ignore_case = TRUE), "estados unidos"),
    texto = str_replace_all(texto, regex("estados unidos mexicanos", ignore_case = TRUE), "méxico"),  # Opcional, si no quieres que cuente como "estados unidos"
    texto = str_replace_all(texto, "armada de méxico", "marina nacional"),
    texto = str_replace_all(texto, "estrategia de seguridad", "estrategia nacional de seguridad"),
    texto = str_replace_all(texto, "joe biden", "biden"),
    texto = str_replace_all(texto, "zona metropolitana de monterrey", "monterrey"),
    texto = str_replace_all(texto, "base aérea militar santa lucía", "santa lucía")
  )

# 7. Definir frases a conservar como tokens únicos
frases_a_conservar <- c(
  "aeropuerto internacional de tulum",
  "alicia bárcena",
  "andrés manuel lópez obrador",
  "antony blinken",
  "alejandro mayorkas",
  "armada de méxico",
  "baja california",
  "benito juárez garcía",
  "campo marte",
  "casa blanca",
  "ciudad de méxico",
  "consejo nacional de seguridad pública",
  "construyendo el futuro",
  "coyuca de benítez",
  "cuarta transformación",
  "cuitláhuac garcía",
  "cumbre de américa del norte",
  "dan kildee",
  "daños colaterales",
  "de marina",
  "de la defensa nacional",
  "defensa patriótica",
  "delincuencia organizada",
  "diálogo de seguridad de alto nivel",
  "diego prieto",
  "diego sinhue rodríguez vallejo",
  "don beyer",
  "el salvador",
  "el tajín",
  "el universal",
  "ejército mexicano",
  "elizabeth sherwood-randall",
  "ernesto lammoglia",
  "estado mexicano",
  "estados unidos",
  "estrategia de seguridad",
  "estrategia nacional de seguridad",
  "ex presidente fox",
  "flores magón",
  "fuerte de san juan de ulúa",
  "fuerza aérea mexicana",
  "fuerzas armadas",
  "gabinete de seguridad",
  "gobierno de méxico",
  "guadalupe tepeyac",
  "guardia nacional",
  "gustavo petro",
  "heroico colegio militar",
  "huautla de jiménez",
  "invasión estadounidense",
  "isla madre",
  "islas marías",
  "istmo de tehuantepec",
  "javier corral",
  "jerry carl",
  "josé rafael ojeda durán",
  "josé revueltas",
  "la montaña",
  "las margaritas",
  "lázaro cárdenas del río",
  "libro de visitantes",
  "lou correa",
  "luis cresencio sandoval gonzález",
  "madre conchita",
  "maggie hassan",
  "maravatío de ocampo",
  "maría sabina",
  "marina nacional",
  "matías romero",
  "medalla belisario domínguez",
  "medio oriente",
  "muros de agua",
  "nayib bukele",
  "nuevo león",
  "otay ii",
  "pedro infante",
  "pedro sainz de baranda",
  "policía federal",
  "programas integrales de bienestar",
  "puerto de guaymas",
  "puerto de veracruz",
  "puerto salina cruz",
  "ramos arizpe",
  "rosa icela rodríguez",
  "rosario ibarra de piedra",
  "san blas",
  "santa cruz xoxocotlán",
  "santa lucía",
  "secretaría de marina",
  "secretaria de relaciones exteriores",
  "secretaría de seguridad y protección ciudadana",
  "secretario de marina",
  "secretario de la defensa",
  "servidores de la nación",
  "sistema nacional de búsqueda",
  "tianguis turístico méxico",
  "tom carper",
  "tren maya"
)

# 8. Sustituir frases por versión con guiones bajos para conservarlas como un token
for (frase in frases_a_conservar) {
  frase_token <- str_replace_all(frase, " ", "_")
  tuits_amlo$texto <- str_replace_all(tuits_amlo$texto, frase, frase_token)
}

# 9. Tokenización
palabras <- tuits_amlo |>
  unnest_tokens(palabra, texto)

# 10. Quitar stopwords, números y palabras cortas
stopwords_es <- stopwords::stopwords("es")  # stopwords en español

tokens_limpios <- palabras %>%
  filter(!palabra %in% stopwords_es) %>%        # quitar stopwords
  filter(!str_detect(palabra, "^\\d+$")) %>%    # quitar números
  filter(str_length(palabra) > 2)               # quitar palabras muy cortas

# 11. Conteo de frecuencia
palabras_conteo <- tokens_limpios |>
  count(palabra, sort = TRUE)

# 12. Visualizar resultados
print(palabras_conteo)