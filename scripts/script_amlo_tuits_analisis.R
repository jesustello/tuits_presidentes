
#  ██░ ██ ▓█████  ██▓     ██▓     ▒█████     █     █░ ▒█████   ██▀███   ██▓    ▓█████▄ 
# ▓██░ ██▒▓█   ▀ ▓██▒    ▓██▒    ▒██▒  ██▒   ▓█░ █ ░█░▒██▒  ██▒▓██ ▒ ██▒▓██▒    ▒██▀ ██▌
# ▒██▀▀██░▒███   ▒██░    ▒██░    ▒██░  ██▒   ▒█░ █ ░█ ▒██░  ██▒▓██ ░▄█ ▒▒██░    ░██   █▌
# ░▓█ ░██ ▒▓█  ▄ ▒██░    ▒██░    ▒██   ██░   ░█░ █ ░█ ▒██   ██░▒██▀▀█▄  ▒██░    ░▓█▄   ▌
# ░▓█▒░██▓░▒████▒░██████▒░██████▒░ ████▓▒░   ░░██▒██▓ ░ ████▓▒░░██▓ ▒██▒░██████▒░▒████▓ 
# ▒ ░░▒░▒░░ ▒░ ░░ ▒░▓  ░░ ▒░▓  ░░ ▒░▒░▒░    ░ ▓░▒ ▒  ░ ▒░▒░▒░ ░ ▒▓ ░▒▓░░ ▒░▓  ░ ▒▒▓  ▒ 
# ▒ ░▒░ ░ ░ ░  ░░ ░ ▒  ░░ ░ ▒  ░  ░ ▒ ▒░      ▒ ░ ░    ░ ▒ ▒░   ░▒ ░ ▒░░ ░ ▒  ░ ░ ▒  ▒ 
# ░  ░░ ░   ░     ░ ░     ░ ░   ░ ░ ░ ▒       ░   ░  ░ ░ ░ ▒    ░░   ░   ░ ░    ░ ░  ░ 
# ░  ░  ░   ░  ░    ░  ░    ░  ░    ░ ░         ░        ░ ░     ░         ░  ░   ░    
# ░      

#                         TUITS DE AMLO SOBRE SEGURIDAD           
# ==============================================================================

# 📚 INFO DEL SCRIPT ===========================================================
# Autor: Jesús Alejandro Tello Cháirez
# Fecha: julio de 2025
# Proyecto: Discurso y políticas de seguridad en México, El Salvador y Colombia
# Notas: Script exploratorio con visuales e ideas sueltas

# 🚧 TO DO: 
# - [ ] Limpiar texto
# - [ ] Explorar temporalidad
# - [ ] Analizar hashtags y temas
# - [ ] Clasificar tuits por tono o discurso dominante

# 🚀 0. LIBRERÍAS ==============================================================
# 📦 Instalar paquetes necesarios si no están ya instalados ----
paquetes <- c("remotes", "wordcloud2", "ggwordcloud", "showtext", "sysfonts")

# Detectar cuáles no están instalados
instalar <- paquetes[!paquetes %in% installed.packages()]

# Instalar los faltantes desde CRAN
if (length(instalar)) install.packages(instalar)

# Instalar los que están solo en GitHub
remotes::install_github("lchiffon/wordcloud2")   # Para nubes de palabras interactivas
remotes::install_github("lepennec/ggwordcloud")  # Para nubes con ggplot

suppressPackageStartupMessages({
  library(tidyverse)  # para manipulación y visualización de datos
  library(tidytext)
  library(janitor)    # limpieza de nombres
  library(lubridate)  # fechas
  library(stringr)    # manejo de texto
  library(readr)
  library(stopwords)
  library(glue)       # strings con variables
  library(scales)     # etiquetas en ejes
  library(crayon)     # 🌈 colores en consola
  library(ggwordcloud)    # Nubes de palabras
  library(showtext)       # Fuentes personalizadas
  library(wordcloud2)     # Cargar {wordcloud2}
  library(ggplot2)
  library(glue)
  library(showtext)
})

# ⚙️ 1. CONFIGURACIÓN INICIAL ==================================================
# Cargar fuentes de Google 
font_add_google("Playfair Display", "playfair")
font_add_google("Roboto", "roboto")
font_add_google("Montserrat", "montserrat")
# Activar uso de showtext
showtext_auto()

# Mensaje de confirmación
message("✔️ Fuentes cargadas y showtext activado correctamente.")
# cat("✔️ Fuentes cargadas y showtext activado correctamente.\n")

cat(green$bold$underline("\n💻 Iniciando análisis...\n"))

# 📁 2. CARGA DE DATOS =========================================================
tuits_amlo <- read_csv("https://raw.githubusercontent.com/jesustello/tuits_presidentes/refs/heads/main/datos/amlo_tuits_texto.csv")
tuits_amlo <- tuits_amlo |> clean_names()

cat(blue$bold("✅ Datos cargados correctamente\n"))

glimpse(tuits_amlo)

# 🧹 3. LIMPIEZA INICIAL ==================================
tuits_amlo <- tuits_amlo |>
  mutate(
    texto = str_to_lower(texto), # pasa todo a minúsculas
    texto = str_squish(texto)    # quita espacios extra
  )

# Eliminar frases completas después de poner en minúsculas y antes de tokenizar
frases_a_eliminar <- str_to_lower(c(
  "conferencia matutina",
  "conferencia de prensa",
  "conferencia de prensa matutina",
  "conferencia en vivo",
  "desde palacio nacional",
  "conferencia de prensa en vivo",
  "conferencia",
  "matutina",
  "https://www.eluniversal.com.mx/deportes/la-nba-permitira-que-los-jugadores-consuman-marihuana-ya-no-habra-sanciones/",
  "https://www.pscp.tv/w/cMjKTzF4TlFhYWVySmVhUWJ8MU1ZR05QUG9iTXp4d8hbJbOiWNvgEpQ4oM0gFnSn2gXyDCQomW0h5F-9994B"
))

# Reemplazamos esas frases por vacío ("")
tuits_amlo <- tuits_amlo |> 
  mutate(texto = str_remove_all(texto,
                                regex(paste(frases_a_eliminar, collapse = "|"),
                                      ignore_case = FALSE)))

# Sustitución de variantes por forma unificada
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

# Definir frases a conservar como tokens únicos
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

# Sustituir frases por versión con guiones bajos para conservarlas como un token
for (frase in frases_a_conservar) {
  frase_token <- str_replace_all(frase, " ", "_")
  tuits_amlo$texto <- str_replace_all(tuits_amlo$texto, frase, frase_token)
}

cat(yellow$bold("\n🧽 Limpieza inicial completada\n"))

# 💬 PALABRAS CLAVE O FRECUENTES =======================
# Tokenización y frecuencia
palabras <- tuits_amlo |>
  unnest_tokens(palabra, texto)

# Quitar stopwords, números y palabras cortas
stopwords_es <- stopwords::stopwords("es")  # stopwords en español

tokens_limpios <- palabras |>
  filter(!palabra %in% stopwords_es) |>        # quitar stopwords
  filter(!str_detect(palabra, "^\\d+$")) |>    # quitar números
  filter(str_length(palabra) > 2)               # quitar palabras muy cortas

# Conteo de frecuencia
palabras_conteo <- tokens_limpios |>
  count(palabra, sort = TRUE)

# Visualizar resultados
print(palabras_conteo)

# ☁️ NUBES DE PALABRAS ==================================
set.seed(123) # Para reproducibilidad
cat("🎲 Semilla aleatoria establecida en 123\n")

# Crear nube de palabras con {wordcloud2} 
# Versión básica:
palabras_conteo |>
  filter(n > 5) |> 
  wordcloud2(backgroundColor = "#EAD1FA", color = "#543A74")

# Versión personalizada:
palabras_conteo |>
  filter(n > 5) |> 
  wordcloud2(
    backgroundColor = "#EAD1FA",
    color = "#543A74",
    rotateRatio = 0.1,   # menos rotación
    gridSize = 8,        # más espacio entre palabras
    size = 0.5,          # escala general más pequeña
    minSize = 11         # tamaño mínimo de palabra
  )

# Nube de palabras con {ggplot2} y {ggwordcloud}

# Estilo global de los gráficos
theme_set(
  theme_void() +
    theme(plot.background = element_rect(fill = "#EAD1FA", linewidth = 0))
)

# Versión 1 ggwordcloud
nube_gg1 <- palabras_conteo |> 
  filter(n > 5) |>                      # Filtra solo palabras con más de n apariciones
  ggplot() +
  aes(label = palabra, size = n) +         # Usa 'palabra' como texto y 'n' como tamaño
  geom_text_wordcloud(                     # Dibuja la nube con forma circular
    shape = "circle", 
    color = "#543A74"
  ) +
  scale_size_continuous(range = c(3, 20))  # Controla tamaño mínimo y máximo de palabras

# Visualizar
nube_gg1

# Guardar con nombre automático
ggsave(
  filename = glue("nube_palabras_amlo_{format(Sys.time(), '%Y-%m-%d_%H-%M-%S')}.png"),
       plot = nube_gg1,
       width = 10, height = 8, dpi = 300) # MUY PROBABLEMENTE ME DECANTE POR ESTA OPCIÓN, SOLO ADAPTANDO ALGUNOS PARÁMETROS

# Versión 2 ggwordcloud
nube_gg1_13 <- palabras_conteo |> 
  filter(n > 5) |>
  ggplot() +
  aes(
    label = palabra, 
    size = n, 
    color = n,
    family = "montserrat"
  ) +
  geom_text_wordcloud(                     
    shape = "circle",                            # Dibuja la nube con forma circular
    rm_outside = TRUE,                            # Evita superposición sacando palabras que no caben
    color = "#543A74",
    grid_size = 1.5                     # Controla el espacio entre palabras
  ) +
  scale_size_continuous(range = c(3, 20)) +
  scale_color_gradient(low = "#BFA8E6", high = "#543A74") +
  labs(title = "Palabras más frecuentes sobre seguridad en tuits de AMLO") +  
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,               # Centrar título
      size = 26,                 # Tamaño grande
      family = "playfair",       # Fuente diferente para el título
      face = "bold",
      color = "#543A74"
    ),
    plot.background = element_rect(fill = "#EAD1FA", linewidth = 0)
  )

# Visualizar
nube_gg1_13

# Guardar con nombre automático
ggsave(
  filename = glue("nube_palabras_amlo_{format(Sys.time(), '%Y-%m-%d_%H-%M-%S')}.png"),
       plot = nube_gg1_13,
       width = 4, height = 3, dpi = 300)

# 🧠 COMENTARIOS FINALES ===============================
cat(magenta$bold("\n✨ Análisis preliminar completo. Recuerda guardar avances y hacer backup.\n"))

# =============================================================================================
# =============================================================================================
# =============================================================================================
# =============================================================================================
# =============================================================================================