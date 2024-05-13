
datosInstrumentos <- read.csv("instrumentos.csv", header = TRUE)

View(datosInstrumentos)

unique(datosInstrumentos$instrumento)

colnames(datosInstrumentos) <- c("Marca.temporal","instrumento", "precio", "dañadoAlgunaVez", "frecDaños", "severidad1", "severidad2", "severidad3", "severidad4" ,"severidad5", "roboAlgunaVez", "arreglo", "tipoDañoFrec", "seguroDisposicion", "pagarDisposicion", "None")

unique(datosInstrumentos$categoria)

library(dplyr)
datosInstrumentos <- datosInstrumentos %>%
  mutate(categoria = case_when(
    grepl("Piano|piano|Teclado", instrumento, ignore.case = TRUE) ~ "Piano",
    grepl("Guitarra|Bajo eléctrico|Ukulele|Cello|Violín|Viola", instrumento, ignore.case = TRUE) ~ "Cuerda",
    grepl("batería|Batería", instrumento, ignore.case = TRUE) ~ "Batería",
    grepl("Flauta transversal|Saxofón", instrumento, ignore.case = TRUE) ~ "Viento",
    TRUE ~ "Otros"  # Cualquier otro instrumento no especificado anteriormente
  ))

datosInstrumentos$precio <- factor(datosInstrumentos$precio)

hist(datosInstrumentos$frecDaños)
library(ggplot2)
ggplot(datosInstrumentos, aes(x = frecDaños)) +
  geom_bar(stat = "count", fill = "skyblue", color = "black") +
  labs(title = "Frecuencia de Daños en Instrumentos",
       x = "Frecuencia de Daños",
       y = "Cantidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Ajusta el texto del eje x para mejor visualización

ggplot(datosInstrumentos, aes(x = severidad1)) +
  geom_bar(stat = "count", fill = "skyblue", color = "black") +
  labs(title = "Severidad de Daños en Instrumentos",
       x = "Severidad de Daños",
       y = "Cantidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Ajusta el texto del eje x para mejor visualización

library(stringr)

datosInstrumentos <- datosInstrumentos %>%
  mutate(severidad1 = str_replace_all(severidad1, "[^0-9.]", "")) %>% # Elimina todo excepto números y puntos
  mutate(severidad1 = ifelse(severidad1 == "", NA, as.numeric(severidad1))) # Convierte a numérico, asigna NA si está vacío


unique(datosInstrumentos$severidad5)

# Definitivo

library(dplyr)
library(stringr)

datosInstrumentos <- datosInstrumentos %>%
  mutate(across(starts_with("severidad"), ~ str_replace_all(., "[^0-9]", "")), # Eliminar caracteres no numéricos
         across(starts_with("severidad"), ~ ifelse(. == "", NA, as.numeric(.)))) # Convertir a numérico


# Función para categorizar valores
categorizar_valores <- function(x) {
  cut(x, breaks = c(-Inf, 0, 1000, 3000, 5000, 10000, Inf),
       labels = c("0", ">0-1000", "1000-3000", "3000-5000", "5000-10000", ">10000"),
       include.lowest = TRUE)
}

# Aplicar la función de categorización
datosInstrumentos <- datosInstrumentos %>%
  mutate(across(starts_with("severidad"), categorizar_valores, .names = "categoria_{.col}"))

library(purrr)
# Crear lista con las tablas de frecuencia para cada categoría
lista_frecuencias <- map(datosInstrumentos %>% select(starts_with("categoria_severidad")),table)

names(lista_frecuencias) <- paste("frecuencia", names(lista_frecuencias), sep = "_")

# Transformar cada tabla en un data frame y preparar para ggplot
data_frames <- map(lista_frecuencias, ~ as.data.frame(.))
data_frames <- map2(data_frames, names(data_frames), ~ mutate(.x, Category = .y))

# TABLAS DE FRECUENCIA ------------------------



