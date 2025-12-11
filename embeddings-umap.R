library(ollamar)
library(uwot) # Para UMAP
library(ggplot2) # Para graficar
library(dplyr) # Para manipulación de datos
library(purrr) # Para iterar (map)
library(plotly)


# 1. CREAMOS DATOS DE EJEMPLO (Pequeño dataset de salud)
# Para enseñar, necesitamos grupos distintos para ver si el modelo los separa.

frases_salud <- tibble(
  texto = c(
    # Grupo A: Oncología / Pulmón
    "Paciente con cáncer pulmonar",
    "Paciente con patología neoplásica pulmonar",
    "Carcinoma de células pequeñas en pulmón derecho",
    "Masa pulmonar sugestiva de malignidad",
    "Neoplasia bronquial avanzada",

    # Grupo B: Dolor Abdominal / Digestivo
    "Paciente dolor abdominal agudo",
    "Dolor intenso en fosa iliaca derecha",
    "Cuadro de apendicitis probable",
    "Malestar estomacal y gastritis",
    "Dolor tipo cólico en hipocondrio",

    # Grupo C: Neurología

    "Cefalea tensional crónica",
    "Migraña con aura visual",
    "Paciente reporta dolor de cabeza intenso",
    "Pérdida de consciencia y mareos",
    #"Prostatectomía radical",
    "Síncope de origen neurológico"
  ),
  categoria = rep(c("Oncología", "Digestivo", "Neurología"), each = 5)
)

# 2. OBTENER EMBEDDINGS
# Iteramos sobre cada texto para obtener su vector de 768 dimensiones

frases_con_embeddings <- frases_salud |>
  mutate(
    # map_dfr itera por cada texto y llama a ollama
    emb_data = map(texto, ~ embed("embeddinggemma:latest", .x))
  )

# 3. PREPARAR LA MATRIZ (FORZANDO LA FORMA)
vector_gigante <- unlist(frases_con_embeddings$emb_data)

# Luego forzamos la creación de la matriz
matriz_embeddings <- matrix(vector_gigante, nrow = 15, byrow = TRUE)

# Verificación final: AHORA SÍ debe dar [1] 15 768
print(dim(matriz_embeddings))

# 4. APLICAR UMAP (Reducción a 2D)
# n_neighbors: Con pocos datos (15), usamos un número bajo (ej. 4 o 5)
umap_res <- umap(matriz_embeddings, n_neighbors = 5, n_components = 2)

# Unimos las coordenadas X e Y con nuestros datos originales
datos_plot <- frases_salud |>
  mutate(
    UMAP_1 = umap_res[, 1],
    UMAP_2 = umap_res[, 2]
  )

# 5. VISUALIZACIÓN
plot_ly(
  datos_plot,
  x = ~UMAP_1,
  y = ~UMAP_2,
  color = ~categoria,
  text = ~texto,
  type = "scatter",
  mode = "markers",
  marker = list(size = 10, opacity = 0.8)
) |>
  layout(
    title = "Espacio Vectorial Reducido (UMAP) - De 768 a 2 dimensiones",
    xaxis = list(title = "Dimensión Latente 1"),
    yaxis = list(title = "Dimensión Latente 2")
  )
