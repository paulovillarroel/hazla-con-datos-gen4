library(reclin2)
library(tidyverse)
library(stringdist)

# =============================================================================
# 1. DATOS DE EJEMPLO
# =============================================================================

# Dataset: 15 registros representando 5 pacientes (3 registros cada uno)
pacientes_urgencia <- tibble(
  registro_id = 1:15,

  rut = c(
    # PACIENTE 1: María González
    "15.234.567-8",
    "15234567-8",
    "15.234.568-8",
    # PACIENTE 2: Juan Pérez
    "12.345.678-9",
    "12345678-9",
    "12.345.678-9",
    # PACIENTE 3: Carmen Silva
    "18.765.432-K",
    "18.765.432-K",
    "18.765.432-K",
    # PACIENTE 4: Pedro Rodríguez
    "9.876.543-2",
    "9876543-2",
    "9.876.543-2",
    # PACIENTE 5: Ana Torres
    "20.123.456-7",
    "20123456-7",
    "20.123.456-7"
  ),

  nombre = c(
    # Paciente 1
    "María José González Muñoz",
    "Maria Jose Gonzalez",
    "María J. González M.",
    # Paciente 2
    "Juan Carlos Pérez López",
    "Juan Perez Lopez",
    "Juan C. López Pérez",
    # Paciente 3
    "Carmen Silva Rojas",
    "Carmen Silva de Contreras",
    "Carmen Contreras",
    # Paciente 4
    "Pedro Rodríguez Vega",
    "Pedro Rodriguez",
    "Pedo Rodrigez Vega",
    # Paciente 5
    "Ana María Torres Pinto",
    "Ana Torres",
    "Ana M. Torres Pinto"
  ),

  fecha_nacimiento = c(
    "15-03-1985",
    "15/03/1985",
    "15-03-1984",
    "22-11-1978",
    "22/11/1978",
    "1978-11-22",
    "08-05-1990",
    "08/05/1990",
    "1990-05-08",
    "10-07-1972",
    "10/07/1972",
    "11-07-1972",
    "30-12-1995",
    "30/12/1995",
    "1995-12-30"
  ),

  direccion = c(
    "Avenida Libertador Bernardo O'Higgins 1234, Santiago",
    "Av. Libertador B. O'Higgins 1234, Stgo",
    "Av. LB O'Higgins 1234, Santiago Centro",

    "Los Aromos 567, Depto 301, Providencia",
    "Los Aromos 567, Dpto 301, Providencia",
    "Los Aromos 567-301, Providencia",

    "Santa Rosa 890, La Florida",
    "General Velásquez 234, Maipú",
    "Gral. Velásquez 234, Maipú",

    "Alameda 4567, Estación Central",
    "Alameda 4567, Est. Central",
    "Alameda 4568, Estación Central",

    "Vicuña Mackenna 8901, Ñuñoa",
    "V. Mackenna 8901, Ñuñoa",
    "Vicuña Mackenna 8901, La Reina"
  ),

  telefono = c(
    "+56912345678",
    "912345678",
    "9 1234 5678",
    "+56987654321",
    "987654321",
    "976543210",
    "956781234",
    "956781234",
    "+56956781234",
    "945678901",
    "945678901",
    "945678902",
    "932109876",
    "+56932109876",
    "9 3210 9876"
  ),

  prevision = c(
    "FONASA A",
    "Fonasa A",
    "FONASA-A",
    "FONASA B",
    "FONASA B",
    "Isapre Banmédica",
    "FONASA C",
    "Fonasa C",
    "FONASA C",
    "Isapre Consalud",
    "Isapre Consalud",
    "I. Consalud",
    "FONASA D",
    "Fonasa D",
    "FONASA-D"
  ),

  diagnostico_ingreso = c(
    "IAM inferior",
    "Infarto agudo al miocardio inferior",
    "IAM de cara inferior",
    "Neumonía adquirida en la comunidad",
    "NAC",
    "Neumonía comunitaria",
    "ACV isquémico",
    "Accidente cerebrovascular isquémico",
    "AVE isquémico",
    "Apendicitis aguda",
    "Apendicitis aguda no complicada",
    "Abdomen agudo apendicitis",
    "Fractura de radio distal",
    "Fx radio distal izquierdo",
    "Fractura muñeca izquierda"
  ),

  fecha_ingreso = c(
    "2024-01-15 14:30:00",
    "2024-01-15 14:35:00",
    "2024-01-15 14:32:00",
    "2024-02-20 09:15:00",
    "2024-02-25 11:00:00",
    "2024-03-01 16:45:00",
    "2024-03-10 08:00:00",
    "2024-03-10 08:05:00",
    "2024-03-10 08:02:00",
    "2024-04-05 22:30:00",
    "2024-04-10 10:00:00",
    "2024-04-05 22:35:00",
    "2024-05-12 13:20:00",
    "2024-05-12 13:25:00",
    "2024-05-12 13:22:00"
  ),

  hospital_ingreso = c(
    "Hospital Clínico Universidad de Chile",
    "H. Clínico U. de Chile",
    "HCUCH",
    "Hospital San José",
    "Hospital San José",
    "Hospital del Salvador",
    "Hospital Barros Luco",
    "H. Barros Luco",
    "HBL",
    "Hospital Sótero del Río",
    "H. Sótero del Río",
    "HSR",
    "Clínica Las Condes",
    "Clínica Las Condes",
    "CLC"
  )
)

print(pacientes_urgencia)

# =============================================================================
# 2. PREPROCESAMIENTO
# =============================================================================

pacientes_limpios <- pacientes_urgencia |>
  mutate(
    # Limpiar RUT
    rut_limpio = str_replace_all(rut, "[.-]", ""),

    # Normalizar nombre
    nombre_limpio = nombre |>
      str_to_lower() |>
      (\(x) chartr("áéíóúüñ", "aeiouun", x))() |>
      str_replace_all("[.,]", "") |>
      str_squish(),

    # Normalizar fecha nacimiento a ISO
    fecha_nac_limpia = case_when(
      str_detect(fecha_nacimiento, "^\\d{4}") ~ fecha_nacimiento,
      str_detect(fecha_nacimiento, "/") ~ {
        partes <- str_split(fecha_nacimiento, "/", simplify = TRUE)
        paste(partes[, 3], partes[, 2], partes[, 1], sep = "-")
      },
      TRUE ~ {
        partes <- str_split(fecha_nacimiento, "-", simplify = TRUE)
        if (nchar(partes[1, 1]) == 4) {
          fecha_nacimiento
        } else {
          paste(partes[, 3], partes[, 2], partes[, 1], sep = "-")
        }
      }
    ),

    # Extraer año para blocking
    anio_nacimiento = str_extract(fecha_nac_limpia, "^\\d{4}")
  )

# =============================================================================
# 3. GENERAR PARES SIN BLOCKING
# =============================================================================

pares <- pair(
  pacientes_limpios,
  pacientes_limpios
)

# =============================================================================
# 4. COMPARAR CON JARO-WINKLER
# =============================================================================

pares <- compare_pairs(
  pares,
  on = c("nombre_limpio", "rut_limpio"),
  default_comparator = cmp_jarowinkler()
)

# =============================================================================
# 5. MÉTODO PROBABILÍSTICO: EM ALGORITHM
# =============================================================================

modelo_em <- problink_em(~nombre_limpio, data = pares)

print(modelo_em)

pares <- predict(modelo_em, pares, add = TRUE)

# Filtrar pares donde un registro se compara consigo mismo
pares <- pares |> filter(.x != .y)

# Visualizar distribución de scores
ggplot(as.data.frame(pares), aes(x = weights)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.8) +
  geom_vline(
    xintercept = 0,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Distribución de Scores Probabilísticos (EM)",
    x = "Log-likelihood ratio (weights)",
    y = "Número de pares"
  ) +
  theme_minimal()

# =============================================================================
# 6. SELECCIONAR PARES POR UMBRAL
# =============================================================================

# Para deduplicación, usar umbral directo (no greedy) para permitir matches transitivos
umbral <- 0.9

pares_finales <- pares |>
  mutate(seleccionado = weights >= umbral & .x < .y)

pares_finales |>
  filter(seleccionado == TRUE) |>
  select(.x, .y, weights, nombre_limpio, rut_limpio) |>
  arrange(desc(weights)) |>
  print()

# =============================================================================
# 7. CREAR DATASET DEDUPLICADO
# =============================================================================

# Usar solo los pares seleccionados por umbral para crear grupos
pares_seleccionados <- pares_finales |>
  filter(seleccionado == TRUE) |>
  select(.x, .y)

# Inicializar cada registro en su propio grupo
grupo_map <- setNames(1:nrow(pacientes_urgencia), 1:nrow(pacientes_urgencia))

# Función para encontrar raíz del grupo (con compresión de ruta)
find_root <- function(x, grupo_map) {
  if (grupo_map[as.character(x)] != x) {
    grupo_map[as.character(x)] <<- find_root(
      grupo_map[as.character(x)],
      grupo_map
    )
  }
  return(grupo_map[as.character(x)])
}

# Unir grupos para cada par seleccionado
for (i in 1:nrow(pares_seleccionados)) {
  root_x <- find_root(pares_seleccionados$.x[i], grupo_map)
  root_y <- find_root(pares_seleccionados$.y[i], grupo_map)
  if (root_x != root_y) {
    # Unir grupos (el menor se convierte en raíz)
    grupo_map[as.character(max(root_x, root_y))] <- min(root_x, root_y)
  }
}

# Asignar grupo final a cada registro
grupos_duplicados <- tibble(
  registro_id = 1:nrow(pacientes_urgencia),
  grupo = sapply(1:nrow(pacientes_urgencia), function(x) {
    find_root(x, grupo_map)
  })
)

print(grupos_duplicados)

# Unir con datos originales
pacientes_con_grupos <- pacientes_urgencia |>
  left_join(grupos_duplicados, by = "registro_id") |>
  arrange(grupo, registro_id)
