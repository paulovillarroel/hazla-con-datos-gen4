# ============================================================================
# Clasificación CIE-10 con RAG (Retrieval-Augmented Generation)
# ============================================================================

# Versión simplificada para enseñanza (sin testing, debugging avanzado ni fallbacks)
# Concepto: El LLM usa tool calling para buscar códigos relevantes automáticamente

library(mall)
library(ellmer)
library(tidyverse)
library(jsonlite)
library(ragnar)

# ----------------------------------------------------------------------------
# 1. CONFIGURACIÓN
# ----------------------------------------------------------------------------

# Activar para ver detalles de debugging (TRUE/FALSE)
DEBUG <- FALSE

llm_model <- "kimi-k2:1t-cloud"

# Configurar chat con Ollama
chat <- chat_ollama(
  model = llm_model,
  system_prompt = paste(
    "Eres un experto en clasificación médica CIE-10.",
    "USA SIEMPRE la herramienta 'retrieve' para buscar códigos en el catálogo.",
    "Responde SOLO con JSON."
  )
)

llm_use(chat)

# Textos médicos de ejemplo
text <- tibble(
  texto = c(
    "tumor subescapular derecho dermatologia tegumentos",
    "tumor cabeza pancreas pancreatoduodenectomia cirugia general",
    "carcinoma in situ piel labio biopsia quir mucosa",
    "tumorectomia mamaria no ges mastectomia parcial cuadrante",
    "quiste pequeño en base de lengua cirugia maxilofacial",
    "tumor palpebral estudio ambos ojos biopsia palperal cirugia oftalmologica",
    "tumor comportamiento incierto desconocido sitio no especificado na neurocirugia",
    "tumor vertebral t7t8 neurocirugia",
    "tumor maligno intestino delgado parte no esSpecificada cirugia digestiva",
    "mieloma multiple requiere hemograma completo y pruebas de funcion renal",
    "extraccion via anal tumor maligno recto cirugia adulto",
    "tumores quistes lesiones pseudoquisticas izquierda musculares tendineas grupo 21 traumatologia",
    "cirugia cabeza cuello d442 tumor glandula paratiroides tiroidectomia total ampliada incluye extirpacion estructuras anatomicas vecinas",
    "c160 tumor maligno cardias hernia inguinal crural umbilical lanea bl",
    "tumor parotida cirugia adulto",
    "inicio de insulina por diabetes mellitus tipo 2 de mal manejo",
    "tumor comportamiento incierto desconocido ovario",
    "linfoma no hodgkin b alto grado",
    "tumor maligno piel cuero cabelludo cuello cirugia cabeza",
    "adenoma benigno prostata cirugia urologia",
    "dolor abdominal difuso con irradiación a la espalda",
    "tumor maligno mama parte no especificada hernia inguinal crural umbilical linea blanca similares recidivada nosimple estrangulada sreseccion intestcu cirugia general adulto",
    "evaluación previa a inicio de quimioetarapia.",
    "obs papiloma labio inferior",
    "lesion cara dorsal lengua, posible lesion de tipo vascular",
    "coledtoliatsis"
  )
)

# ----------------------------------------------------------------------------
# 2. CARGAR CATÁLOGO CIE-10
# ----------------------------------------------------------------------------
cat("Cargando catálogo CIE-10...\n")
cie10_raw <- fromJSON("raw-data/cie10-codes-complete.json")

# Preparar códigos para RAG (todos los niveles para mejor matching)
cie10_chunks <- cie10_raw |>
  mutate(
    chunk_id = paste0("cie10_", code),
    text = paste0(code, ": ", description)
  ) |>
  select(chunk_id, text, code, description, level)

cat("Total de códigos CIE-10:", nrow(cie10_chunks), "\n")
cat("  Distribución por nivel:\n")

cie10_chunks |>
  count(level) |>
  arrange(level) |>
  mutate(label = paste0("    Nivel ", level, ": ", n, " códigos")) |>
  pull(label) |>
  cat(sep = "\n")
cat("\n")

# ----------------------------------------------------------------------------
# 3. CREAR RAG STORE CON EMBEDDINGS
# ----------------------------------------------------------------------------

# NOTA: Si es la primera vez, instalar extensiones de DuckDB
# Descomentar y ejecutar una sola vez:

# library(duckdb)
# con <- dbConnect(duckdb())
# dbExecute(con, "INSTALL fts")
# dbExecute(con, "INSTALL vss")
# dbDisconnect(con)

# IMPORTANTE: Si cambió el número de códigos (ahora incluye todos los niveles),
# debe borrar el store existente para rebuild:
# file.remove("raw-data/cie10_rag_store.duckdb")

store_path <- "raw-data/cie10_rag_store.duckdb"

if (file.exists(store_path)) {
  cat("Conectando a store existente...\n")
  store <- ragnar_store_connect(store_path, read_only = FALSE)
  # Cargar extensiones necesarias
  DBI::dbExecute(store@con, "LOAD fts")
  DBI::dbExecute(store@con, "LOAD vss")
} else {
  cat("Creando RAG store con embeddings...\n")
  cat("(Esto toma varios minutos la primera vez)\n\n")
  # Crear store con embeddings locales de Ollama
  store <- ragnar_store_create(
    store_path,
    version = 1,
    embed = function(x) {
      embed_ollama(
        x,
        base_url = "http://localhost:11434",
        model = "embeddinggemma:latest",
        batch_size = 50L
      )
    }
  )

  # Cargar extensiones
  DBI::dbExecute(store@con, "LOAD fts")
  DBI::dbExecute(store@con, "LOAD vss")
  # Insertar códigos CIE-10
  cat("Insertando", nrow(cie10_chunks), "códigos...\n")
  ragnar_store_insert(store, cie10_chunks)
  # Construir índice de búsqueda
  cat("Construyendo índice híbrido (BM25 + VSS)...\n")
  ragnar_store_build_index(store)
  cat("✓ Store creado!\n\n")
}

# ----------------------------------------------------------------------------
# 4. REGISTRAR TOOL DE RETRIEVAL
# ----------------------------------------------------------------------------

cat("Registrando tool de retrieval para el LLM...\n")
ragnar_register_tool_retrieve(chat, store, top_k = 10)
cat("✓ LLM puede ahora usar la herramienta 'retrieve'\n\n")

# ----------------------------------------------------------------------------
# 5. FUNCIÓN DE CLASIFICACIÓN
# ----------------------------------------------------------------------------

classify_with_rag <- function(medical_text, chat) {
  # Prompt con solicitud de alternativas
  prompt <- paste0(
    "Clasifica este texto médico con el código CIE-10 más apropiado.\n\n",
    "IMPORTANTE: Usa OBLIGATORIAMENTE la herramienta 'retrieve' para buscar códigos en el catálogo.\n",
    "SOLO puedes usar códigos que existan en los resultados de 'retrieve'.\n",
    "NO inventes códigos - DEBES seleccionar de los resultados de búsqueda.\n\n",
    "Responde SOLO con JSON incluyendo el código principal y hasta 2 alternativas:\n",
    "{\"code\":\"X99.9\", \"alternatives\":[\"X99.8\"]}\n\n",
    "Texto: ",
    medical_text
  )

  # El LLM decide cuándo y cómo usar la tool de retrieval
  response <- chat$chat(prompt)

  # Extraer código y alternativas de la respuesta
  parsed <- tryCatch(
    jsonlite::fromJSON(str_trim(response)),
    error = function(e) NULL
  )
  if (!is.null(parsed) && !is.null(parsed$code)) {
    code <- str_trim(parsed$code)
    # Extraer alternativas si existen
    if (!is.null(parsed$alternatives) && length(parsed$alternatives) > 0) {
      alternatives <- str_trim(parsed$alternatives)
    } else {
      alternatives <- character(0)
    }
  } else {
    # Fallback: regex para extraer código CIE-10
    match <- str_match(response, "([A-Z][0-9]{2}(?:\\.[0-9A-Z]{1,4})?)")
    code <- match[, 2]
    alternatives <- character(0)
  }

  # Obtener descripción del catálogo
  if (!is.na(code)) {
    # Limpiar y normalizar el código
    code_clean <- str_trim(code)
    # Remover punto decimal para hacer match con el catálogo (D04.0 → D040)
    code_normalized <- str_replace_all(code_clean, "\\.", "")
    if (DEBUG) {
      cat(
        "\n[DEBUG] Código original:",
        code_clean,
        "→ normalizado:",
        code_normalized,
        "\n"
      )
    }

    # Buscar en el catálogo con código normalizado
    desc_match <- cie10_chunks |>
      filter(code == code_normalized) |>
      pull(description) |>
      first()
    if (DEBUG && (is.null(desc_match) || length(desc_match) == 0)) {
      cat("[DEBUG] No encontrado con matching exacto, intentando flexible...\n")
    }
    # Si no encuentra, intentar matching más flexible
    if (is.null(desc_match) || length(desc_match) == 0) {
      # Buscar si el código está contenido en el texto del catálogo
      desc_match <- cie10_chunks |>
        filter(str_detect(text, paste0("^", code_normalized, ":"))) |>
        pull(description) |>
        first()
    }

    if (DEBUG) {
      if (!is.null(desc_match) && length(desc_match) > 0) {
        # Verificar si desc_match es NA
        if (is.na(desc_match)) {
          cat("[DEBUG] ⚠ Código encontrado pero descripción es NA\n")
        } else {
          cat(
            "[DEBUG] ✓ Encontrado:",
            substr(as.character(desc_match), 1, 50),
            "...\n"
          )
        }
      } else {
        cat("[DEBUG] ✗ No encontrado en catálogo\n")
        # Mostrar códigos similares
        similar <- cie10_chunks |>
          filter(str_detect(
            code,
            paste0("^", substr(code_normalized, 1, 3))
          )) |>
          head(3) |>
          pull(code)
        cat(
          "[DEBUG] Códigos similares en catálogo:",
          paste(similar, collapse = ", "),
          "\n"
        )
      }
    }
    description <- if (is.null(desc_match) || length(desc_match) == 0) {
      NA_character_
    } else {
      desc_match
    }
  } else {
    description <- NA_character_
  }

  # Procesar alternativas
  if (length(alternatives) > 0) {
    # Normalizar alternativas (remover puntos decimales)
    alternatives_normalized <- str_replace_all(alternatives, "\\.", "")
    # Obtener descripciones de alternativas
    alternatives_desc <- map_chr(alternatives_normalized, function(alt_code) {
      desc <- cie10_chunks |>
        filter(code == alt_code) |>
        pull(description) |>
        first()
      if (is.null(desc) || length(desc) == 0 || is.na(desc)) {
        NA_character_
      } else {
        desc
      }
    })

    # Combinar códigos y descripciones
    alternatives_formatted <- paste0(alternatives, " (", alternatives_desc, ")")
    alternatives_str <- paste(alternatives_formatted, collapse = "; ")
  } else {
    alternatives_str <- NA_character_
  }

  # Validar si el código existe en el catálogo
  code_valid <- !is.na(description)
  tibble(
    texto = medical_text,
    codigo = code,
    codigo_valido = code_valid,
    descripcion = description,
    alternativas = alternatives_str
  )
}

# ----------------------------------------------------------------------------
# 6. CLASIFICAR TEXTOS
# ----------------------------------------------------------------------------

cat("Clasificando", nrow(text), "textos médicos...\n\n")

resultados <- text |>
  mutate(
    clasificacion = map(texto, ~ classify_with_rag(.x, chat))
  ) |>
  unnest(clasificacion, names_sep = "_") |>
  select(-clasificacion_texto)

cat("✓ Clasificación completada!\n\n")

# Cerrar conexión
try(DBI::dbDisconnect(store@con), silent = TRUE)
