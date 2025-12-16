# ============================================================================
# CLASIFICADOR CIE-10 CON RAG (Retrieval-Augmented Generation)
# ============================================================================
# Curso: Programación con R y GenAI
# Arquitectura: "Strict RAG" (Recuperación Forzada -> Generación Restringida)

library(mall)
library(ellmer)
library(tidyverse)
library(jsonlite)
library(ragnar)
library(DBI)

# ----------------------------------------------------------------------------
# 1. CONFIGURACIÓN
# ----------------------------------------------------------------------------

DEBUG <- FALSE
llm_model <- "kimi-k2:1t-cloud"

# Configurar chat
chat <- chat_ollama(
  model = llm_model,
  system_prompt = paste(
    "Eres un asistente experto en codificación CIE-10.",
    "Tu trabajo es seleccionar el código más adecuado de una lista proporcionada.",
    "NO uses tu conocimiento interno si contradice la lista.",
    "Responde SIEMPRE en formato JSON válido.",
    sep = "\n"
  )
)

llm_use(chat)

# Datos de prueba
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
# 2. GESTIÓN DE LA BASE VECTORIAL (DUCKDB)
# ----------------------------------------------------------------------------

cat("Preparando Base de Conocimiento CIE-10...\n")
cie10_file <- "raw-data/cie10-codes-complete.json"
store_path <- "raw-data/cie10_rag_store.duckdb"

# Cargar JSON
cie10_raw <- fromJSON(cie10_file)
cie10_chunks <- cie10_raw |>
  mutate(
    chunk_id = paste0("cie10_", code),
    text = paste0(code, ": ", description)
  ) |>
  select(chunk_id, text, code, description, level)

# Inicializar Store
if (file.exists(store_path)) {
  cat("Conectando a store existente...\n")
  store <- ragnar_store_connect(store_path, read_only = FALSE)
  dbExecute(store@con, "LOAD fts")
  dbExecute(store@con, "LOAD vss")
} else {
  cat("Creando índice vectorial (primera ejecución)...\n")
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
  dbExecute(store@con, "LOAD fts")
  dbExecute(store@con, "LOAD vss")
  ragnar_store_insert(store, cie10_chunks)
  ragnar_store_build_index(store)
  cat("✓ Índice creado.\n")
}

# ----------------------------------------------------------------------------
# 3. CORE: FUNCIÓN DE CLASIFICACIÓN (PIPELINE RAG)
# ----------------------------------------------------------------------------

classify_strict_rag <- function(medical_text, chat) {
  # PASO 1: RECUPERACIÓN (Retrieval)
  candidates <- ragnar_retrieve(store, medical_text, top_k = 10)

  codes_available <- candidates |>
    mutate(code = str_extract(text, "^[A-Z0-9]+(?=:)")) |>
    filter(!is.na(code)) |>
    pull(code) |>
    unique()

  if (length(codes_available) == 0) {
    return(tibble(
      texto = medical_text,
      codigo = NA,
      valido = FALSE,
      descripcion = "Sin coincidencias",
      alternativas = NA
    ))
  }

  # PASO 2: GENERACIÓN (Augmented Generation)
  codes_list_str <- paste(codes_available, collapse = ", ")

  prompt <- paste0(
    "Analiza el texto médico: '",
    medical_text,
    "'\n\n",
    "Candidatos CIE-10 disponibles (SOLO elige de aquí):\n",
    codes_list_str,
    "\n\n",
    "INSTRUCCIONES:\n",
    "1. Selecciona el mejor código de la lista anterior.\n",
    # AJUSTE: Pedimos explícitamente 2
    "2. Selecciona 2 códigos alternativos relevantes de la lista (si existen).\n",
    "3. Responde JSON: {\"code\":\"...\", \"alternatives\":[\"Alt1\", \"Alt2\"]}"
  )

  response <- chat$chat(prompt)

  # PASO 3: PARSING Y LIMPIEZA
  parsed <- tryCatch(
    jsonlite::fromJSON(str_trim(response)),
    error = function(e) NULL
  )

  code <- NA_character_
  alternatives <- character(0)

  if (!is.null(parsed)) {
    if (!is.null(parsed$code)) {
      code <- str_trim(parsed$code)
    }
    if (!is.null(parsed$alternatives)) {
      alternatives <- unlist(parsed$alternatives)
    }
  } else {
    match <- str_match(response, "([A-Z][0-9]{2}(?:\\.[0-9A-Z]{1,4})?)")
    if (!is.na(match[1, 2])) code <- match[1, 2]
  }

  # PASO 4: VALIDACIÓN
  code_clean <- if (!is.na(code)) str_replace_all(code, "\\.", "") else NA

  if (!is.na(code_clean) && !code_clean %in% codes_available) {
    if (DEBUG) {
      cat(" [Auto-Fix] El LLM alucinó ", code_clean, ". Usando top 1.\n")
    }
    code_clean <- codes_available[1]
    code <- code_clean
  }

  desc_match <- cie10_chunks |>
    filter(code == code_clean) |>
    pull(description) |>
    first()
  is_valid <- !is.na(desc_match)
  final_desc <- if (is_valid) desc_match else "CÓDIGO NO VÁLIDO"

  # Formatear alternativas
  alt_str <- NA_character_
  if (length(alternatives) > 0) {
    alt_clean <- str_replace_all(alternatives, "\\.", "")
    valid_alts <- cie10_chunks |> filter(code %in% alt_clean)
    if (nrow(valid_alts) > 0) {
      # Mantenemos descripciones concisas aquí para que entren 2 en el string final
      alt_str <- paste(
        paste0(
          valid_alts$code,
          " (",
          str_trunc(valid_alts$description, 50),
          ")"
        ),
        collapse = "; "
      )
    }
  }

  tibble(
    texto = medical_text,
    codigo = code,
    valido = is_valid,
    descripcion = final_desc,
    alternativas = alt_str
  )
}

# ----------------------------------------------------------------------------
# 4. EJECUCIÓN DEL CLASIFICADOR
# ----------------------------------------------------------------------------

cat("\nClasificando", nrow(text), "casos médicos...\n")
pb <- txtProgressBar(min = 0, max = nrow(text), style = 3)

resultados <- text |>
  mutate(
    res = map(texto, function(t) {
      setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
      # Eliminamos 'texto' del resultado de la función para no tener duplicados
      classify_strict_rag(t, chat) |>
        select(-texto)
    })
  ) |>
  unnest(res) # unnest limpio

close(pb)

# ----------------------------------------------------------------------------
# 5. REPORTE DE RESULTADOS (DASHBOARD DOCENTE)
# ----------------------------------------------------------------------------

cat("\n\n", strrep("=", 80), "\n")
cat(" REPORT CARD: MODELO CIE-10 RAG\n")
cat(strrep("=", 80), "\n")

# Métricas
n_total <- nrow(resultados)
n_ok <- sum(resultados$valido, na.rm = TRUE)
cat(
  " Precisión Técnica (Códigos existen): ",
  n_ok,
  "/",
  n_total,
  paste0("(", round(n_ok / n_total * 100), "%)\n\n")
)

# Tabla de Auditoría
resultados |>
  select(texto, codigo, descripcion, alternativas) |>
  mutate(
    texto = str_trunc(texto, 30),
    descripcion = str_trunc(descripcion, 35),
    alternativas = str_trunc(replace_na(alternativas, "-"), 120)
  ) |>
  print(n = Inf)

# Cleanup
try(dbDisconnect(store@con), silent = TRUE)
