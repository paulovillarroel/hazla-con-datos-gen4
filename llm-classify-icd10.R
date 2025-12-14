# ============================================================================
# CIE-10 Classification with RAG (Retrieval-Augmented Generation)
# ============================================================================
# Clasificación mejorada de CIE-10 usando RAG para recuperar códigos relevantes
# en lugar de pasar todos los códigos en el prompt

library(mall)
library(ellmer)
library(tidyverse)
library(jsonlite)
library(ragnar)

# ----------------------------------------------------------------------------
# 1. SETUP
# ----------------------------------------------------------------------------

# Configurar API key (debe estar en .Renviron)
# usethis::edit_r_environ()
# Sys.getenv("GROQ_API_KEY")

llm_model <- "kimi-k2:1t-cloud"

# # Configurar chat con Groq API
# chat <- chat_groq(
#   model = llm_model,
#   system_prompt = "Clasifica CIE-10. Responde SOLO con JSON sin explicaciones: {\"code\":\"...\"}"
# )

chat <- chat_ollama(
  model = llm_model,
  system_prompt = "Clasifica CIE-10. Responde SOLO con JSON sin explicaciones: {\"code\":\"...\"}"
)
llm_use(chat)

# Cargar textos médicos
text <- tibble(
  texto = c(
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
    "tumor maligno mama parte no especificada hernia inguinal crural umbilical linea blanca similares"
  )
)

# ----------------------------------------------------------------------------
# 2. CARGAR Y PREPARAR CIE-10 REFERENCE
# ----------------------------------------------------------------------------

cat("Cargando JSON de CIE-10...\n")

# Cargar CIE-10 reference desde archivo local
cie10_raw <- fromJSON("raw-data/cie10-codes.json")

cat("Total de códigos en JSON:", nrow(cie10_raw), "\n")

# Preparar datos para RAG store
# Combinar nivel 1 y 2 con información estructurada
cie10_chunks <- cie10_raw |>
  filter(level %in% c(1, 2)) |>
  mutate(
    # Crear chunk_id único
    chunk_id = paste0("cie10_", code),
    # Texto para embeddings (código + descripción)
    text = paste0(code, ": ", description),
    # Metadata adicional
    level_priority = ifelse(level == 2, 2, 1)
  ) |>
  select(chunk_id, text, code, description, level, level_priority, everything())

cat("Total de códigos para RAG:", nrow(cie10_chunks), "\n")
cat("  - Nivel 2 (específicos):", sum(cie10_chunks$level == 2), "\n")
cat("  - Nivel 1 (generales):", sum(cie10_chunks$level == 1), "\n\n")

# ----------------------------------------------------------------------------
# 3. CREAR RAG STORE CON EMBEDDINGS
# ----------------------------------------------------------------------------

cat("Creando RAG store con embeddings...\n")
cat("(Esto puede tomar varios minutos la primera vez)\n\n")

# Path para la base de datos DuckDB
store_path <- "raw-data/cie10_rag_store.duckdb"

# Verificar si el store ya existe
if (file.exists(store_path)) {
  cat("Store existente encontrado, conectando...\n")
  store <- ragnar_store_connect(store_path, read_only = FALSE)

  # Cargar extensiones necesarias para FTS y VSS
  cat("Cargando extensiones DuckDB (fts, vss)...\n")
  DBI::dbExecute(store@con, "LOAD fts")
  DBI::dbExecute(store@con, "LOAD vss")
} else {
  cat("Creando nuevo store...\n")

  # Crear store con embeddings de Ollama (local)
  store <- ragnar_store_create(
    store_path,
    version = 1,
    embed = function(x) {
      embed_ollama(
        x,
        base_url = "http://localhost:11434",
        model = "embeddinggemma:latest",
        batch_size = 10L
      )
    }
  )

  # Cargar extensiones necesarias para FTS y VSS
  cat("Cargando extensiones DuckDB (fts, vss)...\n")
  DBI::dbExecute(store@con, "LOAD fts")
  DBI::dbExecute(store@con, "LOAD vss")

  cat("Insertando", nrow(cie10_chunks), "códigos CIE-10...\n")
  ragnar_store_insert(store, cie10_chunks)

  cat("Construyendo índice de búsqueda...\n")
  ragnar_store_build_index(store)

  cat("Store creado exitosamente!\n\n")
}

# ----------------------------------------------------------------------------
# 4. REGISTRAR TOOL DE RAG + FUNCIÓN DE CLASIFICACIÓN
# ----------------------------------------------------------------------------

top_k_default <- 5L
# No usamos ragnar_register_tool_retrieve porque haremos retrieval manual
# ragnar_register_tool_retrieve(chat, store, top_k = top_k_default)

safe_parse_json <- function(x) {
  if (is.na(x) || is.null(x) || !nzchar(x)) {
    return(NULL)
  }
  x <- str_trim(x)
  x <- str_replace_all(x, "^```(json)?\\s*|\\s*```$", "")
  tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)
}

extract_code_fallback <- function(x) {
  if (is.na(x) || is.null(x)) {
    return(NA_character_)
  }
  x <- as.character(x)
  m <- str_match(x, "([A-Z][0-9]{2}(?:\\.[0-9A-Z]{1,4})?)")
  m[, 2]
}

classify_with_rag <- function(medical_text, chat, top_k = top_k_default) {
  # PASO 1: Hacer retrieval ANTES de construir el prompt
  # Usar búsqueda híbrida (BM25 + VSS) para mejor precisión
  # BM25 detecta keywords exactas (ej: "maligno" vs "benigno")
  # VSS detecta similitud semántica
  retrieved <- ragnar_retrieve(
    store,
    medical_text,
    top_k = top_k
  )

  # PASO 2: Formatear códigos recuperados (solo códigos, sin descripciones largas)
  codes_context <- retrieved |>
    mutate(code = str_extract(text, "^[A-Z][0-9]{2}(?:\\.[0-9A-Z]{1,4})?")) |>
    pull(code) |>
    paste(collapse = ", ")

  # PASO 3: Construir prompt directo
  prompt <- paste0(
    "Códigos: ",
    codes_context,
    "\nTexto: ",
    medical_text,
    "\nJSON:"
  )

  # PASO 4: Enviar al LLM con contexto incluido
  raw <- chat$chat(prompt)
  parsed <- safe_parse_json(raw)

  # PASO 5: Parsear respuesta
  if (!is.null(parsed) && !is.null(parsed$code)) {
    code_only <- str_trim(parsed$code)
  } else {
    code_only <- extract_code_fallback(raw)
  }

  # Obtener descripción desde los códigos recuperados
  desc <- retrieved |>
    filter(str_detect(text, paste0("^", code_only, ":"))) |>
    pull(text) |>
    first()

  if (is.null(desc) || length(desc) == 0) {
    desc <- NA_character_
  }

  tibble(
    texto = medical_text,
    cie10_code = code_only,
    cie10_description = desc,
    llm_raw = raw,
    retrieved_codes = list(retrieved),
    n_codes_retrieved = nrow(retrieved),
    top_k_requested = top_k,
    llm_model = llm_model,
    embedder_model = "embeddinggemma:latest",
    retrieval_method = "hybrid_bm25_vss"
  )
}

# ----------------------------------------------------------------------------
# 5. CLASIFICAR TEXTOS CON RAG
# ----------------------------------------------------------------------------

cat("Iniciando clasificación con RAG (Retrieval-Augmented Generation)...\n")
cat("PROCESO:\n")
cat("  1. Búsqueda HÍBRIDA (BM25 + VSS) para cada texto médico\n")
cat("     - BM25: Detecta keywords exactas (ej: 'maligno' vs 'benigno')\n")
cat("     - VSS: Similitud semántica con embeddings\n")
cat(
  "  2. Recuperar top-",
  top_k_default,
  " códigos CIE-10 más relevantes\n",
  sep = ""
)
cat("  3. Pasar códigos al LLM para seleccionar el más apropiado\n")
cat("\nProcesando", nrow(text), "textos médicos...\n\n")

# Aplicar clasificación con RAG a todos los textos
texts_classified <- text |>
  rowwise() |>
  mutate(
    classification = list(classify_with_rag(texto, chat, top_k = top_k_default))
  ) |>
  ungroup() |>
  unnest(classification, names_sep = "_") |>
  select(-classification_texto) # Remover columna duplicada de texto

cat("Clasificación completada!\n\n")

# ----------------------------------------------------------------------------
# 6. POST-PROCESAMIENTO Y VALIDACIÓN
# ----------------------------------------------------------------------------

cat("Post-procesando resultados...\n")

# Extraer solo el código (parte antes de ":")
texts_classified <- texts_classified |>
  mutate(
    code_only = str_trim(classification_cie10_code),

    # Validar que el código existe en la referencia
    code_valid = code_only %in% cie10_chunks$code | code_only == "NO_MATCH",

    # Flagear casos que necesitan revisión
    needs_review = !code_valid
  )

# ----------------------------------------------------------------------------
# 7. RESULTADOS Y ANÁLISIS
# ----------------------------------------------------------------------------

cat("\n")
cat(strrep("=", 70), "\n")
cat("RESULTADOS DE CLASIFICACIÓN CON RAG\n")
cat(strrep("=", 70), "\n\n")

# Vista de resultados
cat("PRIMEROS 10 RESULTADOS:\n")
cat(strrep("-", 70), "\n")
texts_classified |>
  select(texto, classification_cie10_code) |>
  slice_head(n = 10) |>
  print()

cat("\n")
cat("RESUMEN DE CÓDIGOS MÁS FRECUENTES:\n")
cat(strrep("-", 70), "\n")
texts_classified |>
  count(code_only, sort = TRUE) |>
  print()

cat("\n")
cat("ESTADÍSTICAS DE VALIDACIÓN:\n")
cat(strrep("-", 70), "\n")
cat("Total de textos clasificados:", nrow(texts_classified), "\n")
cat("Códigos válidos:", sum(texts_classified$code_valid, na.rm = TRUE), "\n")
cat(
  "Códigos inválidos (necesitan revisión):",
  sum(texts_classified$needs_review, na.rm = TRUE),
  "\n"
)
cat(
  "Casos sin match:",
  sum(texts_classified$code_only == "NO_MATCH", na.rm = TRUE),
  "\n"
)

# Mostrar casos que necesitan revisión (si existen)
if (sum(texts_classified$needs_review, na.rm = TRUE) > 0) {
  cat("\n")
  cat("CASOS QUE NECESITAN REVISIÓN:\n")
  cat(strrep("-", 70), "\n")
  texts_classified |>
    filter(needs_review) |>
    select(texto, classification_cie10_code, code_only) |>
    print()
}

# Distribución por nivel de código
cat("\n")
cat("DISTRIBUCIÓN POR NIVEL DE CÓDIGO:\n")
cat(strrep("-", 70), "\n")
texts_classified |>
  left_join(
    cie10_chunks |> select(code, level_priority),
    by = c("code_only" = "code")
  ) |>
  count(level_priority) |>
  mutate(
    nivel = case_when(
      level_priority == 2 ~ "Nivel 2 (Específico)",
      level_priority == 1 ~ "Nivel 1 (General)",
      TRUE ~ "Sin match o inválido"
    )
  ) |>
  select(nivel, n) |>
  print()

cat("\n")
cat("VENTAJAS DEL ENFOQUE RAG:\n")
cat(strrep("-", 70), "\n")
cat(
  "✓ Solo",
  top_k_default,
  "códigos relevantes por clasificación (vs 1,843 totales)\n"
)
cat("✓ Reducción de ~99% en tokens del prompt\n")
cat("✓ Búsqueda HÍBRIDA: BM25 (keywords) + VSS (semántica)\n")
cat("✓ Mayor precisión: Detecta diferencias críticas (maligno vs benigno)\n")
cat("✓ Embeddings locales con Ollama (privacidad)\n")
cat("✓ Menor costo y menor latencia por clasificación\n")
cat("✓ Escalable a cualquier tamaño de catálogo CIE-10\n")

cat("\n")
cat(strrep("=", 70), "\n")
cat("CLASIFICACIÓN COMPLETADA\n")
cat(strrep("=", 70), "\n")

# ----------------------------------------------------------------------------
# 8. ANÁLISIS DE CÓDIGOS RECUPERADOS (OPCIONAL)
# ----------------------------------------------------------------------------

# Ver códigos recuperados para el primer texto como ejemplo
cat("\n\nEJEMPLO DE CÓDIGOS RECUPERADOS PARA EL PRIMER TEXTO:\n")
cat(strrep("-", 70), "\n")
cat("Texto:", texts_classified$texto[1], "\n\n")
cat("Top 30 códigos más relevantes recuperados:\n")
texts_classified$classification_retrieved_codes[[1]] |>
  select(text) |>
  print(n = 30)

try(DBI::dbDisconnect(store@con), silent = TRUE)

# Guardar resultados (opcional)
# write_csv(texts_classified |> select(-classification_retrieved_codes), "resultados-cie10-rag.csv")
# saveRDS(texts_classified, "resultados-cie10-rag.rds")
