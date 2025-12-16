# ============================================================================
# CLASIFICADOR CIE-10 "HYBRID PRO"
# ============================================================================

library(mall)
library(ellmer)
library(tidyverse)
library(jsonlite)
library(ragnar)
library(DBI)

# ----------------------------------------------------------------------------
# 1. CONFIGURACIÓN
# ----------------------------------------------------------------------------

llm_model <- "kimi-k2:1t-cloud"

# Configuración del Chat con temperatura baja
chat <- chat_ollama(
  model = llm_model,
  system_prompt = "Eres un codificador clínico experto. Tu prioridad es la precisión anatómica y patológica."
)
llm_use(chat)

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
# 2. CARGAR DATOS (ESTO FALTABA)
# ----------------------------------------------------------------------------

cie10_file <- "raw-data/cie10-codes-complete.json"
store_path <- "raw-data/cie10_rag_store.duckdb"

cat("Cargando catálogo CIE-10 en memoria...\n")
# IMPORTANTE: Necesitamos esto en memoria para la validación final
cie10_raw <- fromJSON(cie10_file)
cie10_chunks <- cie10_raw |>
  mutate(
    chunk_id = paste0("cie10_", code),
    text = paste0(code, ": ", description)
  ) |>
  select(chunk_id, text, code, description, level)

# Conectar a DuckDB
if (!file.exists(store_path)) {
  stop(
    "⚠️ Error: No se encuentra el archivo 'cie10_rag_store.duckdb'. Ejecuta el script de creación primero."
  )
}

cat("Conectando a RAG Store...\n")
store <- ragnar_store_connect(store_path, read_only = FALSE)
dbExecute(store@con, "LOAD fts")
dbExecute(store@con, "LOAD vss")

# ----------------------------------------------------------------------------
# 3. FUNCIÓN DE CLASIFICACIÓN AVANZADA
# ----------------------------------------------------------------------------

classify_hybrid_pro <- function(medical_text, chat) {
  # --- FASE 1: ANÁLISIS PREVIO (DETERMINISTA) ---
  explicit_match <- str_extract(medical_text, "(?i)[A-Z][0-9]{2}\\.?[0-9]?")

  hint_text <- ""
  if (!is.na(explicit_match)) {
    clean_hint <- str_to_upper(explicit_match)
    if (!str_detect(clean_hint, "\\.")) {
      clean_hint <- paste0(
        str_sub(clean_hint, 1, 3),
        ".",
        str_sub(clean_hint, 4, 4)
      )
    }
    hint_text <- paste0(
      "\nPISTA IMPORTANTE: El texto menciona explícitamente el código '",
      clean_hint,
      "'. ",
      "Si este código está en la lista de candidatos, PRIORÍZALO."
    )
  }

  # --- FASE 2: RECUPERACIÓN (RETRIEVAL) ---
  candidates <- ragnar_retrieve(store, medical_text, top_k = 20)

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
      descripcion = "Sin candidatos",
      alternativas = NA
    ))
  }

  # --- FASE 3: GENERACIÓN CON RAZONAMIENTO (CoT) ---
  codes_list_str <- paste(codes_available, collapse = ", ")

  prompt <- paste0(
    "Analiza el siguiente caso clínico paso a paso.\n",
    "Texto: '",
    medical_text,
    "'\n",
    hint_text,
    "\n\n",
    "Lista de Códigos Candidatos:\n",
    codes_list_str,
    "\n\n",
    "REGLAS DE RAZONAMIENTO:\n",
    "1. IDENTIFICA LA ANATOMÍA: ¿Qué órgano está afectado? (Ej: Si dice 'vertebral', es hueso, no cerebro).\n",
    "2. FILTRA EL RUIDO: Ignora departamentos (ej: 'neurocirugía', 'urología') si contradicen la anatomía.\n",
    "3. VERIFICA MALIGNIDAD: 'Tumor' sin apellido no siempre es maligno (C). Busca pistas como 'carcinoma', 'benigno', etc.\n",
    "4. SELECCIONA: Elige el código de la lista que mejor se ajuste.\n\n",
    "Responde SOLO JSON: {\"code\":\"...\", \"alternatives\":[\"Alt1\", \"Alt2\"]}"
  )

  response <- chat$chat(prompt)

  # --- FASE 4: PARSING Y VALIDACIÓN ---
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

  code_clean <- if (!is.na(code)) str_replace_all(code, "\\.", "") else NA

  if (!is.na(code_clean) && !code_clean %in% codes_available) {
    if (
      hint_text != "" &&
        str_replace_all(clean_hint, "\\.", "") %in% codes_available
    ) {
      code_clean <- str_replace_all(clean_hint, "\\.", "")
    } else {
      code_clean <- codes_available[1]
    }
    code <- code_clean
  }

  # AQUÍ OCURRÍA EL ERROR ANTES (Ahora cie10_chunks ya existe)
  desc_match <- cie10_chunks |>
    filter(code == code_clean) |>
    pull(description) |>
    first()
  is_valid <- !is.na(desc_match)
  final_desc <- if (is_valid) desc_match else "CÓDIGO NO VÁLIDO"

  alt_str <- NA_character_
  if (length(alternatives) > 0) {
    alt_clean <- str_replace_all(alternatives, "\\.", "")
    valid_alts <- cie10_chunks |> filter(code %in% alt_clean)
    if (nrow(valid_alts) > 0) {
      alt_str <- paste(
        paste0(
          valid_alts$code,
          " (",
          str_trunc(valid_alts$description, 60),
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
# 4. EJECUCIÓN
# ----------------------------------------------------------------------------

cat("\nEjecutando Clasificación Hybrid Pro...\n")
pb <- txtProgressBar(min = 0, max = nrow(text), style = 3)

resultados <- text |>
  mutate(
    res = map(texto, function(t) {
      setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
      classify_hybrid_pro(t, chat) |> select(-texto)
    })
  ) |>
  unnest(res)

close(pb)

# ----------------------------------------------------------------------------
# 5. RESULTADOS
# ----------------------------------------------------------------------------

cat("\n\nRESULTADOS FINALES (OPTIMIZADOS)\n")
cat(strrep("-", 100), "\n")

resultados |>
  select(texto, codigo, descripcion, alternativas) |>
  mutate(
    texto = str_trunc(texto, 40),
    descripcion = str_trunc(descripcion, 50),
    alternativas = str_trunc(replace_na(alternativas, ""), 80)
  ) |>
  print(n = Inf)

try(dbDisconnect(store@con), silent = TRUE)
