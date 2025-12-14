# Resumen del Refactor RAG CIE-10

## Objetivo
Alinear `llm-classify-icd10.R` con el enfoque recomendado en el artículo de InfoWorld "How to create your own RAG applications in R" (Sharon Machlis, Jul 2025).

## Cambios Principales

### 1. **Tool-calling con ellmer** ✅
**Antes:** 
- Retrieval manual con `ragnar_retrieve()`
- Chunks concatenados y pegados en el prompt
- Uso de `llm_custom()` sin tool registration

**Después:**
- Chat object con `system_prompt` claro
- `ragnar_register_tool_retrieve(chat, store, top_k=15)` registra el store como herramienta
- El LLM puede invocar retrieval cuando lo necesita
- Separación explícita instrucciones/datos (anti prompt-injection)

### 2. **Salida estructurada y validación robusta** ✅
**Antes:**
- Prompt pedía "copia EXACTAMENTE el código: descripción"
- Parse frágil con regex `^[^:]+`
- Fallaba con cualquier desviación de formato

**Después:**
- Prompt pide JSON con `{code, description, no_match}`
- `safe_parse_json()` limpia backticks y parsea JSON
- `extract_code_fallback()` con regex como fallback
- Validación explícita contra catálogo CIE-10

### 3. **Trazabilidad y reproducibilidad** ✅
**Antes:**
- Si `*.duckdb` existía, se conectaba sin validar
- Sin tracking de versión del JSON ni modelo de embeddings
- Riesgo de corpus/índice obsoleto silenciosamente

**Después:**
- Sistema de "fingerprint" con hash SHA256 del JSON
- Verifica modelo de embedding, base_url, mtime
- Auto-rebuild si cambia el JSON o embedder
- Metadata guardada en cada clasificación:
  - `llm_model`
  - `embedder_model`
  - `top_k_requested` vs `n_codes_retrieved` (real)

### 4. **Detalles de implementación**
- Dependencia explícita: `library(openssl)` para hash
- Store con `read_only = TRUE` cuando es válido
- `overwrite = TRUE` cuando se reconstruye
- `try(DBI::dbDisconnect(store@con))` al final para cerrar conexión
- Mensajes de consola alineados con `top_k` real (15, no "30")

## Estructura del nuevo flujo

```r
# 1. Setup
llm_model <- "..." 
chat <- chat_groq(model = llm_model, system_prompt = "...")
llm_use(chat)

# 2. Cargar y preparar chunks
cie10_chunks <- cie10_raw |> filter(...) |> mutate(...)

# 3. Store con fingerprint
embedder_spec <- list(provider, model, base_url, batch_size)
need_rebuild <- !fingerprint_ok(...)
if (!need_rebuild) {
  store <- ragnar_store_connect(..., read_only = TRUE)
} else {
  store <- ragnar_store_create(..., overwrite = TRUE)
  ragnar_store_insert(...)
  ragnar_store_build_index(...)
  write_fingerprint(...)
}

# 4. Registrar tool + clasificar
ragnar_register_tool_retrieve(chat, store, top_k = 15)
texts_classified <- text |> 
  rowwise() |> 
  mutate(classification = list(classify_with_rag(texto, chat, top_k = 15)))

# 5. Post-procesamiento
texts_classified <- texts_classified |> 
  mutate(code_only = ..., code_valid = ..., needs_review = ...)

# 6. Cerrar
try(DBI::dbDisconnect(store@con))
```

## Beneficios vs versión anterior

| Aspecto | Antes | Después |
|---------|-------|---------|
| **Tool calling** | Manual | Automático (ellmer) |
| **Salida** | Texto libre frágil | JSON estructurado |
| **Parsing** | Regex único | JSON + fallback |
| **Reproducibilidad** | Sin verificación | Fingerprint SHA256 |
| **Trazabilidad** | Solo `top_k` parámetro | Modelo LLM/embedder + real count |
| **Prompt injection** | Sin protección | Instrucciones explícitas separadas |
| **Rebuild store** | Manual | Auto-detect cambios |

## Alineación con el artículo InfoWorld

### ✅ Cumple con los "5 pasos" del artículo:
1. ✅ Markdown format (no necesario, ya estructurado)
2. ✅ Chunking + metadata (`chunk_id`, `text`, `code`, `level_priority`)
3. ✅ Store create + insert + build_index
4. ✅ Retrieve (con `ragnar_retrieve()` híbrido BM25+VSS)
5. ✅ Tool registration + chat

### ✅ Implementa las recomendaciones clave:
- ✅ `ragnar_store_build_index()` (advertencia del artículo)
- ✅ Tool calling con `ragnar_register_tool_retrieve()`
- ✅ System prompt claro y anti-injection
- ✅ Structured output (JSON)
- ✅ Store persistente + verificación

### 🔶 Pendiente (opcional, artículo avanzado):
- Metadata filtering con `extra_cols` + `filter=` en retrieve
- Re-ranking por score o nivel
- Evaluación con gold set
- Batch async (si disponible en ellmer)

## Testing

### Smoke test
Ejecutar `test-rag-refactor.R` para verificar el flujo con 3 textos:

```r
source("test-rag-refactor.R")
```

Espera ver:
- Conexión al store existente
- 3 clasificaciones con código CIE-10 válido
- JSON raw response visible
- Sin errores de parse

### Full test
Ejecutar el script completo:

```r
source("llm-classify-icd10.R")
```

Espera ver:
- 24 textos clasificados
- Mayoría con `code_valid = TRUE`
- Distribución por nivel (1 vs 2)
- Fingerprint creado/actualizado

## Notas técnicas

- **Groq API**: el modelo `kimi-k2-instruct-0905` debe soportar tool calling
- **Ollama**: `embeddinggemma:latest` debe estar corriendo localmente
- **DuckDB**: v0.9.2+ recomendado por compatibilidad con `ragnar`
- **ragnar**: usar dev version (`pak::pak("tidyverse/ragnar")`)

## Referencias

- Artículo: "How to create your own RAG applications in R" (Sharon Machlis, InfoWorld, Jul 2025)
- Paquetes: `ellmer`, `ragnar`, `mall`, `tidyverse`, `openssl`
- Store format: DuckDB con embeddings vectoriales
