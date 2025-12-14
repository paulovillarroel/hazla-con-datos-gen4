# Clasificación CIE-10 con RAG + LLMs

Script simplificado para enseñar **Retrieval-Augmented Generation (RAG)** con LLMs.

## 📚 Conceptos que enseña

1. **Embeddings**: Convertir texto a vectores numéricos
2. **Vector Store**: Base de datos con búsqueda semántica
3. **RAG**: Retrieval-Augmented Generation
4. **Tool Calling**: LLM usa herramientas automáticamente
5. **Búsqueda Híbrida**: BM25 (keywords) + VSS (embeddings)

## 🚀 Uso

```r
source("llm-classify-icd10-simple.R")
```

## 📖 Flujo del código

### 1. Setup
- Configura LLM (Ollama con kimi-k2)
- Define textos médicos de ejemplo

### 2. Carga catálogo CIE-10
- 1,843 códigos desde JSON
- Filtra niveles 1-2 (más relevantes)

### 3. Crea RAG Store
- **Primera vez**: Genera embeddings de todos los códigos (~5 min)
- **Siguientes veces**: Conecta a store existente (~1 seg)
- Embeddings: `embeddinggemma:latest` (768 dimensiones)
- Store: DuckDB con extensiones FTS + VSS

### 4. Registra Tool de Retrieval
```r
ragnar_register_tool_retrieve(chat, store, top_k = 10)
```
- El LLM ahora tiene acceso a la herramienta `retrieve`
- Puede buscar códigos CIE-10 cuando los necesite

### 5. Clasifica textos
```r
classify_with_rag(texto, chat)
```

**Qué pasa internamente:**
1. Enviamos prompt al LLM: "Clasifica este texto... usa tool 'retrieve'"
2. LLM decide usar `retrieve("tumor maligno pulmón")`
3. Ragnar:
   - Convierte query → embedding (768 dims)
   - Busca en vector store (BM25 + VSS)
   - Devuelve top-10 códigos al LLM
4. LLM selecciona el código más apropiado
5. Responde con JSON: `{"code":"C34.9"}`

### 6. Muestra resultados

## 🎯 Ventajas de RAG

| Sin RAG | Con RAG |
|---------|---------|
| Pasar 1,843 códigos en el prompt | Solo recuperar 10 relevantes |
| ~500K tokens por consulta | ~5K tokens por consulta |
| LLM se confunde con tanto contexto | LLM enfocado en opciones relevantes |
| Costo alto | Costo bajo |

## 🔧 Requisitos

- Ollama instalado y corriendo
- Modelos descargados:
  - `ollama pull kimi-k2:1t-cloud`
  - `ollama pull embeddinggemma:latest`
- Paquetes R: `mall`, `ellmer`, `ragnar`, `tidyverse`, `jsonlite`, `duckdb`
- Archivo: `raw-data/cie10-codes.json`

### Instalar extensiones de DuckDB (primera vez)

```r
library(duckdb)
con <- dbConnect(duckdb())
dbExecute(con, "INSTALL fts")  # Full-Text Search
dbExecute(con, "INSTALL vss")  # Vector Similarity Search
dbDisconnect(con)
```

**Esto solo se hace UNA vez.** Las extensiones quedan instaladas en tu sistema.

## 📊 Output esperado

```
RESULTADOS
======================================================================

texto                                          codigo  descripcion
tumor maligno intestino delgado...            C17.9   Tumor maligno del intestino delgado, parte no especificada
diabetes mellitus tipo 2...                   E11.9   Diabetes mellitus tipo 2, sin complicaciones
adenoma benigno prostata...                   D29.1   Adenoma benigno de la próstata
...

======================================================================
RESUMEN
======================================================================
Textos clasificados: 10
Códigos únicos: 10
```

## 🧠 Para la clase

**Puntos clave a destacar:**

1. **Embeddings vs Keywords**:
   - "tumor maligno" y "neoplasia maligna" tienen embeddings similares
   - Búsqueda por keywords fallaría

2. **Tool Calling**:
   - El LLM DECIDE cuándo buscar
   - Puede hacer múltiples búsquedas si necesita refinar

3. **Escalabilidad**:
   - Con 10 códigos: puede pasar todos en prompt
   - Con 10,000 códigos: RAG es esencial

4. **Búsqueda híbrida**:
   - BM25: Detecta keywords exactas ("maligno" vs "benigno")
   - VSS: Similitud semántica ("cáncer" ≈ "neoplasia")

## 📝 Ejercicios para estudiantes

1. Agregar más textos médicos y ver cómo clasifica
2. Cambiar `top_k` (5, 10, 20) y comparar resultados
3. Probar con otros dominios (legal, técnico, etc.)
4. Medir tiempo de ejecución con/sin cache
5. Comparar con approach sin RAG (pasar todos los códigos)
