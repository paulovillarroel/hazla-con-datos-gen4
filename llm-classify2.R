library(mall)
library(ellmer)
library(tidyverse)

# Configurar API key
# usethis::edit_r_environ()
# Sys.getenv("GROQ_API_KEY")

chat <- chat_groq(model = "moonshotai/kimi-k2-instruct-0905")
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
    "evaluación previa a inicio de quimioetarapia."
  )
)

# Zero-shot
texts_classified <- text |>
  llm_classify(texto, c("cáncer", "no cáncer"))

texts_organs <- text |>
  llm_extract(texto, "órgano humano afectado (en español)")

texts_hemato <- text |>
  llm_verify(
    texto,
    "Coloca 1 si el diagnóstico correponde a hematología. De lo contrario, coloca 0."
  )

texts_cie <- text |>
  llm_extract(texto, "Extrae el código CIE-10")
