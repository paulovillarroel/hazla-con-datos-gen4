library(tidyverse)

# DF de ejemplo
datos_pacientes <- tibble(
  id = 1:6,
  rut = c(
    "12.345.678-9",
    "98765432-1",
    "15.678.234-K",
    "11.222.333-4",
    "9.876.543-2",
    "18765432-K"
  ),
  diagnostico = c(
    "Paciente con cáncer de pulmón código C34",
    "Diagnóstico de influenza severa J09",
    "Fractura de fémur por accidente",
    "Cáncer gástrico código C16 en etapa inicial",
    "Neumonía código J18 confirmada",
    "Influenza código J11 leve"
  ),
  fecha_atencion = c(
    "15-03-2024",
    "28-11-2024",
    "05-06-2024",
    "12-01-2024",
    "30-09-2024",
    "18-12-2024"
  ),
  email = c(
    "juan@minsal.cl",
    "maria@hospital.cl",
    "pedro@gmail.com",
    "ana@minsal.cl",
    "luis@clinic.cl",
    "sofia@minsal.cl"
  )
)

print(datos_pacientes)

# ¿Qué pacientes tienen diagnóstico de "cáncer"?
# str_detect() devuelve TRUE o FALSE
datos_pacientes |>
  mutate(tiene_cancer = str_detect(diagnostico, "cáncer")) |>
  select(id, diagnostico, tiene_cancer)

# Filtrar solo los pacientes con cáncer
datos_pacientes |>
  filter(str_detect(diagnostico, "cáncer"))

# Variabilidad en textos médicos
# En datos reales, las palabras pueden estar escritas de diferentes formas:
# - Con o sin tildes: cáncer vs cancer
# - Mayúsculas o minúsculas: Cáncer vs cáncer
# - Raíces diferentes: neoplasia, neoplásico, neoplasias

# Crear datos con variabilidad
datos_variados <- tibble(
  id = 1:7,
  diagnostico = c(
    "Paciente con cáncer de pulmón",
    "Diagnóstico: Cancer gástrico", # sin tilde
    "Neumonía severa confirmada",
    "Neumonia leve", # sin tilde
    "Neoplasia maligna detectada",
    "Proceso neoplásico en curso",
    "Se observan neoplasias múltiples"
  )
)

print(datos_variados)

# PROBLEMA 1: Buscar "cáncer" con o sin tilde
# Opción A: Usar regex que acepta ambas formas con [aá]
datos_variados |>
  filter(str_detect(diagnostico, "c[aá]ncer")) |>
  select(id, diagnostico)

# PROBLEMA 2: Ignorar mayúsculas/minúsculas
# Opción: Usar (?i) al inicio del patrón
datos_variados |>
  filter(str_detect(diagnostico, "(?i)c[aá]ncer")) |>
  select(id, diagnostico)

# PROBLEMA 3: Buscar "neumonía" con todas sus variaciones
datos_variados |>
  filter(str_detect(diagnostico, "(?i)neumon[ií]a")) |>
  select(id, diagnostico)

# PROBLEMA 4: Buscar palabras con la misma raíz (neoplasia, neoplásico, neoplasias)
# Opción A: Buscar solo la raíz común "neoplas"
datos_variados |>
  filter(str_detect(diagnostico, "(?i)neoplas")) |>
  select(id, diagnostico)

# Opción B: Ser más específico con alternativas usando | (OR)
datos_variados |>
  filter(str_detect(diagnostico, "(?i)(neoplasia|neoplásico|neoplasias)")) |>
  select(id, diagnostico)

# CASO PRÁCTICO: Crear una columna que identifique todos los casos oncológicos
# (cáncer, cancer, neoplasia, neoplásico, tumor, etc.)
datos_variados |>
  mutate(
    es_oncologico = str_detect(
      diagnostico,
      "(?i)(c[aá]ncer|neoplas|tumor|oncol[oó]gico)"
    )
  ) |>
  select(id, diagnostico, es_oncologico)

# CONSEJO: Para datos reales, es mejor usar regex flexible
# Ejemplo completo de patrón robusto:
datos_variados |>
  mutate(
    # Buscar "neumonía" de cualquier forma
    tiene_neumonia = str_detect(diagnostico, "(?i)neumon[ií]a"),

    # Buscar cáncer O neoplasia en una sola línea usando | (OR)
    es_caso_oncologico = str_detect(
      diagnostico,
      "(?i)(c[aá]ncer|neopl[aá]si[aoc]s?)"
    )
  ) |>
  select(id, diagnostico, tiene_neumonia, es_caso_oncologico)


# Caracteres especiales en regex

# El PUNTO (.) = cualquier carácter (como un comodín)
datos_pacientes |>
  filter(str_detect(diagnostico, "C3.")) |>
  select(id, diagnostico)

# El ACENTO CIRCUNFLEJO (^) = inicio del texto
# Busca solo al principio del texto
datos_pacientes |>
  filter(str_detect(diagnostico, "^Paciente")) |>
  select(id, diagnostico)

# El SIGNO DÓLAR ($) = fin del texto
# Ejemplo: encontrar emails que terminan en .cl
datos_pacientes |>
  filter(str_detect(email, "cl$")) |>
  select(id, email)

# Buscar por caracteres

# \\d = cualquier DÍGITO (número del 0 al 9)
# \\d+ significa "uno o más dígitos juntos"
datos_pacientes |>
  mutate(numeros = str_extract_all(diagnostico, "\\d+")) |>
  select(id, diagnostico, numeros) |>
  unnest(numeros)

# [A-Z] = cualquier letra MAYÚSCULA
# Buscar códigos que empiezan con letra mayúscula + dos dígitos
datos_pacientes |>
  mutate(codigo_cie = str_extract(diagnostico, "[A-Z]\\d\\d")) |>
  select(id, diagnostico, codigo_cie)

# Extraer múltiples elementos en un solo paso
datos_procesados <- datos_pacientes |>
  mutate(
    # Extraer código médico CIE (ej: J09, C34, J18)
    codigo_cie = str_extract(diagnostico, "[A-Z]\\d\\d"),

    # Detectar si es email institucional de MINSAL
    es_minsal = str_detect(email, "@minsal\\.cl$")
  )

print(datos_procesados)


# Ver solo códigos específicos (influenza = J09 o J11)
datos_procesados |>
  filter(codigo_cie %in% c("J09", "J11"))

# Limpiar y normalizar los RUTs
datos_limpios <- datos_pacientes |>
  mutate(
    # Quitar los puntos del RUT (usamos \\. porque punto es especial)
    rut_sin_puntos = str_replace_all(rut, "\\.", ""),

    # Extraer solo los números del RUT
    rut_numeros = str_extract(rut_sin_puntos, "\\d+"),

    # Extraer el dígito verificador
    digito_verificador = str_extract(rut, "-[0-9K]") |> str_remove("-")
  ) |>
  select(id, rut, rut_sin_puntos, rut_numeros, digito_verificador)

print(datos_limpios)


# Crear un reporte limpio con toda la información procesada

reporte_final <- datos_pacientes |>
  mutate(
    # Limpiar RUT
    rut_limpio = str_replace_all(rut, "\\.", ""),

    # Extraer código CIE
    codigo_cie = str_extract(diagnostico, "[A-Z]\\d\\d"),

    # Clasificar tipo de enfermedad según código
    tipo_enfermedad = case_when(
      str_detect(codigo_cie, "^C") ~ "Cáncer",
      str_detect(codigo_cie, "^J") ~ "Respiratoria",
      TRUE ~ "Otra"
    ),

    # Validar email institucional
    email_institucional = str_detect(email, "@(minsal|hospital|clinic)\\.cl$"),

    # Extraer año de la fecha
    año = str_extract(fecha_atencion, "\\d\\d\\d\\d")
  ) |>
  select(id, rut_limpio, codigo_cie, tipo_enfermedad, email_institucional, año)

print(reporte_final)
