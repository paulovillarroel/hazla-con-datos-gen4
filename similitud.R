# Instalar el paquete si no lo tienes
# install.packages("stringdist")

# Cargar librería
library(stringdist)

# Definir textos
textos <- c(
  "El paciente presenta cáncer de pulmón",
  "Diagnóstico de cáncer en pulmón derecho",
  "Paciente con insuficiencia cardíaca congestiva",
  "Fractura de fémur en paciente adulto"
)

# Crear una matriz de distancias de Jaccard
# Evalúa distancia entre textos basados en la similitud de conjuntos de palabras
dist_jaccard <- stringdistmatrix(textos, textos, method = "jaccard")

# Crear una matriz de distancias de Damerau-Levenshtein
# Evalúa distancia entre textos basados en la cantidad de operaciones de edición necesarias para transformar un texto en otro
dist_damerau <- stringdistmatrix(textos, textos, method = "dl")

# Crear una matriz de distancias del Coseno
# Evalúa distancia entre textos basados en la similitud del coseno de los vectores de términos
dist_coseno <- stringdistmatrix(textos, textos, method = "cosine")

# Mostrar resultados
print(dist_jaccard)

print(dist_damerau)

print(dist_coseno)

# Convertir distancia a similitud
sim_coseno_pct <- (1 - dist_coseno) * 100
print(sim_coseno_pct)

# Ejemplo
texto1 <- "El paciente presenta cáncer de pulmón"
texto2 <- "Colelitiasis complicada con colecistitis aguda"
texto3 <- "Diagnóstico de cáncer en pulmón derecho"

# Calcular distancia de Jaccard entre dos textos
dist_jaccard_ej <- stringdist(texto1, texto2, method = "lv")
dist_jaccard_ej
