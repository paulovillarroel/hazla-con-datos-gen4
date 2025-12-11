library(ollamar)

# Embeddings
embed("embeddinggemma:latest", "Paciente dolor abdominal")

# Ejemplo 1
embed1 <- embed("embeddinggemma:latest", "Paciente con cáncer pulmonar")
embed2 <- embed(
  "embeddinggemma:latest",
  "Paciente con patología neoplásica pulmonar"
)

# Producto punto
sum(embed1 * embed2)

# Cosine similarity
# Genera función de similaridad coseno
cosine_similarity <- function(vec1, vec2) {
  sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
}

# Calcula similaridad coseno entre los embeddings
cosine_similarity(embed1, embed2)

# Calcula distancia coseno entre los embeddings
1 - cosine_similarity(embed1, embed2)


# Ejemplo 2
embed3 <- embed("embeddinggemma:latest", "Paciente con insuficiencia cardíaca congestiva")
embed4 <- embed("embeddinggemma:latest", "El auto se quemó")

# Producto punto
sum(embed3 * embed4)

# Similitud coseno
cosine_similarity(embed3, embed4)

# Distancia coseno
1 - cosine_similarity(embed3, embed4)
