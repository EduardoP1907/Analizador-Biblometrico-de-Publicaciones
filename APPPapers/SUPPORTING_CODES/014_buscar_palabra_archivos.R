# SUPPORTING_CODES/014_buscar_palabra_archivos.R

# Esta función carga todos los CSV de una carpeta, 
# hace una búsqueda difusa sobre título/palabras clave/resumen
# y devuelve las top N coincidencias con columna Similitud.

buscar_en_archivos <- function(path, query, top_n = 10) {
  library(stringi)
  library(stringdist)

  # 1. Lista todos los CSV dentro de path
  files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

  # 2. Carga y concatena
  df_list <- lapply(files, function(f) read.csv(f, sep="|", stringsAsFactors=FALSE))
  data <- do.call(rbind, df_list)

  # 3. Limpia y normaliza texto
  q_clean      <- stri_trans_general(query, "Latin-ASCII") |> tolower()
  corpus_texts <- paste(data$TITULO, data$PALABRAS_CLAVE, data$RESUMEN, sep = " ")
  corpus_clean <- stri_trans_general(corpus_texts, "Latin-ASCII") |> tolower()

  # 4. Calcula similitud coseno inversa (1−distancia)
  sim <- sapply(corpus_clean, function(txt) {
    1 - stringdist(q_clean, txt, method = "cosine")
  })
  data$Similitud <- sim

  # 5. Ordena y retorna los top_n
  res <- data[order(-data$Similitud), ]
  head(res, n = top_n)
}
