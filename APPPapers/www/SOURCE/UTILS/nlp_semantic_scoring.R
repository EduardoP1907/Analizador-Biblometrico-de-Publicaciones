#=======================================
# Scoring híbrido de recuperación
# Fusión de búsqueda difusa, expansión de sinónimos y embeddings
#=======================================

library(stringi, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# Cargar el sistema de sinónimos
source("www/SOURCE/UTILS/nlp_synonyms_academic.R")

# ── Parámetros de la fusión ──────────────────────────────────────────────────
#
#   score_semantico = 0.60 · s_emb + 0.25 · s_dif + 0.15 · s_sin
#
# Sin embeddings disponibles (modo degradado) los dos componentes léxicos se
# reescalan para que el puntaje siga en [0, 1]:
#
#   score_semantico = (0.25 · s_dif + 0.15 · s_sin) / 0.40
#
# Los pesos se fijaron a priori (no se calibraron sobre las consultas de
# evaluación): el componente vectorial domina porque es el único que resuelve
# sinonimia y consultas ES↔EN; la búsqueda difusa actúa como ancla léxica.
PESO_EMBEDDINGS  <- 0.60
PESO_DIFUSA      <- 0.25
PESO_SINONIMOS   <- 0.15
UMBRAL_HIBRIDO   <- 0.20   # puntaje mínimo para considerar un documento candidato
TOP_K_HIBRIDO    <- 20     # máximo de publicaciones devueltas al usuario
MAX_DIST_DIFUSA  <- 0.15   # distancia máxima de agrep, como fracción del largo del término
SATURACION_SINON <- 3      # nº de sinónimos presentes con que s_sin llega a 1

#' Tokens de la consulta usados por la búsqueda difusa (más de 3 caracteres)
tokens_consulta_difusa <- function(query) {
  raw <- stri_split_boundaries(tolower(as.character(query)), type = "word")[[1]]
  unique(raw[nchar(raw) > 3])
}

#' Texto léxico de cada publicación: título + palabras clave (autor e índice)
texto_lexico <- function(data) {
  campo <- function(col) {
    v <- if (col %in% colnames(data)) tolower(as.character(data[[col]])) else rep("", nrow(data))
    v[is.na(v)] <- ""
    v
  }
  paste(campo("TITULO"), campo("AUTOR_PALABRAS_CLAVES"), campo("INDEX_PALABRAS_CLAVES"), sep = " ")
}

#' s_dif: fracción de tokens de la consulta con coincidencia aproximada
#' (distancia de Levenshtein, agrep) en título y palabras clave.
score_difuso <- function(query, data, max_distance = MAX_DIST_DIFUSA) {
  tokens <- tokens_consulta_difusa(query)
  if (length(tokens) == 0 || nrow(data) == 0) return(rep(0, nrow(data)))

  campo <- texto_lexico(data)
  hits <- vapply(tokens, function(t) agrepl(t, campo, max.distance = max_distance, ignore.case = TRUE),
                 logical(nrow(data)))
  if (is.null(dim(hits))) hits <- matrix(hits, ncol = length(tokens))
  rowMeans(hits)
}

#' s_sin: sinónimos del diccionario presentes en título, resumen o palabras
#' clave, saturado en SATURACION_SINON términos.
score_sinonimos <- function(expansion_result, data) {
  terminos <- unique(vapply(expansion_result$terminos_adicionales, limpiar_texto_para_sinonimos, character(1)))
  terminos <- terminos[nchar(terminos) >= 2]
  if (length(terminos) == 0 || nrow(data) == 0) return(rep(0, nrow(data)))

  resumen <- if ("RESUMEN" %in% colnames(data)) as.character(data$RESUMEN) else rep("", nrow(data))
  resumen[is.na(resumen)] <- ""
  texto <- stri_trans_general(paste(texto_lexico(data), tolower(resumen)), "Latin-ASCII")

  # Coincidencia por palabra completa para no confundir siglas ("ml" en "html")
  presentes <- vapply(terminos, function(t) {
    stri_detect_regex(texto, paste0("\\b", stri_replace_all_regex(t, "([.^$|()\\[\\]{}*+?\\\\-])", "\\\\$1"), "\\b"))
  }, logical(nrow(data)))
  if (is.null(dim(presentes))) presentes <- matrix(presentes, ncol = length(terminos))
  pmin(1, rowSums(presentes) / SATURACION_SINON)
}

#' Calcular los puntajes híbridos de todas las publicaciones
#'
#' @param query Términos de tema de la consulta
#' @param data Dataset de papers
#' @param expansion_result Resultado de expandir_consulta_semantica()
#' @param sims_emb Similitudes coseno por fila (similitudes_embeddings) o NULL
#' @return data.frame con score_embeddings, score_difuso, score_sinonimos y score_semantico
calcular_scores_hibridos <- function(query, data, expansion_result, sims_emb = NULL) {
  s_dif <- score_difuso(query, data)
  s_sin <- score_sinonimos(expansion_result, data)

  if (!is.null(sims_emb)) {
    s_emb <- pmax(0, sims_emb)
    s_emb[is.na(s_emb)] <- 0
    score <- PESO_EMBEDDINGS * s_emb + PESO_DIFUSA * s_dif + PESO_SINONIMOS * s_sin
  } else {
    s_emb <- rep(NA_real_, nrow(data))
    score <- (PESO_DIFUSA * s_dif + PESO_SINONIMOS * s_sin) / (PESO_DIFUSA + PESO_SINONIMOS)
  }

  data.frame(
    score_embeddings = s_emb,
    score_difuso     = s_dif,
    score_sinonimos  = s_sin,
    score_semantico  = score
  )
}

#' Normalizar scores para un conjunto de papers
normalizar_scores <- function(scores, metodo = "minmax") {

  if(length(scores) <= 1) return(scores)

  scores_numericos <- as.numeric(scores)
  scores_numericos[is.na(scores_numericos)] <- 0

  if(metodo == "minmax") {
    min_score <- min(scores_numericos)
    max_score <- max(scores_numericos)

    if(max_score == min_score) return(rep(1, length(scores)))

    scores_norm <- (scores_numericos - min_score) / (max_score - min_score)
    return(scores_norm)

  } else if(metodo == "zscore") {
    mean_score <- mean(scores_numericos)
    sd_score <- sd(scores_numericos)

    if(sd_score == 0) return(rep(0, length(scores)))

    scores_norm <- (scores_numericos - mean_score) / sd_score
    # Convertir z-scores a [0,1]
    scores_norm <- pmax(0, pmin(1, (scores_norm + 3) / 6))
    return(scores_norm)
  }

  return(scores)
}
