#=======================================
# Algoritmos de Scoring Semántico - FASE 1
# Mejora del Motor NLP APPPapers
#=======================================

library(stringi, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# Cargar el sistema de sinónimos
source("www/SOURCE/UTILS/nlp_synonyms_academic.R")

#' Sistema avanzado de scoring semántico para papers académicos
#'
#' Implementa múltiples algoritmos de similitud semántica para mejorar
#' la precisión de búsqueda de papers académicos
#'
#' @author Claude Code - Fase 1 Mejoras NLP
#' @date 2025-09-29

#' Calcular score semántico completo para un paper
#'
#' @param paper_row Fila del dataframe con información del paper
#' @param query_original Consulta original del usuario
#' @param expansion_result Resultado de expansión semántica
#' @param peso_titulo Peso para coincidencias en título (default 3.0)
#' @param peso_resumen Peso para coincidencias en resumen (default 1.5)
#' @param peso_keywords Peso para coincidencias en keywords (default 2.5)
#' @return Numeric: score semántico total
calcular_score_semantico <- function(paper_row, query_original, expansion_result,
                                   peso_titulo = 3.0, peso_resumen = 1.5, peso_keywords = 2.5) {

  # Inicializar score
  score_total <- 0

  # Textos del paper para análisis
  titulo <- ifelse(is.na(paper_row$TITULO) || paper_row$TITULO == "", "", paper_row$TITULO)
  resumen <- ifelse(is.na(paper_row$RESUMEN) || paper_row$RESUMEN == "", "", paper_row$RESUMEN)
  keywords <- ifelse(is.na(paper_row$AUTOR_PALABRAS_CLAVES) || paper_row$AUTOR_PALABRAS_CLAVES == "", "", paper_row$AUTOR_PALABRAS_CLAVES)

  # 1. SCORE POR SIMILITUD EXACTA
  score_exacto <- calcular_score_coincidencia_exacta(
    query_original, titulo, resumen, keywords, peso_titulo, peso_resumen, peso_keywords
  )

  # 2. SCORE POR SIMILITUD SEMÁNTICA (sinónimos)
  score_semantico <- calcular_score_sinonimos(
    expansion_result, titulo, resumen, keywords, peso_titulo, peso_resumen, peso_keywords
  )

  # 3. SCORE POR SIMILITUD DE N-GRAMAS
  score_ngramas <- calcular_score_ngramas(
    query_original, titulo, resumen, keywords
  )

  # 4. SCORE POR CO-OCURRENCIA DE TÉRMINOS
  score_coocurrencia <- calcular_score_coocurrencia(
    expansion_result$terminos_adicionales, titulo, resumen, keywords
  )

  # 5. BONUS POR CALIDAD ACADÉMICA
  bonus_calidad <- calcular_bonus_calidad_academica(paper_row)

  # 6. SCORE TOTAL PONDERADO
  score_total <- (score_exacto * 2.0) +         # Más peso a coincidencias exactas
                 (score_semantico * 1.5) +      # Peso medio a sinónimos
                 (score_ngramas * 1.0) +        # Peso base a n-gramas
                 (score_coocurrencia * 0.8) +   # Peso menor a co-ocurrencia
                 (bonus_calidad * 0.3)          # Bonus por calidad

  return(max(0, score_total))
}

#' Calcular score por coincidencias exactas
calcular_score_coincidencia_exacta <- function(query, titulo, resumen, keywords, peso_titulo, peso_resumen, peso_keywords) {

  query_limpia <- limpiar_texto_para_sinonimos(query)
  score <- 0

  # Coincidencia exacta en título
  if(nchar(titulo) > 0) {
    titulo_limpio <- limpiar_texto_para_sinonimos(titulo)
    if(grepl(query_limpia, titulo_limpio, fixed = TRUE)) {
      score <- score + peso_titulo * 2  # Bonus extra por título
    }
  }

  # Coincidencia exacta en resumen
  if(nchar(resumen) > 0) {
    resumen_limpio <- limpiar_texto_para_sinonimos(resumen)
    if(grepl(query_limpia, resumen_limpio, fixed = TRUE)) {
      score <- score + peso_resumen
    }
  }

  # Coincidencia exacta en keywords
  if(nchar(keywords) > 0) {
    keywords_limpio <- limpiar_texto_para_sinonimos(keywords)
    if(grepl(query_limpia, keywords_limpio, fixed = TRUE)) {
      score <- score + peso_keywords * 1.5  # Bonus por keywords
    }
  }

  return(score)
}

#' Calcular score por similitud semántica (sinónimos)
calcular_score_sinonimos <- function(expansion_result, titulo, resumen, keywords, peso_titulo, peso_resumen, peso_keywords) {

  if(length(expansion_result$terminos_adicionales) == 0) {
    return(0)
  }

  score <- 0
  terminos_expansion <- expansion_result$terminos_adicionales

  # Limpiar textos del paper
  titulo_limpio <- limpiar_texto_para_sinonimos(titulo)
  resumen_limpio <- limpiar_texto_para_sinonimos(resumen)
  keywords_limpio <- limpiar_texto_para_sinonimos(keywords)

  # Contar coincidencias de términos expandidos
  for(termino in terminos_expansion) {
    termino_limpio <- limpiar_texto_para_sinonimos(termino)

    # En título
    if(nchar(titulo_limpio) > 0 && grepl(termino_limpio, titulo_limpio, fixed = TRUE)) {
      score <- score + (peso_titulo * 0.8)  # Menor peso que coincidencia exacta
    }

    # En resumen
    if(nchar(resumen_limpio) > 0 && grepl(termino_limpio, resumen_limpio, fixed = TRUE)) {
      score <- score + (peso_resumen * 0.7)
    }

    # En keywords
    if(nchar(keywords_limpio) > 0 && grepl(termino_limpio, keywords_limpio, fixed = TRUE)) {
      score <- score + (peso_keywords * 0.9)
    }
  }

  # Normalizar por número de términos para evitar inflación
  if(length(terminos_expansion) > 3) {
    score <- score * (3 / length(terminos_expansion))
  }

  return(score)
}

#' Calcular score por similitud de n-gramas
calcular_score_ngramas <- function(query, titulo, resumen, keywords) {

  query_limpia <- limpiar_texto_para_sinonimos(query)
  if(nchar(query_limpia) < 4) return(0)  # Query muy corta

  # Generar bigramas y trigramas de la query
  ngramas_query <- generar_ngramas(query_limpia, n = 2:3)

  if(length(ngramas_query) == 0) return(0)

  score <- 0
  textos_paper <- c(titulo, resumen, keywords)
  pesos_textos <- c(3.0, 1.5, 2.5)

  for(i in 1:length(textos_paper)) {
    if(nchar(textos_paper[i]) > 0) {
      texto_limpio <- limpiar_texto_para_sinonimos(textos_paper[i])
      ngramas_texto <- generar_ngramas(texto_limpio, n = 2:3)

      # Contar n-gramas en común
      ngramas_comunes <- intersect(ngramas_query, ngramas_texto)
      if(length(ngramas_comunes) > 0) {
        # Score proporcional a n-gramas comunes
        score <- score + (length(ngramas_comunes) / length(ngramas_query)) * pesos_textos[i] * 0.5
      }
    }
  }

  return(score)
}

#' Generar n-gramas de un texto
generar_ngramas <- function(texto, n = 2:3) {

  palabras <- unlist(strsplit(texto, "\\s+"))
  palabras <- palabras[nchar(palabras) >= 3]  # Solo palabras significativas

  if(length(palabras) < max(n)) return(character(0))

  ngramas <- character(0)

  for(ng in n) {
    if(length(palabras) >= ng) {
      for(i in 1:(length(palabras) - ng + 1)) {
        ngrama <- paste(palabras[i:(i + ng - 1)], collapse = " ")
        ngramas <- c(ngramas, ngrama)
      }
    }
  }

  return(unique(ngramas))
}

#' Calcular score por co-ocurrencia de términos
calcular_score_coocurrencia <- function(terminos_expansion, titulo, resumen, keywords) {

  if(length(terminos_expansion) < 2) return(0)

  # Combinar todo el texto del paper
  texto_completo <- paste(titulo, resumen, keywords, sep = " ")
  texto_limpio <- limpiar_texto_para_sinonimos(texto_completo)

  if(nchar(texto_limpio) < 10) return(0)

  # Contar cuántos términos de expansión aparecen juntos
  terminos_presentes <- character(0)

  for(termino in terminos_expansion) {
    termino_limpio <- limpiar_texto_para_sinonimos(termino)
    if(grepl(termino_limpio, texto_limpio, fixed = TRUE)) {
      terminos_presentes <- c(terminos_presentes, termino_limpio)
    }
  }

  # Score basado en número de términos que co-ocurren
  if(length(terminos_presentes) >= 2) {
    # Más términos juntos = mayor relevancia semántica
    score <- length(terminos_presentes) * 0.5

    # Bonus si están realmente cerca en el texto
    if(length(terminos_presentes) >= 2) {
      proximidad <- calcular_proximidad_terminos(terminos_presentes, texto_limpio)
      score <- score + proximidad * 0.3
    }

    return(score)
  }

  return(0)
}

#' Calcular qué tan cerca están los términos en el texto
calcular_proximidad_terminos <- function(terminos, texto) {

  posiciones <- list()

  # Encontrar posiciones de cada término
  for(termino in terminos) {
    matches <- gregexpr(termino, texto, fixed = TRUE)[[1]]
    if(matches[1] != -1) {
      posiciones[[termino]] <- as.numeric(matches)
    }
  }

  if(length(posiciones) < 2) return(0)

  # Calcular distancia promedio entre términos
  distancias <- numeric(0)
  terminos_con_posicion <- names(posiciones)

  for(i in 1:(length(terminos_con_posicion) - 1)) {
    for(j in (i + 1):length(terminos_con_posicion)) {
      pos1 <- posiciones[[terminos_con_posicion[i]]]
      pos2 <- posiciones[[terminos_con_posicion[j]]]

      # Distancia mínima entre cualquier par de posiciones
      distancia_min <- min(outer(pos1, pos2, FUN = function(x, y) abs(x - y)))
      distancias <- c(distancias, distancia_min)
    }
  }

  if(length(distancias) > 0) {
    distancia_promedio <- mean(distancias)
    # Convertir distancia a score (menor distancia = mayor score)
    proximidad_score <- max(0, 1 - (distancia_promedio / 500))  # 500 chars = score 0
    return(proximidad_score)
  }

  return(0)
}

#' Calcular bonus por calidad académica del paper
calcular_bonus_calidad_academica <- function(paper_row) {

  bonus <- 0

  # Bonus por SJR
  if(!is.na(paper_row$SJR) && paper_row$SJR != "" && as.numeric(paper_row$SJR) > 0) {
    sjr <- as.numeric(paper_row$SJR)
    bonus <- bonus + (sjr * 0.5)  # SJR contribuye directamente
  }

  # Bonus por citas
  if(!is.na(paper_row$CITADO_POR) && paper_row$CITADO_POR != "" && as.numeric(paper_row$CITADO_POR) > 0) {
    citas <- as.numeric(paper_row$CITADO_POR)
    # Logaritmo para evitar que papers muy citados dominen
    bonus <- bonus + (log(citas + 1) * 0.1)
  }

  # Bonus por año reciente (papers más recientes son más relevantes)
  if(!is.na(paper_row$ANO) && paper_row$ANO != "") {
    ano <- as.numeric(paper_row$ANO)
    ano_actual <- as.numeric(format(Sys.Date(), "%Y"))
    if(ano >= ano_actual - 5) {  # Últimos 5 años
      bonus <- bonus + 0.5
    } else if(ano >= ano_actual - 10) {  # Últimos 10 años
      bonus <- bonus + 0.2
    }
  }

  return(min(bonus, 3.0))  # Cap máximo de bonus
}

#' Calcular similitud coseno entre dos textos usando TF-IDF básico
calcular_similitud_coseno_basica <- function(texto1, texto2) {

  if(nchar(texto1) < 3 || nchar(texto2) < 3) return(0)

  # Tokenizar
  tokens1 <- unlist(strsplit(limpiar_texto_para_sinonimos(texto1), "\\s+"))
  tokens2 <- unlist(strsplit(limpiar_texto_para_sinonimos(texto2), "\\s+"))

  tokens1 <- tokens1[nchar(tokens1) >= 3]
  tokens2 <- tokens2[nchar(tokens2) >= 3]

  if(length(tokens1) == 0 || length(tokens2) == 0) return(0)

  # Crear vocabulario común
  vocabulario <- unique(c(tokens1, tokens2))

  # Crear vectores TF
  vector1 <- sapply(vocabulario, function(x) sum(tokens1 == x))
  vector2 <- sapply(vocabulario, function(x) sum(tokens2 == x))

  # Similitud coseno
  producto_punto <- sum(vector1 * vector2)
  norma1 <- sqrt(sum(vector1^2))
  norma2 <- sqrt(sum(vector2^2))

  if(norma1 == 0 || norma2 == 0) return(0)

  similitud <- producto_punto / (norma1 * norma2)
  return(similitud)
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

#' Función de prueba para validar scoring semántico
probar_scoring_semantico <- function() {
  cat("=== PROBANDO ALGORITMOS DE SCORING SEMÁNTICO ===\n\n")

  # Crear datos de prueba
  paper_test <- data.frame(
    TITULO = "Machine Learning Algorithms for Image Processing",
    RESUMEN = "This study presents novel machine learning approaches for computer vision tasks including object detection and image classification using deep neural networks.",
    AUTOR_PALABRAS_CLAVES = "machine learning, computer vision, neural networks, image processing",
    SJR = "1.25",
    CITADO_POR = "45",
    ANO = "2023",
    stringsAsFactors = FALSE
  )

  # Consultas de prueba
  consultas <- c(
    "machine learning",
    "computer vision",
    "image processing algorithms",
    "neural networks for classification"
  )

  for(consulta in consultas) {
    cat(paste("CONSULTA:", consulta, "\n"))

    # Expandir semánticamente
    expansion <- expandir_consulta_semantica(consulta)

    # Calcular score
    score <- calcular_score_semantico(paper_test, consulta, expansion)

    cat(paste("- Score total:", round(score, 3), "\n"))
    cat(paste("- Términos expandidos:", length(expansion$terminos_adicionales), "\n"))
    cat(paste("- Query expandida:", substr(expansion$query_expandida, 1, 100), "...\n"))
    cat("\n")
  }

  cat("=== FIN PRUEBAS SCORING ===\n")
}

# Ejecutar pruebas si se ejecuta directamente
if(interactive()) {
  # probar_scoring_semantico()
}