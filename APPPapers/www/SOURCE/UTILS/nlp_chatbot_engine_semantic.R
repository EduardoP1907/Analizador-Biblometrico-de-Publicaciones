#=======================================
# Motor PLN Chatbot SEMÁNTICO - FASE 2 EMBEDDINGS
# Búsqueda multilingüe ES↔EN con sentence-transformers
#=======================================

library(stringi, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# Cargar módulos de expansión de sinónimos y scoring híbrido
source("www/SOURCE/UTILS/nlp_synonyms_academic.R")
source("www/SOURCE/UTILS/nlp_semantic_scoring.R")
source("www/SOURCE/UTILS/nlp_temporal_filters.R")

# Cargar módulos de análisis temporal avanzado
source("www/SOURCE/UTILS/nlp_analisis_temporal_avanzado.R")
source("www/SOURCE/UTILS/nlp_analisis_citaciones_temporal.R")
source("www/SOURCE/UTILS/nlp_analisis_autores_temporal.R")

# Cargar módulo de búsqueda por autor (fix: detector corregido)
source("www/SOURCE/UTILS/nlp_busqueda_por_autor.R")

# Cargar motor de embeddings multilingues
source("www/SOURCE/UTILS/nlp_embedding_search.R")

#' Motor PLN semántico
#'
#' Pipeline de una consulta:
#' - Parseo de filtros (autor, rango de años) y términos de tema
#' - Expansión de consultas con sinónimos académicos
#' - Búsqueda híbrida: embeddings + búsqueda difusa + sinónimos (buscar_papers_hibrido)
#' - Resumen extractivo LexRank y síntesis de abstracts
#'
#' @param query Consulta del usuario
#' @param data Dataset de papers
#' @param openai_key Clave OpenAI (opcional)
#' @param usar_expansion_semantica TRUE para usar sinónimos (default)
#' @return Lista con resultados mejorados
proceso_nlp_chatbot_semantico <- function(query, data, openai_key = NULL, usar_expansion_semantica = TRUE) {

  # ===== VALIDACIONES INICIALES =====
  if (is.null(query) || nchar(trimws(query)) == 0) {
    return(list(success = FALSE, message = "Por favor, ingrese una consulta de búsqueda válida."))
  }
  if (is.null(data) || nrow(data) == 0) {
    return(list(success = FALSE, message = "No hay datos disponibles para procesar."))
  }

  cat(paste("🔍 Procesando consulta:", query, "\n"))

  # ===== PARSEAR TODOS LOS FILTROS EN UNA SOLA PASADA =====
  # Extrae autor, rango de años y términos de tema de forma simultánea,
  # lo que permite combinar filtros: "papers de García sobre ML del 2020 al 2023"
  filtros      <- parsear_filtros_query(query)
  filtro_autor <- filtros$autor
  filtro_anos  <- filtros$anos
  terminos_tema <- filtros$terminos_tema

  # ===== PASO 1: FILTRO TEMPORAL =====
  data_filtrada <- data
  if (filtro_anos$tiene) {
    data_filtrada <- aplicar_filtro_anos(data_filtrada, filtro_anos)
    if (nrow(data_filtrada) == 0) {
      return(list(
        success          = TRUE,
        resumen_generado = paste0("No se encontraron papers publicados entre ",
                                   filtro_anos$desde, " y ", filtro_anos$hasta, "."),
        num_papers       = 0,
        papers           = data.frame(),
        filtros_aplicados = filtros
      ))
    }
  }

  # ===== PASO 2: FILTRO DE AUTOR =====
  if (filtro_autor$tiene) {
    cat(paste0("👤 Filtrando por autor: '", filtro_autor$nombre, "'\n"))
    papers_autor <- aplicar_filtro_autor(data_filtrada, filtro_autor$nombre)

    # Solo autor (sin tema de búsqueda) → devolver resultados del autor directamente
    if (nchar(trimws(terminos_tema)) < 3) {
      resumen_autor_txt <- generar_resumen_busqueda_autor(papers_autor, filtro_autor$nombre, query)
      if (filtro_anos$tiene && nrow(papers_autor) > 0) {
        resumen_autor_txt <- paste0(resumen_autor_txt, " Período: ", filtro_anos$desde, " – ", filtro_anos$hasta, ".")
      }
      # Añadir fragmentos de abstracts también para búsquedas por autor
      sintesis_autor <- sintetizar_desde_abstracts(papers_autor)
      contribuciones_html <- crear_seccion_contribuciones_html(sintesis_autor$contribuciones)
      resumen_autor <- paste0(htmltools::htmlEscape(resumen_autor_txt), contribuciones_html)
      papers_info <- preparar_papers_info_mejorado(papers_autor)
      return(list(
        success             = TRUE,
        es_busqueda_autor   = TRUE,
        query_original      = query,
        nombre_autor        = filtro_autor$nombre,
        num_papers          = nrow(papers_autor),
        papers              = papers_info,
        resumen_generado    = resumen_autor,
        confianza_deteccion = 0.9,
        filtros_aplicados   = filtros,
        info_busqueda       = "Búsqueda por autor"
      ))
    }

    # Autor + tema → buscar semánticamente dentro de los papers del autor
    cat(paste0("🔍 Búsqueda de '", terminos_tema, "' en papers de '", filtro_autor$nombre, "'\n"))
    if (nrow(papers_autor) == 0) {
      return(list(
        success          = TRUE,
        resumen_generado = paste0("No se encontraron papers del autor '", filtro_autor$nombre, "'."),
        num_papers       = 0,
        papers           = data.frame(),
        filtros_aplicados = filtros
      ))
    }
    data_filtrada <- papers_autor   # acotar dataset al autor antes de búsqueda semántica
  }

  # ===== PASO 3: BÚSQUEDA SEMÁNTICA =====
  # Usar terminos_tema si hay algo útil; si no, la query completa
  query_busqueda <- if (nchar(trimws(terminos_tema)) >= 3) terminos_tema else query

  expansion_result <- NULL
  if (usar_expansion_semantica) {
    cat("📝 Expandiendo consulta con sinónimos académicos...\n")
    expansion_result <- expandir_consulta_semantica(query_busqueda)
    if (expansion_result$num_expansiones > 0)
      cat(paste("   ✅ Encontrados", expansion_result$num_expansiones, "términos relacionados\n"))
  } else {
    expansion_result <- list(
      query_original       = query_busqueda,
      query_expandida      = query_busqueda,
      terminos_adicionales = character(0),
      num_expansiones      = 0
    )
  }

  papers_encontrados <- buscar_papers_hibrido(query_busqueda, data_filtrada, expansion_result)

  if (nrow(papers_encontrados) == 0) {
    msg <- if (filtro_autor$tiene) {
      paste0("No se encontraron papers de '", filtro_autor$nombre,
             "' relacionados con '", terminos_tema, "'.",
             if (filtro_anos$tiene) paste0(" Período: ", filtro_anos$desde, "-", filtro_anos$hasta, ".") else "")
    } else if (filtro_anos$tiene) {
      paste0("No se encontraron papers sobre '", query_busqueda, "' en el período ",
             filtro_anos$desde, "-", filtro_anos$hasta, ".")
    } else {
      paste0("No se encontraron papers relacionados con '", query, "'. ",
             "Intente con términos diferentes o más generales.")
    }
    return(list(
      success           = TRUE,
      resumen_generado  = msg,
      num_papers        = 0,
      papers            = data.frame(),
      terminos_busqueda = unlist(strsplit(query_busqueda, "\\s+")),
      expansion_info    = expansion_result,
      filtros_aplicados = filtros
    ))
  }

  cat(paste("   ✅ Encontrados", nrow(papers_encontrados), "papers relevantes\n"))

  # ===== PASO 4: ANÁLISIS Y RESUMEN =====
  cat("🧠 Analizando abstracts y generando resumen...\n")
  resumen_estructurado <- generar_resumen_semantico_mejorado(query, papers_encontrados, expansion_result, openai_key)

  # Prefijo de contexto para búsquedas con filtros combinados (HTML-safe)
  if (filtro_autor$tiene) {
    prefijo_txt <- paste0(
      "<strong>Autor: ", htmltools::htmlEscape(filtro_autor$nombre), "</strong>",
      if (filtro_anos$tiene) paste0(" &nbsp;|&nbsp; Período: ", filtro_anos$desde, "–", filtro_anos$hasta) else "",
      "<br>"
    )
    resumen_estructurado <- paste0(prefijo_txt, resumen_estructurado)
  } else if (filtro_anos$tiene) {
    prefijo_txt <- paste0("<strong>[Período ", filtro_anos$desde, "–", filtro_anos$hasta, "]</strong> ")
    resumen_estructurado <- paste0(prefijo_txt, resumen_estructurado)
  }

  papers_info <- preparar_papers_info_mejorado(papers_encontrados)

  cat("✅ Procesamiento completado\n")

  return(list(
    success           = TRUE,
    es_busqueda_autor = filtro_autor$tiene,
    resumen_generado  = resumen_estructurado,
    num_papers        = nrow(papers_encontrados),
    papers            = papers_info,
    terminos_busqueda = unlist(strsplit(query_busqueda, "\\s+")),
    expansion_info    = expansion_result,
    scoring_info      = obtener_info_scoring(papers_encontrados),
    filtros_aplicados = filtros
  ))
}

#' Búsqueda híbrida: fusión de embeddings, búsqueda difusa y sinónimos
#'
#' Calcula score_semantico para todas las publicaciones (calcular_scores_hibridos),
#' descarta las que no alcanzan UMBRAL_HIBRIDO y devuelve las TOP_K_HIBRIDO mejores.
#' Si los embeddings no están disponibles opera en modo degradado (solo léxico).
buscar_papers_hibrido <- function(query, data, expansion_result, top_k = TOP_K_HIBRIDO) {

  # Asegurar columnas necesarias
  columnas_requeridas <- c("TITULO", "RESUMEN", "AUTOR_PALABRAS_CLAVES", "NOMBRE_AUTOR", "ANO", "SJR", "CITADO_POR", "LINK")
  for(col in columnas_requeridas) {
    if(!col %in% colnames(data)) {
      data[[col]] <- ""
    }
  }

  sims <- if (embeddings_listos()) similitudes_embeddings(query, data) else NULL
  cat(if (is.null(sims)) "🔑 Búsqueda híbrida en modo degradado (difusa + sinónimos)...
"
      else "🧠 Búsqueda híbrida (embeddings + difusa + sinónimos)...
")

  scores <- calcular_scores_hibridos(query, data, expansion_result, sims)
  data <- cbind(data, scores)

  # ===== FILTRAR Y ORDENAR RESULTADOS =====
  papers_relevantes <- data[data$score_semantico >= UMBRAL_HIBRIDO, ]

  if(nrow(papers_relevantes) == 0) {
    return(data.frame())
  }

  papers_relevantes <- papers_relevantes[order(papers_relevantes$score_semantico, decreasing = TRUE), ]

  # El corpus repite publicaciones (mismo título en varios registros): se
  # conserva solo la de mayor puntaje para no mostrarla dos veces
  clave_titulo <- stri_replace_all_regex(
    stri_trans_general(tolower(as.character(papers_relevantes$TITULO)), "Latin-ASCII"), "[^a-z0-9]+", " ")
  papers_relevantes <- papers_relevantes[!duplicated(trimws(clave_titulo)), ]
  papers_relevantes <- head(papers_relevantes, top_k)

  # Normalizar scores para mejor interpretación
  papers_relevantes$score_normalizado <- normalizar_scores(papers_relevantes$score_semantico)

  return(papers_relevantes)
}

#' Generar resumen semántico estructurado (con síntesis de abstracts)
generar_resumen_semantico_mejorado <- function(query, papers_encontrados, expansion_result, openai_key = NULL) {

  num_papers <- nrow(papers_encontrados)

  # Análisis de áreas/metodologías/aplicaciones (basado en frecuencia de términos)
  analisis <- analizar_papers_estructurado(papers_encontrados, query)

  # Síntesis real desde los textos de los abstracts
  cat("📄 Sintetizando abstracts de los papers encontrados...\n")
  sintesis <- sintetizar_desde_abstracts(papers_encontrados)

  # Análisis temporal avanzado
  analisis_temporal_avanzado <- NULL
  if (num_papers >= 3) {
    cat("🔬 Ejecutando análisis temporal avanzado...\n")
    analisis_temporal_avanzado <- ejecutar_analisis_temporal_completo(papers_encontrados, query)
  }

  # ===== CONSTRUIR RESUMEN EN HTML PARA MEJOR PRESENTACIÓN =====

  partes <- character(0)

  # 1. Encabezado: cantidad + áreas detectadas
  partes <- c(partes, crear_resumen_general(query, num_papers, analisis$areas_principales))

  # 2. Conceptos más frecuentes en los abstracts (lectura real de RESUMEN)
  if (nchar(sintesis$parrafo_tematico) > 0) {
    partes <- c(partes, sintesis$parrafo_tematico)
  }

  # 3. Metodologías y aplicaciones (análisis de patrón)
  partes <- c(partes, crear_seccion_metodologias(analisis$metodologias_detectadas))
  partes <- c(partes, crear_seccion_aplicaciones(analisis$aplicaciones_detectadas))

  # 4. Tendencias temporales
  if (num_papers >= 5) {
    seccion_tendencias <- crear_seccion_tendencias(papers_encontrados)
    if (nchar(seccion_tendencias) > 0) partes <- c(partes, seccion_tendencias)
  }

  # 5. Análisis temporal avanzado
  if (!is.null(analisis_temporal_avanzado)) {
    sec_temp <- crear_seccion_analisis_temporal_avanzado(analisis_temporal_avanzado)
    if (nchar(sec_temp) > 0) partes <- c(partes, sec_temp)
  }

  parrafo_intro <- mejorar_redaccion_resumen(paste(partes, collapse = " "))

  # 6. Resumen extractivo LexRank sobre los abstracts de las primeras publicaciones
  cat("📝 Generando resumen extractivo (LexRank)...\n")
  seccion_lexrank <- crear_seccion_lexrank_html(resumen_lexrank(papers_encontrados))

  # 7. Fragmentos clave extraídos directamente de los abstracts
  seccion_contribuciones <- crear_seccion_contribuciones_html(sintesis$contribuciones)

  # Ensamblar: párrafo introductorio + resumen LexRank + contribuciones por paper
  resumen_final <- paste0(parrafo_intro, seccion_lexrank, seccion_contribuciones)

  return(resumen_final)
}

#' Resumen extractivo LexRank (Erkan & Radev, 2004)
#'
#' Aplica LexRank sobre los abstracts (campo RESUMEN) de las primeras
#' publicaciones del ranking y devuelve las oraciones más centrales.
#' No interviene en el orden de los resultados.
#'
#' @param papers Publicaciones ya ordenadas por score_semantico
#' @param n_papers Número de publicaciones del ranking que se resumen
#' @param n_oraciones Máximo de oraciones del resumen
#' @return Vector character con las oraciones seleccionadas (puede ser vacío)
resumen_lexrank <- function(papers, n_papers = 5, n_oraciones = 3) {
  if (!"RESUMEN" %in% colnames(papers) || !requireNamespace("lexRankr", quietly = TRUE)) {
    return(character(0))
  }

  abstracts <- as.character(head(papers$RESUMEN, n_papers))
  abstracts <- abstracts[!is.na(abstracts) & nchar(trimws(abstracts)) > 50]
  if (length(abstracts) == 0) return(character(0))

  oraciones <- tryCatch({
    res <- lexRankr::lexRank(abstracts, docId = "create", n = n_oraciones,
                             continuous = TRUE, Verbose = FALSE)
    as.character(res$sentence)
  }, error = function(e) {
    cat(paste0("⚠️  [LEXRANK] ", conditionMessage(e), "\n"))
    character(0)
  })

  head(oraciones, n_oraciones)
}

#' Genera la sección HTML del resumen extractivo LexRank
crear_seccion_lexrank_html <- function(oraciones) {
  if (length(oraciones) == 0) return("")

  items <- paste0("<li style='margin-bottom:6px;'>", htmltools::htmlEscape(oraciones), "</li>")

  paste0(
    "<br><strong style='color:#343a40;'>Resumen extractivo (LexRank):</strong>",
    "<ul style='margin-top:6px;padding-left:18px;font-size:13px;color:#495057;'>",
    paste(items, collapse = ""),
    "</ul>"
  )
}

# ── Síntesis desde los abstracts reales ──────────────────────────────────────

#' Extrae la oración de contribución principal de un abstract académico (en inglés)
extraer_oracion_principal_abstract <- function(resumen) {
  if (is.na(resumen) || nchar(trimws(resumen)) < 20) return(NULL)

  # Dividir en oraciones por punto + espacio o salto de línea
  oraciones <- trimws(unlist(strsplit(as.character(resumen), "(?<=[.!?])\\s+", perl = TRUE)))
  oraciones <- oraciones[nchar(oraciones) > 30]
  if (length(oraciones) == 0) return(NULL)

  # Frases que indican la contribución en papers académicos
  marcadores <- c(
    "we propose", "we present", "we introduce", "we develop", "we describe",
    "we show", "we demonstrate", "we design", "we evaluate", "we build",
    "this paper", "this work", "this study", "this article", "this research",
    "in this paper", "in this work", "in this study", "in this article",
    "our approach", "our method", "our system", "our framework", "our model",
    "novel", "new approach", "new method", "new algorithm", "new framework",
    "we achieve", "achieves", "outperforms", "state-of-the-art", "benchmark"
  )

  for (oracion in oraciones) {
    for (m in marcadores) {
      if (grepl(m, oracion, ignore.case = TRUE)) {
        if (nchar(oracion) > 250) oracion <- paste0(substr(oracion, 1, 247), "...")
        return(oracion)
      }
    }
  }

  # Sin marcador encontrado: usar la primera oración (suele ser el contexto del paper)
  primera <- oraciones[1]
  if (nchar(primera) > 250) primera <- paste0(substr(primera, 1, 247), "...")
  primera
}

#' Sintetiza los abstracts de los papers encontrados
#'
#' Retorna:
#'   $parrafo_tematico  — párrafo con los conceptos más frecuentes
#'   $contribuciones    — lista con título, autor, año y oración clave de top-3 papers
sintetizar_desde_abstracts <- function(papers) {

  resultado <- list(parrafo_tematico = "", contribuciones = list())
  if (!"RESUMEN" %in% colnames(papers)) return(resultado)

  resumenes_validos <- papers$RESUMEN[!is.na(papers$RESUMEN) & nchar(as.character(papers$RESUMEN)) > 20]
  if (length(resumenes_validos) == 0) return(resultado)

  # ── Párrafo temático: conceptos más frecuentes en los abstracts ──
  stop_en <- c(
    "the","a","an","in","of","to","and","is","are","was","were","this","that",
    "these","those","for","with","on","at","by","from","as","be","been","being",
    "have","has","had","do","does","did","will","would","could","should","may",
    "might","shall","can","not","it","its","their","we","our","us","which","also",
    "such","than","more","into","using","based","used","results","show","paper",
    "papers","approach","method","methods","study","work","present","propose",
    "proposed","system","data","model","models","performance","experiments",
    "different","various","several","many","all","each","other","well","new",
    "two","three","high","large","small","both","only","number","first","then",
    "however","while","when","where","what","how","between","through","over",
    "after","before","during","without","within","under","about","among","both"
  )

  texto_total <- paste(resumenes_validos, collapse = " ")
  palabras <- unlist(strsplit(tolower(texto_total), "[^a-z]+"))
  palabras <- palabras[nchar(palabras) >= 4 & !palabras %in% stop_en]

  if (length(palabras) > 0) {
    freq <- sort(table(palabras), decreasing = TRUE)
    top6 <- names(head(freq, 6))
    if (length(top6) >= 2) {
      resultado$parrafo_tematico <- paste0(
        "El análisis de los resúmenes de los ", length(resumenes_validos),
        " papers encontrados revela que los conceptos más recurrentes en la literatura son: ",
        paste(top6, collapse = ", "), "."
      )
    }
  }

  # ── Contribuciones: oraciones clave de los top-3 papers ──
  # Ordenar por relevancia semántica, luego por citas
  if ("score_normalizado" %in% colnames(papers) && any(!is.na(papers$score_normalizado))) {
    papers_ord <- papers[order(papers$score_normalizado, decreasing = TRUE), ]
  } else if ("CITADO_POR" %in% colnames(papers)) {
    citas <- suppressWarnings(as.numeric(papers$CITADO_POR))
    papers_ord <- papers[order(citas, decreasing = TRUE, na.last = TRUE), ]
  } else {
    papers_ord <- papers
  }

  contribuciones <- list()
  for (i in seq_len(min(3, nrow(papers_ord)))) {
    paper   <- papers_ord[i, ]
    oracion <- extraer_oracion_principal_abstract(paper$RESUMEN)
    if (!is.null(oracion)) {
      contribuciones[[length(contribuciones) + 1]] <- list(
        titulo   = as.character(paper$TITULO),
        autor    = as.character(paper$NOMBRE_AUTOR),
        ano      = as.character(paper$ANO),
        fragmento = oracion
      )
    }
  }
  resultado$contribuciones <- contribuciones

  resultado
}

#' Genera la sección HTML de contribuciones clave extraídas de los abstracts
crear_seccion_contribuciones_html <- function(contribuciones) {
  if (length(contribuciones) == 0) return("")

  items <- sapply(contribuciones, function(c) {
    paste0(
      "<li style='margin-bottom:8px;'>",
      "<strong>", htmltools::htmlEscape(c$titulo), "</strong>",
      " <span style='color:#6c757d;font-size:12px;'>(", htmltools::htmlEscape(c$autor),
      ", ", c$ano, ")</span><br>",
      "<em style='color:#495057;font-size:13px;'>", htmltools::htmlEscape(c$fragmento), "</em>",
      "</li>"
    )
  })

  paste0(
    "<br><strong style='color:#343a40;'>Fragmentos clave de los abstracts:</strong>",
    "<ul style='margin-top:6px;padding-left:18px;'>",
    paste(items, collapse = ""),
    "</ul>"
  )
}

#' Analizar papers de forma estructurada
analizar_papers_estructurado <- function(papers, query) {

  # Combinar todos los textos para análisis global
  todos_titulos <- paste(papers$TITULO, collapse = " ")
  todos_resumenes <- paste(papers$RESUMEN, collapse = " ")
  todas_keywords <- paste(papers$AUTOR_PALABRAS_CLAVES, collapse = " ")

  # Texto completo para análisis
  texto_completo <- paste(todos_titulos, todos_resumenes, todas_keywords, sep = " ")
  texto_limpio <- limpiar_texto_para_sinonimos(texto_completo)

  # ===== IDENTIFICAR ÁREAS PRINCIPALES =====
  areas_principales <- identificar_areas_investigacion(texto_limpio)

  # ===== DETECTAR METODOLOGÍAS =====
  metodologias_detectadas <- detectar_metodologias_automatico(texto_limpio)

  # ===== IDENTIFICAR APLICACIONES =====
  aplicaciones_detectadas <- identificar_aplicaciones_automatico(texto_limpio)

  # ===== ANÁLISIS DE TÉRMINOS FRECUENTES =====
  terminos_frecuentes <- analizar_terminos_frecuentes(texto_limpio)

  return(list(
    areas_principales = areas_principales,
    metodologias_detectadas = metodologias_detectadas,
    aplicaciones_detectadas = aplicaciones_detectadas,
    terminos_frecuentes = terminos_frecuentes
  ))
}

#' Identificar áreas de investigación automáticamente
identificar_areas_investigacion <- function(texto) {

  areas_patrones <- list(
    "Machine Learning" = c("machine learning", "ml", "neural network", "deep learning", "algorithm", "classification", "regression", "clustering"),
    "Computer Vision" = c("image processing", "computer vision", "object detection", "image classification", "pattern recognition", "opencv", "segmentation"),
    "Natural Language Processing" = c("nlp", "natural language", "text mining", "sentiment analysis", "language model", "text processing"),
    "Data Science" = c("data mining", "big data", "analytics", "data analysis", "business intelligence", "data visualization"),
    "Software Engineering" = c("software engineering", "software development", "programming", "software design", "agile", "framework"),
    "Optimization" = c("optimization", "genetic algorithm", "evolutionary", "metaheuristic", "optimization problem"),
    "Bioinformatics" = c("bioinformatics", "genomics", "proteomics", "computational biology", "medical informatics"),
    "Cybersecurity" = c("security", "cybersecurity", "cryptography", "network security", "information security")
  )

  areas_detectadas <- list()

  for(area in names(areas_patrones)) {
    score_area <- 0
    patrones <- areas_patrones[[area]]

    for(patron in patrones) {
      coincidencias <- length(gregexpr(patron, texto, ignore.case = TRUE)[[1]])
      if(coincidencias > 0) {
        score_area <- score_area + coincidencias
      }
    }

    if(score_area > 0) {
      areas_detectadas[[area]] <- score_area
    }
  }

  # Ordenar por relevancia
  if(length(areas_detectadas) > 0) {
    areas_ordenadas <- areas_detectadas[order(unlist(areas_detectadas), decreasing = TRUE)]
    return(names(areas_ordenadas)[1:min(3, length(areas_ordenadas))])
  }

  return("Investigación General")
}

#' Detectar metodologías automáticamente
detectar_metodologias_automatico <- function(texto) {

  metodologias_patrones <- c(
    "support vector machine", "svm", "random forest", "decision tree", "neural network",
    "convolutional neural network", "cnn", "recurrent neural network", "rnn", "lstm",
    "k-means", "clustering", "pca", "principal component analysis",
    "regression", "classification", "supervised learning", "unsupervised learning",
    "genetic algorithm", "evolutionary algorithm", "particle swarm optimization",
    "gradient descent", "backpropagation", "cross validation"
  )

  metodologias_encontradas <- character(0)

  for(metodologia in metodologias_patrones) {
    if(grepl(metodologia, texto, ignore.case = TRUE)) {
      metodologias_encontradas <- c(metodologias_encontradas, metodologia)
    }
  }

  return(unique(metodologias_encontradas))
}

#' Identificar aplicaciones automáticamente
identificar_aplicaciones_automatico <- function(texto) {

  aplicaciones_patrones <- c(
    "medical diagnosis", "healthcare", "autonomous vehicle", "robotics",
    "finance", "banking", "recommendation system", "social media",
    "education", "e-learning", "gaming", "entertainment",
    "agriculture", "manufacturing", "smart city", "iot",
    "web development", "mobile application", "cloud computing"
  )

  aplicaciones_encontradas <- character(0)

  for(aplicacion in aplicaciones_patrones) {
    if(grepl(aplicacion, texto, ignore.case = TRUE)) {
      aplicaciones_encontradas <- c(aplicaciones_encontradas, aplicacion)
    }
  }

  return(unique(aplicaciones_encontradas))
}

#' Crear resumen general estructurado
crear_resumen_general <- function(query, num_papers, areas_principales) {

  if(length(areas_principales) > 0) {
    areas_texto <- paste(areas_principales, collapse = ", ")
    resumen <- paste0(
      "Se encontraron ", num_papers, " papers relacionados con '", query,
      "', abarcando principalmente investigaciones en ", areas_texto, ". "
    )
  } else {
    resumen <- paste0(
      "Se encontraron ", num_papers, " papers relacionados con '", query,
      "', cubriendo diversos enfoques metodológicos en el área de investigación. "
    )
  }

  return(resumen)
}

#' Crear sección de metodologías
crear_seccion_metodologias <- function(metodologias) {

  if(length(metodologias) == 0) {
    return("Los estudios emplean diversas metodologías de investigación.")
  }

  metodologias_principales <- head(metodologias, 4)

  if(length(metodologias_principales) == 1) {
    return(paste("La metodología principal empleada es", metodologias_principales[1], "."))
  } else {
    return(paste0("Las metodologías más frecuentes incluyen ",
                  paste(metodologias_principales, collapse = ", "), "."))
  }
}

#' Crear sección de aplicaciones
crear_seccion_aplicaciones <- function(aplicaciones) {

  if(length(aplicaciones) == 0) {
    return("Las aplicaciones abarcan diversos dominios de investigación y desarrollo.")
  }

  aplicaciones_principales <- head(aplicaciones, 3)
  return(paste0("Las principales aplicaciones se enfocan en ",
                paste(aplicaciones_principales, collapse = ", "), "."))
}

#' Crear sección de tendencias temporales
crear_seccion_tendencias <- function(papers) {

  if(!"ANO" %in% colnames(papers)) {
    return("")
  }

  anos <- as.numeric(papers$ANO)
  anos <- anos[!is.na(anos)]

  if(length(anos) < 3) {
    return("")
  }

  ano_min <- min(anos)
  ano_max <- max(anos)
  anos_recientes <- sum(anos >= (ano_max - 3))

  tendencia_texto <- paste0("Los estudios abarcan desde ", ano_min, " hasta ", ano_max,
                           ", con ", anos_recientes, " publicaciones en los últimos 3 años, ",
                           "indicando un área de investigación activa.")

  return(tendencia_texto)
}

#' Mejorar redacción del resumen final
mejorar_redaccion_resumen <- function(resumen) {

  # Limpiar espacios múltiples
  resumen <- gsub("\\s+", " ", resumen)
  resumen <- trimws(resumen)

  # Mejorar conectores
  resumen <- gsub("\\. Los estudios", ". Estos estudios", resumen)
  resumen <- gsub("\\. Las metodologías", ". En términos metodológicos, los trabajos", resumen)
  resumen <- gsub("\\. Las principales aplicaciones", ". Las aplicaciones principales", resumen)

  return(resumen)
}

preparar_papers_info_mejorado <- function(papers) {
  if (nrow(papers) == 0) return(data.frame())
  if (!"score_semantico"  %in% colnames(papers)) papers$score_semantico  <- 0
  if (!"score_normalizado" %in% colnames(papers)) papers$score_normalizado <- 0

  cols <- intersect(
    c("TITULO","NOMBRE_AUTOR","ANO","LINK","SJR","CITADO_POR","score_semantico","score_normalizado"),
    colnames(papers)
  )
  papers_info <- papers[, cols, drop = FALSE]
  papers_info <- papers_info[order(papers_info$score_normalizado, decreasing = TRUE), ]
  return(papers_info)
}

obtener_info_scoring <- function(papers) {
  s <- if ("score_semantico"  %in% colnames(papers)) papers$score_semantico  else rep(0, nrow(papers))
  n <- if ("score_normalizado" %in% colnames(papers)) papers$score_normalizado else rep(0, nrow(papers))
  return(list(
    score_promedio         = mean(s, na.rm = TRUE),
    score_maximo           = max(s, na.rm = TRUE),
    papers_alta_relevancia = sum(n > 0.7, na.rm = TRUE)
  ))
}

analizar_terminos_frecuentes <- function(texto) {
  tokens <- unlist(strsplit(texto, "\\s+"))
  tokens <- tokens[nchar(tokens) >= 4]
  freq_table <- table(tokens)
  return(head(sort(freq_table, decreasing = TRUE), 10))
}

# Para compatibilidad, mantener la función original pero redirigir al motor semántico
proceso_nlp_chatbot_simple <- function(query, data, openai_key = NULL) {
  return(proceso_nlp_chatbot_semantico(query, data, openai_key, usar_expansion_semantica = TRUE))
}

#=======================================
# FUNCIONES DE ANÁLISIS TEMPORAL AVANZADO - FASE 2
#=======================================

#' Ejecutar análisis temporal completo de los papers encontrados
#' @param papers_encontrados Papers filtrados por la búsqueda semántica
#' @param query_original Query original del usuario
ejecutar_analisis_temporal_completo <- function(papers_encontrados, query_original) {

  # Ejecutar todos los análisis temporales
  resultados_analisis <- list()

  try({
    # 1. Detección de tendencias temporales
    resultados_analisis$tendencias <- detectar_tendencias_temporales(papers_encontrados, query_original)
  }, silent = TRUE)

  try({
    # 2. Análisis de evolución de temas
    resultados_analisis$evolucion_temas <- analizar_evolucion_temas(papers_encontrados, query_original)
  }, silent = TRUE)

  try({
    # 3. Análisis de citaciones temporal
    resultados_analisis$citaciones_temporal <- analizar_papers_mas_citados_temporal(papers_encontrados)
  }, silent = TRUE)

  try({
    # 4. Análisis de productividad de autores
    resultados_analisis$autores_temporal <- analizar_productividad_autores_temporal(papers_encontrados)
  }, silent = TRUE)

  # Crear resumen ejecutivo
  resultados_analisis$resumen_ejecutivo <- crear_resumen_ejecutivo_temporal(resultados_analisis, query_original)

  return(resultados_analisis)
}

#' Crear sección de análisis temporal avanzado para el resumen
#' @param analisis_temporal_avanzado Resultados del análisis temporal completo
crear_seccion_analisis_temporal_avanzado <- function(analisis_temporal_avanzado) {

  if(is.null(analisis_temporal_avanzado)) {
    return("")
  }

  seccion <- ""

  # Información sobre tendencias
  if(!is.null(analisis_temporal_avanzado$tendencias) && analisis_temporal_avanzado$tendencias$tendencia_detectada) {
    tendencias <- analisis_temporal_avanzado$tendencias
    seccion <- paste0(seccion, "📈 ", tendencias$resumen_textual, " ")
  }

  # Información sobre evolución de temas
  if(!is.null(analisis_temporal_avanzado$evolucion_temas) && analisis_temporal_avanzado$evolucion_temas$evolucion_detectada) {
    evolucion <- analisis_temporal_avanzado$evolucion_temas
    seccion <- paste0(seccion, evolucion$resumen_evolucion, " ")
  }

  # Información sobre citaciones
  if(!is.null(analisis_temporal_avanzado$citaciones_temporal) && analisis_temporal_avanzado$citaciones_temporal$analisis_completado) {
    citaciones <- analisis_temporal_avanzado$citaciones_temporal
    seccion <- paste0(seccion, citaciones$resumen_general, " ")
  }

  # Información sobre productividad de autores
  if(!is.null(analisis_temporal_avanzado$autores_temporal) && analisis_temporal_avanzado$autores_temporal$analisis_completado) {
    autores <- analisis_temporal_avanzado$autores_temporal
    seccion <- paste0(seccion, autores$resumen_general, " ")
  }

  return(seccion)
}

#' Crear resumen ejecutivo del análisis temporal
#' @param resultados_analisis Todos los resultados de análisis temporal
#' @param query_original Query original del usuario
crear_resumen_ejecutivo_temporal <- function(resultados_analisis, query_original) {

  resumen_ejecutivo <- list(
    query_analizada = query_original,
    analisis_ejecutados = character(0),
    principales_hallazgos = character(0),
    recomendaciones = character(0)
  )

  # Identificar análisis ejecutados exitosamente
  if(!is.null(resultados_analisis$tendencias) && resultados_analisis$tendencias$tendencia_detectada) {
    resumen_ejecutivo$analisis_ejecutados <- c(resumen_ejecutivo$analisis_ejecutados, "Análisis de Tendencias Temporales")

    # Agregar hallazgo principal
    tendencia_tipo <- resultados_analisis$tendencias$tipo_tendencia
    hallazgo <- paste("Tendencia temporal:", tendencia_tipo)
    resumen_ejecutivo$principales_hallazgos <- c(resumen_ejecutivo$principales_hallazgos, hallazgo)
  }

  if(!is.null(resultados_analisis$evolucion_temas) && resultados_analisis$evolucion_temas$evolucion_detectada) {
    resumen_ejecutivo$analisis_ejecutados <- c(resumen_ejecutivo$analisis_ejecutados, "Evolución de Temas")

    # Agregar hallazgo sobre estabilidad temática
    if(!is.null(resultados_analisis$evolucion_temas$cambios_tematicos$estabilidad_promedio)) {
      estabilidad <- round(resultados_analisis$evolucion_temas$cambios_tematicos$estabilidad_promedio * 100, 1)
      hallazgo <- paste("Estabilidad temática:", paste0(estabilidad, "%"))
      resumen_ejecutivo$principales_hallazgos <- c(resumen_ejecutivo$principales_hallazgos, hallazgo)
    }
  }

  if(!is.null(resultados_analisis$citaciones_temporal) && resultados_analisis$citaciones_temporal$analisis_completado) {
    resumen_ejecutivo$analisis_ejecutados <- c(resumen_ejecutivo$analisis_ejecutados, "Análisis de Citaciones por Período")

    # Agregar hallazgo sobre el mejor período
    if(!is.null(resultados_analisis$citaciones_temporal$analisis_comparativo$mejor_periodo_impacto)) {
      mejor_periodo <- resultados_analisis$citaciones_temporal$analisis_comparativo$mejor_periodo_impacto$periodo
      hallazgo <- paste("Período de mayor impacto:", mejor_periodo)
      resumen_ejecutivo$principales_hallazgos <- c(resumen_ejecutivo$principales_hallazgos, hallazgo)
    }
  }

  if(!is.null(resultados_analisis$autores_temporal) && resultados_analisis$autores_temporal$analisis_completado) {
    resumen_ejecutivo$analisis_ejecutados <- c(resumen_ejecutivo$analisis_ejecutados, "Productividad de Autores")

    # Agregar hallazgo sobre autores consistentes
    autores_consistentes <- resultados_analisis$autores_temporal$autores_consistentes$total_autores_consistentes
    if(autores_consistentes > 0) {
      hallazgo <- paste("Autores con actividad consistente:", autores_consistentes)
      resumen_ejecutivo$principales_hallazgos <- c(resumen_ejecutivo$principales_hallazgos, hallazgo)
    }
  }

  # Generar recomendaciones básicas
  if(length(resumen_ejecutivo$analisis_ejecutados) >= 3) {
    resumen_ejecutivo$recomendaciones <- c(
      "Análisis temporal completo ejecutado exitosamente",
      "Datos suficientes para análisis longitudinal detallado",
      "Considerar análisis de colaboraciones entre autores destacados"
    )
  } else {
    resumen_ejecutivo$recomendaciones <- c(
      "Análisis temporal parcial - considerar ampliar criterios de búsqueda",
      "Datos limitados para algunos tipos de análisis temporal"
    )
  }

  return(resumen_ejecutivo)
}
