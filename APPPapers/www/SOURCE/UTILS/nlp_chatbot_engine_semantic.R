#=======================================
# Motor PLN Chatbot SEMÁNTICO - FASE 2 EMBEDDINGS
# Búsqueda multilingüe ES↔EN con sentence-transformers
#=======================================

library(stringi, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# Cargar módulos semánticos (keyword search - fallback)
source("www/SOURCE/UTILS/nlp_synonyms_academic.R")
source("www/SOURCE/UTILS/nlp_semantic_scoring.R")
source("www/SOURCE/UTILS/nlp_temporal_filters.R")

# Cargar módulos de análisis temporal avanzado
source("www/SOURCE/UTILS/nlp_analisis_temporal_avanzado.R")
source("www/SOURCE/UTILS/nlp_analisis_citaciones_temporal.R")
source("www/SOURCE/UTILS/nlp_analisis_autores_temporal.R")
source("www/SOURCE/UTILS/nlp_visualizaciones_temporales.R")

# Cargar módulo de búsqueda por autor (fix: detector corregido)
source("www/SOURCE/UTILS/nlp_busqueda_por_autor.R")

# Cargar motor de embeddings multilingues
source("www/SOURCE/UTILS/nlp_embedding_search.R")

#' Motor PLN semántico mejorado - FASE 1
#'
#' Implementa búsqueda semántica avanzada con:
#' - Expansión de consultas con sinónimos académicos
#' - Scoring semántico multi-algoritmo
#' - Análisis de co-ocurrencia de términos
#' - Generación de resúmenes estructurados
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

  papers_encontrados <- NULL

  if (embeddings_listos()) {
    cat("🧠 [EMBED] Búsqueda por similitud vectorial ES<>EN...\n")
    papers_encontrados <- buscar_por_embeddings(query = query_busqueda, data = data_filtrada, top_k = 20)
  }

  if (is.null(papers_encontrados)) {
    cat("🔑 Búsqueda semántica clásica (fallback)...\n")
    papers_encontrados <- buscar_papers_semantico_mejorado(query_busqueda, data_filtrada, expansion_result)
  }

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

#' Búsqueda semántica mejorada con múltiples estrategias
buscar_papers_semantico_mejorado <- function(query, data, expansion_result) {

  # Asegurar columnas necesarias
  columnas_requeridas <- c("TITULO", "RESUMEN", "AUTOR_PALABRAS_CLAVES", "NOMBRE_AUTOR", "ANO", "SJR", "CITADO_POR", "LINK")
  for(col in columnas_requeridas) {
    if(!col %in% colnames(data)) {
      data[[col]] <- ""
    }
  }

  # Inicializar scores
  data$score_semantico <- 0
  data$score_detalles <- ""

  cat(paste("   📊 Analizando", nrow(data), "papers en la base de datos...\n"))

  # ===== APLICAR SCORING SEMÁNTICO A CADA PAPER =====
  for(i in 1:nrow(data)) {
    if(i %% 500 == 0) {  # Progress para datasets grandes
      cat(paste("      Procesados", i, "papers...\n"))
    }

    paper_row <- data[i, ]
    score <- calcular_score_semantico(paper_row, query, expansion_result)

    if(score > 0.1) {  # Threshold mínimo
      data$score_semantico[i] <- score
      data$score_detalles[i] <- crear_detalle_scoring(paper_row, query, expansion_result, score)
    }
  }

  # ===== FILTRAR Y ORDENAR RESULTADOS =====
  papers_relevantes <- data[data$score_semantico > 0.1, ]

  if(nrow(papers_relevantes) == 0) {
    return(data.frame())
  }

  # Ordenar por score semántico
  papers_relevantes <- papers_relevantes[order(papers_relevantes$score_semantico, decreasing = TRUE), ]

  # Normalizar scores para mejor interpretación
  papers_relevantes$score_normalizado <- normalizar_scores(papers_relevantes$score_semantico)

  # Limitar resultados para eficiencia (top 20)
  if(nrow(papers_relevantes) > 20) {
    papers_relevantes <- papers_relevantes[1:20, ]
    cat("   ⚡ Limitando a top 20 resultados más relevantes\n")
  }

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

  # 6. Fragmentos clave extraídos directamente de los abstracts (sección nueva)
  seccion_contribuciones <- crear_seccion_contribuciones_html(sintesis$contribuciones)

  # Ensamblar: párrafo introductorio + contribuciones por paper
  if (nchar(seccion_contribuciones) > 0) {
    resumen_final <- paste0(parrafo_intro, seccion_contribuciones)
  } else {
    resumen_final <- parrafo_intro
  }

  return(resumen_final)
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

#' Funciones auxiliares mejoradas
extraer_terminos_basicos_mejorado <- function(query) {
  expansion <- expandir_consulta_semantica(query)
  terminos_originales <- unlist(strsplit(limpiar_texto_para_sinonimos(query), "\\s+"))
  terminos_expandidos <- head(expansion$terminos_adicionales, 5)

  return(unique(c(terminos_originales, terminos_expandidos)))
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

generar_mensaje_sin_resultados <- function(query, expansion_result) {
  if(expansion_result$num_expansiones > 0) {
    return(paste0("No se encontraron papers relacionados con '", query,
                 "' ni con sus términos relacionados. Intente con términos más específicos o diferentes."))
  } else {
    return(paste0("No se encontraron papers relacionados con '", query,
                 "'. Intente con términos diferentes o más generales."))
  }
}

#' Generar mensaje sin resultados considerando filtros temporales
generar_mensaje_sin_resultados_temporal <- function(query, restricciones_temporales, expansion_result) {

  mensaje_base <- if(expansion_result$num_expansiones > 0) {
    paste0("No se encontraron papers relacionados con '", query, "' ni con sus términos relacionados")
  } else {
    paste0("No se encontraron papers relacionados con '", query, "'")
  }

  if(restricciones_temporales$tiene_restriccion) {
    mensaje_temporal <- paste0(" que cumplan el criterio temporal: ", restricciones_temporales$descripcion_filtro)
    mensaje_completo <- paste0(mensaje_base, mensaje_temporal, ". ")

    # Sugerencias específicas para filtros temporales
    if(!is.null(restricciones_temporales$ano_desde) && restricciones_temporales$ano_desde >= 2020) {
      mensaje_completo <- paste0(mensaje_completo, "Intente ampliar el rango de años o buscar términos más generales.")
    } else {
      mensaje_completo <- paste0(mensaje_completo, "Intente con términos diferentes o ajustar el período temporal.")
    }

    return(mensaje_completo)
  } else {
    return(paste0(mensaje_base, ". Intente con términos diferentes o más generales."))
  }
}

crear_detalle_scoring <- function(paper_row, query, expansion_result, score) {
  return(paste("Score:", round(score, 2)))
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

#' Función de prueba integrada
probar_motor_semantico <- function() {
  cat("=== PROBANDO MOTOR SEMÁNTICO INTEGRADO ===\n\n")

  # Crear datos de prueba
  datos_test <- data.frame(
    TITULO = c(
      "Machine Learning Algorithms for Image Classification",
      "Deep Neural Networks in Computer Vision Applications",
      "Optimization Techniques Using Genetic Algorithms",
      "Data Mining Approaches for Business Intelligence"
    ),
    RESUMEN = c(
      "This paper presents machine learning algorithms for image classification using convolutional neural networks",
      "Deep learning approaches for computer vision with applications in object detection and recognition",
      "Genetic algorithms and evolutionary computation for optimization problems in engineering",
      "Data mining techniques and analytics for extracting business insights from large datasets"
    ),
    AUTOR_PALABRAS_CLAVES = c(
      "machine learning, image classification, CNN, computer vision",
      "deep learning, neural networks, object detection, computer vision",
      "genetic algorithms, optimization, evolutionary computation",
      "data mining, business intelligence, analytics, big data"
    ),
    NOMBRE_AUTOR = c("Smith, J.", "Johnson, A.", "Brown, K.", "Davis, R."),
    ANO = c("2023", "2022", "2023", "2021"),
    SJR = c("1.2", "1.5", "0.8", "1.1"),
    CITADO_POR = c("25", "45", "12", "33"),
    LINK = c("link1", "link2", "link3", "link4"),
    stringsAsFactors = FALSE
  )

  # Consultas de prueba
  consultas_test <- c(
    "machine learning",
    "computer vision algorithms",
    "genetic optimization",
    "data analysis techniques"
  )

  for(consulta in consultas_test) {
    cat(paste("🔍 PRUEBA:", consulta, "\n"))
    cat("=" * 50, "\n")

    resultado <- proceso_nlp_chatbot_semantico(consulta, datos_test, usar_expansion_semantica = TRUE)

    if(resultado$success) {
      cat(paste("✅ Papers encontrados:", resultado$num_papers, "\n"))
      cat(paste("📝 Resumen:", substr(resultado$resumen_generado, 1, 150), "...\n"))
      cat(paste("🔧 Expansiones semánticas:", resultado$expansion_info$num_expansiones, "\n"))
      if(resultado$num_papers > 0) {
        cat(paste("⭐ Score máximo:", round(max(resultado$papers$score_normalizado), 3), "\n"))
      }
    } else {
      cat(paste("❌ Error:", resultado$message, "\n"))
    }
    cat("\n")
  }

  cat("=== FIN PRUEBAS MOTOR SEMÁNTICO ===\n")
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

#' Función de demostración de análisis temporal avanzado
#' @param query Query de ejemplo
#' @param data Dataset de prueba
demostrar_analisis_temporal_avanzado <- function(query = "machine learning", data = NULL) {

  cat("🔬 DEMOSTRACIÓN DE ANÁLISIS TEMPORAL AVANZADO - FASE 2\n")
  cat("=" * 60, "\n\n")

  if(is.null(data)) {
    # Crear dataset de prueba más extenso
    data <- data.frame(
      TITULO = c(
        "Machine Learning Algorithms for Image Classification in 2020",
        "Deep Learning Approaches in Computer Vision 2021",
        "Advanced Neural Networks for Pattern Recognition 2022",
        "AI Applications in Medical Diagnosis 2023",
        "Evolutionary Algorithms for Optimization Problems 2019",
        "Genetic Programming in Software Engineering 2020",
        "Data Mining Techniques for Business Intelligence 2021",
        "Big Data Analytics Using Machine Learning 2022",
        "Computer Vision for Autonomous Vehicles 2023",
        "Natural Language Processing in Healthcare 2020"
      ),
      ANO = c("2020", "2021", "2022", "2023", "2019", "2020", "2021", "2022", "2023", "2020"),
      RESUMEN = c(
        "Machine learning algorithms applied to image classification tasks",
        "Deep learning methodologies for computer vision applications",
        "Advanced neural network architectures for pattern recognition",
        "Artificial intelligence applications in medical diagnosis",
        "Evolutionary computation for complex optimization problems",
        "Genetic programming techniques in software engineering",
        "Data mining approaches for business intelligence",
        "Big data analytics using machine learning techniques",
        "Computer vision systems for autonomous vehicle navigation",
        "Natural language processing applications in healthcare"
      ),
      AUTOR_PALABRAS_CLAVES = c(
        "machine learning, image classification, algorithms",
        "deep learning, computer vision, neural networks",
        "neural networks, pattern recognition, AI",
        "artificial intelligence, medical diagnosis, healthcare",
        "evolutionary algorithms, optimization, genetic algorithms",
        "genetic programming, software engineering, automation",
        "data mining, business intelligence, analytics",
        "big data, machine learning, analytics",
        "computer vision, autonomous vehicles, AI",
        "natural language processing, healthcare, NLP"
      ),
      NOMBRE_AUTOR = c(
        "Smith, J.", "Johnson, A.", "Brown, K.", "Davis, R.", "Wilson, M.",
        "Johnson, A.", "Smith, J.", "Brown, K.", "Davis, R.", "Wilson, M."
      ),
      SJR = c("1.2", "1.5", "1.8", "2.1", "0.9", "1.1", "1.3", "1.6", "1.9", "1.4"),
      CITADO_POR = c("25", "45", "67", "89", "12", "23", "34", "56", "78", "43"),
      LINK = paste0("link", 1:10),
      stringsAsFactors = FALSE
    )
  }

  cat("📊 Dataset de prueba:\n")
  cat(paste("  - Papers:", nrow(data), "\n"))
  cat(paste("  - Autores únicos:", length(unique(data$NOMBRE_AUTOR)), "\n"))
  cat(paste("  - Rango temporal:", min(data$ANO), "-", max(data$ANO), "\n\n"))

  # Ejecutar análisis temporal completo
  cat("🚀 Ejecutando análisis temporal completo...\n\n")

  resultados <- ejecutar_analisis_temporal_completo(data, query)

  # Mostrar resultados
  if(!is.null(resultados$tendencias)) {
    cat("📈 TENDENCIAS DETECTADAS:\n")
    cat(paste("  -", resultados$tendencias$resumen_textual, "\n"))
  }

  if(!is.null(resultados$evolucion_temas)) {
    cat("\n🔄 EVOLUCIÓN DE TEMAS:\n")
    cat(paste("  -", resultados$evolucion_temas$resumen_evolucion, "\n"))
  }

  if(!is.null(resultados$citaciones_temporal)) {
    cat("\n📊 ANÁLISIS DE CITACIONES:\n")
    cat(paste("  -", resultados$citaciones_temporal$resumen_general, "\n"))
  }

  if(!is.null(resultados$autores_temporal)) {
    cat("\n👥 PRODUCTIVIDAD DE AUTORES:\n")
    cat(paste("  -", resultados$autores_temporal$resumen_general, "\n"))
  }

  if(!is.null(resultados$resumen_ejecutivo)) {
    cat("\n📋 RESUMEN EJECUTIVO:\n")
    cat(paste("  - Análisis ejecutados:", length(resultados$resumen_ejecutivo$analisis_ejecutados), "\n"))
    cat(paste("  - Principales hallazgos:", length(resultados$resumen_ejecutivo$principales_hallazgos), "\n"))
  }

  cat("\n✅ DEMOSTRACIÓN COMPLETADA\n")
  cat("=" * 60, "\n")

  return(resultados)
}