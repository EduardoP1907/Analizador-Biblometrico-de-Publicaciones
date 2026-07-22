#=======================================
# Motor PLN Chatbot SIMPLIFICADO V2 - Solo procesamiento local
#=======================================

library(stringi, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

#' Motor PLN principal - Solo procesamiento local de abstracts
proceso_nlp_chatbot_simple <- function(query, data, openai_key = NULL) {
  
  # Validaciones básicas
  if(is.null(query) || nchar(trimws(query)) == 0) {
    return(list(
      success = FALSE,
      message = "Por favor, ingrese una consulta de búsqueda válida."
    ))
  }
  
  if(is.null(data) || nrow(data) == 0) {
    return(list(
      success = FALSE,
      message = "No hay datos disponibles para procesar."
    ))
  }
  
  # Búsqueda robusta y simple
  papers_encontrados <- buscar_papers_robusto(query, data)
  
  if(nrow(papers_encontrados) == 0) {
    terminos <- extraer_terminos_basicos(query)
    return(list(
      success = TRUE,
      resumen_generado = paste("No se encontraron papers relacionados con '", query, "'. Intente con términos diferentes."),
      num_papers = 0,
      papers = data.frame(),
      terminos_busqueda = terminos
    ))
  }
  
  # Generar resumen NUEVO basado en los abstracts encontrados
  resumen_nuevo <- generar_resumen_desde_abstracts(query, papers_encontrados)
  
  # Preparar papers para mostrar
  papers_info <- preparar_papers_info(papers_encontrados)
  
  return(list(
    success = TRUE,
    resumen_generado = resumen_nuevo,
    num_papers = nrow(papers_encontrados),
    papers = papers_info,
    terminos_busqueda = extraer_terminos_basicos(query)
  ))
}

#' Búsqueda robusta con detección inteligente de consultas específicas
buscar_papers_robusto <- function(query, data) {
  
  tryCatch({
    # Limpiar query
    query_limpio <- tolower(trimws(as.character(query)))
    query_original <- trimws(as.character(query))
    
    cat(paste("Analizando consulta:", query_original, "\n"))
    
    # DETECTAR TIPO DE CONSULTA ESPECÍFICA
    tipo_consulta <- detectar_tipo_consulta(query_limpio)
    cat(paste("Tipo de consulta detectado:", tipo_consulta$tipo, "\n"))
    
    # Inicializar score
    data$score_busqueda <- 0
    
    # EJECUTAR BÚSQUEDA SEGÚN EL TIPO DETECTADO
    if(tipo_consulta$tipo == "AUTOR_ESPECIFICO") {
      # Búsqueda estricta de autor
      papers_encontrados <- busqueda_autor_estricta(tipo_consulta$valores, data)
      
    } else if(tipo_consulta$tipo == "ANO_ESPECIFICO") {
      # Búsqueda estricta de año
      papers_encontrados <- busqueda_ano_estricta(tipo_consulta$valores, data)
      
    } else if(tipo_consulta$tipo == "TITULO_ESPECIFICO") {
      # Búsqueda estricta en títulos
      papers_encontrados <- busqueda_titulo_estricta(tipo_consulta$valores, data)
      
    } else {
      # Búsqueda general (como antes)
      papers_encontrados <- busqueda_general(query_limpio, data)
    }
    
    cat(paste("Papers encontrados:", nrow(papers_encontrados), "\n"))
    return(papers_encontrados)
    
  }, error = function(e) {
    cat("Error en búsqueda:", e$message, "\n")
    return(data.frame())
  })
}

#' Detectar el tipo de consulta específica
detectar_tipo_consulta <- function(query) {
  
  # DETECTAR BÚSQUEDA DE AUTOR
  if(grepl("autor.*sea|autor.*es|papers.*de|trabajos.*de|publicaciones.*de|autor.*llamado|autor.*nombre", query)) {
    # Extraer nombre del autor de la consulta
    nombres_detectados <- extraer_nombres_de_consulta(query)
    if(length(nombres_detectados) > 0) {
      return(list(tipo = "AUTOR_ESPECIFICO", valores = nombres_detectados))
    }
  }
  
  # DETECTAR BÚSQUEDA POR AÑO ESPECÍFICO
  if(grepl("año|del.*20|en.*20|durante.*20|papers.*20|estudios.*20", query)) {
    anos_detectados <- as.numeric(unlist(regmatches(query, gregexpr("\\b(19|20)\\d{2}\\b", query))))
    if(length(anos_detectados) > 0) {
      return(list(tipo = "ANO_ESPECIFICO", valores = anos_detectados))
    }
  }
  
  # DETECTAR BÚSQUEDA POR TÍTULO ESPECÍFICO
  if(grepl("titulo.*sea|titulo.*es|paper.*llamado|articulo.*llamado|trabajo.*llamado", query)) {
    return(list(tipo = "TITULO_ESPECIFICO", valores = query))
  }
  
  # Búsqueda general por defecto
  return(list(tipo = "GENERAL", valores = query))
}

#' Extraer nombres de autor de la consulta
extraer_nombres_de_consulta <- function(query) {
  
  # Patrones para extraer nombres después de palabras clave
  patrones <- c(
    "autor.*sea\\s+([a-záéíóúñ\\s]+)",
    "autor.*es\\s+([a-záéíóúñ\\s]+)",
    "papers.*de\\s+([a-záéíóúñ\\s]+)",
    "trabajos.*de\\s+([a-záéíóúñ\\s]+)",
    "publicaciones.*de\\s+([a-záéíóúñ\\s]+)",
    "autor.*llamado\\s+([a-záéíóúñ\\s]+)",
    "autor.*nombre\\s+([a-záéíóúñ\\s]+)"
  )
  
  nombres_encontrados <- c()
  
  for(patron in patrones) {
    matches <- regmatches(query, regexpr(patron, query, ignore.case = TRUE))
    if(length(matches) > 0) {
      # Extraer solo la parte del nombre
      nombre_extraido <- gsub(patron, "\\1", matches, ignore.case = TRUE)
      nombre_limpio <- trimws(nombre_extraido)
      if(nchar(nombre_limpio) > 2) {
        nombres_encontrados <- c(nombres_encontrados, nombre_limpio)
      }
    }
  }
  
  return(unique(nombres_encontrados))
}

#' Búsqueda estricta de autor
busqueda_autor_estricta <- function(nombres_autor, data) {
  
  cat("Ejecutando búsqueda estricta de autor:", paste(nombres_autor, collapse = ", "), "\n")
  
  if(!"NOMBRE_AUTOR" %in% colnames(data)) {
    cat("Columna NOMBRE_AUTOR no existe\n")
    return(data.frame())
  }
  
  # Limpiar columna de autores
  autores_data <- data$NOMBRE_AUTOR
  autores_data[is.na(autores_data)] <- ""
  autores_data <- tolower(as.character(autores_data))
  
  indices_encontrados <- c()
  
  for(nombre_buscado in nombres_autor) {
    nombre_limpio <- tolower(trimws(nombre_buscado))
    
    # Dividir el nombre en palabras
    palabras_nombre <- unlist(strsplit(nombre_limpio, "\\s+"))
    palabras_nombre <- palabras_nombre[nchar(palabras_nombre) >= 2]
    
    if(length(palabras_nombre) == 0) next
    
    # Buscar autores que contengan TODAS las palabras del nombre
    for(i in seq_len(nrow(data))) {
      autor_actual <- autores_data[i]
      
      if(nchar(autor_actual) > 0) {
        # Verificar que TODAS las palabras del nombre estén presentes
        todas_presentes <- all(sapply(palabras_nombre, function(palabra) {
          grepl(palabra, autor_actual, fixed = TRUE)
        }))
        
        if(todas_presentes) {
          indices_encontrados <- c(indices_encontrados, i)
          cat(paste("Encontrado:", data$NOMBRE_AUTOR[i], "\n"))
        }
      }
    }
  }
  
  if(length(indices_encontrados) == 0) {
    cat("No se encontraron autores exactos\n")
    return(data.frame())
  }
  
  return(data[unique(indices_encontrados), , drop = FALSE])
}

#' Búsqueda estricta de año
busqueda_ano_estricta <- function(anos, data) {
  
  cat("Ejecutando búsqueda estricta de año:", paste(anos, collapse = ", "), "\n")
  
  if(!"ANO" %in% colnames(data)) {
    cat("Columna ANO no existe\n")
    return(data.frame())
  }
  
  indices_encontrados <- c()
  
  for(ano in anos) {
    matches <- which(data$ANO == ano & !is.na(data$ANO))
    indices_encontrados <- c(indices_encontrados, matches)
    cat(paste("Año", ano, ":", length(matches), "papers\n"))
  }
  
  if(length(indices_encontrados) == 0) {
    return(data.frame())
  }
  
  return(data[unique(indices_encontrados), , drop = FALSE])
}

#' Búsqueda estricta en títulos
busqueda_titulo_estricta <- function(termino_titulo, data) {
  
  cat("Ejecutando búsqueda estricta en títulos\n")
  
  if(!"TITULO" %in% colnames(data)) {
    return(data.frame())
  }
  
  titulos_data <- data$TITULO
  titulos_data[is.na(titulos_data)] <- ""
  titulos_data <- tolower(as.character(titulos_data))
  
  # Buscar coincidencias en títulos
  matches <- grepl(termino_titulo, titulos_data, fixed = TRUE)
  
  if(any(matches)) {
    return(data[matches, , drop = FALSE])
  }
  
  return(data.frame())
}

#' Búsqueda general (método anterior mejorado)
busqueda_general <- function(query_limpio, data) {
  
  cat("Ejecutando búsqueda general\n")
  
  # Inicializar score
  data$score_busqueda <- 0
  
  # Columnas de búsqueda con pesos ajustados
  columnas_pesos <- list(
    "TITULO" = 5,
    "NOMBRE_AUTOR" = 4,
    "AUTOR_PALABRAS_CLAVES" = 3,
    "RESUMEN" = 2,
    "FUENTE" = 1
  )
  
  # 1. Búsqueda de query completa
  for(columna in names(columnas_pesos)) {
    if(columna %in% colnames(data)) {
      
      columna_datos <- data[[columna]]
      columna_datos[is.na(columna_datos)] <- ""
      columna_datos <- tolower(as.character(columna_datos))
      
      matches <- grepl(query_limpio, columna_datos, fixed = TRUE)
      peso <- columnas_pesos[[columna]]
      
      data$score_busqueda[matches] <- data$score_busqueda[matches] + peso
    }
  }
  
  # 2. Búsqueda por palabras individuales (peso menor)
  palabras <- unlist(strsplit(query_limpio, "\\s+"))
  palabras <- palabras[nchar(palabras) >= 3]
  
  for(palabra in palabras) {
    for(columna in names(columnas_pesos)) {
      if(columna %in% colnames(data)) {
        
        columna_datos <- data[[columna]]
        columna_datos[is.na(columna_datos)] <- ""
        columna_datos <- tolower(as.character(columna_datos))
        
        matches <- grepl(palabra, columna_datos, fixed = TRUE)
        peso_reducido <- columnas_pesos[[columna]] * 0.3  # Peso menor para palabras individuales
        
        data$score_busqueda[matches] <- data$score_busqueda[matches] + peso_reducido
      }
    }
  }
  
  # Filtrar y ordenar
  indices_con_score <- seq_len(nrow(data))[data$score_busqueda > 0]
  
  if(length(indices_con_score) == 0) {
    return(data.frame())
  }
  
  papers_relevantes <- data[indices_con_score, , drop = FALSE]
  papers_relevantes <- papers_relevantes[order(papers_relevantes$score_busqueda, decreasing = TRUE), , drop = FALSE]
  papers_relevantes$score_busqueda <- NULL
  
  return(papers_relevantes)
}

#' Preparar información de papers para mostrar
preparar_papers_info <- function(papers) {
  
  # Asegurar que existan las columnas necesarias
  columnas_necesarias <- c("TITULO", "NOMBRE_AUTOR", "ANO", "LINK", "SJR", "CITADO_POR")
  
  for(col in columnas_necesarias) {
    if(!col %in% colnames(papers)) {
      papers[[col]] <- NA
    }
  }
  
  # Seleccionar y ordenar
  papers_info <- papers[, columnas_necesarias, drop = FALSE]
  
  # Convertir SJR y CITADO_POR a numérico de forma segura
  papers_info$SJR <- suppressWarnings(as.numeric(as.character(papers_info$SJR)))
  papers_info$SJR[is.na(papers_info$SJR)] <- 0
  
  papers_info$CITADO_POR <- suppressWarnings(as.numeric(as.character(papers_info$CITADO_POR)))
  papers_info$CITADO_POR[is.na(papers_info$CITADO_POR)] <- 0
  
  # Ordenar por calidad (SJR y citas)
  papers_info <- papers_info[order(papers_info$SJR + papers_info$CITADO_POR, decreasing = TRUE), , drop = FALSE]
  
  return(papers_info)
}

#' Extrae términos básicos de forma segura
extraer_terminos_basicos <- function(texto) {
  
  if(is.null(texto) || nchar(trimws(texto)) == 0) {
    return(character(0))
  }
  
  # Limpiar texto
  texto_limpio <- tolower(trimws(as.character(texto)))
  
  # Dividir en palabras
  palabras <- unlist(strsplit(texto_limpio, "\\s+"))
  
  # Filtrar palabras cortas y comunes
  stopwords <- c("el", "la", "los", "las", "de", "en", "con", "por", "para", "y", "o", "que", "como", "sobre", 
                 "the", "a", "an", "and", "or", "in", "on", "at", "to", "for", "of", "with", "by")
  
  palabras_filtradas <- palabras[
    nchar(palabras) >= 3 & 
    !palabras %in% stopwords &
    !grepl("^\\d+$", palabras)
  ]
  
  return(unique(palabras_filtradas))
}

#' Generar resumen COMPLETAMENTE NUEVO basado en abstracts de papers encontrados
generar_resumen_desde_abstracts <- function(query, papers) {
  
  num_papers <- nrow(papers)
  
  if(num_papers == 0) {
    return(paste("No se encontraron papers relacionados con '", query, "'."))
  }
  
  cat(paste("Generando resumen desde", num_papers, "abstracts...\n"))
  
  # 1. EXTRAER Y LIMPIAR TODOS LOS ABSTRACTS
  abstracts_raw <- papers$RESUMEN
  if(is.null(abstracts_raw)) {
    return(paste("Se encontraron", num_papers, "papers sobre '", query, "', pero no hay abstracts disponibles para generar un resumen."))
  }
  
  # Limpiar abstracts (remover NA, vacíos, etc.)
  abstracts_validos <- abstracts_raw[!is.na(abstracts_raw) & nchar(trimws(abstracts_raw)) > 20]
  
  if(length(abstracts_validos) == 0) {
    return(paste("Se encontraron", num_papers, "papers sobre '", query, "', pero los abstracts no están disponibles para análisis."))
  }
  
  # 2. COMBINAR TODOS LOS ABSTRACTS EN UN TEXTO UNIFICADO
  texto_completo <- paste(abstracts_validos, collapse = " ")
  texto_limpio <- limpiar_y_normalizar_texto(texto_completo)
  
  # 3. EXTRAER CONCEPTOS CLAVE DE LOS ABSTRACTS
  conceptos_principales <- extraer_conceptos_desde_abstracts(texto_limpio, query)
  
  # 4. IDENTIFICAR METODOLOGÍAS MENCIONADAS
  metodologias <- extraer_metodologias_desde_abstracts(texto_limpio)
  
  # 5. IDENTIFICAR APLICACIONES Y DOMINIOS
  aplicaciones <- extraer_aplicaciones_desde_abstracts(texto_limpio)
  
  # 6. GENERAR RESUMEN COMPLETAMENTE NUEVO
  resumen_generado <- construir_resumen_personalizado(query, num_papers, conceptos_principales, metodologias, aplicaciones)
  
  cat("Resumen generado exitosamente\n")
  return(resumen_generado)
}

#' Limpiar y normalizar el texto de abstracts
limpiar_y_normalizar_texto <- function(texto) {
  # Convertir a minúsculas
  texto <- tolower(texto)
  
  # Remover caracteres especiales pero mantener puntuación importante
  texto <- gsub("[^a-z0-9\\s\\.\\,\\;\\:]", " ", texto)
  
  # Normalizar espacios
  texto <- gsub("\\s+", " ", texto)
  
  # Remover espacios al inicio/final
  texto <- trimws(texto)
  
  return(texto)
}

#' Extraer conceptos principales desde los abstracts
extraer_conceptos_desde_abstracts <- function(texto, query) {
  
  # Dividir en palabras
  palabras <- unlist(strsplit(texto, "\\s+"))
  
  # Contar frecuencias
  freq_table <- table(palabras)
  
  # Filtrar palabras relevantes (frecuencia >= 2, longitud >= 4)
  palabras_relevantes <- names(freq_table[freq_table >= 2 & nchar(names(freq_table)) >= 4])
  
  # Remover stopwords comunes
  stopwords <- c("this", "that", "with", "from", "they", "were", "been", "have", "their", "such", 
                 "using", "used", "also", "more", "only", "these", "than", "most", "some", "other", 
                 "which", "would", "could", "about", "between", "through", "during", "method", 
                 "methods", "results", "study", "research", "analysis", "approach", "data", "based", 
                 "paper", "work", "system", "different", "important", "significant", "show", "shows", 
                 "shown", "found", "present", "presented", "provide", "provides")
  
  palabras_filtradas <- palabras_relevantes[!palabras_relevantes %in% stopwords]
  
  # Ordenar por frecuencia y tomar las más relevantes
  freq_filtrada <- freq_table[palabras_filtradas]
  conceptos_ordenados <- names(sort(freq_filtrada, decreasing = TRUE))
  
  # Devolver top conceptos
  return(head(conceptos_ordenados, 8))
}

#' Extraer metodologías desde abstracts
extraer_metodologias_desde_abstracts <- function(texto) {
  
  metodologias_detectadas <- c()
  
  # Patrones de metodologías comunes
  if(grepl("machine learning|ml |artificial intelligence|neural network", texto)) {
    metodologias_detectadas <- c(metodologias_detectadas, "machine learning y redes neuronales")
  }
  if(grepl("algorithm|optimization|genetic|evolutionary", texto)) {
    metodologias_detectadas <- c(metodologias_detectadas, "algoritmos de optimización")
  }
  if(grepl("statistical|statistics|regression|classification", texto)) {
    metodologias_detectadas <- c(metodologias_detectadas, "métodos estadísticos y clasificación")
  }
  if(grepl("simulation|modeling|computational|numerical", texto)) {
    metodologias_detectadas <- c(metodologias_detectadas, "simulación y modelado computacional")
  }
  if(grepl("experimental|empirical|case study|survey", texto)) {
    metodologias_detectadas <- c(metodologias_detectadas, "estudios experimentales y empíricos")
  }
  if(grepl("deep learning|convolutional|lstm|transformer", texto)) {
    metodologias_detectadas <- c(metodologias_detectadas, "deep learning y arquitecturas avanzadas")
  }
  
  return(unique(metodologias_detectadas))
}

#' Extraer aplicaciones y dominios desde abstracts
extraer_aplicaciones_desde_abstracts <- function(texto) {
  
  aplicaciones_detectadas <- c()
  
  # Patrones de aplicaciones por dominio
  if(grepl("medical|health|healthcare|clinical|disease|patient", texto)) {
    aplicaciones_detectadas <- c(aplicaciones_detectadas, "medicina y salud")
  }
  if(grepl("image|vision|computer vision|processing|recognition", texto)) {
    aplicaciones_detectadas <- c(aplicaciones_detectadas, "procesamiento de imágenes y visión computacional")
  }
  if(grepl("software|engineering|programming|development|system", texto)) {
    aplicaciones_detectadas <- c(aplicaciones_detectadas, "ingeniería de software y sistemas")
  }
  if(grepl("finance|financial|economic|business|market", texto)) {
    aplicaciones_detectadas <- c(aplicaciones_detectadas, "finanzas y economía")
  }
  if(grepl("education|educational|learning|teaching|academic", texto)) {
    aplicaciones_detectadas <- c(aplicaciones_detectadas, "educación y aprendizaje")
  }
  if(grepl("security|cyber|privacy|attack|protection", texto)) {
    aplicaciones_detectadas <- c(aplicaciones_detectadas, "seguridad y ciberseguridad")
  }
  if(grepl("network|internet|web|communication|protocol", texto)) {
    aplicaciones_detectadas <- c(aplicaciones_detectadas, "redes y comunicaciones")
  }
  if(grepl("database|data mining|big data|information", texto)) {
    aplicaciones_detectadas <- c(aplicaciones_detectadas, "bases de datos y minería de datos")
  }
  
  return(unique(aplicaciones_detectadas))
}

#' Construir resumen personalizado y completamente nuevo
construir_resumen_personalizado <- function(query, num_papers, conceptos, metodologias, aplicaciones) {
  
  # Introducción
  intro <- paste0("Se encontraron ", num_papers, " papers relacionados con '", query, "'")
  
  # Sección de conceptos principales - Limitada y bien estructurada
  seccion_conceptos <- ""
  if(length(conceptos) > 0) {
    conceptos_principales <- head(conceptos, 4)  # Reducir a 4 conceptos
    seccion_conceptos <- paste0(", abarcando principalmente conceptos como ", 
                               paste(conceptos_principales, collapse = ", "))
  }
  
  # Sección de metodologías - Mejorada y más concisa
  seccion_metodologias <- ""
  if(length(metodologias) > 0) {
    if(length(metodologias) == 1) {
      seccion_metodologias <- paste0(". Las investigaciones emplean principalmente ", metodologias[1])
    } else if(length(metodologias) == 2) {
      seccion_metodologias <- paste0(". Las metodologías utilizadas incluyen ", 
                                    paste(metodologias, collapse = " y "))
    } else {
      # Para múltiples metodologías, usar formato más compacto
      metodologias_principales <- head(metodologias, 3)
      seccion_metodologias <- paste0(". Las principales metodologías incluyen ", 
                                    paste(metodologias_principales, collapse = ", "),
                                    if(length(metodologias) > 3) " entre otras" else "")
    }
  }
  
  # Sección de aplicaciones - Mejorada y más concisa
  seccion_aplicaciones <- ""
  if(length(aplicaciones) > 0) {
    if(length(aplicaciones) == 1) {
      seccion_aplicaciones <- paste0(". El campo de aplicación principal es ", aplicaciones[1])
    } else if(length(aplicaciones) == 2) {
      seccion_aplicaciones <- paste0(". Los principales campos de aplicación son ", 
                                    paste(aplicaciones, collapse = " y "))
    } else {
      # Para múltiples aplicaciones, formato más compacto
      aplicaciones_principales <- head(aplicaciones, 3)
      seccion_aplicaciones <- paste0(". Los estudios se aplican principalmente en ", 
                                    paste(aplicaciones_principales, collapse = ", "),
                                    if(length(aplicaciones) > 3) " y otras áreas relacionadas" else "")
    }
  }
  
  # Conclusión más específica según el número de papers
  conclusion <- ""
  if(num_papers < 10) {
    conclusion <- ". Este conjunto de trabajos ofrece una perspectiva específica del área de investigación"
  } else if(num_papers < 100) {
    conclusion <- ". Esta colección representa un cuerpo sólido de investigación en el área consultada"
  } else if(num_papers < 1000) {
    conclusion <- ". Este amplio conjunto de trabajos refleja la diversidad y madurez del campo de investigación"
  } else {
    conclusion <- ". Esta extensa base de estudios demuestra la riqueza y amplitud de la investigación en el área"
  }
  
  # Ensamblar resumen completo
  resumen_final <- paste0(intro, seccion_conceptos, seccion_metodologias, seccion_aplicaciones, conclusion, ".")
  
  # AUMENTAR el límite y mejorar el manejo de longitud
  if(nchar(resumen_final) > 800) {
    # Si es muy largo, reescribir de forma más compacta
    resumen_compacto <- construir_resumen_compacto(query, num_papers, conceptos, metodologias, aplicaciones)
    return(resumen_compacto)
  }
  
  return(resumen_final)
}

#' Construir resumen compacto para casos con mucha información
construir_resumen_compacto <- function(query, num_papers, conceptos, metodologias, aplicaciones) {
  
  # Versión compacta para casos con mucha información
  intro <- paste0("Se encontraron ", num_papers, " papers sobre '", query, "'")
  
  # Solo los elementos más relevantes
  elementos <- c()
  
  if(length(conceptos) > 0) {
    elementos <- c(elementos, paste("conceptos clave:", paste(head(conceptos, 3), collapse = ", ")))
  }
  
  if(length(metodologias) > 0) {
    elementos <- c(elementos, paste("metodologías:", paste(head(metodologias, 2), collapse = " y ")))
  }
  
  if(length(aplicaciones) > 0) {
    elementos <- c(elementos, paste("aplicaciones:", paste(head(aplicaciones, 2), collapse = " y ")))
  }
  
  if(length(elementos) > 0) {
    resumen_final <- paste0(intro, " abarcando ", paste(elementos, collapse = "; "), 
                           ". Conjunto integral de investigación en el área.")
  } else {
    resumen_final <- paste0(intro, " que cubren diversos aspectos de investigación en el área consultada.")
  }
  
  return(resumen_final)
}