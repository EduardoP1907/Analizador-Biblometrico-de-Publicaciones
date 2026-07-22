#=======================================
# Motor PLN Chatbot SIMPLIFICADO (solo paquetes básicos)
#=======================================

# Solo bibliotecas esenciales
library(stringi, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# Bibliotecas opcionales para funcionalidades avanzadas
usar_httr <- requireNamespace("httr", quietly = TRUE)
usar_jsonlite <- requireNamespace("jsonlite", quietly = TRUE)

if(usar_httr) library(httr, warn.conflicts = FALSE)
if(usar_jsonlite) library(jsonlite, warn.conflicts = FALSE)

#' Motor PLN simplificado para procesamiento de consultas bibliométricas
proceso_nlp_chatbot_simple <- function(query, data, openai_key = NULL) {
  
  # Validaciones iniciales
  if(is.null(query) || nchar(trimws(query)) == 0) {
    return(list(
      success = FALSE,
      message = "Por favor, ingrese una consulta de búsqueda válida."
    ))
  }
  
  if(nrow(data) == 0) {
    return(list(
      success = FALSE,
      message = "No hay datos disponibles para procesar."
    ))
  }
  
  # Paso 1: Búsqueda simplificada de papers relevantes
  papers_encontrados <- buscar_papers_simple(query, data)
  
  if(nrow(papers_encontrados) == 0) {
    return(list(
      success = TRUE,
      resumen_generado = paste("No se encontraron papers relacionados con '", query, "'. Intente con términos diferentes o más específicos."),
      num_papers = 0,
      papers = data.frame(),
      terminos_busqueda = extraer_terminos_robustos(query)
    ))
  }
  
  # Paso 2: Generar resumen simple basado en papers encontrados
  resumen_nuevo <- generar_resumen_simple(query, papers_encontrados, openai_key)
  
  # Preparar información de papers para mostrar
  papers_info <- papers_encontrados %>%
    select(TITULO, NOMBRE_AUTOR, ANO, LINK, SJR, CITADO_POR) %>%
    arrange(desc(as.numeric(ifelse(is.na(SJR) | SJR == "", 0, SJR))), 
            desc(as.numeric(ifelse(is.na(CITADO_POR) | CITADO_POR == "", 0, CITADO_POR))))
  
  return(list(
    success = TRUE,
    resumen_generado = resumen_nuevo,
    num_papers = nrow(papers_encontrados),
    papers = papers_info,
    terminos_busqueda = extraer_terminos_robustos(query)
  ))
}

#' Búsqueda ULTRA ROBUSTA de papers
#' Puede buscar por: autor, título, año, palabras clave, resumen, o combinaciones
buscar_papers_simple <- function(query, data) {
  
  # Validar entrada
  if(is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  
  # Limpiar consulta original
  query_original <- trimws(tolower(as.character(query)))
  if(nchar(query_original) == 0) {
    return(data.frame())
  }
  
  # Asegurar que existan todas las columnas posibles
  columnas_necesarias <- c("TITULO", "RESUMEN", "AUTOR_PALABRAS_CLAVES", "NOMBRE_AUTOR", "ANO", "FUENTE", "DOI", "AFILIACIÓN_AUTOR")
  for(col in columnas_necesarias) {
    if(!col %in% colnames(data)) {
      data[[col]] <- ""
    }
  }
  
  # Inicializar scores
  data$score <- 0
  data$match_info <- ""  # Para debug y información
  
  # ESTRATEGIA 1: Búsqueda EXACTA de la consulta completa
  cat("Buscando coincidencia exacta...\n")
  busqueda_exacta_multiple(query_original, data)
  
  # ESTRATEGIA 2: Búsqueda por TÉRMINOS INDIVIDUALES  
  cat("Buscando por términos individuales...\n")
  terminos <- extraer_terminos_robustos(query_original)
  busqueda_por_terminos_multiple(terminos, data)
  
  # ESTRATEGIA 3: Búsqueda ESPECÍFICA por tipo (año, autor, etc.)
  cat("Detectando tipo de búsqueda específica...\n")
  busqueda_especifica(query_original, data)
  
  # ESTRATEGIA 4: Búsqueda APROXIMADA/Fuzzy para capturar más resultados
  cat("Búsqueda aproximada...\n")
  busqueda_aproximada(query_original, terminos, data)
  
  # Filtrar y ordenar resultados
  indices_relevantes <- which(data$score > 0)
  
  if(length(indices_relevantes) == 0) {
    cat("No se encontraron coincidencias\n")
    return(data.frame())
  }
  
  papers_relevantes <- data[indices_relevantes, , drop = FALSE]
  
  # Ordenar por score (mayor a menor)
  papers_relevantes <- papers_relevantes[order(papers_relevantes$score, decreasing = TRUE), , drop = FALSE]
  
  # Limitar resultados
  if(nrow(papers_relevantes) > 20) {
    papers_relevantes <- papers_relevantes[1:20, , drop = FALSE]
  }
  
  # Limpiar columnas temporales
  papers_relevantes$match_info <- NULL
  
  cat(paste("Encontrados", nrow(papers_relevantes), "papers relevantes\n"))
  return(papers_relevantes)
}

#' Búsqueda exacta en múltiples campos
busqueda_exacta_multiple <- function(query, data) {
  # Lista de campos donde buscar (en orden de importancia)
  campos_busqueda <- list(
    list(campo = "TITULO", peso = 5),
    list(campo = "NOMBRE_AUTOR", peso = 4),
    list(campo = "AUTOR_PALABRAS_CLAVES", peso = 3),
    list(campo = "RESUMEN", peso = 2),
    list(campo = "FUENTE", peso = 2),
    list(campo = "AFILIACIÓN_AUTOR", peso = 1)
  )
  
  for(campo_info in campos_busqueda) {
    campo <- campo_info$campo
    peso <- campo_info$peso
    
    if(campo %in% colnames(data)) {
      # Limpiar campo para búsqueda
      campo_limpio <- vapply(data[[campo]], function(x) {
        if(is.na(x) || x == "") return("")
        limpiar_texto_simple(as.character(x))
      }, character(1))
      
      # Buscar coincidencia exacta
      matches <- grepl(query, campo_limpio, fixed = TRUE, ignore.case = TRUE)
      
      if(any(matches)) {
        data$score[matches] <<- data$score[matches] + peso
        data$match_info[matches] <<- paste(data$match_info[matches], paste("Exacta en", campo), sep = "; ")
      }
    }
  }
}

#' Búsqueda por términos individuales en múltiples campos
busqueda_por_terminos_multiple <- function(terminos, data) {
  if(length(terminos) == 0) return()
  
  campos_pesos <- list(
    "TITULO" = 3,
    "NOMBRE_AUTOR" = 4,  # Peso alto para autores
    "AUTOR_PALABRAS_CLAVES" = 2.5,
    "RESUMEN" = 1.5,
    "FUENTE" = 1,
    "AFILIACIÓN_AUTOR" = 1
  )
  
  for(termino in terminos) {
    if(nchar(termino) < 2) next
    
    for(campo in names(campos_pesos)) {
      if(campo %in% colnames(data)) {
        peso <- campos_pesos[[campo]]
        
        # Limpiar campo
        campo_limpio <- vapply(data[[campo]], function(x) {
          if(is.na(x) || x == "") return("")
          limpiar_texto_simple(as.character(x))
        }, character(1))
        
        # Buscar término
        matches <- grepl(termino, campo_limpio, fixed = TRUE, ignore.case = TRUE)
        
        if(any(matches)) {
          data$score[matches] <<- data$score[matches] + peso
          data$match_info[matches] <<- paste(data$match_info[matches], paste(termino, "en", campo), sep = "; ")
        }
      }
    }
  }
}

#' Búsqueda específica por tipo detectado automáticamente
busqueda_especifica <- function(query, data) {
  
  # DETECTAR AÑO (4 dígitos entre 1990-2030)
  if(grepl("\\b(19|20)\\d{2}\\b", query)) {
    anos <- as.numeric(unlist(regmatches(query, gregexpr("\\b(19|20)\\d{2}\\b", query))))
    
    for(ano in anos) {
      if("ANO" %in% colnames(data)) {
        matches <- data$ANO == ano
        matches[is.na(matches)] <- FALSE
        
        if(any(matches)) {
          data$score[matches] <<- data$score[matches] + 6  # Peso alto para años exactos
          data$match_info[matches] <<- paste(data$match_info[matches], paste("Año", ano), sep = "; ")
        }
      }
    }
  }
  
  # DETECTAR DOI (formato típico)
  if(grepl("10\\.", query)) {
    if("DOI" %in% colnames(data)) {
      matches <- grepl(query, data$DOI, fixed = TRUE, ignore.case = TRUE)
      
      if(any(matches)) {
        data$score[matches] <<- data$score[matches] + 10  # Peso máximo para DOI exacto
        data$match_info[matches] <<- paste(data$match_info[matches], "DOI exacto", sep = "; ")
      }
    }
  }
  
  # DETECTAR NOMBRES (palabras en mayúscula o patrones de apellido)
  palabras <- unlist(strsplit(query, "\\s+"))
  nombres_posibles <- palabras[grepl("^[A-Z][a-z]+", palabras) | nchar(palabras) > 5]
  
  if(length(nombres_posibles) > 0 && "NOMBRE_AUTOR" %in% colnames(data)) {
    for(nombre in nombres_posibles) {
      matches <- grepl(nombre, data$NOMBRE_AUTOR, ignore.case = TRUE)
      
      if(any(matches)) {
        data$score[matches] <<= data$score[matches] + 5
        data$match_info[matches] <<- paste(data$match_info[matches], paste("Autor:", nombre), sep = "; ")
      }
    }
  }
  
  # DETECTAR UNIVERSIDADES/INSTITUCIONES
  instituciones <- c("universidad", "university", "instituto", "institute", "college", "school", "usach", "chile", "santiago")
  for(inst in instituciones) {
    if(grepl(inst, query, ignore.case = TRUE)) {
      if("AFILIACIÓN_AUTOR" %in% colnames(data) || "UNIVERSIDAD" %in% colnames(data)) {
        
        campos_institucion <- intersect(c("AFILIACIÓN_AUTOR", "UNIVERSIDAD"), colnames(data))
        for(campo in campos_institucion) {
          matches <- grepl(inst, data[[campo]], ignore.case = TRUE)
          
          if(any(matches)) {
            data$score[matches] <<- data$score[matches] + 3
            data$match_info[matches] <<- paste(data$match_info[matches], paste("Institución:", inst), sep = "; ")
          }
        }
      }
    }
  }
}

#' Búsqueda aproximada/fuzzy para mayor cobertura
busqueda_aproximada <- function(query, terminos, data) {
  
  # Solo aplicar para términos largos (>= 5 caracteres)
  terminos_largos <- terminos[nchar(terminos) >= 5]
  
  if(length(terminos_largos) == 0) return()
  
  campos_aproximados <- c("TITULO", "RESUMEN", "AUTOR_PALABRAS_CLAVES")
  
  for(termino in terminos_largos) {
    for(campo in campos_aproximados) {
      if(campo %in% colnames(data)) {
        
        # Crear variaciones del término para búsqueda aproximada
        variaciones <- c(
          substr(termino, 1, nchar(termino)-1),  # Sin última letra
          paste0(termino, "s"),                   # Plural
          gsub("s$", "", termino)                 # Sin 's' final
        )
        
        for(variacion in variaciones) {
          if(nchar(variacion) >= 4) {
            matches <- grepl(variacion, data[[campo]], ignore.case = TRUE, fixed = TRUE)
            
            if(any(matches)) {
              data$score[matches] <<- data$score[matches] + 0.5  # Peso menor para aproximada
              data$match_info[matches] <<- paste(data$match_info[matches], paste("Aprox:", variacion, "en", campo), sep = "; ")
            }
          }
        }
      }
    }
  }
}

#' Extracción robusta de términos de búsqueda
extraer_terminos_robustos <- function(texto) {
  if(is.null(texto) || nchar(texto) == 0) return(character(0))
  
  # Stopwords expandidas (español e inglés)
  stopwords <- c(
    # Español
    "el", "la", "los", "las", "un", "una", "de", "del", "en", "con", "por", "para", "y", "o", "que", "como", "sobre", "entre", "sin", "hasta", "desde", "hacia", "bajo", "tras", "durante", "mediante", "según", "contra", "dentro", "fuera", "encima", "debajo",
    # Inglés  
    "the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for", "of", "with", "by", "from", "about", "into", "through", "during", "before", "after", "above", "below", "between", "among", "within", "without", "under", "over",
    # Palabras académicas comunes
    "paper", "study", "research", "analysis", "method", "approach", "result", "conclusion", "abstract", "article", "journal"
  )
  
  # Limpiar y tokenizar
  texto_limpio <- limpiar_texto_simple(texto)
  tokens <- unlist(strsplit(texto_limpio, "\\s+"))
  
  # Filtrar tokens
  tokens_filtrados <- tokens[
    nchar(tokens) >= 2 &                    # Al menos 2 caracteres
    !tokens %in% stopwords &               # No stopwords
    !grepl("^\\d+$", tokens) &             # No solo números (excepto años que se manejan aparte)
    !grepl("^[^a-z0-9]+$", tokens)         # No solo símbolos
  ]
  
  return(unique(tokens_filtrados))
}

#' Extrae términos clave de forma simple
extraer_terminos_simple <- function(texto) {
  if(is.null(texto) || nchar(texto) == 0) return(character(0))
  
  # Stopwords básicas
  stopwords <- c("el", "la", "los", "las", "un", "una", "de", "del", "en", "con", "por", "para", "y", "o", "que", "como", "sobre", "entre", "the", "a", "an", "and", "or", "in", "on", "at", "to", "for", "of", "with", "by")
  
  # Tokenizar
  texto_limpio <- limpiar_texto_simple(texto)
  tokens <- unlist(strsplit(texto_limpio, "\\s+"))
  
  # Filtrar tokens
  tokens <- tokens[nchar(tokens) >= 3]  # Mínimo 3 caracteres
  tokens <- tokens[!tokens %in% stopwords]  # Remover stopwords
  tokens <- tokens[!grepl("^\\d+$", tokens)]  # Remover números puros
  
  return(unique(tokens))
}

#' Limpia texto de forma simple y robusta
limpiar_texto_simple <- function(texto) {
  # Manejo seguro de valores NULL/NA/vacío
  if(is.null(texto) || is.na(texto) || length(texto) == 0) return("")
  
  # Convertir a character de forma segura
  texto <- tryCatch({
    as.character(texto[1])  # Tomar solo el primer elemento si es vector
  }, error = function(e) {
    return("")
  })
  
  # Verificar que no esté vacío después de conversión
  if(is.na(texto) || nchar(texto) == 0) return("")
  
  # Convertir a minúsculas de forma segura
  texto <- tryCatch({
    tolower(texto)
  }, error = function(e) {
    return(texto)
  })
  
  # Remover acentos usando stringi si está disponible
  texto <- tryCatch({
    if(requireNamespace("stringi", quietly = TRUE)) {
      stringi::stri_trans_general(texto, "Latin-ASCII")
    } else {
      # Fallback manual para acentos comunes
      texto <- gsub("á|à|â|ä|ã", "a", texto)
      texto <- gsub("é|è|ê|ë", "e", texto) 
      texto <- gsub("í|ì|î|ï", "i", texto)
      texto <- gsub("ó|ò|ô|ö|õ", "o", texto)
      texto <- gsub("ú|ù|û|ü", "u", texto)
      texto <- gsub("ñ", "n", texto)
      texto <- gsub("ç", "c", texto)
      texto
    }
  }, error = function(e) {
    texto
  })
  
  # Remover caracteres especiales, mantener letras, números, espacios y guiones
  texto <- tryCatch({
    gsub("[^a-z0-9\\s\\-]", " ", texto)
  }, error = function(e) {
    texto
  })
  
  # Normalizar espacios múltiples
  texto <- tryCatch({
    gsub("\\s+", " ", texto)
  }, error = function(e) {
    texto
  })
  
  # Remover espacios al inicio y final
  texto <- tryCatch({
    trimws(texto)
  }, error = function(e) {
    gsub("^\\s+|\\s+$", "", texto)  # Fallback manual
  })
  
  return(texto)
}

#' Genera resumen simple basado en papers encontrados
generar_resumen_simple <- function(query, papers, openai_key = NULL) {
  
  num_papers <- nrow(papers)
  
  if(num_papers == 0) {
    return(paste("No se encontraron papers relacionados con '", query, "'."))
  }
  
  # Intentar usar OpenAI si hay clave y está disponible
  if(!is.null(openai_key) && usar_httr && usar_jsonlite) {
    resumen_gpt <- tryCatch({
      generar_con_openai_simple(query, papers, openai_key)
    }, error = function(e) {
      NULL
    })
    
    if(!is.null(resumen_gpt) && nchar(resumen_gpt) > 20) {
      return(resumen_gpt)
    }
  }
  
  # Fallback: generar resumen local simple
  return(generar_resumen_local_simple(query, papers))
}

#' Genera resumen usando OpenAI (versión simplificada)
generar_con_openai_simple <- function(query, papers, api_key) {
  
  if(!usar_httr || !usar_jsonlite) return(NULL)
  
  # Crear texto con información de papers
  papers_info <- paste(
    papers$TITULO[1:min(5, nrow(papers))],  # Solo primeros 5 títulos
    collapse = ". "
  )
  
  # Prompt simplificado
  prompt <- paste0(
    "Analiza estos ", nrow(papers), " papers académicos sobre '", query, 
    "' y genera un resumen en español de 2-3 oraciones que describa los temas principales y aplicaciones. ",
    "Títulos de algunos papers: ", substr(papers_info, 1, 500),
    ". Responde SOLO con el resumen en español, sin introducción."
  )
  
  tryCatch({
    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ),
      body = jsonlite::toJSON(list(
        model = "gpt-3.5-turbo",
        messages = list(list(
          role = "user", 
          content = prompt
        )),
        max_tokens = 150,
        temperature = 0.7
      ), auto_unbox = TRUE)
    )
    
    if(httr::status_code(response) == 200) {
      resultado <- jsonlite::fromJSON(httr::content(response, "text"))
      return(trimws(resultado$choices[[1]]$message$content))
    }
    
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

#' Genera resumen local usando técnicas básicas
generar_resumen_local_simple <- function(query, papers) {
  
  num_papers <- nrow(papers)
  
  # Análisis básico de palabras frecuentes en títulos
  todos_titulos <- paste(papers$TITULO, collapse = " ")
  palabras_titulo <- extraer_terminos_simple(todos_titulos)
  
  # Contar frecuencias
  freq_table <- table(palabras_titulo)
  palabras_frecuentes <- names(freq_table[freq_table >= 2])
  
  # Identificar áreas principales basándose en palabras frecuentes
  areas_identificadas <- c()
  
  # Términos técnicos comunes
  if(any(grepl("machine|learning|neural|network|algorithm", palabras_frecuentes))) {
    areas_identificadas <- c(areas_identificadas, "machine learning y algoritmos")
  }
  if(any(grepl("image|vision|processing", palabras_frecuentes))) {
    areas_identificadas <- c(areas_identificadas, "procesamiento de imágenes")
  }
  if(any(grepl("data|mining|analysis|big", palabras_frecuentes))) {
    areas_identificadas <- c(areas_identificadas, "análisis de datos")
  }
  if(any(grepl("software|system|development", palabras_frecuentes))) {
    areas_identificadas <- c(areas_identificadas, "desarrollo de software")
  }
  if(any(grepl("optimization|genetic|evolutionary", palabras_frecuentes))) {
    areas_identificadas <- c(areas_identificadas, "optimización")
  }
  
  # Construir resumen
  resumen_base <- paste0("Se encontraron ", num_papers, " papers relacionados con '", query, "'")
  
  if(length(areas_identificadas) > 0) {
    areas_texto <- if(length(areas_identificadas) == 1) {
      areas_identificadas[1]
    } else if(length(areas_identificadas) == 2) {
      paste(areas_identificadas, collapse = " y ")
    } else {
      paste(c(areas_identificadas[-length(areas_identificadas)], 
              paste("y", tail(areas_identificadas, 1))), collapse = ", ")
    }
    
    resumen_completo <- paste0(
      resumen_base, 
      ", abarcando principalmente investigaciones en ", areas_texto, 
      ". Los estudios incluyen diversas metodologías y aplicaciones en estas áreas de investigación."
    )
  } else {
    resumen_completo <- paste0(
      resumen_base,
      ", que abordan diversos enfoques metodológicos y aplicaciones en el campo de estudio."
    )
  }
  
  return(resumen_completo)
}