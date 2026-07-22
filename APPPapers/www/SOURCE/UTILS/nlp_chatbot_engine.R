#=======================================
# Motor PLN Chatbot para Análisis Bibliométrico
#=======================================

library(httr)
library(jsonlite)
library(tm)
library(stringi)
library(dplyr)

# Verificar paquetes opcionales
has_text <- requireNamespace("text", quietly = TRUE)
has_hunspell <- requireNamespace("RcppHunspell", quietly = TRUE)

#' Motor PLN principal para procesamiento de consultas bibliométricas
#' 
#' Esta función implementa un motor de procesamiento de lenguaje natural
#' que busca papers relacionados con una consulta, analiza sus abstracts
#' y genera un resumen completamente nuevo en español
#' 
#' @param query Consulta de búsqueda del usuario
#' @param data DataFrame con los datos de papers
#' @param openai_key Clave API de OpenAI para generación de resúmenes
#' @return Lista con resumen generado y papers encontrados
proceso_nlp_chatbot <- function(query, data, openai_key = NULL) {
  
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
  
  # Paso 1: Búsqueda semántica de papers relevantes
  papers_encontrados <- buscar_papers_semantico(query, data)
  
  if(nrow(papers_encontrados) == 0) {
    return(list(
      success = TRUE,
      resumen_generado = "No se encontraron papers relacionados con su búsqueda. Intente con términos diferentes o más específicos.",
      num_papers = 0,
      papers = data.frame()
    ))
  }
  
  # Paso 2: Extraer y procesar abstracts en inglés
  abstracts_procesados <- procesar_abstracts(papers_encontrados)
  
  # Paso 3: Generar resumen completamente nuevo usando GPT
  resumen_nuevo <- generar_resumen_gpt(query, abstracts_procesados, openai_key)
  
  # Paso 4: Traducir resumen a español si está en inglés
  resumen_final <- traducir_a_espanol(resumen_nuevo)
  
  # Preparar información de papers para mostrar
  papers_info <- papers_encontrados %>%
    select(TITULO, NOMBRE_AUTOR, ANO, LINK, SJR, CITADO_POR) %>%
    arrange(desc(as.numeric(ifelse(is.na(SJR), 0, SJR))), desc(as.numeric(ifelse(is.na(CITADO_POR), 0, CITADO_POR))))
  
  return(list(
    success = TRUE,
    resumen_generado = resumen_final,
    num_papers = nrow(papers_encontrados),
    papers = papers_info,
    terminos_busqueda = extraer_terminos_clave(query)
  ))
}

#' Búsqueda semántica mejorada de papers
#' Utiliza múltiples estrategias para encontrar papers relevantes
buscar_papers_semantico <- function(query, data) {
  
  # Limpiar y preparar la consulta
  query_limpia <- limpiar_texto(query)
  terminos <- extraer_terminos_clave(query_limpia)
  
  # Columnas a buscar
  columnas_busqueda <- c("TITULO", "RESUMEN", "AUTOR_PALABRAS_CLAVES", "INDEX_PALABRAS_CLAVES")
  
  # Estrategia 1: Coincidencia exacta de frases
  indices_exactos <- buscar_coincidencia_exacta(data, query_limpia, columnas_busqueda)
  
  # Estrategia 2: Coincidencia por términos individuales con pesos
  indices_terminos <- buscar_por_terminos_ponderados(data, terminos, columnas_busqueda)
  
  # Estrategia 3: Coincidencia aproximada usando distancia de cadenas
  indices_aproximados <- buscar_aproximado(data, terminos, columnas_busqueda)
  
  # Combinar y rankear resultados
  todos_indices <- unique(c(indices_exactos, indices_terminos, indices_aproximados))
  
  if(length(todos_indices) == 0) {
    return(data.frame())
  }
  
  # Calcular scores de relevancia
  papers_con_score <- calcular_relevancia(data[todos_indices, ], query_limpia, terminos)
  
  # Filtrar por score mínimo y devolver top resultados
  papers_relevantes <- papers_con_score[papers_con_score$score_relevancia >= 0.1, ]
  papers_relevantes <- papers_relevantes[order(papers_relevantes$score_relevancia, decreasing = TRUE), ]
  
  # Limitar a máximo 15 papers para procesamiento eficiente
  if(nrow(papers_relevantes) > 15) {
    papers_relevantes <- papers_relevantes[1:15, ]
  }
  
  return(papers_relevantes)
}

#' Extrae términos clave de una consulta
extraer_terminos_clave <- function(texto) {
  # Stopwords en español e inglés
  stopwords_es <- c("el", "la", "los", "las", "un", "una", "de", "del", "en", "con", "por", "para", "y", "o", "pero", "que", "como", "sobre", "entre", "sin", "hasta", "desde", "hacia", "bajo", "tras", "durante", "mediante", "según", "contra", "hacia", "desde")
  stopwords_en <- c("the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for", "of", "with", "by", "from", "up", "about", "into", "through", "during", "before", "after", "above", "below", "between", "among", "within", "without", "under", "over", "across", "along", "around", "behind", "beside", "beyond", "through", "throughout", "toward", "upon", "within", "without")
  
  todos_stopwords <- c(stopwords_es, stopwords_en)
  
  # Tokenizar y limpiar
  tokens <- unlist(strsplit(tolower(texto), "\\W+"))
  tokens <- tokens[nchar(tokens) >= 3]  # Palabras de al menos 3 caracteres
  tokens <- tokens[!tokens %in% todos_stopwords]
  
  # Remover números puros
  tokens <- tokens[!grepl("^\\d+$", tokens)]
  
  return(unique(tokens))
}

#' Limpia y normaliza texto para búsqueda
limpiar_texto <- function(texto) {
  # Normalizar caracteres
  texto <- stringi::stri_trans_general(texto, "Latin-ASCII")
  # Convertir a minúsculas
  texto <- tolower(texto)
  # Remover caracteres especiales excepto espacios y guiones
  texto <- gsub("[^a-z0-9\\s\\-]", " ", texto)
  # Normalizar espacios
  texto <- gsub("\\s+", " ", texto)
  texto <- trimws(texto)
  
  return(texto)
}

#' Busca coincidencias exactas de la consulta completa
buscar_coincidencia_exacta <- function(data, query, columnas) {
  indices <- c()
  
  for(col in columnas) {
    if(col %in% colnames(data)) {
      col_limpia <- sapply(data[[col]], function(x) {
        if(is.na(x) || x == "") return("")
        limpiar_texto(as.character(x))
      })
      
      coincidencias <- grepl(query, col_limpia, fixed = TRUE)
      indices <- c(indices, which(coincidencias))
    }
  }
  
  return(unique(indices))
}

#' Busca por términos individuales con pesos por columna
buscar_por_terminos_ponderados <- function(data, terminos, columnas) {
  # Pesos por columna (más peso a título y palabras clave)
  pesos <- list(
    "TITULO" = 3,
    "AUTOR_PALABRAS_CLAVES" = 2.5,
    "INDEX_PALABRAS_CLAVES" = 2,
    "RESUMEN" = 1
  )
  
  scores <- rep(0, nrow(data))
  
  for(col in columnas) {
    if(col %in% colnames(data)) {
      peso <- ifelse(col %in% names(pesos), pesos[[col]], 1)
      
      col_limpia <- sapply(data[[col]], function(x) {
        if(is.na(x) || x == "") return("")
        limpiar_texto(as.character(x))
      })
      
      for(termino in terminos) {
        coincidencias <- grepl(termino, col_limpia, fixed = TRUE)
        scores[coincidencias] <- scores[coincidencias] + peso
      }
    }
  }
  
  # Devolver índices con score > 0
  return(which(scores > 0))
}

#' Búsqueda aproximada usando distancia de cadenas
buscar_aproximado <- function(data, terminos, columnas) {
  indices <- c()
  
  for(termino in terminos) {
    if(nchar(termino) >= 4) {  # Solo para términos largos
      for(col in columnas) {
        if(col %in% colnames(data)) {
          col_limpia <- sapply(data[[col]], function(x) {
            if(is.na(x) || x == "") return("")
            limpiar_texto(as.character(x))
          })
          
          # Buscar coincidencias aproximadas
          aproximadas <- which(sapply(col_limpia, function(x) {
            if(nchar(x) == 0) return(FALSE)
            # Buscar el término con hasta 1 error por cada 4 caracteres
            max_errores <- max(1, floor(nchar(termino) / 4))
            agrep(termino, x, max.distance = max_errores, ignore.case = TRUE)
          }))
          
          indices <- c(indices, aproximadas)
        }
      }
    }
  }
  
  return(unique(indices))
}

#' Calcula puntuación de relevancia para papers encontrados
calcular_relevancia <- function(papers, query, terminos) {
  papers$score_relevancia <- 0
  
  for(i in 1:nrow(papers)) {
    score <- 0
    
    # Texto combinado para análisis
    texto_completo <- paste(
      ifelse(is.na(papers$TITULO[i]), "", papers$TITULO[i]),
      ifelse(is.na(papers$RESUMEN[i]), "", papers$RESUMEN[i]),
      ifelse(is.na(papers$AUTOR_PALABRAS_CLAVES[i]), "", papers$AUTOR_PALABRAS_CLAVES[i]),
      sep = " "
    )
    
    texto_limpio <- limpiar_texto(texto_completo)
    
    # Score por coincidencia exacta de la query completa
    if(grepl(query, texto_limpio, fixed = TRUE)) {
      score <- score + 10
    }
    
    # Score por términos individuales
    for(termino in terminos) {
      coincidencias <- length(gregexpr(termino, texto_limpio, fixed = TRUE)[[1]])
      if(coincidencias > 0 && !is.na(coincidencias)) {
        score <- score + (coincidencias * 2)
      }
    }
    
    # Bonus por calidad del paper (SJR y citas)
    if(!is.na(papers$SJR[i]) && as.numeric(papers$SJR[i]) > 0) {
      score <- score + (as.numeric(papers$SJR[i]) * 0.5)
    }
    
    if(!is.na(papers$CITADO_POR[i]) && as.numeric(papers$CITADO_POR[i]) > 0) {
      score <- score + (log(as.numeric(papers$CITADO_POR[i]) + 1) * 0.2)
    }
    
    papers$score_relevancia[i] <- score
  }
  
  return(papers)
}

#' Procesa los abstracts encontrados para extracción de información clave
procesar_abstracts <- function(papers) {
  if(nrow(papers) == 0) return("")
  
  abstracts <- papers$RESUMEN[!is.na(papers$RESUMEN) & papers$RESUMEN != ""]
  
  if(length(abstracts) == 0) return("")
  
  # Combinar abstracts con separadores claros
  texto_combinado <- paste(abstracts, collapse = " [SEP] ")
  
  # Limpiar texto combinado
  texto_limpio <- gsub("\\s+", " ", texto_combinado)
  texto_limpio <- trimws(texto_limpio)
  
  return(texto_limpio)
}

#' Genera un resumen completamente nuevo usando GPT
generar_resumen_gpt <- function(query, abstracts_texto, openai_key) {
  
  if(is.null(openai_key) || nchar(openai_key) == 0) {
    return(generar_resumen_local(query, abstracts_texto))
  }
  
  # Prompt para GPT
  prompt <- paste0(
    "Eres un experto investigador académico. Analiza los siguientes abstracts de papers científicos relacionados con '", query, "' y genera un resumen completamente nuevo que describa:\n",
    "1. Las principales áreas y temas de investigación encontrados\n",
    "2. Las metodologías o enfoques más comunes\n",
    "3. Las aplicaciones o dominios de estudio\n",
    "4. Los hallazgos o tendencias principales\n\n",
    "Abstracts a analizar:\n", substr(abstracts_texto, 1, 3000), "\n\n",
    "Genera un resumen de 3-4 oraciones en inglés que sintetice la información de manera original, no copies frases de los abstracts originales:"
  )
  
  tryCatch({
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(
        "Authorization" = paste("Bearer", openai_key),
        "Content-Type" = "application/json"
      ),
      body = toJSON(list(
        model = "gpt-3.5-turbo",
        messages = list(list(
          role = "user",
          content = prompt
        )),
        max_tokens = 200,
        temperature = 0.7
      ), auto_unbox = TRUE)
    )
    
    if(status_code(response) == 200) {
      resultado <- fromJSON(content(response, "text"))
      return(resultado$choices[[1]]$message$content)
    } else {
      return(generar_resumen_local(query, abstracts_texto))
    }
  }, error = function(e) {
    return(generar_resumen_local(query, abstracts_texto))
  })
}

#' Genera resumen local usando técnicas de NLP básicas
generar_resumen_local <- function(query, abstracts_texto) {
  
  if(nchar(abstracts_texto) == 0) {
    return(paste("Se realizó una búsqueda sobre", query, "pero no se encontraron abstracts disponibles para análisis."))
  }
  
  # Tokenizar y limpiar el texto
  tokens <- unlist(strsplit(tolower(abstracts_texto), "\\W+"))
  tokens <- tokens[nchar(tokens) >= 4]
  
  # Contar frecuencias
  freq_table <- table(tokens)
  freq_df <- data.frame(word = names(freq_table), freq = as.numeric(freq_table))
  freq_df <- freq_df[order(freq_df$freq, decreasing = TRUE), ]
  
  # Obtener términos más frecuentes (excluyendo stopwords)
  stopwords_comunes <- c("this", "that", "with", "from", "they", "were", "been", "have", "their", "such", "using", "used", "also", "more", "only", "these", "than", "most", "some", "other", "which", "would", "could", "about", "between", "through", "during", "method", "methods", "results", "study", "research", "analysis", "approach", "data", "based", "paper", "work", "system", "model", "different", "important", "significant", "performance", "effective", "proposed", "developed", "applied", "show", "shows", "shown", "found", "present", "presented", "provide", "provides", "algorithm", "technique", "techniques", "application", "applications")
  
  terms_importantes <- freq_df[!freq_df$word %in% stopwords_comunes & freq_df$freq >= 2, ]
  
  if(nrow(terms_importantes) >= 5) {
    top_terms <- head(terms_importantes$word, 8)
    resumen <- paste0(
      "The research on ", query, " encompasses multiple approaches including ",
      paste(top_terms[1:3], collapse = ", "), ". ",
      "Key methodologies involve ", paste(top_terms[4:6], collapse = " and "), ". ",
      "Applications span across ", paste(tail(top_terms, 2), collapse = " and "), " domains."
    )
  } else {
    resumen <- paste0("The search for '", query, "' reveals diverse research approaches and methodologies in this field.")
  }
  
  return(resumen)
}

#' Traduce texto a español usando múltiples estrategias
traducir_a_espanol <- function(texto_ingles) {
  
  if(is.null(texto_ingles) || nchar(trimws(texto_ingles)) == 0) {
    return("No se pudo generar un resumen para esta búsqueda.")
  }
  
  # Estrategia 1: Intentar con Google Translate API gratuito (si está disponible)
  traduccion_google <- intentar_google_translate(texto_ingles)
  if(!is.null(traduccion_google)) {
    return(traduccion_google)
  }
  
  # Estrategia 2: Traducción usando diccionario básico
  traduccion_basica <- traducir_diccionario_basico(texto_ingles)
  
  return(traduccion_basica)
}

#' Intenta traducir usando Google Translate (método simple)
intentar_google_translate <- function(texto) {
  tryCatch({
    # URL de Google Translate
    base_url <- "https://translate.googleapis.com/translate_a/single"
    params <- list(
      client = "gtx",
      sl = "en",
      tl = "es",
      dt = "t",
      q = texto
    )
    
    response <- GET(base_url, query = params)
    
    if(status_code(response) == 200) {
      content_text <- content(response, "text", encoding = "UTF-8")
      # Parsear la respuesta de Google Translate
      traduccion <- extraer_traduccion_google(content_text)
      if(!is.null(traduccion)) {
        return(traduccion)
      }
    }
    
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

#' Extrae la traducción de la respuesta de Google
extraer_traduccion_google <- function(response_text) {
  tryCatch({
    # Google devuelve un array JSON, extraer la traducción
    json_response <- fromJSON(response_text)
    if(length(json_response) > 0 && length(json_response[[1]]) > 0) {
      traduccion <- paste(sapply(json_response[[1]], function(x) x[1]), collapse = "")
      return(trimws(traduccion))
    }
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

#' Traducción básica usando diccionario de términos técnicos
traducir_diccionario_basico <- function(texto_ingles) {
  
  # Diccionario básico de términos técnicos comunes
  diccionario <- list(
    # Términos de investigación
    "research" = "investigación",
    "study" = "estudio",
    "analysis" = "análisis", 
    "method" = "método",
    "methods" = "métodos",
    "approach" = "enfoque",
    "technique" = "técnica",
    "techniques" = "técnicas",
    "algorithm" = "algoritmo",
    "algorithms" = "algoritmos",
    "model" = "modelo",
    "models" = "modelos",
    "system" = "sistema",
    "systems" = "sistemas",
    "data" = "datos",
    "results" = "resultados",
    "performance" = "rendimiento",
    "application" = "aplicación",
    "applications" = "aplicaciones",
    
    # Machine Learning
    "machine learning" = "aprendizaje automático",
    "artificial intelligence" = "inteligencia artificial",
    "deep learning" = "aprendizaje profundo",
    "neural network" = "red neuronal",
    "neural networks" = "redes neuronales",
    "classification" = "clasificación",
    "regression" = "regresión",
    "clustering" = "agrupamiento",
    "supervised" = "supervisado",
    "unsupervised" = "no supervisado",
    "feature" = "característica",
    "features" = "características",
    "dataset" = "conjunto de datos",
    "training" = "entrenamiento",
    "prediction" = "predicción",
    "accuracy" = "precisión",
    
    # Términos generales
    "including" = "incluyendo",
    "different" = "diferentes",
    "various" = "varios",
    "multiple" = "múltiples",
    "several" = "varios",
    "important" = "importantes",
    "significant" = "significativos",
    "effective" = "efectivos",
    "efficient" = "eficientes",
    "proposed" = "propuestos",
    "developed" = "desarrollados",
    "applied" = "aplicados",
    "encompasses" = "abarca",
    "involves" = "involucra",
    "domains" = "dominios",
    "field" = "campo",
    "across" = "a través de",
    "methodologies" = "metodologías",
    "approaches" = "enfoques",
    
    # Conectores y estructura
    "the" = "",
    "and" = "y",
    "on" = "sobre",
    "in" = "en",
    "of" = "de",
    "for" = "para",
    "with" = "con",
    "from" = "de",
    "reveals" = "revela",
    "span" = "abarcan",
    "key" = "clave"
  )
  
  # Aplicar traducción palabra por palabra
  palabras <- unlist(strsplit(tolower(texto_ingles), "\\b"))
  
  texto_traducido <- paste(sapply(palabras, function(palabra) {
    palabra_limpia <- gsub("[^a-z\\s]", "", palabra)
    if(palabra_limpia %in% names(diccionario) && diccionario[[palabra_limpia]] != "") {
      return(diccionario[[palabra_limpia]])
    } else if(grepl("^[a-z]+$", palabra_limpia) && nchar(palabra_limpia) > 0) {
      return(palabra_limpia)  # Mantener palabras no traducidas
    } else {
      return(palabra)  # Mantener puntuación y otros caracteres
    }
  }), collapse = "")
  
  # Limpiar el resultado
  texto_traducido <- gsub("\\s+", " ", texto_traducido)
  texto_traducido <- trimws(texto_traducido)
  
  # Si la traducción está muy incompleta, devolver un mensaje genérico
  if(nchar(texto_traducido) < (nchar(texto_ingles) * 0.5)) {
    return("Se encontraron múltiples estudios relacionados con la consulta de búsqueda, abarcando diferentes enfoques metodológicos y aplicaciones en el campo de investigación.")
  }
  
  return(paste0("Se encontraron estudios sobre: ", texto_traducido))
}