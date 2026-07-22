#=======================================
# Sistema de Sinónimos Académicos - FASE 1
# Mejora del Motor NLP APPPapers
#=======================================

library(stringi, warn.conflicts = FALSE)

#' Sistema completo de expansión semántica con sinónimos académicos
#'
#' Este módulo proporciona expansión inteligente de consultas usando
#' sinónimos especializados en terminología académica y científica
#'
#' @author Claude Code - Fase 1 Mejoras NLP
#' @date 2025-09-29

#' Diccionario principal de sinónimos académicos
get_diccionario_sinonimos <- function() {
  list(
    # MACHINE LEARNING Y AI
    "machine learning" = c("aprendizaje automatico", "aprendizaje maquina", "ml", "algoritmos aprendizaje", "inteligencia artificial", "ai", "automated learning", "statistical learning"),
    "artificial intelligence" = c("inteligencia artificial", "ai", "machine learning", "ml", "sistemas inteligentes", "cognitive computing"),
    "deep learning" = c("aprendizaje profundo", "redes profundas", "neural networks", "redes neuronales", "dl", "deep neural networks"),

    # REDES NEURONALES
    "neural networks" = c("redes neuronales", "redes neuronales artificiales", "ann", "perceptron", "multilayer perceptron", "feedforward", "backpropagation"),
    "cnn" = c("convolutional neural network", "red neuronal convolucional", "convnet", "redes convolucionales"),
    "rnn" = c("recurrent neural network", "red neuronal recurrente", "lstm", "gru", "redes recurrentes"),
    "lstm" = c("long short term memory", "memoria corto largo plazo", "rnn", "recurrent networks"),

    # PROCESAMIENTO DE IMÁGENES
    "image processing" = c("procesamiento imagenes", "vision computacional", "computer vision", "analisis imagenes", "image analysis", "opencv", "tratamiento imagenes"),
    "computer vision" = c("vision computacional", "vision artificial", "image processing", "procesamiento imagenes", "pattern recognition", "reconocimiento patrones"),
    "object detection" = c("deteccion objetos", "reconocimiento objetos", "localizacion objetos", "detection", "object recognition"),
    "image segmentation" = c("segmentacion imagenes", "segmentacion", "region growing", "clustering imagenes"),

    # OPTIMIZACIÓN
    "optimization" = c("optimizacion", "optimizacion", "mejora", "busqueda optima", "programming optimization"),
    "genetic algorithm" = c("algoritmo genetico", "algoritmos geneticos", "evolutionary algorithm", "algoritmos evolutivos", "ga", "computacion evolutiva"),
    "evolutionary algorithm" = c("algoritmos evolutivos", "computacion evolutiva", "genetic algorithm", "algoritmos geneticos", "evolutionary computing"),
    "metaheuristic" = c("metaheuristica", "metaheuristicas", "heuristic", "heuristicas", "optimization algorithms"),
    "particle swarm" = c("enjambre particulas", "pso", "particle swarm optimization", "swarm intelligence"),

    # DATA SCIENCE Y BIG DATA
    "data science" = c("ciencia datos", "analisis datos", "data analysis", "data mining", "mineria datos", "analytics", "big data"),
    "data mining" = c("mineria datos", "extraccion conocimiento", "knowledge discovery", "data analysis", "pattern mining", "text mining"),
    "big data" = c("grandes volumenes datos", "datos masivos", "analytics", "data science", "hadoop", "spark"),
    "machine learning" = c("aprendizaje automatico", "data mining", "statistical learning", "pattern recognition"),

    # PROCESAMIENTO DE LENGUAJE NATURAL
    "natural language processing" = c("procesamiento lenguaje natural", "nlp", "text processing", "procesamiento texto", "computational linguistics", "linguistica computacional"),
    "text mining" = c("mineria texto", "text analysis", "analisis texto", "text processing", "nlp", "information extraction"),
    "sentiment analysis" = c("analisis sentimientos", "opinion mining", "mineria opiniones", "emotion detection", "polarity analysis"),
    "information retrieval" = c("recuperacion informacion", "busqueda informacion", "search engines", "document retrieval"),

    # SOFTWARE ENGINEERING
    "software engineering" = c("ingenieria software", "desarrollo software", "software development", "programming", "programacion", "software design"),
    "software development" = c("desarrollo software", "programming", "programacion", "coding", "software engineering", "agile development"),
    "programming" = c("programacion", "coding", "development", "desarrollo", "software development"),
    "algorithm" = c("algoritmo", "algorithms", "algorithmic", "procedure", "procedimiento", "method", "metodo"),

    # BASES DE DATOS
    "database" = c("base datos", "bd", "db", "bases datos", "data storage", "almacenamiento datos", "dbms"),
    "sql" = c("structured query language", "consultas", "database queries", "relational database"),
    "nosql" = c("no sql", "document database", "mongodb", "big data storage"),

    # REDES Y SISTEMAS
    "network" = c("red", "redes", "networking", "comunicaciones", "network topology", "topologia red"),
    "distributed systems" = c("sistemas distribuidos", "distributed computing", "computacion distribuida", "cluster computing"),
    "cloud computing" = c("computacion nube", "nube", "cloud", "aws", "azure", "google cloud"),

    # METODOLOGÍAS DE INVESTIGACIÓN
    "methodology" = c("metodologia", "method", "metodo", "approach", "enfoque", "procedure", "procedimiento"),
    "experimental design" = c("diseño experimental", "experiment design", "experimental methodology", "controlled experiment"),
    "case study" = c("estudio caso", "case analysis", "analisis caso", "empirical study"),
    "survey" = c("encuesta", "questionnaire", "cuestionario", "survey research", "poll"),

    # MÉTRICAS Y EVALUACIÓN
    "evaluation" = c("evaluacion", "assessment", "valoracion", "performance evaluation", "testing", "validation"),
    "accuracy" = c("precision", "exactitud", "correctness", "performance", "rendimiento"),
    "performance" = c("rendimiento", "desempeño", "efficiency", "eficiencia", "speed", "velocidad"),
    "validation" = c("validacion", "verification", "verificacion", "testing", "pruebas"),

    # TÉRMINOS GENERALES ACADÉMICOS
    "research" = c("investigacion", "study", "estudio", "analysis", "analisis", "exploration", "exploracion"),
    "analysis" = c("analisis", "examination", "examen", "study", "estudio", "investigation", "investigacion"),
    "implementation" = c("implementacion", "desarrollo", "development", "construction", "construccion"),
    "framework" = c("marco trabajo", "estructura", "architecture", "arquitectura", "platform", "plataforma"),
    "model" = c("modelo", "representation", "representacion", "simulation", "simulacion"),
    "system" = c("sistema", "platform", "plataforma", "framework", "architecture", "arquitectura")
  )
}

#' Expandir consulta con sinónimos académicos
#'
#' @param query Consulta original del usuario
#' @return Lista con consulta expandida y términos adicionales
expandir_consulta_semantica <- function(query) {

  if(is.null(query) || nchar(trimws(query)) == 0) {
    return(list(
      query_original = query,
      query_expandida = query,
      sinonimos_encontrados = character(0),
      terminos_adicionales = character(0)
    ))
  }

  # Limpiar consulta
  query_limpia <- limpiar_texto_para_sinonimos(query)

  # Obtener diccionario
  diccionario <- get_diccionario_sinonimos()

  # Encontrar coincidencias en la consulta
  sinonimos_encontrados <- list()
  terminos_adicionales <- character(0)

  for(termino_clave in names(diccionario)) {
    # Buscar término clave en la consulta (flexible)
    if(detectar_termino_en_consulta(query_limpia, termino_clave)) {
      sinonimos_encontrados[[termino_clave]] <- diccionario[[termino_clave]]
      terminos_adicionales <- c(terminos_adicionales, diccionario[[termino_clave]])
    }
  }

  # También buscar en sinónimos (búsqueda inversa)
  for(termino_clave in names(diccionario)) {
    sinonimos <- diccionario[[termino_clave]]
    for(sinonimo in sinonimos) {
      if(detectar_termino_en_consulta(query_limpia, sinonimo)) {
        if(!termino_clave %in% names(sinonimos_encontrados)) {
          sinonimos_encontrados[[termino_clave]] <- diccionario[[termino_clave]]
          # Agregar el término clave también
          terminos_adicionales <- c(terminos_adicionales, termino_clave, diccionario[[termino_clave]])
        }
      }
    }
  }

  # Limpiar duplicados
  terminos_adicionales <- unique(terminos_adicionales)
  terminos_adicionales <- terminos_adicionales[terminos_adicionales != query_limpia]

  # Crear consulta expandida
  if(length(terminos_adicionales) > 0) {
    # Limitar a los 8 términos más relevantes para evitar ruido
    terminos_adicionales <- head(terminos_adicionales, 8)
    query_expandida <- paste(query_limpia, paste(terminos_adicionales, collapse = " "))
  } else {
    query_expandida <- query_limpia
  }

  return(list(
    query_original = query,
    query_expandida = query_expandida,
    sinonimos_encontrados = sinonimos_encontrados,
    terminos_adicionales = terminos_adicionales,
    num_expansiones = length(terminos_adicionales)
  ))
}

#' Detectar si un término está presente en la consulta (búsqueda flexible)
detectar_termino_en_consulta <- function(consulta, termino) {
  # Normalizar ambos
  consulta_norm <- limpiar_texto_para_sinonimos(consulta)
  termino_norm <- limpiar_texto_para_sinonimos(termino)

  # Búsqueda exacta
  if(grepl(termino_norm, consulta_norm, fixed = TRUE)) {
    return(TRUE)
  }

  # Búsqueda por palabras individuales para términos compuestos
  palabras_termino <- unlist(strsplit(termino_norm, "\\s+"))
  palabras_consulta <- unlist(strsplit(consulta_norm, "\\s+"))

  # Si todas las palabras del término están en la consulta
  if(length(palabras_termino) > 1) {
    coincidencias <- sum(palabras_termino %in% palabras_consulta)
    if(coincidencias >= length(palabras_termino) * 0.7) {  # 70% de coincidencia
      return(TRUE)
    }
  }

  # Búsqueda aproximada para términos largos
  if(nchar(termino_norm) >= 6) {
    # Permitir 1 error por cada 5 caracteres
    max_errores <- max(1, floor(nchar(termino_norm) / 5))
    if(length(agrep(termino_norm, consulta_norm, max.distance = max_errores, ignore.case = TRUE)) > 0) {
      return(TRUE)
    }
  }

  return(FALSE)
}

#' Limpiar texto específicamente para el sistema de sinónimos
limpiar_texto_para_sinonimos <- function(texto) {
  if(is.null(texto) || is.na(texto) || length(texto) == 0) return("")

  # Convertir a character y tomar primer elemento
  texto <- as.character(texto[1])
  if(is.na(texto) || nchar(texto) == 0) return("")

  # Convertir a minúsculas
  texto <- tolower(texto)

  # Remover acentos
  if(requireNamespace("stringi", quietly = TRUE)) {
    texto <- stringi::stri_trans_general(texto, "Latin-ASCII")
  } else {
    # Fallback manual
    texto <- gsub("á|à|â|ä|ã", "a", texto)
    texto <- gsub("é|è|ê|ë", "e", texto)
    texto <- gsub("í|ì|î|ï", "i", texto)
    texto <- gsub("ó|ò|ô|ö|õ", "o", texto)
    texto <- gsub("ú|ù|û|ü", "u", texto)
    texto <- gsub("ñ", "n", texto)
    texto <- gsub("ç", "c", texto)
  }

  # Mantener solo letras, números, espacios y guiones
  texto <- gsub("[^a-z0-9\\s\\-]", " ", texto)

  # Normalizar espacios
  texto <- gsub("\\s+", " ", texto)
  texto <- trimws(texto)

  return(texto)
}

#' Generar variaciones de un término para búsqueda más robusta
generar_variaciones_termino <- function(termino) {
  variaciones <- c(termino)

  # Variaciones comunes
  if(nchar(termino) > 4) {
    # Singular/Plural básico
    if(endsWith(termino, "s")) {
      variaciones <- c(variaciones, substr(termino, 1, nchar(termino)-1))
    } else {
      variaciones <- c(variaciones, paste0(termino, "s"))
    }

    # Variaciones de terminación
    if(endsWith(termino, "ing")) {
      base <- substr(termino, 1, nchar(termino)-3)
      variaciones <- c(variaciones, base, paste0(base, "e"))
    }

    if(endsWith(termino, "ed")) {
      base <- substr(termino, 1, nchar(termino)-2)
      variaciones <- c(variaciones, base, paste0(base, "ing"))
    }
  }

  return(unique(variaciones))
}

#' Calcular relevancia de un término expandido
calcular_relevancia_sinonimo <- function(termino_original, sinonimo, contexto_consulta) {
  # Relevancia base
  relevancia <- 0.5

  # Bonus por longitud (términos más específicos son más relevantes)
  relevancia <- relevancia + (min(nchar(sinonimo), 15) / 30)

  # Bonus si aparece en contexto similar
  palabras_contexto <- unlist(strsplit(contexto_consulta, "\\s+"))
  if(any(grepl(sinonimo, palabras_contexto, ignore.case = TRUE))) {
    relevancia <- relevancia + 0.3
  }

  # Penalizar sinónimos muy genéricos
  terminos_genericos <- c("method", "system", "analysis", "study", "research", "approach")
  if(sinonimo %in% terminos_genericos) {
    relevancia <- relevancia - 0.2
  }

  return(max(0.1, min(1.0, relevancia)))
}

#' Función de prueba para validar el sistema de sinónimos
probar_sistema_sinonimos <- function() {
  cat("=== PROBANDO SISTEMA DE SINÓNIMOS ACADÉMICOS ===\n\n")

  # Casos de prueba
  casos_prueba <- c(
    "machine learning",
    "redes neuronales",
    "procesamiento de imágenes",
    "algoritmos genéticos",
    "minería de datos",
    "inteligencia artificial en medicina",
    "deep learning para clasificación"
  )

  for(caso in casos_prueba) {
    cat(paste("CONSULTA:", caso, "\n"))
    resultado <- expandir_consulta_semantica(caso)

    cat(paste("- Query expandida:", resultado$query_expandida, "\n"))
    cat(paste("- Términos adicionales:", length(resultado$terminos_adicionales), "\n"))
    if(length(resultado$terminos_adicionales) > 0) {
      cat(paste("- Sinónimos:", paste(head(resultado$terminos_adicionales, 5), collapse = ", "), "\n"))
    }
    cat("\n")
  }

  cat("=== FIN PRUEBAS ===\n")
}

# Ejecutar pruebas si se ejecuta directamente
if(interactive()) {
  # probar_sistema_sinonimos()
}