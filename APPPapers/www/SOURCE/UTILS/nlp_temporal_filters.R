#=======================================
# Filtros Temporales Inteligentes - FIX CRÍTICO
# Detección automática de restricciones de fecha en consultas
#=======================================

library(stringi, warn.conflicts = FALSE)

#' Sistema de detección y aplicación automática de filtros temporales
#'
#' Detecta automáticamente restricciones temporales en consultas como:
#' - "papers desde 2023"
#' - "algoritmos del 2020 en adelante"
#' - "estudios recientes"
#' - "últimos 3 años"
#'
#' @author Claude Code - Fix Crítico Fase 1
#' @date 2025-09-29

#' Detectar restricciones temporales en una consulta
#'
#' @param query Consulta del usuario
#' @return Lista con filtros temporales detectados
detectar_restricciones_temporales <- function(query) {

  if(is.null(query) || nchar(trimws(query)) == 0) {
    return(list(
      tiene_restriccion = FALSE,
      ano_desde = NULL,
      ano_hasta = NULL,
      query_sin_temporal = query,
      tipo_restriccion = "ninguna"
    ))
  }

  query_limpia <- tolower(trimws(query))
  ano_actual <- as.numeric(format(Sys.Date(), "%Y"))

  # ===== PATRONES DE DETECCIÓN TEMPORAL =====

  # 1. AÑOS ESPECÍFICOS: "desde 2023", "del 2020", "año 2022"
  patron_ano_desde <- "(?:desde|from|a partir de|after|después de)\\s*(\\d{4})"
  patron_ano_hasta <- "(?:hasta|until|before|antes de)\\s*(\\d{4})"
  patron_ano_en <- "(?:en el|en|del|in)\\s*(\\d{4})"
  patron_ano_adelante <- "(\\d{4})\\s*(?:en adelante|onwards?|forward|hacia adelante)"

  # 2. RANGOS: "entre 2020 y 2023", "from 2019 to 2022"
  patron_rango <- "(?:entre|between|from)\\s*(\\d{4})\\s*(?:y|and|to|-|hasta)\\s*(\\d{4})"

  # 3. RELATIVOS: "últimos 3 años", "recent papers", "estudios recientes"
  patron_ultimos_anos <- "(?:últimos?|last|past)\\s*(\\d+)\\s*(?:años?|years?)"
  patron_recientes <- "(?:recientes?|recent|new|newest|latest|actuales?|current)"

  # 4. PERÍODOS: "última década", "siglo XXI"
  patron_decada <- "(?:última|last)\\s*(?:década|decade)"
  patron_siglo21 <- "(?:siglo\\s*xxi|21st\\s*century|century\\s*21)"

  # ===== APLICAR DETECCIÓN =====

  ano_desde <- NULL
  ano_hasta <- NULL
  tipo_restriccion <- "ninguna"
  query_temporal_removida <- query_limpia

  # 1. Detectar "desde año"
  match_desde <- regexec(patron_ano_desde, query_limpia, perl = TRUE)
  if(match_desde[[1]][1] != -1) {
    ano_capturado <- regmatches(query_limpia, match_desde)[[1]][2]
    ano_desde <- as.numeric(ano_capturado)
    tipo_restriccion <- "desde_ano"
    query_temporal_removida <- gsub(patron_ano_desde, "", query_temporal_removida, perl = TRUE)
  }

  # 2. Detectar "año en adelante"
  if(is.null(ano_desde)) {
    match_adelante <- regexec(patron_ano_adelante, query_limpia, perl = TRUE)
    if(match_adelante[[1]][1] != -1) {
      ano_capturado <- regmatches(query_limpia, match_adelante)[[1]][2]
      ano_desde <- as.numeric(ano_capturado)
      tipo_restriccion <- "ano_adelante"
      query_temporal_removida <- gsub(patron_ano_adelante, "", query_temporal_removida, perl = TRUE)
    }
  }

  # 3. Detectar rangos de años
  if(is.null(ano_desde)) {
    match_rango <- regexec(patron_rango, query_limpia, perl = TRUE)
    if(match_rango[[1]][1] != -1) {
      anos_capturados <- regmatches(query_limpia, match_rango)[[1]]
      ano_desde <- as.numeric(anos_capturados[2])
      ano_hasta <- as.numeric(anos_capturados[3])
      tipo_restriccion <- "rango_anos"
      query_temporal_removida <- gsub(patron_rango, "", query_temporal_removida, perl = TRUE)
    }
  }

  # 4. Detectar "últimos X años"
  if(is.null(ano_desde)) {
    match_ultimos <- regexec(patron_ultimos_anos, query_limpia, perl = TRUE)
    if(match_ultimos[[1]][1] != -1) {
      num_anos <- as.numeric(regmatches(query_limpia, match_ultimos)[[1]][2])
      ano_desde <- ano_actual - num_anos
      tipo_restriccion <- paste0("ultimos_", num_anos, "_anos")
      query_temporal_removida <- gsub(patron_ultimos_anos, "", query_temporal_removida, perl = TRUE)
    }
  }

  # 5. Detectar términos "recientes"
  if(is.null(ano_desde)) {
    if(grepl(patron_recientes, query_limpia, perl = TRUE)) {
      ano_desde <- ano_actual - 3  # Últimos 3 años por defecto
      tipo_restriccion <- "recientes"
      query_temporal_removida <- gsub(patron_recientes, "", query_temporal_removida, perl = TRUE)
    }
  }

  # 6. Detectar "última década"
  if(is.null(ano_desde)) {
    if(grepl(patron_decada, query_limpia, perl = TRUE)) {
      ano_desde <- ano_actual - 10
      tipo_restriccion <- "ultima_decada"
      query_temporal_removida <- gsub(patron_decada, "", query_temporal_removida, perl = TRUE)
    }
  }

  # 7. Detectar "siglo XXI"
  if(is.null(ano_desde)) {
    if(grepl(patron_siglo21, query_limpia, perl = TRUE)) {
      ano_desde <- 2000
      tipo_restriccion <- "siglo_xxi"
      query_temporal_removida <- gsub(patron_siglo21, "", query_temporal_removida, perl = TRUE)
    }
  }

  # ===== LIMPIAR QUERY SIN TÉRMINOS TEMPORALES =====

  # Remover palabras temporales residuales
  palabras_temporales_extra <- c("en adelante", "onwards?", "forward", "hacia adelante",
                                "recientes?", "recent", "actuales?", "current", "new", "latest")

  for(palabra in palabras_temporales_extra) {
    query_temporal_removida <- gsub(palabra, "", query_temporal_removida, ignore.case = TRUE)
  }

  # Limpiar espacios múltiples y signos de puntuación residuales
  query_temporal_removida <- gsub("\\s+", " ", query_temporal_removida)
  query_temporal_removida <- gsub("^\\s*,|,\\s*$", "", query_temporal_removida)  # Comas al inicio/final
  query_temporal_removida <- trimws(query_temporal_removida)

  # Si quedó muy vacía, mantener query original sin términos temporales obvios
  if(nchar(query_temporal_removida) < 3) {
    query_temporal_removida <- gsub("\\b\\d{4}\\b", "", query)  # Solo remover años
    query_temporal_removida <- trimws(query_temporal_removida)
  }

  # ===== VALIDACIONES =====

  # Validar años lógicos
  if(!is.null(ano_desde) && (ano_desde < 1990 || ano_desde > ano_actual + 5)) {
    ano_desde <- NULL
    tipo_restriccion <- "ninguna"
  }

  if(!is.null(ano_hasta) && (ano_hasta < 1990 || ano_hasta > ano_actual + 5)) {
    ano_hasta <- NULL
  }

  # Si año_desde > año_hasta, intercambiar
  if(!is.null(ano_desde) && !is.null(ano_hasta) && ano_desde > ano_hasta) {
    temp <- ano_desde
    ano_desde <- ano_hasta
    ano_hasta <- temp
  }

  # ===== RESULTADO FINAL =====

  tiene_restriccion <- !is.null(ano_desde) || !is.null(ano_hasta)

  return(list(
    tiene_restriccion = tiene_restriccion,
    ano_desde = ano_desde,
    ano_hasta = ano_hasta,
    query_sin_temporal = query_temporal_removida,
    query_original = query,
    tipo_restriccion = tipo_restriccion,
    descripcion_filtro = generar_descripcion_filtro(ano_desde, ano_hasta, tipo_restriccion)
  ))
}

#' Aplicar filtros temporales a un dataset de papers
#'
#' @param papers Dataset de papers
#' @param restricciones_temporales Resultado de detectar_restricciones_temporales()
#' @return Dataset filtrado por fecha
aplicar_filtros_temporales <- function(papers, restricciones_temporales) {

  if(!restricciones_temporales$tiene_restriccion || nrow(papers) == 0) {
    return(papers)
  }

  # Asegurar que existe columna ANO
  if(!"ANO" %in% colnames(papers)) {
    warning("Columna 'ANO' no encontrada en el dataset. No se pueden aplicar filtros temporales.")
    return(papers)
  }

  # Convertir años a numérico y manejar NAs
  papers$ANO_num <- suppressWarnings(as.numeric(papers$ANO))
  papers_con_ano <- papers[!is.na(papers$ANO_num), ]

  if(nrow(papers_con_ano) == 0) {
    warning("No hay papers con años válidos para filtrar.")
    return(data.frame())  # Dataset vacío
  }

  # Aplicar filtros
  papers_filtrados <- papers_con_ano

  if(!is.null(restricciones_temporales$ano_desde)) {
    papers_filtrados <- papers_filtrados[papers_filtrados$ANO_num >= restricciones_temporales$ano_desde, ]
  }

  if(!is.null(restricciones_temporales$ano_hasta)) {
    papers_filtrados <- papers_filtrados[papers_filtrados$ANO_num <= restricciones_temporales$ano_hasta, ]
  }

  # Remover columna temporal auxiliar
  papers_filtrados$ANO_num <- NULL

  return(papers_filtrados)
}

#' Generar descripción legible del filtro aplicado
generar_descripcion_filtro <- function(ano_desde, ano_hasta, tipo_restriccion) {

  if(is.null(ano_desde) && is.null(ano_hasta)) {
    return("Sin restricciones temporales")
  }

  if(!is.null(ano_desde) && !is.null(ano_hasta)) {
    return(paste0("Papers entre ", ano_desde, " y ", ano_hasta))
  }

  if(!is.null(ano_desde)) {
    return(paste0("Papers desde ", ano_desde, " en adelante"))
  }

  if(!is.null(ano_hasta)) {
    return(paste0("Papers hasta ", ano_hasta))
  }

  return("Filtro temporal desconocido")
}

#' Función de prueba para validar detección temporal
probar_deteccion_temporal <- function() {
  cat("=== PROBANDO DETECCIÓN DE RESTRICCIONES TEMPORALES ===\n\n")

  # Casos de prueba realistas
  casos_prueba <- c(
    "papers de algoritmos desde el año 2023 en adelante",
    "machine learning del 2020 en adelante",
    "estudios recientes sobre redes neuronales",
    "últimos 5 años de investigación en computer vision",
    "inteligencia artificial entre 2019 y 2023",
    "optimización hasta 2022",
    "algoritmos genéticos en el 2021",
    "data mining papers from 2020 onwards",
    "recent studies on deep learning",
    "última década de investigación en NLP",
    "papers del siglo XXI sobre bioinformática",
    "machine learning sin restricciones temporales"
  )

  for(caso in casos_prueba) {
    cat(paste("🔍 CONSULTA:", caso, "\n"))

    resultado <- detectar_restricciones_temporales(caso)

    cat(paste("   📅 Tiene restricción:", ifelse(resultado$tiene_restriccion, "✅ SÍ", "❌ NO"), "\n"))

    if(resultado$tiene_restriccion) {
      cat(paste("   📊 Tipo:", resultado$tipo_restriccion, "\n"))
      cat(paste("   📈 Desde:", ifelse(is.null(resultado$ano_desde), "N/A", resultado$ano_desde), "\n"))
      cat(paste("   📉 Hasta:", ifelse(is.null(resultado$ano_hasta), "N/A", resultado$ano_hasta), "\n"))
      cat(paste("   🎯 Descripción:", resultado$descripcion_filtro, "\n"))
      cat(paste("   🧹 Query limpia:", resultado$query_sin_temporal, "\n"))
    }

    cat("\n")
  }

  cat("=== FIN PRUEBAS DETECCIÓN TEMPORAL ===\n")
}

#' Función de prueba con dataset real
probar_filtros_con_dataset <- function() {
  cat("=== PROBANDO FILTROS CON DATASET REAL ===\n\n")

  # Crear dataset de prueba con años variados
  papers_test <- data.frame(
    TITULO = c(
      "Machine Learning 2014", "AI Algorithms 2019", "Deep Learning 2020",
      "Computer Vision 2021", "Neural Networks 2022", "Data Science 2023",
      "Optimization 2024", "Bioinformatics 2018"
    ),
    ANO = c("2014", "2019", "2020", "2021", "2022", "2023", "2024", "2018"),
    RESUMEN = paste("Resumen paper año", c("2014", "2019", "2020", "2021", "2022", "2023", "2024", "2018")),
    stringsAsFactors = FALSE
  )

  cat(paste("📊 Dataset original:", nrow(papers_test), "papers (2014-2024)\n\n"))

  # Probar consulta problemática reportada
  consulta_problema <- "papers de algoritmos desde el año 2023 en adelante"

  cat(paste("🔍 CONSULTA PROBLEMA:", consulta_problema, "\n"))

  restricciones <- detectar_restricciones_temporales(consulta_problema)

  cat("📅 DETECCIÓN:\n")
  cat(paste("   ✅ Detectado restricción:", restricciones$tiene_restriccion, "\n"))
  cat(paste("   📈 Año desde:", restricciones$ano_desde, "\n"))
  cat(paste("   🧹 Query limpia:", restricciones$query_sin_temporal, "\n"))

  # Aplicar filtros
  papers_filtrados <- aplicar_filtros_temporales(papers_test, restricciones)

  cat("\n🎯 RESULTADO FILTRADO:\n")
  cat(paste("   📊 Papers encontrados:", nrow(papers_filtrados), "\n"))

  if(nrow(papers_filtrados) > 0) {
    cat("   📋 Papers que cumplen criterio:\n")
    for(i in 1:nrow(papers_filtrados)) {
      cat(paste("      -", papers_filtrados$TITULO[i], "(", papers_filtrados$ANO[i], ")\n"))
    }
  }

  # Verificar que NO incluye papers anteriores a 2023
  papers_incorrectos <- papers_filtrados[as.numeric(papers_filtrados$ANO) < 2023, ]

  if(nrow(papers_incorrectos) > 0) {
    cat("   ❌ ERROR: Encontrados papers anteriores a 2023:\n")
    for(i in 1:nrow(papers_incorrectos)) {
      cat(paste("      -", papers_incorrectos$TITULO[i], "(", papers_incorrectos$ANO[i], ")\n"))
    }
  } else {
    cat("   ✅ CORRECTO: No hay papers anteriores a 2023\n")
  }

  cat("\n=== FIN PRUEBA CON DATASET ===\n")
}

# Ejecutar pruebas si se carga directamente
if(interactive()) {
  # probar_deteccion_temporal()
  # probar_filtros_con_dataset()
}