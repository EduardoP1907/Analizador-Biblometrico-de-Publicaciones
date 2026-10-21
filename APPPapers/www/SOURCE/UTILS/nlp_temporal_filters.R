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
