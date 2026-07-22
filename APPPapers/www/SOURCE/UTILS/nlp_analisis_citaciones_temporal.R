#=======================================
# ANÁLISIS DE CITACIONES TEMPORAL - FASE 2
# Identificación de papers más citados por período y análisis de impacto
#=======================================

#' Analizar papers más citados por período temporal
#' @param data Dataset de papers
#' @param periodo_anos Años a agrupar (default: 3)
#' @param top_papers Número de top papers por período (default: 5)
analizar_papers_mas_citados_temporal <- function(data, periodo_anos = 3, top_papers = 5) {

  # Preparar datos
  data$ANO_NUM <- as.numeric(data$ANO)
  data$CITADO_POR_NUM <- as.numeric(data$CITADO_POR)

  data_valida <- data[!is.na(data$ANO_NUM) & !is.na(data$CITADO_POR_NUM), ]

  if(nrow(data_valida) == 0) {
    return(list(
      analisis_completado = FALSE,
      mensaje = "No hay datos válidos con años y citaciones"
    ))
  }

  # Crear períodos temporales
  ano_min <- min(data_valida$ANO_NUM)
  ano_max <- max(data_valida$ANO_NUM)

  periodos <- crear_periodos_temporales(ano_min, ano_max, periodo_anos)

  # Analizar cada período
  resultados_periodos <- list()

  for(i in 1:nrow(periodos)) {
    periodo_info <- periodos[i, ]
    papers_periodo <- data_valida[
      data_valida$ANO_NUM >= periodo_info$ano_inicio &
      data_valida$ANO_NUM <= periodo_info$ano_fin,
    ]

    if(nrow(papers_periodo) > 0) {
      analisis_periodo <- analizar_periodo_citaciones(
        papers_periodo,
        periodo_info,
        top_papers
      )

      resultados_periodos[[periodo_info$nombre]] <- analisis_periodo
    }
  }

  # Análisis comparativo entre períodos
  analisis_comparativo <- comparar_periodos_citaciones(resultados_periodos)

  # Detectar papers con impacto sostenido
  papers_impacto_sostenido <- detectar_impacto_sostenido(data_valida, periodos)

  return(list(
    analisis_completado = TRUE,
    periodo_anos = periodo_anos,
    num_periodos = length(resultados_periodos),
    resultados_por_periodo = resultados_periodos,
    analisis_comparativo = analisis_comparativo,
    papers_impacto_sostenido = papers_impacto_sostenido,
    resumen_general = generar_resumen_citaciones_temporal(resultados_periodos, analisis_comparativo)
  ))
}

#' Crear períodos temporales para análisis
crear_periodos_temporales <- function(ano_min, ano_max, periodo_anos) {

  periodos <- data.frame()

  ano_actual <- ano_min
  while(ano_actual <= ano_max) {
    ano_fin <- min(ano_actual + periodo_anos - 1, ano_max)

    nombre_periodo <- if(ano_actual == ano_fin) {
      as.character(ano_actual)
    } else {
      paste0(ano_actual, "-", ano_fin)
    }

    periodos <- rbind(periodos, data.frame(
      nombre = nombre_periodo,
      ano_inicio = ano_actual,
      ano_fin = ano_fin,
      duracion = ano_fin - ano_actual + 1
    ))

    ano_actual <- ano_fin + 1
  }

  return(periodos)
}

#' Analizar citaciones en un período específico
analizar_periodo_citaciones <- function(papers_periodo, periodo_info, top_papers) {

  # Ordenar por citaciones
  papers_ordenados <- papers_periodo[order(papers_periodo$CITADO_POR_NUM, decreasing = TRUE), ]

  # Top papers del período
  top_papers_periodo <- head(papers_ordenados, min(top_papers, nrow(papers_ordenados)))

  # Estadísticas del período
  estadisticas <- list(
    total_papers = nrow(papers_periodo),
    citaciones_total = sum(papers_periodo$CITADO_POR_NUM, na.rm = TRUE),
    citaciones_promedio = mean(papers_periodo$CITADO_POR_NUM, na.rm = TRUE),
    citaciones_mediana = median(papers_periodo$CITADO_POR_NUM, na.rm = TRUE),
    citaciones_max = max(papers_periodo$CITADO_POR_NUM, na.rm = TRUE),
    citaciones_min = min(papers_periodo$CITADO_POR_NUM, na.rm = TRUE),
    papers_sin_citas = sum(papers_periodo$CITADO_POR_NUM == 0, na.rm = TRUE),
    papers_alta_citacion = sum(papers_periodo$CITADO_POR_NUM > quantile(papers_periodo$CITADO_POR_NUM, 0.8, na.rm = TRUE), na.rm = TRUE)
  )

  # Análisis de distribución de citaciones
  distribucion <- analizar_distribucion_citaciones(papers_periodo)

  # Identificar autores más productivos del período
  autores_productivos <- analizar_autores_periodo(papers_periodo)

  return(list(
    periodo = periodo_info$nombre,
    anos = paste0(periodo_info$ano_inicio, "-", periodo_info$ano_fin),
    top_papers = preparar_top_papers(top_papers_periodo),
    estadisticas = estadisticas,
    distribucion_citaciones = distribucion,
    autores_destacados = autores_productivos,
    factor_impacto_periodo = calcular_factor_impacto_periodo(papers_periodo)
  ))
}

#' Preparar información de top papers
preparar_top_papers <- function(top_papers) {

  if(nrow(top_papers) == 0) return(list())

  papers_info <- list()

  for(i in 1:nrow(top_papers)) {
    paper <- top_papers[i, ]

    papers_info[[i]] <- list(
      ranking = i,
      titulo = paper$TITULO,
      autor = paper$NOMBRE_AUTOR,
      ano = paper$ANO,
      citaciones = paper$CITADO_POR_NUM,
      sjr = ifelse(!is.na(paper$SJR), paper$SJR, "No disponible"),
      palabras_clave = paper$AUTOR_PALABRAS_CLAVES,
      link = ifelse(!is.na(paper$LINK), paper$LINK, "No disponible")
    )
  }

  return(papers_info)
}

#' Analizar distribución de citaciones
analizar_distribucion_citaciones <- function(papers) {

  citaciones <- papers$CITADO_POR_NUM

  # Cuartiles
  cuartiles <- quantile(citaciones, c(0.25, 0.5, 0.75), na.rm = TRUE)

  # Distribución por rangos
  rangos <- list(
    sin_citas = sum(citaciones == 0, na.rm = TRUE),
    pocas_citas = sum(citaciones > 0 & citaciones <= 5, na.rm = TRUE),
    citas_moderadas = sum(citaciones > 5 & citaciones <= 20, na.rm = TRUE),
    muchas_citas = sum(citaciones > 20 & citaciones <= 50, na.rm = TRUE),
    altamente_citado = sum(citaciones > 50, na.rm = TRUE)
  )

  return(list(
    cuartiles = cuartiles,
    rangos_distribucion = rangos,
    coeficiente_variacion = sd(citaciones, na.rm = TRUE) / mean(citaciones, na.rm = TRUE),
    concentracion_gini = calcular_gini_citaciones(citaciones)
  ))
}

#' Calcular coeficiente de Gini para concentración de citaciones
calcular_gini_citaciones <- function(citaciones) {

  citaciones <- citaciones[!is.na(citaciones)]
  citaciones <- sort(citaciones)
  n <- length(citaciones)

  if(n == 0 || sum(citaciones) == 0) return(0)

  # Fórmula del coeficiente de Gini
  gini <- (2 * sum((1:n) * citaciones)) / (n * sum(citaciones)) - (n + 1) / n

  return(round(gini, 3))
}

#' Analizar autores más productivos por período
analizar_autores_periodo <- function(papers_periodo, top_autores = 3) {

  # Agrupar por autor
  autores_stats <- aggregate(
    list(
      papers = rep(1, nrow(papers_periodo)),
      citaciones_total = papers_periodo$CITADO_POR_NUM
    ),
    by = list(autor = papers_periodo$NOMBRE_AUTOR),
    FUN = function(x) if(is.numeric(x)) sum(x, na.rm = TRUE) else length(x)
  )

  # Calcular métricas adicionales
  autores_stats$citaciones_promedio <- autores_stats$citaciones_total / autores_stats$papers
  autores_stats$indice_productividad <- autores_stats$papers * log(1 + autores_stats$citaciones_promedio)

  # Ordenar por índice de productividad
  autores_ordenados <- autores_stats[order(autores_stats$indice_productividad, decreasing = TRUE), ]

  # Top autores
  top_autores_periodo <- head(autores_ordenados, min(top_autores, nrow(autores_ordenados)))

  return(list(
    total_autores = nrow(autores_stats),
    top_autores = top_autores_periodo,
    autor_mas_papers = autores_stats[which.max(autores_stats$papers), ],
    autor_mas_citado = autores_stats[which.max(autores_stats$citaciones_total), ]
  ))
}

#' Calcular factor de impacto del período
calcular_factor_impacto_periodo <- function(papers_periodo) {

  if(nrow(papers_periodo) == 0) return(0)

  # Factor de impacto simple: citaciones promedio ponderado por antigüedad
  ano_actual <- as.numeric(format(Sys.Date(), "%Y"))

  factor_impacto <- 0
  for(i in 1:nrow(papers_periodo)) {
    paper <- papers_periodo[i, ]
    anos_desde_publicacion <- ano_actual - paper$ANO_NUM + 1

    # Peso decreciente por antigüedad (papers más recientes tienen más peso)
    peso_temporal <- 1 / sqrt(anos_desde_publicacion)
    factor_impacto <- factor_impacto + (paper$CITADO_POR_NUM * peso_temporal)
  }

  return(round(factor_impacto / nrow(papers_periodo), 2))
}

#' Comparar períodos de citaciones
comparar_periodos_citaciones <- function(resultados_periodos) {

  if(length(resultados_periodos) < 2) {
    return(list(comparacion_disponible = FALSE, mensaje = "Necesario al menos 2 períodos"))
  }

  # Extraer métricas de todos los períodos
  metricas_periodos <- data.frame()

  for(periodo_nombre in names(resultados_periodos)) {
    periodo <- resultados_periodos[[periodo_nombre]]

    metricas_periodos <- rbind(metricas_periodos, data.frame(
      periodo = periodo_nombre,
      total_papers = periodo$estadisticas$total_papers,
      citaciones_promedio = periodo$estadisticas$citaciones_promedio,
      factor_impacto = periodo$factor_impacto_periodo,
      papers_alta_citacion = periodo$estadisticas$papers_alta_citacion,
      concentracion_gini = periodo$distribucion_citaciones$concentracion_gini
    ))
  }

  # Identificar mejores y peores períodos
  mejor_periodo_productividad <- metricas_periodos[which.max(metricas_periodos$total_papers), ]
  mejor_periodo_impacto <- metricas_periodos[which.max(metricas_periodos$factor_impacto), ]
  mejor_periodo_citaciones <- metricas_periodos[which.max(metricas_periodos$citaciones_promedio), ]

  # Calcular tendencias entre períodos
  tendencias <- calcular_tendencias_citaciones(metricas_periodos)

  return(list(
    comparacion_disponible = TRUE,
    metricas_todos_periodos = metricas_periodos,
    mejor_periodo_productividad = mejor_periodo_productividad,
    mejor_periodo_impacto = mejor_periodo_impacto,
    mejor_periodo_citaciones = mejor_periodo_citaciones,
    tendencias = tendencias,
    evolucion_calidad = analizar_evolucion_calidad(metricas_periodos)
  ))
}

#' Calcular tendencias en métricas de citación
calcular_tendencias_citaciones <- function(metricas_periodos) {

  if(nrow(metricas_periodos) < 3) return(list(tendencias_disponibles = FALSE))

  # Tendencia en productividad (número de papers)
  tendencia_productividad <- lm(total_papers ~ I(1:nrow(metricas_periodos)), data = metricas_periodos)

  # Tendencia en calidad (citaciones promedio)
  tendencia_calidad <- lm(citaciones_promedio ~ I(1:nrow(metricas_periodos)), data = metricas_periodos)

  # Tendencia en impacto
  tendencia_impacto <- lm(factor_impacto ~ I(1:nrow(metricas_periodos)), data = metricas_periodos)

  return(list(
    tendencias_disponibles = TRUE,
    productividad = list(
      pendiente = coef(tendencia_productividad)[2],
      r_cuadrado = summary(tendencia_productividad)$r.squared,
      direccion = ifelse(coef(tendencia_productividad)[2] > 0.1, "creciente",
                        ifelse(coef(tendencia_productividad)[2] < -0.1, "decreciente", "estable"))
    ),
    calidad = list(
      pendiente = coef(tendencia_calidad)[2],
      r_cuadrado = summary(tendencia_calidad)$r.squared,
      direccion = ifelse(coef(tendencia_calidad)[2] > 0.1, "mejorando",
                        ifelse(coef(tendencia_calidad)[2] < -0.1, "deteriorando", "estable"))
    ),
    impacto = list(
      pendiente = coef(tendencia_impacto)[2],
      r_cuadrado = summary(tendencia_impacto)$r.squared,
      direccion = ifelse(coef(tendencia_impacto)[2] > 0.1, "aumentando",
                        ifelse(coef(tendencia_impacto)[2] < -0.1, "disminuyendo", "estable"))
    )
  ))
}

#' Analizar evolución de calidad investigativa
analizar_evolucion_calidad <- function(metricas_periodos) {

  # Índice de calidad compuesto
  metricas_periodos$indice_calidad <- (
    scale(metricas_periodos$citaciones_promedio)[,1] +
    scale(metricas_periodos$factor_impacto)[,1] -
    scale(metricas_periodos$concentracion_gini)[,1]
  ) / 3

  # Clasificar períodos por calidad
  metricas_periodos$clasificacion_calidad <- cut(
    metricas_periodos$indice_calidad,
    breaks = c(-Inf, -0.5, 0.5, Inf),
    labels = c("Calidad Baja", "Calidad Media", "Calidad Alta")
  )

  return(list(
    indices_calidad = metricas_periodos[, c("periodo", "indice_calidad", "clasificacion_calidad")],
    periodo_mayor_calidad = metricas_periodos[which.max(metricas_periodos$indice_calidad), "periodo"],
    evolucion_general = ifelse(
      tail(metricas_periodos$indice_calidad, 1) > head(metricas_periodos$indice_calidad, 1),
      "mejorando", "deteriorando"
    )
  ))
}

#' Detectar papers con impacto sostenido a través del tiempo
detectar_impacto_sostenido <- function(data_valida, periodos, umbral_citaciones = 10) {

  # Identificar papers que aparecen en múltiples períodos con alta citación
  papers_sostenidos <- list()

  for(i in 1:nrow(data_valida)) {
    paper <- data_valida[i, ]

    if(paper$CITADO_POR_NUM >= umbral_citaciones) {
      # Verificar en cuántos períodos este paper sería considerado relevante
      periodos_relevante <- 0

      for(j in 1:nrow(periodos)) {
        if(paper$ANO_NUM >= periodos[j, "ano_inicio"] &&
           paper$ANO_NUM <= periodos[j, "ano_fin"]) {
          periodos_relevante <- periodos_relevante + 1
        }
      }

      if(periodos_relevante > 0) {
        papers_sostenidos[[length(papers_sostenidos) + 1]] <- list(
          titulo = paper$TITULO,
          autor = paper$NOMBRE_AUTOR,
          ano = paper$ANO,
          citaciones = paper$CITADO_POR_NUM,
          periodos_impacto = periodos_relevante,
          indicador_sostenibilidad = paper$CITADO_POR_NUM / max(1, as.numeric(format(Sys.Date(), "%Y")) - paper$ANO_NUM)
        )
      }
    }
  }

  # Ordenar por indicador de sostenibilidad
  if(length(papers_sostenidos) > 0) {
    papers_sostenidos <- papers_sostenidos[order(sapply(papers_sostenidos, function(x) x$indicador_sostenibilidad), decreasing = TRUE)]
  }

  return(list(
    papers_detectados = length(papers_sostenidos),
    papers_sostenidos = head(papers_sostenidos, 10), # Top 10
    umbral_utilizado = umbral_citaciones
  ))
}

#' Generar resumen de análisis de citaciones temporal
generar_resumen_citaciones_temporal <- function(resultados_periodos, analisis_comparativo) {

  num_periodos <- length(resultados_periodos)

  if(num_periodos == 0) {
    return("No se pudieron analizar períodos de citación.")
  }

  # Estadísticas generales
  total_papers <- sum(sapply(resultados_periodos, function(x) x$estadisticas$total_papers))

  resumen <- paste0(
    "📊 ANÁLISIS TEMPORAL DE CITACIONES COMPLETADO: ",
    num_periodos, " períodos analizados con ", total_papers, " papers totales. "
  )

  # Información sobre el mejor período
  if(analisis_comparativo$comparacion_disponible) {
    mejor_periodo <- analisis_comparativo$mejor_periodo_impacto$periodo
    mejor_impacto <- round(analisis_comparativo$mejor_periodo_impacto$factor_impacto, 2)

    resumen <- paste0(
      resumen,
      "Período de mayor impacto: ", mejor_periodo, " (factor de impacto: ", mejor_impacto, "). "
    )

    # Tendencias
    if(analisis_comparativo$tendencias$tendencias_disponibles) {
      tendencia_calidad <- analisis_comparativo$tendencias$calidad$direccion
      resumen <- paste0(resumen, "Tendencia en calidad: ", tendencia_calidad, ".")
    }
  }

  return(resumen)
}