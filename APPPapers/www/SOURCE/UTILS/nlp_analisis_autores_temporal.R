#=======================================
# ANÁLISIS DE PRODUCTIVIDAD DE AUTORES TEMPORAL - FASE 2
# Análisis completo de productividad y evolución de autores a través del tiempo
#=======================================

#' Analizar productividad de autores a través del tiempo
#' @param data Dataset de papers
#' @param ventana_anos Años para agrupar análisis (default: 2)
#' @param min_papers Mínimo de papers para considerar autor (default: 2)
analizar_productividad_autores_temporal <- function(data, ventana_anos = 2, min_papers = 2) {

  # Preparar datos
  data$ANO_NUM <- as.numeric(data$ANO)
  data$CITADO_POR_NUM <- as.numeric(data$CITADO_POR)

  data_valida <- data[!is.na(data$ANO_NUM) & !is.na(data$NOMBRE_AUTOR), ]

  if(nrow(data_valida) == 0) {
    return(list(
      analisis_completado = FALSE,
      mensaje = "No hay datos válidos para análisis de autores"
    ))
  }

  # Crear ventanas temporales
  anos_disponibles <- sort(unique(data_valida$ANO_NUM))
  ano_min <- min(anos_disponibles)
  ano_max <- max(anos_disponibles)

  ventanas <- crear_ventanas_temporales(ano_min, ano_max, ventana_anos)

  # Analizar productividad por ventana
  productividad_por_ventana <- analizar_productividad_por_ventana(data_valida, ventanas)

  # Identificar autores consistentes a través del tiempo
  autores_consistentes <- identificar_autores_consistentes(data_valida, ventanas, min_papers)

  # Analizar evolución de autores individuales
  evolucion_autores <- analizar_evolucion_autores_destacados(data_valida, ventanas)

  # Análisis de colaboración temporal
  colaboracion_temporal <- analizar_colaboracion_temporal(data_valida, ventanas)

  # Identificar autores emergentes y en declive
  autores_emergentes_declive <- identificar_autores_emergentes_declive(productividad_por_ventana)

  return(list(
    analisis_completado = TRUE,
    ventana_anos = ventana_anos,
    num_ventanas = length(ventanas),
    total_autores_analizados = length(unique(data_valida$NOMBRE_AUTOR)),
    productividad_por_ventana = productividad_por_ventana,
    autores_consistentes = autores_consistentes,
    evolucion_autores_destacados = evolucion_autores,
    colaboracion_temporal = colaboracion_temporal,
    autores_emergentes = autores_emergentes_declive$emergentes,
    autores_en_declive = autores_emergentes_declive$en_declive,
    resumen_general = generar_resumen_productividad_temporal(productividad_por_ventana, autores_consistentes)
  ))
}

#' Crear ventanas temporales para análisis
crear_ventanas_temporales <- function(ano_min, ano_max, ventana_anos) {

  ventanas <- list()
  ano_actual <- ano_min

  while(ano_actual <= ano_max) {
    ano_fin <- min(ano_actual + ventana_anos - 1, ano_max)

    ventana <- list(
      nombre = paste0(ano_actual, "-", ano_fin),
      ano_inicio = ano_actual,
      ano_fin = ano_fin,
      punto_medio = ano_actual + (ano_fin - ano_actual) / 2
    )

    ventanas[[length(ventanas) + 1]] <- ventana
    ano_actual <- ano_fin + 1
  }

  return(ventanas)
}

#' Analizar productividad por ventana temporal
analizar_productividad_por_ventana <- function(data_valida, ventanas) {

  resultados_ventanas <- list()

  for(i in 1:length(ventanas)) {
    ventana <- ventanas[[i]]

    # Filtrar papers de esta ventana
    papers_ventana <- data_valida[
      data_valida$ANO_NUM >= ventana$ano_inicio &
      data_valida$ANO_NUM <= ventana$ano_fin,
    ]

    if(nrow(papers_ventana) > 0) {
      # Analizar autores en esta ventana
      stats_autores <- calcular_estadisticas_autores_ventana(papers_ventana)

      resultados_ventanas[[ventana$nombre]] <- list(
        ventana_info = ventana,
        total_papers = nrow(papers_ventana),
        total_autores = length(unique(papers_ventana$NOMBRE_AUTOR)),
        stats_autores = stats_autores,
        productividad_promedio = mean(stats_autores$papers_count, na.rm = TRUE),
        top_autores = head(stats_autores[order(stats_autores$indice_productividad, decreasing = TRUE), ], 5),
        distribucion_productividad = analizar_distribucion_productividad(stats_autores)
      )
    }
  }

  return(resultados_ventanas)
}

#' Calcular estadísticas de autores para una ventana
calcular_estadisticas_autores_ventana <- function(papers_ventana) {

  # Agrupar por autor
  stats_por_autor <- aggregate(
    list(
      papers_count = rep(1, nrow(papers_ventana)),
      citaciones_total = papers_ventana$CITADO_POR_NUM,
      anos_activo = papers_ventana$ANO_NUM
    ),
    by = list(autor = papers_ventana$NOMBRE_AUTOR),
    FUN = function(x) {
      if(is.numeric(x)) {
        if(all(is.na(x))) return(0)
        if(length(unique(x)) > 1) return(length(unique(x))) # Para años: diversidad
        return(sum(x, na.rm = TRUE)) # Para conteos y citaciones: suma
      }
      return(length(x)) # Para conteos
    }
  )

  # Calcular métricas derivadas
  stats_por_autor$citaciones_promedio <- ifelse(
    stats_por_autor$papers_count > 0,
    stats_por_autor$citaciones_total / stats_por_autor$papers_count,
    0
  )

  # Índice de productividad combinado
  stats_por_autor$indice_productividad <- (
    stats_por_autor$papers_count * 0.4 +
    log(1 + stats_por_autor$citaciones_promedio) * 0.4 +
    log(1 + stats_por_autor$anos_activo) * 0.2
  )

  # Clasificación de productividad
  stats_por_autor$clasificacion <- cut(
    stats_por_autor$indice_productividad,
    breaks = quantile(stats_por_autor$indice_productividad, c(0, 0.6, 0.85, 1), na.rm = TRUE),
    labels = c("Productividad Baja", "Productividad Media", "Productividad Alta"),
    include.lowest = TRUE
  )

  return(stats_por_autor)
}

#' Analizar distribución de productividad
analizar_distribucion_productividad <- function(stats_autores) {

  productividad <- stats_autores$indice_productividad

  return(list(
    media = mean(productividad, na.rm = TRUE),
    mediana = median(productividad, na.rm = TRUE),
    desviacion_estandar = sd(productividad, na.rm = TRUE),
    coeficiente_variacion = sd(productividad, na.rm = TRUE) / mean(productividad, na.rm = TRUE),
    cuartiles = quantile(productividad, c(0.25, 0.5, 0.75), na.rm = TRUE),
    concentracion_gini = calcular_gini_productividad(stats_autores$papers_count)
  ))
}

#' Calcular coeficiente de Gini para productividad
calcular_gini_productividad <- function(papers_counts) {

  papers_counts <- papers_counts[!is.na(papers_counts) & papers_counts > 0]
  papers_counts <- sort(papers_counts)
  n <- length(papers_counts)

  if(n == 0 || sum(papers_counts) == 0) return(0)

  gini <- (2 * sum((1:n) * papers_counts)) / (n * sum(papers_counts)) - (n + 1) / n
  return(round(gini, 3))
}

#' Identificar autores consistentes a través del tiempo
identificar_autores_consistentes <- function(data_valida, ventanas, min_papers) {

  # Calcular presencia de cada autor en cada ventana
  presencia_autores <- list()

  for(autor in unique(data_valida$NOMBRE_AUTOR)) {
    papers_autor <- data_valida[data_valida$NOMBRE_AUTOR == autor, ]

    ventanas_activo <- 0
    total_papers <- nrow(papers_autor)
    total_citaciones <- sum(papers_autor$CITADO_POR_NUM, na.rm = TRUE)
    anos_carrera <- max(papers_autor$ANO_NUM) - min(papers_autor$ANO_NUM) + 1

    ventanas_con_papers <- c()

    for(ventana in ventanas) {
      papers_en_ventana <- papers_autor[
        papers_autor$ANO_NUM >= ventana$ano_inicio &
        papers_autor$ANO_NUM <= ventana$ano_fin,
      ]

      if(nrow(papers_en_ventana) >= min_papers) {
        ventanas_activo <- ventanas_activo + 1
        ventanas_con_papers <- c(ventanas_con_papers, ventana$nombre)
      }
    }

    if(ventanas_activo >= 2) { # Al menos 2 ventanas activo
      presencia_autores[[autor]] <- list(
        autor = autor,
        ventanas_activo = ventanas_activo,
        proporcion_ventanas = ventanas_activo / length(ventanas),
        total_papers = total_papers,
        total_citaciones = total_citaciones,
        anos_carrera = anos_carrera,
        papers_por_ano = total_papers / anos_carrera,
        citaciones_promedio = total_citaciones / total_papers,
        ventanas_con_actividad = ventanas_con_papers,
        indice_consistencia = calcular_indice_consistencia(ventanas_activo, length(ventanas), total_papers, anos_carrera)
      )
    }
  }

  # Ordenar por índice de consistencia
  if(length(presencia_autores) > 0) {
    presencia_autores <- presencia_autores[order(sapply(presencia_autores, function(x) x$indice_consistencia), decreasing = TRUE)]
  }

  return(list(
    total_autores_consistentes = length(presencia_autores),
    autores_consistentes = head(presencia_autores, 10), # Top 10
    criterio_minimo_papers = min_papers
  ))
}

#' Calcular índice de consistencia
calcular_indice_consistencia <- function(ventanas_activo, total_ventanas, total_papers, anos_carrera) {

  # Componentes del índice
  persistencia = ventanas_activo / total_ventanas
  productividad = log(1 + total_papers)
  longevidad = log(1 + anos_carrera)

  # Índice combinado (ponderado)
  indice <- (persistencia * 0.5) + (productividad * 0.3) + (longevidad * 0.2)

  return(round(indice, 3))
}

#' Analizar evolución de autores destacados
analizar_evolucion_autores_destacados <- function(data_valida, ventanas, top_n = 5) {

  # Identificar autores más productivos globalmente
  productividad_global <- aggregate(
    list(papers_total = rep(1, nrow(data_valida))),
    by = list(autor = data_valida$NOMBRE_AUTOR),
    FUN = length
  )

  top_autores <- head(productividad_global[order(productividad_global$papers_total, decreasing = TRUE), ], top_n)

  # Analizar evolución temporal de cada top autor
  evoluciones <- list()

  for(i in 1:nrow(top_autores)) {
    autor <- top_autores$autor[i]
    papers_autor <- data_valida[data_valida$NOMBRE_AUTOR == autor, ]

    evolucion_temporal <- data.frame()

    for(ventana in ventanas) {
      papers_ventana <- papers_autor[
        papers_autor$ANO_NUM >= ventana$ano_inicio &
        papers_autor$ANO_NUM <= ventana$ano_fin,
      ]

      evolucion_temporal <- rbind(evolucion_temporal, data.frame(
        ventana = ventana$nombre,
        punto_medio = ventana$punto_medio,
        papers = nrow(papers_ventana),
        citaciones = sum(papers_ventana$CITADO_POR_NUM, na.rm = TRUE),
        citaciones_promedio = ifelse(nrow(papers_ventana) > 0, mean(papers_ventana$CITADO_POR_NUM, na.rm = TRUE), 0)
      ))
    }

    # Calcular tendencias
    if(nrow(evolucion_temporal) >= 3) {
      tendencia_papers <- lm(papers ~ punto_medio, data = evolucion_temporal)
      tendencia_calidad <- lm(citaciones_promedio ~ punto_medio, data = evolucion_temporal)

      evoluciones[[autor]] <- list(
        autor = autor,
        papers_total = top_autores$papers_total[i],
        evolucion_temporal = evolucion_temporal,
        tendencia_productividad = list(
          pendiente = coef(tendencia_papers)[2],
          r_cuadrado = summary(tendencia_papers)$r.squared,
          direccion = ifelse(coef(tendencia_papers)[2] > 0.1, "creciente",
                            ifelse(coef(tendencia_papers)[2] < -0.1, "decreciente", "estable"))
        ),
        tendencia_calidad = list(
          pendiente = coef(tendencia_calidad)[2],
          r_cuadrado = summary(tendencia_calidad)$r.squared,
          direccion = ifelse(coef(tendencia_calidad)[2] > 0.1, "mejorando",
                            ifelse(coef(tendencia_calidad)[2] < -0.1, "deteriorando", "estable"))
        )
      )
    }
  }

  return(evoluciones)
}

#' Analizar colaboración temporal
analizar_colaboracion_temporal <- function(data_valida, ventanas) {

  resultados_colaboracion <- list()

  for(ventana in ventanas) {
    papers_ventana <- data_valida[
      data_valida$ANO_NUM >= ventana$ano_inicio &
      data_valida$ANO_NUM <= ventana$ano_fin,
    ]

    if(nrow(papers_ventana) > 0) {
      # Análisis básico de colaboración (simulado - requeriría datos reales de coautores)
      autores_ventana <- unique(papers_ventana$NOMBRE_AUTOR)
      total_autores <- length(autores_ventana)

      # Simular indicadores de colaboración
      papers_por_autor <- aggregate(
        list(papers = rep(1, nrow(papers_ventana))),
        by = list(autor = papers_ventana$NOMBRE_AUTOR),
        FUN = length
      )

      # Índice de concentración de autoría
      concentracion_autoria <- calcular_gini_productividad(papers_por_autor$papers)

      resultados_colaboracion[[ventana$nombre]] <- list(
        ventana = ventana$nombre,
        total_autores = total_autores,
        total_papers = nrow(papers_ventana),
        papers_por_autor_promedio = mean(papers_por_autor$papers),
        concentracion_autoria = concentracion_autoria,
        diversidad_autoria = total_autores / nrow(papers_ventana)
      )
    }
  }

  return(resultados_colaboracion)
}

#' Identificar autores emergentes y en declive
identificar_autores_emergentes_declive <- function(productividad_por_ventana) {

  if(length(productividad_por_ventana) < 3) {
    return(list(
      emergentes = list(),
      en_declive = list(),
      mensaje = "Necesarias al menos 3 ventanas para detectar tendencias"
    ))
  }

  # Obtener autores que aparecen en múltiples ventanas
  autores_multiples_ventanas <- list()

  for(ventana_nombre in names(productividad_por_ventana)) {
    ventana <- productividad_por_ventana[[ventana_nombre]]

    for(i in 1:nrow(ventana$stats_autores)) {
      autor <- ventana$stats_autores$autor[i]

      if(is.null(autores_multiples_ventanas[[autor]])) {
        autores_multiples_ventanas[[autor]] <- list()
      }

      autores_multiples_ventanas[[autor]][[ventana_nombre]] <- list(
        papers = ventana$stats_autores$papers_count[i],
        productividad = ventana$stats_autores$indice_productividad[i],
        punto_medio = ventana$ventana_info$punto_medio
      )
    }
  }

  # Analizar tendencias de cada autor
  autores_emergentes <- list()
  autores_en_declive <- list()

  for(autor in names(autores_multiples_ventanas)) {
    datos_autor <- autores_multiples_ventanas[[autor]]

    if(length(datos_autor) >= 3) {
      # Crear serie temporal
      puntos_tiempo <- sapply(datos_autor, function(x) x$punto_medio)
      productividades <- sapply(datos_autor, function(x) x$productividad)

      # Calcular tendencia
      tendencia <- lm(productividades ~ puntos_tiempo)
      pendiente <- coef(tendencia)[2]
      r_cuadrado <- summary(tendencia)$r.squared

      if(r_cuadrado > 0.5) { # Tendencia clara
        if(pendiente > 0.1) {
          autores_emergentes[[autor]] <- list(
            autor = autor,
            pendiente = pendiente,
            r_cuadrado = r_cuadrado,
            ventanas_activo = length(datos_autor),
            productividad_inicial = head(productividades, 1),
            productividad_final = tail(productividades, 1)
          )
        } else if(pendiente < -0.1) {
          autores_en_declive[[autor]] <- list(
            autor = autor,
            pendiente = pendiente,
            r_cuadrado = r_cuadrado,
            ventanas_activo = length(datos_autor),
            productividad_inicial = head(productividades, 1),
            productividad_final = tail(productividades, 1)
          )
        }
      }
    }
  }

  # Ordenar por magnitud de cambio
  if(length(autores_emergentes) > 0) {
    autores_emergentes <- autores_emergentes[order(sapply(autores_emergentes, function(x) x$pendiente), decreasing = TRUE)]
  }

  if(length(autores_en_declive) > 0) {
    autores_en_declive <- autores_en_declive[order(sapply(autores_en_declive, function(x) abs(x$pendiente)), decreasing = TRUE)]
  }

  return(list(
    emergentes = head(autores_emergentes, 5),
    en_declive = head(autores_en_declive, 5)
  ))
}

#' Generar resumen de productividad temporal
generar_resumen_productividad_temporal <- function(productividad_por_ventana, autores_consistentes) {

  num_ventanas <- length(productividad_por_ventana)

  if(num_ventanas == 0) {
    return("No se pudieron analizar ventanas de productividad.")
  }

  # Estadísticas generales
  total_papers <- sum(sapply(productividad_por_ventana, function(x) x$total_papers))
  total_autores_unicos <- length(unique(unlist(sapply(productividad_por_ventana, function(x) x$stats_autores$autor))))

  resumen <- paste0(
    "👥 ANÁLISIS DE PRODUCTIVIDAD TEMPORAL COMPLETADO: ",
    num_ventanas, " ventanas analizadas con ", total_papers, " papers de ", total_autores_unicos, " autores únicos. "
  )

  # Información sobre consistencia
  if(autores_consistentes$total_autores_consistentes > 0) {
    resumen <- paste0(
      resumen,
      "Autores consistentes detectados: ", autores_consistentes$total_autores_consistentes, ". "
    )

    if(length(autores_consistentes$autores_consistentes) > 0) {
      mejor_autor <- autores_consistentes$autores_consistentes[[1]]
      resumen <- paste0(
        resumen,
        "Autor más consistente: ", mejor_autor$autor, " (", mejor_autor$total_papers, " papers en ",
        mejor_autor$ventanas_activo, " ventanas)."
      )
    }
  } else {
    resumen <- paste0(resumen, "No se detectaron autores con actividad consistente entre ventanas.")
  }

  return(resumen)
}