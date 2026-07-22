#=======================================
# ANÁLISIS TEMPORAL AVANZADO - FASE 2
# Detección automática de tendencias y patrones temporales
#=======================================

#' Detectar tendencias temporales automáticamente
#' @param data Dataset de papers
#' @param query_original Query del usuario
#' @param anos_analizar Vector de años a analizar (NULL = todos)
#' @return Lista con análisis temporal completo
detectar_tendencias_temporales <- function(data, query_original, anos_analizar = NULL) {

  # Preparar datos temporales
  if(is.null(anos_analizar)) {
    anos_disponibles <- sort(unique(as.numeric(data$ANO)))
    anos_analizar <- anos_disponibles[anos_disponibles >= (max(anos_disponibles, na.rm = TRUE) - 10)]
  }

  # Filtrar datos por años de análisis
  data_temporal <- data[as.numeric(data$ANO) %in% anos_analizar, ]

  if(nrow(data_temporal) == 0) {
    return(list(
      tendencia_detectada = FALSE,
      mensaje = "No hay suficientes datos para análisis temporal",
      anos_analizados = anos_analizar
    ))
  }

  # Contar papers por año
  conteo_por_ano <- table(data_temporal$ANO)
  conteo_df <- data.frame(
    ano = as.numeric(names(conteo_por_ano)),
    papers = as.numeric(conteo_por_ano)
  )
  conteo_df <- conteo_df[order(conteo_df$ano), ]

  # Calcular tendencia (regresión lineal simple)
  modelo_tendencia <- lm(papers ~ ano, data = conteo_df)
  pendiente <- coef(modelo_tendencia)[2]
  r_cuadrado <- summary(modelo_tendencia)$r.squared

  # Clasificar tendencia
  tendencia_tipo <- "estable"
  if(abs(pendiente) > 0.5 && r_cuadrado > 0.3) {
    if(pendiente > 0) {
      tendencia_tipo <- "creciente"
    } else {
      tendencia_tipo <- "decreciente"
    }
  }

  # Detectar picos y valles
  picos_valles <- detectar_picos_temporales(conteo_df)

  # Análisis de aceleración (cambio en la tendencia)
  aceleracion <- calcular_aceleracion_temporal(conteo_df)

  # Predicción para próximos años
  prediccion <- predecir_tendencia_futura(modelo_tendencia, 2)

  return(list(
    tendencia_detectada = TRUE,
    tipo_tendencia = tendencia_tipo,
    pendiente = round(pendiente, 2),
    r_cuadrado = round(r_cuadrado, 3),
    anos_analizados = range(conteo_df$ano),
    datos_anuales = conteo_df,
    picos_valles = picos_valles,
    aceleracion = aceleracion,
    prediccion_futura = prediccion,
    total_papers = nrow(data_temporal),
    anos_mas_productivos = obtener_anos_mas_productivos(conteo_df),
    resumen_textual = generar_resumen_tendencia(tendencia_tipo, pendiente, conteo_df)
  ))
}

#' Detectar picos y valles en publicaciones
detectar_picos_temporales <- function(conteo_df) {

  if(nrow(conteo_df) < 3) return(list(picos = c(), valles = c()))

  papers <- conteo_df$papers
  anos <- conteo_df$ano

  picos <- c()
  valles <- c()

  for(i in 2:(length(papers)-1)) {
    # Pico: valor mayor que vecinos
    if(papers[i] > papers[i-1] && papers[i] > papers[i+1]) {
      picos <- c(picos, anos[i])
    }
    # Valle: valor menor que vecinos
    if(papers[i] < papers[i-1] && papers[i] < papers[i+1]) {
      valles <- c(valles, anos[i])
    }
  }

  return(list(
    picos = picos,
    valles = valles,
    num_picos = length(picos),
    num_valles = length(valles)
  ))
}

#' Calcular aceleración en la tendencia
calcular_aceleracion_temporal <- function(conteo_df) {

  if(nrow(conteo_df) < 3) return(list(aceleracion = 0, tipo = "insuficientes_datos"))

  # Calcular diferencias año a año
  diferencias <- diff(conteo_df$papers)

  # Calcular aceleración (cambio en las diferencias)
  if(length(diferencias) >= 2) {
    aceleracion_valores <- diff(diferencias)
    aceleracion_promedio <- mean(aceleracion_valores, na.rm = TRUE)

    tipo_aceleracion <- "constante"
    if(abs(aceleracion_promedio) > 0.5) {
      if(aceleracion_promedio > 0) {
        tipo_aceleracion <- "acelerando"
      } else {
        tipo_aceleracion <- "desacelerando"
      }
    }

    return(list(
      aceleracion = round(aceleracion_promedio, 2),
      tipo = tipo_aceleracion,
      diferencias_anuales = diferencias
    ))
  }

  return(list(aceleracion = 0, tipo = "insuficientes_datos"))
}

#' Predecir tendencia futura
predecir_tendencia_futura <- function(modelo, anos_futuros = 2) {

  ultimo_ano <- max(modelo$model$ano)
  anos_prediccion <- (ultimo_ano + 1):(ultimo_ano + anos_futuros)

  predicciones <- predict(modelo, data.frame(ano = anos_prediccion))
  predicciones <- pmax(0, round(predicciones)) # No permitir negativos

  return(data.frame(
    ano = anos_prediccion,
    papers_predichos = predicciones
  ))
}

#' Obtener años más productivos
obtener_anos_mas_productivos <- function(conteo_df, top_n = 3) {

  top_anos <- conteo_df[order(conteo_df$papers, decreasing = TRUE), ]
  top_anos <- head(top_anos, min(top_n, nrow(top_anos)))

  return(list(
    anos = top_anos$ano,
    papers = top_anos$papers,
    porcentaje_total = round((top_anos$papers / sum(conteo_df$papers)) * 100, 1)
  ))
}

#' Generar resumen textual de la tendencia
generar_resumen_tendencia <- function(tipo_tendencia, pendiente, conteo_df) {

  anos_rango <- paste(min(conteo_df$ano), "-", max(conteo_df$ano))
  total_papers <- sum(conteo_df$papers)

  resumen <- switch(tipo_tendencia,
    "creciente" = paste0("📈 TENDENCIA CRECIENTE: La investigación en esta área muestra un crecimiento sostenido durante ", anos_rango, " con un incremento promedio de ", abs(round(pendiente, 1)), " papers por año. Total analizado: ", total_papers, " papers."),

    "decreciente" = paste0("📉 TENDENCIA DECRECIENTE: La investigación en esta área muestra una disminución durante ", anos_rango, " con una reducción promedio de ", abs(round(pendiente, 1)), " papers por año. Total analizado: ", total_papers, " papers."),

    "estable" = paste0("📊 TENDENCIA ESTABLE: La investigación en esta área se mantiene relativamente constante durante ", anos_rango, ". Total analizado: ", total_papers, " papers.")
  )

  return(resumen)
}

#' Análisis de evolución de temas por años
#' @param data Dataset de papers
#' @param query_original Query del usuario
#' @param ventana_anos Años a agrupar para análisis (default: 2)
analizar_evolucion_temas <- function(data, query_original, ventana_anos = 2) {

  # Preparar datos
  data$ANO_NUM <- as.numeric(data$ANO)
  data_valida <- data[!is.na(data$ANO_NUM), ]

  if(nrow(data_valida) == 0) {
    return(list(evolucion_detectada = FALSE, mensaje = "No hay datos válidos con años"))
  }

  # Crear ventanas temporales
  ano_min <- min(data_valida$ANO_NUM)
  ano_max <- max(data_valida$ANO_NUM)

  ventanas <- seq(ano_min, ano_max, by = ventana_anos)
  if(tail(ventanas, 1) < ano_max) {
    ventanas <- c(ventanas, ano_max + 1)
  }

  # Analizar cada ventana
  evolucion_resultados <- list()

  for(i in 1:(length(ventanas)-1)) {
    ano_inicio <- ventanas[i]
    ano_fin <- ventanas[i+1] - 1

    # Filtrar papers de esta ventana
    papers_ventana <- data_valida[data_valida$ANO_NUM >= ano_inicio & data_valida$ANO_NUM <= ano_fin, ]

    if(nrow(papers_ventana) > 0) {
      # Extraer temas principales de títulos y resúmenes
      temas_ventana <- extraer_temas_principales(papers_ventana)

      evolucion_resultados[[paste0(ano_inicio, "-", ano_fin)]] <- list(
        periodo = paste0(ano_inicio, "-", ano_fin),
        num_papers = nrow(papers_ventana),
        temas_principales = temas_ventana,
        autores_activos = length(unique(papers_ventana$NOMBRE_AUTOR)),
        citaciones_promedio = mean(as.numeric(papers_ventana$CITADO_POR), na.rm = TRUE)
      )
    }
  }

  # Detectar cambios en temas entre períodos
  cambios_temas <- detectar_cambios_tematicos(evolucion_resultados)

  return(list(
    evolucion_detectada = TRUE,
    ventana_anos = ventana_anos,
    periodos_analizados = length(evolucion_resultados),
    evolucion_por_periodo = evolucion_resultados,
    cambios_tematicos = cambios_temas,
    resumen_evolucion = generar_resumen_evolucion(evolucion_resultados, cambios_temas)
  ))
}

#' Extraer temas principales de un conjunto de papers
extraer_temas_principales <- function(papers, top_n = 5) {

  # Combinar títulos y palabras clave
  texto_completo <- paste(
    papers$TITULO,
    papers$AUTOR_PALABRAS_CLAVES,
    collapse = " "
  )

  # Limpiar y tokenizar
  texto_limpio <- tolower(texto_completo)
  texto_limpio <- gsub("[^a-z\\s]", " ", texto_limpio)

  # Dividir en palabras
  palabras <- unlist(strsplit(texto_limpio, "\\s+"))
  palabras <- palabras[nchar(palabras) > 3] # Solo palabras de más de 3 caracteres

  # Contar frecuencias
  frecuencias <- table(palabras)
  frecuencias <- sort(frecuencias, decreasing = TRUE)

  # Filtrar palabras comunes (stopwords básicas)
  stopwords_basicas <- c("the", "and", "for", "with", "from", "this", "that", "are", "was", "were",
                        "una", "para", "con", "por", "del", "las", "los", "este", "esta", "como")

  frecuencias <- frecuencias[!names(frecuencias) %in% stopwords_basicas]

  # Retornar top temas
  top_temas <- head(frecuencias, top_n)

  return(list(
    temas = names(top_temas),
    frecuencias = as.numeric(top_temas),
    total_palabras_analizadas = length(palabras)
  ))
}

#' Detectar cambios temáticos entre períodos
detectar_cambios_tematicos <- function(evolucion_resultados) {

  if(length(evolucion_resultados) < 2) {
    return(list(cambios_detectados = FALSE, mensaje = "Necesario al menos 2 períodos"))
  }

  periodos <- names(evolucion_resultados)
  cambios <- list()

  for(i in 1:(length(periodos)-1)) {
    periodo_actual <- evolucion_resultados[[periodos[i]]]
    periodo_siguiente <- evolucion_resultados[[periodos[i+1]]]

    # Comparar temas principales
    temas_actuales <- periodo_actual$temas_principales$temas
    temas_siguientes <- periodo_siguiente$temas_principales$temas

    # Temas nuevos y desaparecidos
    temas_nuevos <- setdiff(temas_siguientes, temas_actuales)
    temas_desaparecidos <- setdiff(temas_actuales, temas_siguientes)
    temas_mantenidos <- intersect(temas_actuales, temas_siguientes)

    cambios[[paste0(periodos[i], "_a_", periodos[i+1])]] <- list(
      de_periodo = periodos[i],
      a_periodo = periodos[i+1],
      temas_nuevos = temas_nuevos,
      temas_desaparecidos = temas_desaparecidos,
      temas_mantenidos = temas_mantenidos,
      cambio_intensidad = length(temas_nuevos) + length(temas_desaparecidos),
      estabilidad = length(temas_mantenidos) / max(length(temas_actuales), 1)
    )
  }

  return(list(
    cambios_detectados = TRUE,
    num_transiciones = length(cambios),
    cambios_detallados = cambios,
    estabilidad_promedio = mean(sapply(cambios, function(x) x$estabilidad), na.rm = TRUE)
  ))
}

#' Generar resumen de evolución temática
generar_resumen_evolucion <- function(evolucion_resultados, cambios_temas) {

  num_periodos <- length(evolucion_resultados)

  if(num_periodos == 0) {
    return("No se detectaron períodos para analizar.")
  }

  # Estadísticas generales
  periodos_nombres <- names(evolucion_resultados)
  periodo_total <- paste(
    strsplit(periodos_nombres[1], "-")[[1]][1],
    "-",
    strsplit(tail(periodos_nombres, 1), "-")[[1]][2]
  )

  total_papers <- sum(sapply(evolucion_resultados, function(x) x$num_papers))

  resumen <- paste0(
    "🔄 EVOLUCIÓN TEMÁTICA DETECTADA: ",
    "Análisis de ", num_periodos, " períodos durante ", periodo_total, ". ",
    "Total de ", total_papers, " papers analizados. "
  )

  # Información sobre cambios
  if(cambios_temas$cambios_detectados) {
    estabilidad_pct <- round(cambios_temas$estabilidad_promedio * 100, 1)

    if(estabilidad_pct > 70) {
      resumen <- paste0(resumen, "La investigación muestra alta estabilidad temática (", estabilidad_pct, "%).")
    } else if(estabilidad_pct > 40) {
      resumen <- paste0(resumen, "La investigación muestra evolución temática moderada (", estabilidad_pct, "% estabilidad).")
    } else {
      resumen <- paste0(resumen, "La investigación muestra alta dinamicidad temática (", estabilidad_pct, "% estabilidad).")
    }
  }

  return(resumen)
}