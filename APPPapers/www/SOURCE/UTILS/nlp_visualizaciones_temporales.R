#=======================================
# VISUALIZACIONES TEMPORALES INTERACTIVAS - FASE 2
# Gráficos interactivos para análisis temporal de papers y autores
#=======================================

library(plotly)
library(ggplot2)
library(dplyr)

#' Crear visualización de tendencias temporales
#' @param resultados_tendencias Resultado de detectar_tendencias_temporales()
#' @param titulo Título del gráfico
crear_grafico_tendencias_temporales <- function(resultados_tendencias, titulo = "Tendencias de Publicación por Año") {

  if(!resultados_tendencias$tendencia_detectada) {
    return(NULL)
  }

  datos_anuales <- resultados_tendencias$datos_anuales

  # Crear gráfico base con ggplot2
  p <- ggplot(datos_anuales, aes(x = ano, y = papers)) +
    geom_line(color = "#007bff", size = 1.2, alpha = 0.8) +
    geom_point(color = "#0056b3", size = 3, alpha = 0.9) +
    geom_smooth(method = "lm", se = TRUE, color = "#dc3545", linetype = "dashed", alpha = 0.6) +
    labs(
      title = titulo,
      subtitle = paste0(
        "Tendencia: ", resultados_tendencias$tipo_tendencia,
        " (R² = ", resultados_tendencias$r_cuadrado, ")"
      ),
      x = "Año",
      y = "Número de Papers",
      caption = paste0(
        "Período: ", paste(resultados_tendencias$anos_analizados, collapse = " - "),
        " | Total papers: ", resultados_tendencias$total_papers
      )
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", color = "#2c3e50"),
      plot.subtitle = element_text(size = 12, color = "#34495e"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11, face = "bold"),
      plot.caption = element_text(size = 9, color = "#7f8c8d"),
      panel.grid.minor = element_blank()
    )

  # Marcar picos y valles si existen
  if(length(resultados_tendencias$picos_valles$picos) > 0) {
    picos_data <- datos_anuales[datos_anuales$ano %in% resultados_tendencias$picos_valles$picos, ]
    p <- p + geom_point(data = picos_data, aes(x = ano, y = papers),
                       color = "#28a745", size = 4, shape = 17)
  }

  if(length(resultados_tendencias$picos_valles$valles) > 0) {
    valles_data <- datos_anuales[datos_anuales$ano %in% resultados_tendencias$picos_valles$valles, ]
    p <- p + geom_point(data = valles_data, aes(x = ano, y = papers),
                       color = "#dc3545", size = 4, shape = 25)
  }

  # Convertir a plotly para interactividad
  p_interactivo <- ggplotly(p, tooltip = c("x", "y")) %>%
    layout(
      showlegend = FALSE,
      hoverlabel = list(bgcolor = "white", font = list(size = 12))
    )

  return(p_interactivo)
}

#' Crear visualización de evolución de temas
#' @param resultados_evolucion Resultado de analizar_evolucion_temas()
crear_grafico_evolucion_temas <- function(resultados_evolucion) {

  if(!resultados_evolucion$evolucion_detectada) {
    return(NULL)
  }

  # Preparar datos para visualización
  datos_temas <- data.frame()

  for(periodo_nombre in names(resultados_evolucion$evolucion_por_periodo)) {
    periodo <- resultados_evolucion$evolucion_por_periodo[[periodo_nombre]]

    if(length(periodo$temas_principales$temas) > 0) {
      for(i in 1:length(periodo$temas_principales$temas)) {
        datos_temas <- rbind(datos_temas, data.frame(
          periodo = periodo_nombre,
          tema = periodo$temas_principales$temas[i],
          frecuencia = periodo$temas_principales$frecuencias[i],
          total_papers = periodo$num_papers,
          proporcion = periodo$temas_principales$frecuencias[i] / periodo$temas_principales$total_palabras_analizadas
        ))
      }
    }
  }

  if(nrow(datos_temas) == 0) return(NULL)

  # Crear gráfico de barras apiladas
  p <- ggplot(datos_temas, aes(x = periodo, y = frecuencia, fill = tema)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
    labs(
      title = "Evolución de Temas Principales por Período",
      subtitle = paste0("Análisis de ", length(unique(datos_temas$periodo)), " períodos temporales"),
      x = "Período",
      y = "Frecuencia de Términos",
      fill = "Temas Principales"
    ) +
    scale_fill_brewer(type = "qual", palette = "Set3") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )

  # Convertir a plotly
  p_interactivo <- ggplotly(p, tooltip = c("x", "y", "fill")) %>%
    layout(legend = list(orientation = "h", x = 0.1, y = -0.1))

  return(p_interactivo)
}

#' Crear visualización de citaciones temporales
#' @param resultados_citaciones Resultado de analizar_papers_mas_citados_temporal()
crear_grafico_citaciones_temporales <- function(resultados_citaciones) {

  if(!resultados_citaciones$analisis_completado) {
    return(NULL)
  }

  # Preparar datos de estadísticas por período
  datos_estadisticas <- data.frame()

  for(periodo_nombre in names(resultados_citaciones$resultados_por_periodo)) {
    periodo <- resultados_citaciones$resultados_por_periodo[[periodo_nombre]]
    stats <- periodo$estadisticas

    datos_estadisticas <- rbind(datos_estadisticas, data.frame(
      periodo = periodo_nombre,
      total_papers = stats$total_papers,
      citaciones_promedio = stats$citaciones_promedio,
      citaciones_mediana = stats$citaciones_mediana,
      factor_impacto = periodo$factor_impacto_periodo,
      papers_alta_citacion = stats$papers_alta_citacion
    ))
  }

  # Crear subplot con múltiples métricas
  p1 <- ggplot(datos_estadisticas, aes(x = periodo, y = citaciones_promedio, group = 1)) +
    geom_line(color = "#007bff", size = 1.2) +
    geom_point(color = "#0056b3", size = 3) +
    labs(title = "Citaciones Promedio por Período", y = "Citaciones Promedio") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  p2 <- ggplot(datos_estadisticas, aes(x = periodo, y = factor_impacto, group = 1)) +
    geom_line(color = "#28a745", size = 1.2) +
    geom_point(color = "#155724", size = 3) +
    labs(title = "Factor de Impacto por Período", y = "Factor de Impacto") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Convertir a plotly
  p1_plotly <- ggplotly(p1)
  p2_plotly <- ggplotly(p2)

  # Combinar en subplot
  p_combinado <- subplot(p1_plotly, p2_plotly, nrows = 2, shareX = TRUE) %>%
    layout(title = "Análisis Temporal de Citaciones")

  return(p_combinado)
}

#' Crear visualización de productividad de autores
#' @param resultados_autores Resultado de analizar_productividad_autores_temporal()
crear_grafico_productividad_autores <- function(resultados_autores) {

  if(!resultados_autores$analisis_completado) {
    return(NULL)
  }

  # Preparar datos de productividad por ventana
  datos_productividad <- data.frame()

  for(ventana_nombre in names(resultados_autores$productividad_por_ventana)) {
    ventana <- resultados_autores$productividad_por_ventana[[ventana_nombre]]

    datos_productividad <- rbind(datos_productividad, data.frame(
      ventana = ventana_nombre,
      total_autores = ventana$total_autores,
      total_papers = ventana$total_papers,
      productividad_promedio = ventana$productividad_promedio,
      papers_por_autor = ventana$total_papers / ventana$total_autores
    ))
  }

  # Crear gráfico de barras con doble eje
  p <- ggplot(datos_productividad, aes(x = ventana)) +
    geom_bar(aes(y = total_autores, fill = "Autores"), stat = "identity", alpha = 0.7) +
    geom_line(aes(y = papers_por_autor * 10, group = 1, color = "Papers por Autor"), size = 1.2) +
    geom_point(aes(y = papers_por_autor * 10, color = "Papers por Autor"), size = 3) +
    scale_y_continuous(
      name = "Número de Autores",
      sec.axis = sec_axis(~ . / 10, name = "Papers por Autor")
    ) +
    scale_fill_manual(values = c("Autores" = "#007bff")) +
    scale_color_manual(values = c("Papers por Autor" = "#dc3545")) +
    labs(
      title = "Productividad de Autores por Ventana Temporal",
      subtitle = paste0("Total autores analizados: ", resultados_autores$total_autores_analizados),
      x = "Ventana Temporal",
      fill = "",
      color = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  # Convertir a plotly
  p_interactivo <- ggplotly(p, tooltip = c("x", "y", "fill", "color"))

  return(p_interactivo)
}

#' Crear heatmap de actividad temporal
#' @param data Dataset original
#' @param query_original Query del usuario
crear_heatmap_actividad_temporal <- function(data, query_original) {

  # Preparar datos
  data$ANO_NUM <- as.numeric(data$ANO)
  data_valida <- data[!is.na(data$ANO_NUM), ]

  if(nrow(data_valida) == 0) return(NULL)

  # Crear matriz de actividad (año x autor top)
  # Seleccionar top 15 autores más productivos
  top_autores <- head(
    sort(table(data_valida$NOMBRE_AUTOR), decreasing = TRUE),
    15
  )

  # Filtrar solo estos autores
  data_top_autores <- data_valida[data_valida$NOMBRE_AUTOR %in% names(top_autores), ]

  # Crear matriz de conteo
  matriz_actividad <- table(data_top_autores$NOMBRE_AUTOR, data_top_autores$ANO_NUM)

  # Convertir a data frame para ggplot
  datos_heatmap <- expand.grid(
    autor = rownames(matriz_actividad),
    ano = as.numeric(colnames(matriz_actividad))
  )
  datos_heatmap$papers <- as.vector(matriz_actividad)

  # Crear heatmap
  p <- ggplot(datos_heatmap, aes(x = ano, y = autor, fill = papers)) +
    geom_tile(color = "white", size = 0.1) +
    scale_fill_gradient2(
      low = "#f8f9fa",
      mid = "#007bff",
      high = "#dc3545",
      midpoint = max(datos_heatmap$papers) / 2,
      name = "Papers"
    ) +
    labs(
      title = "Mapa de Calor: Actividad de Autores por Año",
      subtitle = paste0("Top 15 autores más productivos - Query: '", query_original, "'"),
      x = "Año",
      y = "Autor"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )

  # Convertir a plotly
  p_interactivo <- ggplotly(p, tooltip = c("x", "y", "fill")) %>%
    layout(
      xaxis = list(title = "Año"),
      yaxis = list(title = "Autor")
    )

  return(p_interactivo)
}

#' Crear dashboard temporal completo
#' @param data Dataset original
#' @param query_original Query del usuario
crear_dashboard_temporal_completo <- function(data, query_original) {

  cat("🎨 Generando dashboard temporal completo...\\n")

  # Ejecutar todos los análisis
  tendencias <- detectar_tendencias_temporales(data, query_original)
  evolucion_temas <- analizar_evolucion_temas(data, query_original)
  citaciones_temporal <- analizar_papers_mas_citados_temporal(data)
  autores_temporal <- analizar_productividad_autores_temporal(data)

  # Crear visualizaciones
  dashboard <- list()

  # 1. Gráfico de tendencias
  if(tendencias$tendencia_detectada) {
    dashboard$tendencias <- crear_grafico_tendencias_temporales(tendencias)
  }

  # 2. Evolución de temas
  if(evolucion_temas$evolucion_detectada) {
    dashboard$evolucion_temas <- crear_grafico_evolucion_temas(evolucion_temas)
  }

  # 3. Citaciones temporales
  if(citaciones_temporal$analisis_completado) {
    dashboard$citaciones <- crear_grafico_citaciones_temporales(citaciones_temporal)
  }

  # 4. Productividad de autores
  if(autores_temporal$analisis_completado) {
    dashboard$productividad_autores <- crear_grafico_productividad_autores(autores_temporal)
  }

  # 5. Heatmap de actividad
  dashboard$heatmap_actividad <- crear_heatmap_actividad_temporal(data, query_original)

  # Estadísticas del dashboard
  dashboard$estadisticas <- list(
    total_papers_analizados = nrow(data),
    anos_analizados = range(as.numeric(data$ANO), na.rm = TRUE),
    autores_unicos = length(unique(data$NOMBRE_AUTOR)),
    query_original = query_original,
    fecha_generacion = Sys.time()
  )

  cat("✅ Dashboard temporal generado exitosamente\\n")

  return(dashboard)
}

#' Crear resumen visual de métricas temporales
#' @param dashboard Dashboard completo
crear_resumen_metricas_temporales <- function(dashboard) {

  if(is.null(dashboard$estadisticas)) return(NULL)

  stats <- dashboard$estadisticas

  # Crear gráfico de resumen con métricas clave
  metricas_resumen <- data.frame(
    metrica = c("Papers Analizados", "Años Cubiertos", "Autores Únicos"),
    valor = c(
      stats$total_papers_analizados,
      diff(stats$anos_analizados) + 1,
      stats$autores_unicos
    ),
    color = c("#007bff", "#28a745", "#dc3545")
  )

  p <- ggplot(metricas_resumen, aes(x = metrica, y = valor, fill = metrica)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(aes(label = valor), vjust = -0.5, size = 5, fontface = "bold") +
    scale_fill_manual(values = metricas_resumen$color) +
    labs(
      title = "Resumen de Métricas del Análisis Temporal",
      subtitle = paste0("Query: '", stats$query_original, "' | Generado: ", format(stats$fecha_generacion, "%Y-%m-%d %H:%M")),
      x = "",
      y = "Cantidad"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "none",
      axis.text.x = element_text(size = 12, face = "bold"),
      panel.grid.major.x = element_blank()
    )

  # Convertir a plotly
  p_interactivo <- ggplotly(p, tooltip = c("x", "y")) %>%
    layout(showlegend = FALSE)

  return(p_interactivo)
}

#' Exportar dashboard a HTML
#' @param dashboard Dashboard completo
#' @param archivo_salida Nombre del archivo HTML
exportar_dashboard_html <- function(dashboard, archivo_salida = "dashboard_temporal.html") {

  # Crear página HTML con todos los gráficos
  html_content <- paste0("
    <!DOCTYPE html>
    <html>
    <head>
        <title>Dashboard Temporal - Análisis Bibliométrico</title>
        <script src='https://cdn.plot.ly/plotly-latest.min.js'></script>
        <style>
            body { font-family: Arial, sans-serif; margin: 20px; }
            .header { text-align: center; margin-bottom: 30px; }
            .chart-container { margin-bottom: 40px; }
            .footer { text-align: center; margin-top: 40px; color: #666; }
        </style>
    </head>
    <body>
        <div class='header'>
            <h1>📊 Dashboard Temporal - Análisis Bibliométrico</h1>
            <p>Query: '", dashboard$estadisticas$query_original, "'</p>
            <p>Generado: ", format(dashboard$estadisticas$fecha_generacion, "%Y-%m-%d %H:%M"), "</p>
        </div>
  ")

  # Agregar cada gráfico
  graficos_nombres <- c("Tendencias", "Evolución de Temas", "Citaciones", "Productividad de Autores", "Heatmap de Actividad")
  graficos_keys <- c("tendencias", "evolucion_temas", "citaciones", "productividad_autores", "heatmap_actividad")

  for(i in 1:length(graficos_keys)) {
    key <- graficos_keys[i]
    nombre <- graficos_nombres[i]

    if(!is.null(dashboard[[key]])) {
      html_content <- paste0(html_content, "
        <div class='chart-container'>
            <h2>", nombre, "</h2>
            <div id='chart", i, "'></div>
        </div>
      ")
    }
  }

  html_content <- paste0(html_content, "
        <div class='footer'>
            <p>Generated by APPPapers - R Shiny Bibliometric Analysis Tool</p>
        </div>
    </body>
    </html>
  ")

  # Escribir archivo HTML
  writeLines(html_content, archivo_salida)

  cat("📄 Dashboard exportado a:", archivo_salida, "\\n")

  return(archivo_salida)
}