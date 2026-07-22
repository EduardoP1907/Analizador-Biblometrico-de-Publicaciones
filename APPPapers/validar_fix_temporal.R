#=======================================
# VALIDACIÓN RÁPIDA - FIX TEMPORAL
# Verificar que el problema reportado esté resuelto
#=======================================

cat("🔧 VALIDANDO FIX DE FILTROS TEMPORALES...\n\n")

# Cargar sistema
tryCatch({
  source("www/SOURCE/UTILS/nlp_chatbot_engine_semantic.R")
  cat("✅ Motor semántico con filtros temporales cargado\n")
}, error = function(e) {
  cat("❌ Error cargando motor semántico\n")
  stop("No se puede continuar sin el motor semántico")
})

# Dataset mínimo para prueba
datos_prueba <- data.frame(
  TITULO = c(
    "Machine Learning 2014", "AI Algorithms 2023", "Deep Learning 2024",
    "Computer Vision 2020", "Neural Networks 2022"
  ),
  ANO = c("2014", "2023", "2024", "2020", "2022"),
  RESUMEN = c(
    "ML research from 2014", "AI algorithms study 2023", "Deep learning advances 2024",
    "Computer vision methods 2020", "Neural network optimization 2022"
  ),
  AUTOR_PALABRAS_CLAVES = c("ml, algorithms", "ai, algorithms", "deep learning", "computer vision", "neural networks"),
  NOMBRE_AUTOR = c("Author A", "Author B", "Author C", "Author D", "Author E"),
  SJR = c("1.0", "1.5", "1.2", "1.3", "1.1"),
  CITADO_POR = c("10", "5", "3", "8", "12"),
  LINK = c("link1", "link2", "link3", "link4", "link5"),
  stringsAsFactors = FALSE
)

cat(paste("📊 Dataset de prueba:", nrow(datos_prueba), "papers (años 2014, 2020, 2022, 2023, 2024)\n\n"))

# PRUEBA ESPECÍFICA: Consulta problemática reportada
consulta_reportada <- "papers de algoritmos desde el año 2023 en adelante"

cat(paste("🔍 CONSULTA PROBLEMA:", consulta_reportada, "\n"))
cat("📅 EXPECTATIVA: Solo papers de 2023 y 2024\n")
cat("❌ PROBLEMA ANTERIOR: Incluía papers de 2014\n\n")

# Ejecutar búsqueda
resultado <- tryCatch({
  proceso_nlp_chatbot_semantico(consulta_reportada, datos_prueba)
}, error = function(e) {
  cat("❌ Error ejecutando búsqueda:", e$message, "\n")
  return(NULL)
})

if(!is.null(resultado) && resultado$success) {
  cat("🎯 RESULTADO:\n")
  cat(paste("   📊 Papers encontrados:", resultado$num_papers, "\n"))

  if(resultado$num_papers > 0) {
    cat("   📋 Papers retornados:\n")
    for(i in 1:nrow(resultado$papers)) {
      paper <- resultado$papers[i, ]
      cat(paste("      ", i, ". AÑO:", paper$ANO, "- TÍTULO:", substr(paper$TITULO, 1, 40), "...\n"))
    }

    # VERIFICACIÓN CRÍTICA
    anos_encontrados <- as.numeric(resultado$papers$ANO)
    papers_antes_2023 <- sum(anos_encontrados < 2023, na.rm = TRUE)

    cat("\n📊 ANÁLISIS:\n")
    cat(paste("   📈 Años encontrados:", paste(sort(anos_encontrados), collapse = ", "), "\n"))
    cat(paste("   ❌ Papers anteriores a 2023:", papers_antes_2023, "\n"))

    if(papers_antes_2023 > 0) {
      cat("\n   ❌ PROBLEMA PERSISTE: Aún se incluyen papers anteriores a 2023\n")
      cat("   🔧 Es necesario revisar la implementación del filtro temporal\n")
    } else {
      cat("\n   ✅ PROBLEMA RESUELTO: Solo papers de 2023 en adelante\n")
      cat("   🎉 El fix funciona correctamente\n")
    }

    # Mostrar información del filtro aplicado
    if(!is.null(resultado$filtros_temporales)) {
      cat("\n📅 INFORMACIÓN DEL FILTRO:\n")
      cat(paste("   🎯 Filtro detectado:", resultado$filtros_temporales$tiene_restriccion, "\n"))
      cat(paste("   📊 Descripción:", resultado$filtros_temporales$descripcion_filtro, "\n"))
      cat(paste("   🧹 Query procesada:", resultado$filtros_temporales$query_sin_temporal, "\n"))
    }

  } else {
    cat("   ℹ️ No se encontraron papers (puede ser normal si el dataset es muy pequeño)\n")
  }

} else {
  cat("❌ Error en la búsqueda o resultado inválido\n")
}

# Prueba adicional de detección
cat("\n🔍 VERIFICACIÓN DE DETECCIÓN TEMPORAL:\n")
deteccion <- detectar_restricciones_temporales(consulta_reportada)

cat(paste("   📅 Restricción detectada:", deteccion$tiene_restriccion, "\n"))
if(deteccion$tiene_restriccion) {
  cat(paste("   📊 Tipo:", deteccion$tipo_restriccion, "\n"))
  cat(paste("   📈 Año desde:", deteccion$ano_desde, "\n"))
  cat(paste("   🧹 Query sin temporal:", deteccion$query_sin_temporal, "\n"))
} else {
  cat("   ❌ No se detectó restricción temporal (PROBLEMA)\n")
}

cat("\n╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║                            RESUMEN                                  ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n")
cat("🎯 Fix implementado para consultas temporales\n")
cat("📁 Archivo principal: nlp_temporal_filters.R\n")
cat("🔧 Integrado en: nlp_chatbot_engine_semantic.R\n")
cat("🧪 Pruebas en: test_filtros_temporales.R\n")
cat("\n💡 Para prueba completa: source('test_filtros_temporales.R'); main()\n")
cat("🚀 Para usar: shiny::runApp() y probar la consulta reportada\n")