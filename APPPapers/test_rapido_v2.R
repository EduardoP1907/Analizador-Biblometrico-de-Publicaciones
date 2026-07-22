# Test rápido de la versión 2 sin errores
cat("=== PROBANDO VERSIÓN 2 SIN ERRORES ===\n")

# Cargar la nueva versión
source("www/SOURCE/UTILS/nlp_chatbot_engine_simple_v2.R")

# Datos de prueba simples
datos_test <- data.frame(
  TITULO = c(
    "Análisis matemático de algoritmos",
    "Machine Learning en medicina", 
    "Redes neuronales artificiales",
    "Manuel García - Biografía",
    "Estudios de 2022 en física"
  ),
  NOMBRE_AUTOR = c("Juan Pérez", "María López", "Carlos García", "Manuel García", "Ana Martín"),
  ANO = c(2020, 2021, 2022, 2019, 2022),
  RESUMEN = c(
    "Este trabajo analiza métodos matemáticos para algoritmos",
    "Machine learning aplicado al diagnóstico médico",
    "Desarrollo de redes neuronales para procesamiento",
    "Biografía del matemático Manuel García",
    "Investigación en física teórica del año 2022"
  ),
  LINK = paste0("link", 1:5),
  SJR = c(1.2, 0.8, 1.5, 0.3, 2.1),
  CITADO_POR = c(15, 8, 25, 2, 45),
  stringsAsFactors = FALSE
)

cat("Datos de prueba:", nrow(datos_test), "papers\n")

# Probar las 3 consultas que fallaron
consultas <- c("matematicas", "manuel", "machine learning")

for(consulta in consultas) {
  cat(paste("\n--- Probando:", consulta, "---\n"))
  
  resultado <- tryCatch({
    proceso_nlp_chatbot_simple(consulta, datos_test, NULL)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
  
  if(resultado$success) {
    cat("✅ ÉXITO - Papers encontrados:", resultado$num_papers, "\n")
    if(resultado$num_papers > 0) {
      cat("Títulos:\n")
      for(i in 1:min(2, nrow(resultado$papers))) {
        cat(paste("  -", resultado$papers$TITULO[i], "\n"))
      }
    }
    cat("Resumen:", substr(resultado$resumen_generado, 1, 80), "...\n")
  } else {
    if("error" %in% names(resultado)) {
      cat("❌ ERROR:", resultado$error, "\n")
    } else {
      cat("⚠️ Sin resultados:", resultado$message, "\n")
    }
  }
}

cat("\n🎯 Test completado. Si ve ✅ en todas, está solucionado.\n")