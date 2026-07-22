# Test simple para verificar que la búsqueda funciona
cat("=== Probando búsqueda simple ===\n")

# Cargar las funciones
source("www/SOURCE/UTILS/nlp_chatbot_engine_simple.R")

# Crear datos de prueba simples
datos_test <- data.frame(
  TITULO = c("Análisis matemático de algoritmos", "Física aplicada", "Matemáticas discretas"),
  NOMBRE_AUTOR = c("Juan", "María", "Pedro"), 
  ANO = c(2020, 2021, 2022),
  RESUMEN = c("Este estudio analiza matemáticas", "Estudio de física", "Matemáticas en computación"),
  LINK = c("link1", "link2", "link3"),
  SJR = c(1.0, 0.5, 1.2),
  CITADO_POR = c(10, 5, 15),
  AUTOR_PALABRAS_CLAVES = c("matemáticas, algoritmos", "física", "matemáticas, computación"),
  stringsAsFactors = FALSE
)

cat("Datos de prueba creados:", nrow(datos_test), "registros\n")

# Probar búsqueda con "matemáticas"
cat("\n--- Probando búsqueda: 'matematicas' ---\n")

resultado <- tryCatch({
  proceso_nlp_chatbot_simple("matematicas", datos_test, NULL)
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  return(NULL)
})

if(!is.null(resultado)) {
  if(resultado$success) {
    cat("✅ Búsqueda exitosa!\n")
    cat("Papers encontrados:", resultado$num_papers, "\n")
    cat("Resumen:", resultado$resumen_generado, "\n")
  } else {
    cat("⚠️ Sin resultados:", resultado$message, "\n")
  }
} else {
  cat("❌ Error en la búsqueda\n")
}

cat("\n--- Probando funciones individuales ---\n")

# Probar limpiar_texto_simple
test_texto <- limpiar_texto_simple("Matemáticas y Análisis")
cat("Texto limpio:", test_texto, "\n")

# Probar extraer_terminos_simple
test_terminos <- extraer_terminos_simple("matemáticas aplicadas")
cat("Términos:", paste(test_terminos, collapse = ", "), "\n")

cat("\n=== Fin de pruebas ===\n")