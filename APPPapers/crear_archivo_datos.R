# Script SIMPLE para crear el archivo de datos necesario
cat("=== Creando archivo de datos para la aplicación ===\n")

# Verificar archivos disponibles
archivos <- c(
  "www/BD/BD_papers_DIINF_2025_04.csv",
  "www/BD/BD_papers_DMCC_2025_04.csv", 
  "www/BD/BD_papers_USACH_ICB.csv"
)

archivo_encontrado <- NULL
for(archivo in archivos) {
  if(file.exists(archivo)) {
    cat(paste("✓ Encontrado:", archivo, "\n"))
    archivo_encontrado <- archivo
    break
  }
}

if(is.null(archivo_encontrado)) {
  stop("❌ No se encontró ningún archivo CSV")
}

# Copiar el primer archivo encontrado como BD_papers.csv
archivo_destino <- "www/BD/BD_papers.csv"

tryCatch({
  file.copy(archivo_encontrado, archivo_destino, overwrite = TRUE)
  cat(paste("✅ Archivo copiado exitosamente a:", archivo_destino, "\n"))
  
  # Verificar que se puede leer
  datos_test <- read.csv(archivo_destino, header = TRUE, sep = "|", quote = "", nrows = 5)
  cat(paste("✅ Verificación: archivo legible,", ncol(datos_test), "columnas\n"))
  
  cat("\n🚀 ¡Listo! Ahora puede ejecutar:\n")
  cat("shiny::runApp()\n")
  
}, error = function(e) {
  cat(paste("❌ Error:", e$message, "\n"))
})