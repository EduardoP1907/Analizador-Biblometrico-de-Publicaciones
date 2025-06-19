procesar_archivos_csv <- function(carpeta, indice, revistas, salida = "CONSOLIDADO_CLASIFICADO.xlsx") {
  
  # 1. Listar archivos CSV en carpeta
  archivos_csv <- list.files(carpeta, pattern = "\\.csv$", full.names = TRUE)
  
  # 2. Inicializar consolidado
  consolidado <- data.frame()
  
  # 3. Procesar cada archivo
  for (archivo_actual in archivos_csv) {
    cat("🔄 Procesando:", archivo_actual, "\n")
    
    datos <- read.csv(archivo_actual, header = TRUE, stringsAsFactors = FALSE)
    
    # 🔥 Eliminar símbolo "|" y comillas simples o dobles en todas las celdas de texto
    datos <- as.data.frame(lapply(datos, function(col) {
      if (is.character(col)) gsub("[\"'|]", "", col) else col
    }), stringsAsFactors = FALSE)
    
    datos <- limpiar_columnas_y_renombrar(datos)
    datos <- agregar_desde_indice(datos, archivo_actual, indice)
    datos <- anotar_con_sjr(datos, revistas)
    
    # Acumular resultados
    consolidado <- dplyr::bind_rows(consolidado, datos)
  }
  
  # 4. Guardar resultado como Excel
  writexl::write_xlsx(consolidado, salida)
  cat("✅ Archivo consolidado guardado en:", salida, "\n")
  
  return(consolidado)
}
