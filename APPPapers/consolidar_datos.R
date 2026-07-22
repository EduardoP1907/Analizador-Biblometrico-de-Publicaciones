#=======================================
# Script para consolidar datos CSV
#=======================================

cat("=== Consolidando archivos de datos ===\n")

# Verificar que exista el directorio de datos
if(!dir.exists("www/BD")) {
  stop("❌ No existe el directorio www/BD")
}

# Lista de archivos CSV disponibles
archivos_disponibles <- list.files("www/BD", pattern = "\\.csv$", full.names = TRUE)

if(length(archivos_disponibles) == 0) {
  stop("❌ No se encontraron archivos CSV en www/BD")
}

cat("Archivos CSV encontrados:\n")
for(archivo in archivos_disponibles) {
  cat(paste("  -", basename(archivo), "\n"))
}

# Intentar leer y consolidar todos los archivos
datos_consolidados <- NULL

for(archivo in archivos_disponibles) {
  cat(paste("Procesando:", basename(archivo), "...\n"))
  
  tryCatch({
    # Intentar leer con diferentes separadores
    datos_temp <- NULL
    
    # Probar separador pipe (|)
    tryCatch({
      datos_temp <- read.csv(archivo, sep = "|", header = TRUE, quote = "", stringsAsFactors = FALSE)
      cat("  ✓ Leído con separador '|'\n")
    }, error = function(e) {
      # Probar separador coma (,)
      tryCatch({
        datos_temp <- read.csv(archivo, sep = ",", header = TRUE, quote = "\"", stringsAsFactors = FALSE)
        cat("  ✓ Leído con separador ','\n")
      }, error = function(e2) {
        # Probar separador punto y coma (;)
        tryCatch({
          datos_temp <- read.csv(archivo, sep = ";", header = TRUE, quote = "\"", stringsAsFactors = FALSE)
          cat("  ✓ Leído con separador ';'\n")
        }, error = function(e3) {
          cat("  ✗ No se pudo leer el archivo\n")
          return(NULL)
        })
      })
    })
    
    if(!is.null(datos_temp) && nrow(datos_temp) > 0) {
      cat(paste("  Registros:", nrow(datos_temp), "\n"))
      cat(paste("  Columnas:", ncol(datos_temp), "\n"))
      
      # Verificar columnas principales
      columnas_esperadas <- c("UNIVERSIDAD", "NOMBRE_AUTOR", "TITULO", "ANO", "RESUMEN", "LINK")
      columnas_presentes <- intersect(columnas_esperadas, colnames(datos_temp))
      
      if(length(columnas_presentes) >= 4) {
        cat("  ✓ Estructura válida\n")
        
        # Agregar a datos consolidados
        if(is.null(datos_consolidados)) {
          datos_consolidados <- datos_temp
        } else {
          # Asegurar que tengan las mismas columnas
          columnas_comunes <- intersect(colnames(datos_consolidados), colnames(datos_temp))
          
          if(length(columnas_comunes) > 0) {
            datos_consolidados <- rbind(
              datos_consolidados[, columnas_comunes, drop = FALSE],
              datos_temp[, columnas_comunes, drop = FALSE]
            )
          }
        }
      } else {
        cat("  ⚠️ Estructura no válida, ignorando archivo\n")
      }
    }
    
  }, error = function(e) {
    cat(paste("  ✗ Error procesando archivo:", e$message, "\n"))
  })
}

# Verificar si se consolidaron datos
if(is.null(datos_consolidados) || nrow(datos_consolidados) == 0) {
  stop("❌ No se pudieron consolidar datos de ningún archivo")
}

cat("\n=== Datos consolidados ===\n")
cat(paste("Total de registros:", nrow(datos_consolidados), "\n"))
cat(paste("Total de columnas:", ncol(datos_consolidados), "\n"))

# Mostrar muestra de las primeras filas
if(nrow(datos_consolidados) > 0) {
  cat("\nPrimeras columnas disponibles:\n")
  cat(paste(head(colnames(datos_consolidados), 10), collapse = ", "), "\n")
  
  # Verificar columnas críticas
  columnas_criticas <- c("TITULO", "NOMBRE_AUTOR", "RESUMEN", "LINK", "ANO")
  for(col in columnas_criticas) {
    if(col %in% colnames(datos_consolidados)) {
      cat(paste("✓", col, "disponible\n"))
    } else {
      cat(paste("⚠️", col, "faltante\n"))
    }
  }
}

# Guardar archivo consolidado
archivo_salida <- "www/BD/BD_papers.csv"

tryCatch({
  write.table(
    datos_consolidados,
    file = archivo_salida,
    sep = "|",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    fileEncoding = "UTF-8"
  )
  
  cat(paste("\n✅ Datos guardados en:", archivo_salida, "\n"))
  cat("La aplicación Shiny ahora puede iniciarse correctamente.\n")
  
}, error = function(e) {
  cat(paste("❌ Error guardando archivo:", e$message, "\n"))
  
  # Fallback: crear enlace simbólico o copia del primer archivo válido
  if(length(archivos_disponibles) > 0) {
    cat("Intentando usar el primer archivo como fallback...\n")
    file.copy(archivos_disponibles[1], archivo_salida, overwrite = TRUE)
    cat(paste("✓ Copiado", basename(archivos_disponibles[1]), "como BD_papers.csv\n"))
  }
})

cat("\n🚀 ¡Listo! Ahora puede ejecutar:\n")
cat("shiny::runApp()\n")