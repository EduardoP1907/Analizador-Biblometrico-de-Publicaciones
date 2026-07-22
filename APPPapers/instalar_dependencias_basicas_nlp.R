#=======================================
# Script de instalación BÁSICA para Motor NLP
#=======================================

cat("=== Instalando dependencias BÁSICAS para Motor NLP ===\n")

# Solo paquetes esenciales y estables
paquetes_esenciales <- c(
  "httr",        # Para APIs web
  "jsonlite",    # Para JSON
  "stringi",     # Para manipulación de texto
  "tm",          # Para text mining
  "dplyr",       # Para manipulación de datos
  "shiny",       # Ya debería estar instalado
  "shinyjs"      # Para interactividad
)

# Función para instalar solo si es necesario
instalar_basico <- function(paquetes) {
  for(paquete in paquetes) {
    if(!require(paquete, character.only = TRUE, quietly = TRUE)) {
      cat(paste("Instalando", paquete, "...\n"))
      tryCatch({
        install.packages(paquete, dependencies = TRUE, quiet = TRUE)
        library(paquete, character.only = TRUE)
        cat(paste("✓", paquete, "instalado\n"))
      }, error = function(e) {
        cat(paste("✗ Error con", paquete, "\n"))
      })
    } else {
      cat(paste("✓", paquete, "OK\n"))
    }
  }
}

# Instalar paquetes básicos
instalar_basico(paquetes_esenciales)

cat("\n=== Verificación final ===\n")

# Verificar que los críticos están disponibles
criticos <- c("httr", "jsonlite", "stringi", "dplyr")
todos_ok <- TRUE

for(pkg in criticos) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste("✗", pkg, "FALTA\n"))
    todos_ok <- FALSE
  } else {
    cat(paste("✓", pkg, "OK\n"))
  }
}

if(todos_ok) {
  cat("\n🎉 ¡Instalación básica completada!\n")
  cat("El motor NLP funcionará con capacidades básicas.\n")
  cat("Para funcionalidades avanzadas, instale opcionalmente:\n")
  cat("- text (análisis avanzado)\n") 
  cat("- RcppHunspell (corrección ortográfica)\n")
  cat("- lexRankr (resúmenes extractivos)\n")
} else {
  cat("\n❌ Faltan dependencias críticas. Intente:\n")
  cat("install.packages(c('httr', 'jsonlite', 'stringi', 'dplyr'))\n")
}

cat("\n✅ Puede probar el chatbot ahora con funciones básicas.\n")