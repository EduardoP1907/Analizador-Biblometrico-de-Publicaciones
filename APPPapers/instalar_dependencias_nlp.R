#=======================================
# Script para instalar dependencias del Motor NLP
#=======================================

# Este script instala todas las dependencias necesarias para el 
# Motor de Procesamiento de Lenguaje Natural del chatbot bibliométrico

cat("=== Instalando dependencias para Motor NLP Chatbot ===\n")

# Lista de paquetes necesarios
paquetes_necesarios <- c(
  # Paquetes básicos de Shiny (ya existentes)
  "shiny", "shinyjs", "shinyWidgets", "shinyalert", "shinybusy", 
  "shinycssloaders", "shinyFeedback",
  
  # Manipulación de datos
  "dplyr", "tidyr", "tidyverse", "stringr", "stringi", "stringdist",
  
  # Visualización
  "ggplot2", "plotly", "DT", "fontawesome",
  
  # Procesamiento de texto y NLP
  "tm", "textrank", "wordcloud", "wordcloud2",
  
  # APIs y web scraping
  "httr", "jsonlite", "rvest",
  
  # Nuevas dependencias para NLP avanzado
  "text",           # Para análisis de texto avanzado
  "RcppHunspell",   # Para corrección ortográfica y análisis
  "lexRankr",       # Para resumen extractivo (si no está instalado)
  "writexl",        # Para exportar archivos Excel
  "readr"           # Para lectura eficiente de archivos
)

# Paquetes opcionales avanzados (comentados por defecto)
paquetes_opcionales <- c(
  # "quanteda",     # Análisis cuantitativo de texto
  # "spacyr",       # Interface con spaCy para NLP avanzado  
  # "cleanNLP",     # Pipeline de NLP limpio
  # "udpipe",       # Procesamiento universal de dependencias
  # "openNLPdata",  # Datos para OpenNLP
  # "RWeka"         # Interface con Weka para ML
)

# Función para verificar e instalar paquetes
instalar_si_necesario <- function(paquetes) {
  for(paquete in paquetes) {
    if(!require(paquete, character.only = TRUE, quietly = TRUE)) {
      cat(paste("Instalando", paquete, "...\n"))
      tryCatch({
        install.packages(paquete, dependencies = TRUE)
        library(paquete, character.only = TRUE)
        cat(paste("✓", paquete, "instalado correctamente\n"))
      }, error = function(e) {
        cat(paste("✗ Error instalando", paquete, ":", e$message, "\n"))
        cat(paste("  Intente instalar manualmente con: install.packages('", paquete, "')\n"))
      })
    } else {
      cat(paste("✓", paquete, "ya está instalado\n"))
    }
  }
}

# Instalar paquetes básicos
cat("\n--- Instalando paquetes básicos ---\n")
instalar_si_necesario(paquetes_necesarios)

# Verificar instalaciones críticas para NLP
cat("\n--- Verificando instalaciones críticas ---\n")
paquetes_criticos <- c("httr", "jsonlite", "stringi", "tm", "textrank")

errores_criticos <- c()
for(paquete in paquetes_criticos) {
  if(!require(paquete, character.only = TRUE, quietly = TRUE)) {
    errores_criticos <- c(errores_criticos, paquete)
  }
}

if(length(errores_criticos) > 0) {
  cat("\n⚠️ ATENCIÓN: Los siguientes paquetes críticos no se pudieron instalar:\n")
  cat(paste("-", errores_criticos, collapse = "\n"))
  cat("\nEl motor NLP podría no funcionar correctamente sin estos paquetes.\n")
} else {
  cat("\n✅ Todas las dependencias críticas están instaladas correctamente.\n")
}

# Configuraciones adicionales recomendadas
cat("\n--- Configuraciones recomendadas ---\n")

# Aumentar tiempo límite para descargas
options(timeout = 300)
cat("✓ Tiempo límite de descarga aumentado a 5 minutos\n")

# Configurar encoding UTF-8
options(encoding = "UTF-8")
cat("✓ Encoding configurado a UTF-8\n")

# Configurar repositorio CRAN
options(repos = c(CRAN = "https://cloud.r-project.org/"))
cat("✓ Repositorio CRAN configurado\n")

cat("\n=== Instalación completada ===\n")
cat("\n📝 NOTAS IMPORTANTES:\n")
cat("1. Si tiene una clave de OpenAI, podrá obtener resúmenes más precisos\n")
cat("2. Sin clave OpenAI, el sistema usará procesamiento local (funcional pero básico)\n")
cat("3. Para mejor rendimiento, considere instalar paquetes opcionales comentados\n")
cat("4. El sistema de traducción usa múltiples estrategias de fallback\n")

cat("\n🚀 Su motor NLP está listo para usar!\n")
cat("Ejecute la aplicación Shiny y vaya a la pestaña 'Chatbot NLP'\n")