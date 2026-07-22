#=======================================
# Script de PRUEBA para Motor NLP
#=======================================

cat("=== Prueba del Motor NLP ===\n")

# 1. Probar instalación básica
cat("1. Verificando dependencias básicas...\n")

paquetes_basicos <- c("stringi", "dplyr")
faltan <- c()

for(pkg in paquetes_basicos) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    faltan <- c(faltan, pkg)
    cat(paste("✗", pkg, "FALTA\n"))
  } else {
    cat(paste("✓", pkg, "OK\n"))
  }
}

if(length(faltan) > 0) {
  cat("\n❌ Instalando dependencias faltantes...\n")
  install.packages(faltan, quiet = TRUE)
}

# 2. Cargar motor NLP simplificado
cat("\n2. Cargando motor NLP...\n")
tryCatch({
  source("www/SOURCE/UTILS/nlp_chatbot_engine_simple.R")
  cat("✓ Motor NLP cargado correctamente\n")
}, error = function(e) {
  cat("✗ Error cargando motor NLP:", e$message, "\n")
  stop("No se puede cargar el motor NLP")
})

# 3. Crear datos de prueba
cat("\n3. Creando datos de prueba...\n")
datos_prueba <- data.frame(
  TITULO = c(
    "Machine Learning for Medical Diagnosis",
    "Neural Networks in Image Processing", 
    "Genetic Algorithms for Optimization",
    "Deep Learning Applications in Healthcare",
    "Computer Vision for Autonomous Vehicles"
  ),
  NOMBRE_AUTOR = c("Juan Pérez", "María García", "Carlos López", "Ana Martín", "Luis Rodríguez"),
  ANO = c(2020, 2021, 2019, 2022, 2023),
  RESUMEN = c(
    "This paper presents a machine learning approach for medical diagnosis using supervised learning techniques",
    "We propose neural network architectures for image processing and computer vision applications",
    "Genetic algorithms are applied to solve complex optimization problems in engineering",
    "Deep learning methods are evaluated for healthcare applications including medical imaging",
    "Computer vision techniques are developed for autonomous vehicle navigation and obstacle detection"
  ),
  LINK = paste0("https://example.com/paper", 1:5),
  SJR = c(1.2, 0.8, 1.5, 2.1, 1.0),
  CITADO_POR = c(15, 8, 25, 45, 12),
  AUTOR_PALABRAS_CLAVES = c(
    "machine learning, medical diagnosis, supervised learning",
    "neural networks, image processing, computer vision",
    "genetic algorithms, optimization, engineering",
    "deep learning, healthcare, medical imaging", 
    "computer vision, autonomous vehicles, navigation"
  ),
  stringsAsFactors = FALSE
)

cat("✓ Datos de prueba creados:", nrow(datos_prueba), "papers\n")

# 4. Probar búsquedas
cat("\n4. Probando búsquedas...\n")

consultas_prueba <- c(
  "machine learning",
  "redes neuronales", 
  "algoritmos genéticos",
  "visión artificial"
)

for(consulta in consultas_prueba) {
  cat(paste("\n--- Probando consulta:", consulta, "---\n"))
  
  tryCatch({
    resultado <- proceso_nlp_chatbot_simple(
      query = consulta,
      data = datos_prueba,
      openai_key = NULL
    )
    
    if(resultado$success) {
      cat("✓ Búsqueda exitosa\n")
      cat("Papers encontrados:", resultado$num_papers, "\n")
      cat("Términos:", paste(resultado$terminos_busqueda, collapse = ", "), "\n")
      cat("Resumen:", substr(resultado$resumen_generado, 1, 100), "...\n")
    } else {
      cat("⚠️ Búsqueda sin resultados:", resultado$message, "\n")
    }
  }, error = function(e) {
    cat("✗ Error en búsqueda:", e$message, "\n")
  })
}

# 5. Verificar funciones auxiliares
cat("\n5. Probando funciones auxiliares...\n")

# Probar limpieza de texto
texto_prueba <- "¡Hola! Este es un TEXTO con acentós y símbolos #$%"
texto_limpio <- limpiar_texto_simple(texto_prueba)
cat("Texto original:", texto_prueba, "\n")
cat("Texto limpio:", texto_limpio, "\n")

# Probar extracción de términos
terminos <- extraer_terminos_simple("machine learning y redes neuronales")
cat("Términos extraídos:", paste(terminos, collapse = ", "), "\n")

cat("\n=== FIN DE PRUEBAS ===\n")

# Verificación final
if(exists("proceso_nlp_chatbot_simple")) {
  cat("🎉 ¡Motor NLP funcionando correctamente!\n")
  cat("Puede usar el chatbot en la aplicación Shiny.\n")
  cat("\nPara ejecutar la app:\n")
  cat("shiny::runApp()\n")
} else {
  cat("❌ Problemas con el motor NLP.\n")
  cat("Revise los errores anteriores.\n")
}