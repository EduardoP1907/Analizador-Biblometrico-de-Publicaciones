# TESTS - BÚSQUEDA ESPECÍFICA POR AUTOR

cat("👤 INICIANDO TESTS - BÚSQUEDA POR AUTOR\n")

# Cargar módulo
source("www/SOURCE/UTILS/nlp_busqueda_por_autor.R")
source("www/SOURCE/UTILS/nlp_chatbot_engine_semantic.R")

# Test principal
test_busqueda_autor_simple <- function() {

  cat("🔍 TEST BÚSQUEDA POR AUTOR\n")

  # Dataset de prueba
  data_test <- data.frame(
    TITULO = c("ML Paper 1", "CV Paper", "AI Study", "ML Paper 2"),
    NOMBRE_AUTOR = c("Manuel Villalobos García", "Smith, John", "Manuel Villalobos", "Johnson, Anna"),
    ANO = c("2023", "2022", "2021", "2023"),
    CITADO_POR = c("15", "23", "31", "8"),
    AUTOR_PALABRAS_CLAVES = c("machine learning", "computer vision", "AI", "data science"),
    LINK = c("link1", "link2", "link3", "link4"),
    stringsAsFactors = FALSE
  )

  # Test detección
  deteccion <- detectar_busqueda_por_autor("publicaciones de manuel villalobos")
  cat("✅ Detección autor:", deteccion$es_busqueda_autor, "\n")

  if(deteccion$es_busqueda_autor) {
    cat("   Autor detectado:", deteccion$nombre_autor_detectado, "\n")

    # Test búsqueda
    resultado <- proceso_busqueda_por_autor("publicaciones de manuel villalobos", data_test)
    cat("   Papers encontrados:", resultado$num_papers, "\n")

    # Test integración
    resultado_integrado <- proceso_nlp_chatbot_semantico("publicaciones de manuel villalobos", data_test)
    cat("   Integración exitosa:", resultado_integrado$success, "\n")

    return(TRUE)
  }

  return(FALSE)
}

# Ejecutar test
cat("🚀 Ejecutando test de búsqueda por autor...\n")
resultado <- test_busqueda_autor_simple()
cat("📊 Test completado:", if(resultado) "✅ ÉXITO" else "❌ FALLÓ", "\n")