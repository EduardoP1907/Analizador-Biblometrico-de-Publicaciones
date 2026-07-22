#=======================================
# TEST SIMPLE - VERIFICAR FIX TEMPORAL
#=======================================

cat("🔧 PRUEBA SIMPLE DEL FIX TEMPORAL\n\n")

# Cargar sistema
source("www/SOURCE/UTILS/nlp_chatbot_engine_semantic.R")

# Dataset mínimo y claro
datos_simple <- data.frame(
  TITULO = c(
    "Machine Learning Old Study",      # 2018 - NO debe aparecer con "desde 2020"
    "AI Algorithms Modern Research",   # 2021 - SÍ debe aparecer con "desde 2020"
    "Deep Learning Recent Work",       # 2023 - SÍ debe aparecer con "desde 2020"
    "Computer Vision Classic Method"   # 2019 - NO debe aparecer con "desde 2020"
  ),
  ANO = c("2018", "2021", "2023", "2019"),
  RESUMEN = c(
    "Machine learning study from 2018",
    "AI algorithms research from 2021",
    "Deep learning work from 2023",
    "Computer vision method from 2019"
  ),
  AUTOR_PALABRAS_CLAVES = c("machine learning", "ai algorithms", "deep learning", "computer vision"),
  NOMBRE_AUTOR = c("Author A", "Author B", "Author C", "Author D"),
  SJR = c("1.0", "1.1", "1.2", "1.0"),
  CITADO_POR = c("10", "5", "3", "8"),
  LINK = c("link1", "link2", "link3", "link4"),
  stringsAsFactors = FALSE
)

cat("📊 Dataset de prueba:\n")
for(i in 1:nrow(datos_simple)) {
  cat(paste("   ", i, ". AÑO:", datos_simple$ANO[i], "- TÍTULO:", datos_simple$TITULO[i], "\n"))
}

# Test 1: Sin filtro temporal
cat("\n🔍 TEST 1: machine learning (sin filtro temporal)\n")
resultado1 <- proceso_nlp_chatbot_semantico("machine learning", datos_simple)
cat(paste("Papers encontrados:", resultado1$num_papers, "\n"))
if(resultado1$num_papers > 0) {
  anos1 <- resultado1$papers$ANO
  cat(paste("Años:", paste(anos1, collapse = ", "), "\n"))
}

# Test 2: Con filtro "desde 2020"
cat("\n🔍 TEST 2: machine learning desde 2020 (CON filtro temporal)\n")
resultado2 <- proceso_nlp_chatbot_semantico("machine learning desde 2020", datos_simple)
cat(paste("Papers encontrados:", resultado2$num_papers, "\n"))
if(resultado2$num_papers > 0) {
  anos2 <- resultado2$papers$ANO
  cat(paste("Años:", paste(anos2, collapse = ", "), "\n"))

  # Verificación crítica
  anos_numericos <- as.numeric(anos2)
  papers_antes_2020 <- sum(anos_numericos < 2020, na.rm = TRUE)

  if(papers_antes_2020 > 0) {
    cat("❌ ERROR: Hay papers anteriores a 2020\n")
  } else {
    cat("✅ CORRECTO: Solo papers de 2020 en adelante\n")
  }
} else {
  cat("ℹ️ Sin resultados\n")
}

# Test 3: Verificar detección temporal directa
cat("\n🔍 TEST 3: Verificar detección temporal\n")
deteccion <- detectar_restricciones_temporales("machine learning desde 2020")
cat(paste("Restricción detectada:", deteccion$tiene_restriccion, "\n"))
cat(paste("Año desde:", deteccion$ano_desde, "\n"))
cat(paste("Query limpia:", deteccion$query_sin_temporal, "\n"))

# Test 4: Aplicar filtro directamente
cat("\n🔍 TEST 4: Aplicar filtro directamente\n")
filtrado_directo <- aplicar_filtros_temporales(datos_simple, deteccion)
cat(paste("Papers después del filtro:", nrow(filtrado_directo), "\n"))
if(nrow(filtrado_directo) > 0) {
  cat("Papers filtrados:\n")
  for(i in 1:nrow(filtrado_directo)) {
    cat(paste("   ", filtrado_directo$ANO[i], "-", filtrado_directo$TITULO[i], "\n"))
  }
}

cat("\n🎯 RESUMEN: Si el fix funciona, el TEST 2 debe mostrar solo papers de 2021 y 2023\n")