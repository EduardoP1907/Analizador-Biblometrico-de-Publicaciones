#=======================================
# TEST DEFINITIVO - FIX TEMPORAL CORRECTO
# Validar que el filtro temporal funciona como esperado
#=======================================

cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║                    TEST FIX DEFINITIVO TEMPORAL                     ║\n")
cat("║               Validar comportamiento correcto                       ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n\n")

# Cargar sistema corregido
cat("🔧 Cargando motor semántico con fix temporal...\n")
source("www/SOURCE/UTILS/nlp_chatbot_engine_semantic.R")

# Dataset realista con años variados
crear_dataset_realista <- function() {
  data.frame(
    TITULO = c(
      # Papers antiguos (no deberían aparecer con filtro "desde 2020")
      "Machine Learning Algorithms Historical Study",
      "Computer Vision Methods 2015",
      "Neural Networks Research 2018",

      # Papers de 2020 en adelante (SÍ deberían aparecer)
      "Advanced Machine Learning Techniques 2020",
      "Deep Learning Applications 2021",
      "Computer Vision with AI 2022",
      "Neural Network Optimization 2023",
      "Modern AI Algorithms 2024",

      # Papers fuera de cualquier filtro temporal específico
      "Bioinformatics Analysis 2016",
      "Software Engineering Best Practices 2019"
    ),
    RESUMEN = c(
      "Historical perspective on machine learning algorithms developed before 2020",
      "Computer vision methodologies and techniques from 2015 research",
      "Neural network architectures and training methods from 2018",
      "Advanced machine learning techniques and methodologies from 2020",
      "Deep learning applications in various domains studied in 2021",
      "Computer vision enhanced with artificial intelligence in 2022",
      "Neural network optimization strategies developed in 2023",
      "Modern artificial intelligence algorithms and implementations in 2024",
      "Bioinformatics data analysis tools and methods from 2016",
      "Software engineering best practices and methodologies from 2019"
    ),
    AUTOR_PALABRAS_CLAVES = c(
      "machine learning, algorithms, historical",
      "computer vision, methods, 2015",
      "neural networks, research",
      "machine learning, advanced, techniques",
      "deep learning, applications",
      "computer vision, artificial intelligence",
      "neural networks, optimization",
      "artificial intelligence, algorithms, modern",
      "bioinformatics, analysis",
      "software engineering, practices"
    ),
    NOMBRE_AUTOR = c(
      "Smith, J. et al.", "Johnson, A. et al.", "Brown, K. et al.",
      "Davis, R. et al.", "Wilson, M. et al.", "Garcia, L. et al.",
      "Martinez, C. et al.", "Anderson, P. et al.", "Taylor, S. et al.",
      "Thompson, D. et al."
    ),
    ANO = c("2014", "2015", "2018", "2020", "2021", "2022", "2023", "2024", "2016", "2019"),
    SJR = c("1.0", "1.1", "1.2", "1.4", "1.5", "1.6", "1.7", "1.8", "0.9", "1.3"),
    CITADO_POR = c("120", "85", "67", "45", "32", "28", "15", "8", "95", "52"),
    LINK = paste0("https://scopus.com/paper/test_", 1:10),
    stringsAsFactors = FALSE
  )
}

# Ejecutar pruebas específicas
ejecutar_pruebas_comportamiento <- function() {

  cat("📊 Creando dataset realista (años 2014-2024)...\n")
  datos_test <- crear_dataset_realista()
  cat(paste("✅ Dataset creado:", nrow(datos_test), "papers\n"))
  cat("   📅 Años disponibles:", paste(sort(unique(datos_test$ANO)), collapse = ", "), "\n\n")

  # ===== PRUEBA 1: SIN FILTRO TEMPORAL =====
  cat("╭─────────────────────────────────────────╮\n")
  cat("│           PRUEBA 1: SIN FILTRO          │\n")
  cat("│           TEMPORAL (CONTROL)            │\n")
  cat("╰─────────────────────────────────────────╯\n\n")

  consulta_sin_filtro <- "machine learning algorithms"
  cat(paste("🔍 CONSULTA:", consulta_sin_filtro, "\n"))
  cat("📅 EXPECTATIVA: Todos los papers relevantes (cualquier año)\n\n")

  resultado_sin_filtro <- proceso_nlp_chatbot_semantico(consulta_sin_filtro, datos_test)

  if(resultado_sin_filtro$success) {
    cat(paste("✅ Papers encontrados:", resultado_sin_filtro$num_papers, "\n"))
    if(resultado_sin_filtro$num_papers > 0) {
      anos_sin_filtro <- sort(as.numeric(resultado_sin_filtro$papers$ANO))
      cat(paste("📅 Años en resultados:", paste(anos_sin_filtro, collapse = ", "), "\n"))
    }
  }

  # ===== PRUEBA 2: CON FILTRO "DESDE 2020" =====
  cat("\n╭─────────────────────────────────────────╮\n")
  cat("│           PRUEBA 2: DESDE 2020          │\n")
  cat("│         (CASO REPORTADO)                │\n")
  cat("╰─────────────────────────────────────────╯\n\n")

  consulta_desde_2020 <- "machine learning algorithms desde 2020"
  cat(paste("🔍 CONSULTA:", consulta_desde_2020, "\n"))
  cat("📅 EXPECTATIVA: Solo papers de 2020, 2021, 2022, 2023, 2024\n"))
  cat("❌ NO DEBE INCLUIR: 2014, 2015, 2016, 2018, 2019\n\n")

  resultado_desde_2020 <- proceso_nlp_chatbot_semantico(consulta_desde_2020, datos_test)

  if(resultado_desde_2020$success) {
    cat(paste("✅ Papers encontrados:", resultado_desde_2020$num_papers, "\n"))

    if(resultado_desde_2020$num_papers > 0) {
      cat("📋 Papers retornados:\n")
      for(i in 1:nrow(resultado_desde_2020$papers)) {
        paper <- resultado_desde_2020$papers[i, ]
        cat(paste("   ", i, ". AÑO:", paper$ANO, "- TÍTULO:", substr(paper$TITULO, 1, 50), "...\n"))
      }

      # VERIFICACIÓN CRÍTICA
      anos_encontrados <- as.numeric(resultado_desde_2020$papers$ANO)
      papers_antes_2020 <- sum(anos_encontrados < 2020, na.rm = TRUE)
      papers_2020_adelante <- sum(anos_encontrados >= 2020, na.rm = TRUE)

      cat("\n📊 ANÁLISIS CRÍTICO:\n")
      cat(paste("   ✅ Papers de 2020 en adelante:", papers_2020_adelante, "\n"))
      cat(paste("   ❌ Papers anteriores a 2020:", papers_antes_2020, "\n"))

      if(papers_antes_2020 > 0) {
        cat("\n   🚨 PROBLEMA: Aún se incluyen papers anteriores a 2020\n")
        papers_incorrectos <- resultado_desde_2020$papers[anos_encontrados < 2020, ]
        for(i in 1:nrow(papers_incorrectos)) {
          cat(paste("      ❌", papers_incorrectos$ANO[i], "-", papers_incorrectos$TITULO[i], "\n"))
        }
      } else {
        cat("\n   🎉 ÉXITO: Solo papers de 2020 en adelante\n")
      }

      # Información del filtro
      if(!is.null(resultado_desde_2020$filtros_temporales)) {
        cat(paste("\n📅 Filtro aplicado:", resultado_desde_2020$filtros_temporales$descripcion_filtro, "\n"))
        cat(paste("🧹 Query procesada:", resultado_desde_2020$filtros_temporales$query_sin_temporal, "\n"))
      }
    }
  } else {
    cat("❌ Error en búsqueda:", resultado_desde_2020$message, "\n")
  }

  # ===== PRUEBA 3: FILTRO DIFERENTE "DESDE 2022" =====
  cat("\n╭─────────────────────────────────────────╮\n")
  cat("│           PRUEBA 3: DESDE 2022          │\n")
  cat("│         (VALIDAR FLEXIBILIDAD)          │\n")
  cat("╰─────────────────────────────────────────╯\n\n")

  consulta_desde_2022 <- "neural networks desde 2022 en adelante"
  cat(paste("🔍 CONSULTA:", consulta_desde_2022, "\n"))
  cat("📅 EXPECTATIVA: Solo papers de 2022, 2023, 2024\n\n")

  resultado_desde_2022 <- proceso_nlp_chatbot_semantico(consulta_desde_2022, datos_test)

  if(resultado_desde_2022$success && resultado_desde_2022$num_papers > 0) {
    anos_2022 <- as.numeric(resultado_desde_2022$papers$ANO)
    papers_antes_2022 <- sum(anos_2022 < 2022, na.rm = TRUE)

    cat(paste("✅ Papers encontrados:", resultado_desde_2022$num_papers, "\n"))
    cat(paste("📅 Años:", paste(sort(anos_2022), collapse = ", "), "\n"))
    cat(paste("❌ Papers anteriores a 2022:", papers_antes_2022, "\n"))

    if(papers_antes_2022 == 0) {
      cat("🎉 CORRECTO: Solo papers de 2022 en adelante\n")
    } else {
      cat("🚨 ERROR: Incluye papers anteriores a 2022\n")
    }
  }

  # ===== PRUEBA 4: RANGO ESPECÍFICO =====
  cat("\n╭─────────────────────────────────────────╮\n")
  cat("│           PRUEBA 4: RANGO               │\n")
  cat("│         ENTRE 2020 Y 2022               │\n")
  cat("╰─────────────────────────────────────────╯\n\n")

  consulta_rango <- "computer vision entre 2020 y 2022"
  cat(paste("🔍 CONSULTA:", consulta_rango, "\n"))
  cat("📅 EXPECTATIVA: Solo papers de 2020, 2021, 2022\n\n")

  resultado_rango <- proceso_nlp_chatbot_semantico(consulta_rango, datos_test)

  if(resultado_rango$success && resultado_rango$num_papers > 0) {
    anos_rango <- as.numeric(resultado_rango$papers$ANO)
    papers_en_rango <- sum(anos_rango >= 2020 & anos_rango <= 2022, na.rm = TRUE)
    papers_fuera_rango <- sum(anos_rango < 2020 | anos_rango > 2022, na.rm = TRUE)

    cat(paste("✅ Papers encontrados:", resultado_rango$num_papers, "\n"))
    cat(paste("📅 Años:", paste(sort(anos_rango), collapse = ", "), "\n"))
    cat(paste("✅ Papers en rango 2020-2022:", papers_en_rango, "\n"))
    cat(paste("❌ Papers fuera del rango:", papers_fuera_rango, "\n"))

    if(papers_fuera_rango == 0) {
      cat("🎉 CORRECTO: Solo papers en el rango 2020-2022\n")
    } else {
      cat("🚨 ERROR: Incluye papers fuera del rango\n")
    }
  }

  # ===== RESUMEN FINAL =====
  cat("\n╭─────────────────────────────────────────╮\n")
  cat("│              RESUMEN FINAL              │\n")
  cat("│           VALIDACIÓN COMPLETA           │\n")
  cat("╰─────────────────────────────────────────╯\n\n")

  cat("🎯 COMPORTAMIENTO ESPERADO:\n")
  cat("   ✅ Sin filtro temporal: Cualquier año\n")
  cat("   ✅ 'desde 2020': Solo 2020, 2021, 2022, 2023, 2024...\n")
  cat("   ✅ 'desde 2022': Solo 2022, 2023, 2024...\n")
  cat("   ✅ 'entre 2020 y 2022': Solo 2020, 2021, 2022\n\n")

  # Verificar que el filtro se aplicó correctamente comparando con caso sin filtro
  if(resultado_sin_filtro$success && resultado_desde_2020$success) {
    papers_sin_filtro <- resultado_sin_filtro$num_papers
    papers_con_filtro <- resultado_desde_2020$num_papers

    cat("📊 COMPARACIÓN SIN VS CON FILTRO:\n")
    cat(paste("   📈 Sin filtro temporal:", papers_sin_filtro, "papers\n"))
    cat(paste("   📉 Con filtro 'desde 2020':", papers_con_filtro, "papers\n"))

    if(papers_con_filtro <= papers_sin_filtro) {
      cat("   ✅ LÓGICO: El filtro reduce o mantiene la cantidad\n")
    } else {
      cat("   ❌ ERROR: El filtro aumentó la cantidad (imposible)\n")
    }
  }

  cat("\n💡 Para usar en la aplicación:\n")
  cat("   shiny::runApp()\n")
  cat("   Ir a 'Chatbot NLP'\n")
  cat("   Probar: 'machine learning algorithms desde 2020'\n")
}

# Función principal
main <- function() {
  tryCatch({
    ejecutar_pruebas_comportamiento()
  }, error = function(e) {
    cat("❌ ERROR durante las pruebas:\n")
    cat(paste("   ", e$message, "\n"))
  })
}

if(!interactive()) {
  main()
} else {
  cat("💡 Ejecutar: source('test_fix_definitivo_temporal.R'); main()\n")
}