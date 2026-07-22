#=======================================
# TESTS COMPLETOS - FASE 2 ANÁLISIS TEMPORAL AVANZADO
# Validación integral de todas las funcionalidades implementadas
#=======================================

cat("🧪 INICIANDO TESTS COMPLETOS - FASE 2 ANÁLISIS TEMPORAL AVANZADO\n")
cat("=" * 70, "\n\n")

# Cargar todas las funciones necesarias
source("www/SOURCE/UTILS/nlp_chatbot_engine_semantic.R")

# ===== DATASET DE PRUEBA COMPREHENSIVO =====
crear_dataset_prueba_completo <- function() {

  # Dataset con variedad temporal, de autores y citaciones
  data.frame(
    TITULO = c(
      # 2019 - Fundacional
      "Machine Learning Foundations for Computer Science Applications",
      "Introduction to Neural Networks and Deep Learning",
      "Optimization Algorithms in Artificial Intelligence",

      # 2020 - Crecimiento
      "Advanced Machine Learning Techniques for Image Processing",
      "Deep Learning Applications in Medical Diagnosis",
      "Genetic Algorithms for Complex Optimization Problems",
      "Natural Language Processing with Transformer Models",
      "Computer Vision Systems for Autonomous Navigation",

      # 2021 - Expansión
      "Reinforcement Learning in Robotics Applications",
      "Ensemble Methods for Improved Machine Learning Performance",
      "Graph Neural Networks for Social Network Analysis",
      "Federated Learning for Privacy-Preserving AI",
      "Explainable AI for Medical Decision Support",
      "Quantum Machine Learning Algorithms",

      # 2022 - Maduración
      "Large Language Models and Their Applications",
      "Multi-modal Learning for Computer Vision and NLP",
      "Edge Computing for Real-time AI Applications",
      "Adversarial Machine Learning and Security",
      "Meta-learning for Few-shot Learning Problems",

      # 2023 - Especialización
      "Foundation Models and Transfer Learning",
      "Sustainable AI: Energy-efficient Machine Learning",
      "AI for Climate Change and Environmental Monitoring",
      "Neuromorphic Computing for Edge AI",
      "Human-AI Collaboration in Decision Making"
    ),
    ANO = c(
      "2019", "2019", "2019",                    # 3 papers 2019
      "2020", "2020", "2020", "2020", "2020",   # 5 papers 2020
      "2021", "2021", "2021", "2021", "2021", "2021",  # 6 papers 2021
      "2022", "2022", "2022", "2022", "2022",   # 5 papers 2022
      "2023", "2023", "2023", "2023", "2023"    # 5 papers 2023
    ),
    RESUMEN = c(
      # 2019
      "Foundational concepts in machine learning with applications to computer science problems",
      "Introduction to neural network architectures and deep learning methodologies",
      "Comprehensive review of optimization algorithms used in artificial intelligence",

      # 2020
      "Advanced machine learning techniques applied to image processing and computer vision",
      "Deep learning methodologies for medical diagnosis and healthcare applications",
      "Genetic algorithms and evolutionary computation for solving complex optimization problems",
      "Natural language processing using transformer models and attention mechanisms",
      "Computer vision systems designed for autonomous vehicle navigation and control",

      # 2021
      "Reinforcement learning algorithms applied to robotics and autonomous systems",
      "Ensemble methods and model combination techniques for improving ML performance",
      "Graph neural networks for analyzing social networks and complex relationships",
      "Federated learning approaches for privacy-preserving machine learning",
      "Explainable artificial intelligence methods for medical decision support systems",
      "Quantum computing approaches to machine learning and optimization problems",

      # 2022
      "Large language models and their applications in natural language understanding",
      "Multi-modal learning combining computer vision and natural language processing",
      "Edge computing solutions for real-time artificial intelligence applications",
      "Adversarial machine learning techniques and cybersecurity applications",
      "Meta-learning algorithms for few-shot learning and rapid adaptation",

      # 2023
      "Foundation models and transfer learning for multiple domains",
      "Sustainable artificial intelligence focusing on energy-efficient algorithms",
      "AI applications for climate change monitoring and environmental protection",
      "Neuromorphic computing architectures for edge artificial intelligence",
      "Human-AI collaboration frameworks for enhanced decision making"
    ),
    AUTOR_PALABRAS_CLAVES = c(
      # 2019
      "machine learning, algorithms, computer science",
      "neural networks, deep learning, AI",
      "optimization, algorithms, artificial intelligence",

      # 2020
      "machine learning, image processing, computer vision",
      "deep learning, medical diagnosis, healthcare",
      "genetic algorithms, optimization, evolutionary computation",
      "natural language processing, transformers, NLP",
      "computer vision, autonomous vehicles, navigation",

      # 2021
      "reinforcement learning, robotics, autonomous systems",
      "ensemble methods, machine learning, performance",
      "graph neural networks, social networks, analysis",
      "federated learning, privacy, machine learning",
      "explainable AI, medical, decision support",
      "quantum computing, machine learning, optimization",

      # 2022
      "large language models, NLP, understanding",
      "multi-modal learning, computer vision, NLP",
      "edge computing, real-time AI, applications",
      "adversarial learning, security, cybersecurity",
      "meta-learning, few-shot learning, adaptation",

      # 2023
      "foundation models, transfer learning, domains",
      "sustainable AI, energy-efficient, algorithms",
      "AI, climate change, environmental monitoring",
      "neuromorphic computing, edge AI, architectures",
      "human-AI collaboration, decision making, frameworks"
    ),
    NOMBRE_AUTOR = c(
      # Autores con evolución temporal
      "Smith, J.", "Johnson, A.", "Brown, K.",                           # 2019 - Fundadores
      "Smith, J.", "Davis, R.", "Johnson, A.", "Wilson, M.", "Garcia, L.",  # 2020 - Expansión
      "Martinez, P.", "Smith, J.", "Johnson, A.", "Chen, W.", "Davis, R.", "Rodriguez, S.",  # 2021 - Nuevos + veteranos
      "Wilson, M.", "Chen, W.", "Taylor, B.", "Martinez, P.", "Anderson, K.",  # 2022 - Mix
      "Garcia, L.", "Taylor, B.", "Lee, H.", "Martinez, P.", "Thompson, D."   # 2023 - Nuevos líderes
    ),
    SJR = c(
      # Calidad creciente a través del tiempo
      "0.8", "0.9", "0.7",                    # 2019 - Calidad inicial
      "1.1", "1.3", "1.0", "1.2", "1.4",     # 2020 - Mejora
      "1.5", "1.2", "1.6", "1.8", "1.4", "1.3",  # 2021 - Alta calidad
      "1.9", "2.1", "1.7", "1.8", "2.0",     # 2022 - Excelencia
      "2.2", "2.4", "2.1", "2.3", "2.5"      # 2023 - Top tier
    ),
    CITADO_POR = c(
      # Citaciones que reflejan impacto temporal
      "45", "38", "29",                       # 2019 - Impact inicial (más tiempo = más citas)
      "67", "58", "72", "49", "63",           # 2020 - Crecimiento
      "34", "41", "28", "52", "37", "31",     # 2021 - Moderado (menos tiempo)
      "19", "25", "22", "17", "21",           # 2022 - Pocas citas (reciente)
      "8", "12", "5", "9", "7"                # 2023 - Muy pocas (muy reciente)
    ),
    LINK = paste0("https://example.com/paper", 1:24),
    stringsAsFactors = FALSE
  )
}

# ===== TEST 1: DETECCIÓN DE TENDENCIAS TEMPORALES =====
test_tendencias_temporales <- function() {
  cat("📈 TEST 1: DETECCIÓN DE TENDENCIAS TEMPORALES\n")
  cat("-" * 50, "\n")

  data_test <- crear_dataset_prueba_completo()

  # Test con query básica
  resultado <- detectar_tendencias_temporales(data_test, "machine learning")

  cat("✅ Tendencia detectada:", resultado$tendencia_detectada, "\n")
  if(resultado$tendencia_detectada) {
    cat("   - Tipo:", resultado$tipo_tendencia, "\n")
    cat("   - R²:", resultado$r_cuadrado, "\n")
    cat("   - Años analizados:", paste(resultado$anos_analizados, collapse = "-"), "\n")
    cat("   - Total papers:", resultado$total_papers, "\n")
    cat("   - Picos detectados:", length(resultado$picos_valles$picos), "\n")
    cat("   - Valles detectados:", length(resultado$picos_valles$valles), "\n")
  }

  cat("📝 Resumen:", resultado$resumen_textual, "\n\n")

  return(resultado$tendencia_detectada)
}

# ===== TEST 2: EVOLUCIÓN DE TEMAS =====
test_evolucion_temas <- function() {
  cat("🔄 TEST 2: EVOLUCIÓN DE TEMAS\n")
  cat("-" * 50, "\n")

  data_test <- crear_dataset_prueba_completo()

  resultado <- analizar_evolucion_temas(data_test, "machine learning", ventana_anos = 2)

  cat("✅ Evolución detectada:", resultado$evolucion_detectada, "\n")
  if(resultado$evolucion_detectada) {
    cat("   - Períodos analizados:", resultado$periodos_analizados, "\n")
    cat("   - Ventana de años:", resultado$ventana_anos, "\n")

    if(resultado$cambios_tematicos$cambios_detectados) {
      cat("   - Estabilidad promedio:", round(resultado$cambios_tematicos$estabilidad_promedio * 100, 1), "%\n")
    }
  }

  cat("📝 Resumen:", resultado$resumen_evolucion, "\n\n")

  return(resultado$evolucion_detectada)
}

# ===== TEST 3: ANÁLISIS DE CITACIONES TEMPORAL =====
test_citaciones_temporal <- function() {
  cat("📊 TEST 3: ANÁLISIS DE CITACIONES TEMPORAL\n")
  cat("-" * 50, "\n")

  data_test <- crear_dataset_prueba_completo()

  resultado <- analizar_papers_mas_citados_temporal(data_test, periodo_anos = 2, top_papers = 3)

  cat("✅ Análisis completado:", resultado$analisis_completado, "\n")
  if(resultado$analisis_completado) {
    cat("   - Períodos analizados:", resultado$num_periodos, "\n")
    cat("   - Período de años:", resultado$periodo_anos, "\n")

    if(resultado$analisis_comparativo$comparacion_disponible) {
      mejor_periodo <- resultado$analisis_comparativo$mejor_periodo_impacto
      cat("   - Mejor período (impacto):", mejor_periodo$periodo, "\n")
      cat("   - Factor de impacto:", mejor_periodo$factor_impacto, "\n")
    }

    cat("   - Papers con impacto sostenido:", resultado$papers_impacto_sostenido$papers_detectados, "\n")
  }

  cat("📝 Resumen:", resultado$resumen_general, "\n\n")

  return(resultado$analisis_completado)
}

# ===== TEST 4: PRODUCTIVIDAD DE AUTORES =====
test_productividad_autores <- function() {
  cat("👥 TEST 4: PRODUCTIVIDAD DE AUTORES\n")
  cat("-" * 50, "\n")

  data_test <- crear_dataset_prueba_completo()

  resultado <- analizar_productividad_autores_temporal(data_test, ventana_anos = 2, min_papers = 1)

  cat("✅ Análisis completado:", resultado$analisis_completado, "\n")
  if(resultado$analisis_completado) {
    cat("   - Total autores analizados:", resultado$total_autores_analizados, "\n")
    cat("   - Ventanas temporales:", resultado$num_ventanas, "\n")
    cat("   - Autores consistentes:", resultado$autores_consistentes$total_autores_consistentes, "\n")

    if(length(resultado$autores_emergentes) > 0) {
      cat("   - Autores emergentes:", length(resultado$autores_emergentes), "\n")
    }

    if(length(resultado$autores_en_declive) > 0) {
      cat("   - Autores en declive:", length(resultado$autores_en_declive), "\n")
    }
  }

  cat("📝 Resumen:", resultado$resumen_general, "\n\n")

  return(resultado$analisis_completado)
}

# ===== TEST 5: VISUALIZACIONES TEMPORALES =====
test_visualizaciones_temporales <- function() {
  cat("🎨 TEST 5: VISUALIZACIONES TEMPORALES\n")
  cat("-" * 50, "\n")

  data_test <- crear_dataset_prueba_completo()

  # Test de tendencias
  tendencias <- detectar_tendencias_temporales(data_test, "machine learning")
  if(tendencias$tendencia_detectada) {
    grafico_tendencias <- crear_grafico_tendencias_temporales(tendencias)
    cat("✅ Gráfico de tendencias creado:", !is.null(grafico_tendencias), "\n")
  }

  # Test de evolución temas
  evolucion <- analizar_evolucion_temas(data_test, "machine learning")
  if(evolucion$evolucion_detectada) {
    grafico_evolucion <- crear_grafico_evolucion_temas(evolucion)
    cat("✅ Gráfico de evolución creado:", !is.null(grafico_evolucion), "\n")
  }

  # Test de heatmap
  heatmap <- crear_heatmap_actividad_temporal(data_test, "machine learning")
  cat("✅ Heatmap de actividad creado:", !is.null(heatmap), "\n")

  # Test de dashboard completo
  dashboard <- crear_dashboard_temporal_completo(data_test, "machine learning")
  cat("✅ Dashboard completo creado:", !is.null(dashboard), "\n")
  cat("   - Componentes del dashboard:", length(dashboard), "\n")

  if(!is.null(dashboard$estadisticas)) {
    cat("   - Papers analizados:", dashboard$estadisticas$total_papers_analizados, "\n")
    cat("   - Autores únicos:", dashboard$estadisticas$autores_unicos, "\n")
  }

  cat("\n")

  return(!is.null(dashboard))
}

# ===== TEST 6: INTEGRACIÓN CON MOTOR SEMÁNTICO =====
test_integracion_motor_semantico <- function() {
  cat("🔬 TEST 6: INTEGRACIÓN CON MOTOR SEMÁNTICO\n")
  cat("-" * 50, "\n")

  data_test <- crear_dataset_prueba_completo()

  # Test con query que debería activar análisis temporal
  resultado <- proceso_nlp_chatbot_semantico("machine learning desde 2020", data_test, usar_expansion_semantica = TRUE)

  cat("✅ Proceso semántico exitoso:", resultado$success, "\n")
  if(resultado$success) {
    cat("   - Papers encontrados:", resultado$num_papers, "\n")
    cat("   - Filtros temporales aplicados:", !is.null(resultado$restriccion_temporal_aplicada), "\n")
    cat("   - Expansión semántica:", resultado$expansion_info$num_expansiones, "\n")

    # Verificar que el resumen incluye análisis temporal avanzado
    resumen_incluye_temporal <- grepl("📈|🔄|📊|👥", resultado$resumen_generado)
    cat("   - Resumen incluye análisis temporal:", resumen_incluye_temporal, "\n")

    cat("📝 Primeras 200 caracteres del resumen:\n")
    cat("   ", substr(resultado$resumen_generado, 1, 200), "...\n")
  }

  cat("\n")

  return(resultado$success)
}

# ===== TEST 7: DEMOSTRACIÓN COMPLETA =====
test_demostracion_completa <- function() {
  cat("🚀 TEST 7: DEMOSTRACIÓN COMPLETA\n")
  cat("-" * 50, "\n")

  data_test <- crear_dataset_prueba_completo()

  # Ejecutar demostración
  resultado_demo <- demostrar_analisis_temporal_avanzado("machine learning", data_test)

  # Verificar componentes
  componentes_exitosos <- 0

  if(!is.null(resultado_demo$tendencias)) componentes_exitosos <- componentes_exitosos + 1
  if(!is.null(resultado_demo$evolucion_temas)) componentes_exitosos <- componentes_exitosos + 1
  if(!is.null(resultado_demo$citaciones_temporal)) componentes_exitosos <- componentes_exitosos + 1
  if(!is.null(resultado_demo$autores_temporal)) componentes_exitosos <- componentes_exitosos + 1
  if(!is.null(resultado_demo$resumen_ejecutivo)) componentes_exitosos <- componentes_exitosos + 1

  cat("✅ Demostración completada\n")
  cat("   - Componentes ejecutados exitosamente:", componentes_exitosos, "/5\n")

  if(!is.null(resultado_demo$resumen_ejecutivo)) {
    resumen_ejec <- resultado_demo$resumen_ejecutivo
    cat("   - Análisis ejecutados:", length(resumen_ejec$analisis_ejecutados), "\n")
    cat("   - Principales hallazgos:", length(resumen_ejec$principales_hallazgos), "\n")
    cat("   - Recomendaciones:", length(resumen_ejec$recomendaciones), "\n")
  }

  cat("\n")

  return(componentes_exitosos >= 4)
}

# ===== EJECUTAR TODOS LOS TESTS =====
ejecutar_tests_completos <- function() {

  cat("🔬 EJECUTANDO BATERÍA COMPLETA DE TESTS - FASE 2\n")
  cat("=" * 70, "\n\n")

  resultados_tests <- list()

  # Ejecutar cada test
  resultados_tests$tendencias <- test_tendencias_temporales()
  resultados_tests$evolucion_temas <- test_evolucion_temas()
  resultados_tests$citaciones <- test_citaciones_temporal()
  resultados_tests$autores <- test_productividad_autores()
  resultados_tests$visualizaciones <- test_visualizaciones_temporales()
  resultados_tests$integracion <- test_integracion_motor_semantico()
  resultados_tests$demostracion <- test_demostracion_completa()

  # Calcular resultados
  tests_exitosos <- sum(unlist(resultados_tests))
  total_tests <- length(resultados_tests)
  porcentaje_exito <- round((tests_exitosos / total_tests) * 100, 1)

  cat("📊 RESUMEN DE RESULTADOS\n")
  cat("=" * 70, "\n")
  cat("✅ Tests exitosos:", tests_exitosos, "/", total_tests, "\n")
  cat("📈 Porcentaje de éxito:", porcentaje_exito, "%\n\n")

  # Detalles por test
  cat("📋 DETALLES POR TEST:\n")
  for(test_name in names(resultados_tests)) {
    status <- if(resultados_tests[[test_name]]) "✅ PASÓ" else "❌ FALLÓ"
    cat("   ", test_name, ":", status, "\n")
  }

  cat("\n")

  if(porcentaje_exito >= 85) {
    cat("🎉 FASE 2 - ANÁLISIS TEMPORAL AVANZADO: IMPLEMENTACIÓN EXITOSA\n")
    cat("   Todas las funcionalidades principales están operativas.\n")
  } else if(porcentaje_exito >= 70) {
    cat("⚠️  FASE 2 - ANÁLISIS TEMPORAL AVANZADO: IMPLEMENTACIÓN PARCIAL\n")
    cat("   La mayoría de funcionalidades están operativas, revisar tests fallidos.\n")
  } else {
    cat("❌ FASE 2 - ANÁLISIS TEMPORAL AVANZADO: REQUIERE CORRECCIONES\n")
    cat("   Múltiples tests fallaron, revisar implementación.\n")
  }

  cat("\n" * 2)
  cat("🏁 TESTS COMPLETADOS - FASE 2 ANÁLISIS TEMPORAL AVANZADO\n")
  cat("=" * 70, "\n")

  return(list(
    exito = porcentaje_exito >= 85,
    porcentaje = porcentaje_exito,
    tests_exitosos = tests_exitosos,
    total_tests = total_tests,
    detalles = resultados_tests
  ))
}

# ===== FUNCIÓN PRINCIPAL =====
cat("🚀 Para ejecutar los tests, use: ejecutar_tests_completos()\n")
cat("📝 Para ver un test específico, use las funciones test_*() individuales\n\n")

# Ejecutar automáticamente si se llama directamente
if(!interactive()) {
  resultado_final <- ejecutar_tests_completos()
}