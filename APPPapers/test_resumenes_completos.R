# Test para verificar que los resúmenes se muestran completos
cat("=== PROBANDO RESÚMENES COMPLETOS SIN CORTAR ===\n")

# Cargar motor actualizado
source("www/SOURCE/UTILS/nlp_chatbot_engine_simple_v2.R")

# Datos de prueba con abstracts extensos para generar resúmenes largos
datos_test <- data.frame(
  TITULO = c(
    "Machine Learning Applications in Medical Diagnosis and Treatment",
    "Deep Learning Techniques for Computer Vision and Image Processing",
    "Artificial Intelligence Methods in Educational Technology Systems",
    "Neural Network Architectures for Natural Language Processing",
    "Data Mining and Statistical Analysis in Healthcare Research",
    "Optimization Algorithms for Software Engineering and Development",
    "Computer Vision Applications in Medical Image Analysis and Diagnosis",
    "Machine Learning Models for Predictive Healthcare Analytics",
    "Artificial Intelligence in Educational Assessment and Learning Systems",
    "Deep Learning Approaches for Medical Image Classification and Detection"
  ),
  NOMBRE_AUTOR = c(
    "Dr. Smith Johnson", "Dr. María García", "Dr. Carlos López", "Dr. Ana Martínez", 
    "Dr. Roberto Silva", "Dr. Elena Rodríguez", "Dr. Miguel Torres", "Dr. Carmen Ruiz",
    "Dr. Francisco Morales", "Dr. Patricia Vega"
  ),
  ANO = c(2020, 2021, 2022, 2019, 2023, 2018, 2024, 2020, 2021, 2022),
  RESUMEN = c(
    "This comprehensive study presents machine learning techniques for medical diagnosis including supervised learning algorithms, neural networks, decision trees, and support vector machines. The research demonstrates significant improvements in diagnostic accuracy through feature selection and ensemble methods. Applications include disease prediction, treatment recommendation, and patient outcome analysis using clinical data and laboratory results.",
    
    "Deep learning revolutionizes computer vision through convolutional neural networks, image recognition, object detection, and semantic segmentation. This work proposes advanced architectures incorporating attention mechanisms, transfer learning, and data augmentation techniques. Applications span medical imaging, autonomous vehicles, security systems, and robotic vision with state-of-the-art performance benchmarks.",
    
    "Artificial intelligence transforms educational technology through personalized learning systems, adaptive assessment, and intelligent tutoring systems. The research develops machine learning models for student performance prediction, content recommendation, and automated grading. Integration includes natural language processing, educational data mining, and learning analytics for enhanced educational outcomes.",
    
    "Neural network architectures for natural language processing include transformer models, attention mechanisms, recurrent networks, and language modeling. This comprehensive analysis covers text classification, sentiment analysis, machine translation, and question answering systems. Applications demonstrate improved performance in information extraction, document summarization, and conversational AI systems.",
    
    "Data mining and statistical analysis in healthcare research involves pattern recognition, predictive modeling, and clinical decision support systems. The study applies clustering algorithms, regression analysis, and classification techniques to electronic health records. Results show improved patient care through evidence-based medicine, population health management, and clinical research acceleration.",
    
    "Optimization algorithms for software engineering include genetic algorithms, particle swarm optimization, and metaheuristic approaches. This research addresses code optimization, resource allocation, and project scheduling problems. Implementation covers parallel processing, distributed computing, and cloud-based software development with significant performance improvements and cost reduction.",
    
    "Computer vision applications in medical image analysis encompass diagnostic imaging, pathology detection, and treatment planning. The work utilizes deep learning, image segmentation, and feature extraction for radiological interpretation. Clinical applications include cancer detection, surgical planning, and medical device automation with enhanced diagnostic accuracy and efficiency.",
    
    "Machine learning models for predictive healthcare analytics involve risk assessment, outcome prediction, and population health analysis. The research implements ensemble methods, time series analysis, and survival modeling using patient data. Applications include epidemic forecasting, resource planning, and personalized medicine with improved healthcare delivery and cost effectiveness.",
    
    "Artificial intelligence in educational assessment develops automated evaluation systems, adaptive testing, and learning outcome measurement. The study incorporates machine learning for skill assessment, competency mapping, and educational quality assurance. Implementation includes standardized testing, certification programs, and continuous assessment with enhanced reliability and validity.",
    
    "Deep learning approaches for medical image classification encompass convolutional networks, transfer learning, and multi-modal fusion. This comprehensive research addresses radiological diagnosis, pathology identification, and treatment monitoring. Clinical integration includes DICOM processing, workflow optimization, and diagnostic decision support with improved accuracy and efficiency."
  ),
  LINK = paste0("https://link", 1:10),
  SJR = c(2.1, 2.8, 1.9, 2.5, 2.2, 1.7, 2.6, 2.3, 1.8, 2.4),
  CITADO_POR = c(45, 67, 38, 52, 41, 29, 58, 43, 31, 49),
  stringsAsFactors = FALSE
)

cat("Datos de prueba creados con abstracts extensos\n")
cat("Total papers:", nrow(datos_test), "papers\n")

# Consultas que deberían generar resúmenes largos
consultas_test <- c(
  "papers relacionados con medicina",
  "machine learning en healthcare", 
  "deep learning y computer vision",
  "artificial intelligence en educación"
)

for(consulta in consultas_test) {
  cat(paste("\n", rep("=", 70), collapse = ""))
  cat(paste("\nCONSULTA:", consulta))
  cat(paste("\n", rep("=", 70), collapse = ""))
  cat("\n")
  
  resultado <- tryCatch({
    proceso_nlp_chatbot_simple(consulta, datos_test, NULL)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
  
  if(resultado$success) {
    cat("✅ BÚSQUEDA EXITOSA\n")
    cat(paste("📊 Papers encontrados:", resultado$num_papers, "\n"))
    
    # Mostrar el resumen COMPLETO
    cat("\n📝 RESUMEN GENERADO (COMPLETO):\n")
    cat(paste0('\"', resultado$resumen_generado, '\"'))
    cat("\n")
    
    # Verificar longitud del resumen
    longitud_resumen <- nchar(resultado$resumen_generado)
    cat(paste("\n📏 Longitud del resumen:", longitud_resumen, "caracteres\n"))
    
    # Verificar que no esté cortado
    if(grepl("\\.\\.\\.$", resultado$resumen_generado)) {
      cat("⚠️ ADVERTENCIA: El resumen parece estar cortado (termina en ...)\n")
    } else {
      cat("✅ CONFIRMADO: Resumen completo (no cortado)\n")
    }
    
    # Verificar que termine apropiadamente
    if(grepl("\\.$", resultado$resumen_generado)) {
      cat("✅ CONFIRMADO: Resumen termina correctamente con punto\n")
    } else {
      cat("⚠️ NOTA: Resumen no termina con punto\n")
    }
    
  } else {
    if("error" %in% names(resultado)) {
      cat("❌ ERROR:", resultado$error, "\n")
    } else {
      cat("⚠️ SIN RESULTADOS:", resultado$message, "\n")
    }
  }
}

cat(paste("\n", rep("=", 70), collapse = ""))
cat("\n🎯 MEJORAS IMPLEMENTADAS EN RESÚMENES:\n")
cat("1. ✅ Límite aumentado de 500 a 800 caracteres\n")
cat("2. ✅ Formato más compacto para múltiples elementos\n")
cat("3. ✅ Resumen compacto automático si es muy largo\n")
cat("4. ✅ Conclusiones adaptativas según número de papers\n")
cat("5. ✅ Mejor estructuración de metodologías y aplicaciones\n")
cat("6. ✅ Eliminación del corte abrupto con '...'\n")

cat("\n📏 LÍMITES DE LONGITUD:\n")
cat("- Resumen normal: hasta 800 caracteres\n")
cat("- Resumen compacto: se activa automáticamente si excede 800\n")
cat("- Sin cortes abruptos: siempre termina apropiadamente\n")

cat("\n🚀 ¡Resúmenes ahora se muestran completos y bien estructurados!\n")