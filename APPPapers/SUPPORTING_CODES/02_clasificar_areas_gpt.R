clasificar_areas_gpt <- function(datos, api_key) {
  
  # Validación de columna
  if (!"RESUMEN" %in% names(datos)) {
    stop("El data.frame debe contener una columna llamada 'RESUMEN'.")
  }
  
  # Función para enviar consulta a la API
  question2chatgpt <- function(consulta, api_key) {
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type_json(),
      encode = "json",
      body = list(
        model = "gpt-3.5-turbo",
        max_tokens = 100,
        temperature = 0,
        messages = list(list(role = "user", content = consulta))
      )
    )
    
    out <- httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
    
    if (!is.null(out$choices) && length(out$choices) > 0) {
      return(out$choices[[1]]$message$content)
    } else {
      return("ERROR_API")
    }
  }
  
  # Inicializar vector de respuesta
  respuesta_final <- character(nrow(datos))
  
  # Loop principal
  for (a in seq_len(nrow(datos))) {
    cat("→ Evaluando resumen número:", a, "\n")
    
    resumen_actual <- datos$RESUMEN[a]
    
    consulta <- paste(
      "Clasifica el siguiente resumen en exactamente UNA de estas categorías del área de informática o ciencia de la computación.",
      "Si NO pertenece a ninguna, responde exactamente así: (18) It does not belong to the computer science field.",
      "SI pertenece a alguna, responde exactamente copiando UNA de las siguientes líneas, sin cambiar nada, incluyendo el número entre párentesis:",
      "SI lo vas a clasificar como (18), antes de hacerlo vuelve a revisarque no esté asociado a (1), (2), (4) o (8) porque puede ser un trabajo aplicado.
       ¡Casi nunca es (18)!","También fíjate que si dice en algún lado optimización en español o inglés; debes evaluar si pertenece a (1) o (3)",
      "(1) Algorithms and Complexity", 
      "(2) Architecture and Organization",
      "(3) Artificial Intelligence",
      "(4) Data Management",
      "(5) Foundations of Programming Languages",
      "(6) Graphics and Interactive Techniques",
      "(7) Human-Computer Interaction",
      "(8) Mathematical and Statistical Foundations",
      "(9) Networking and Communication",
      "(10) Operating Systems",
      "(11) Parallel and Distributed Computing",
      "(12) Security",
      "(13) Society, Ethics and Professionalism",
      "(14) Software Development Fundamentals",
      "(15) Software Engineering",
      "(16) Specialized Platform Development",
      "(17) Systems Fundamentals",
      "(18) It does not belong to the computer science field",
      "NO AÑADAS NINGUNA PALABRA EXTRA. NO EXPLIQUES. NO CAMBIES EL FORMATO.",
      "Responde solamente copiando y pegando exactamente una de las opciones anteriores.",
      "Resumen:", resumen_actual
    )
    
    respuesta <- tryCatch({
      question2chatgpt(consulta, api_key)
    }, error = function(e) {
      cat("❌ Error en resumen", a, ":", conditionMessage(e), "\n")
      return("ERROR_API")
    })
    
    if (is.null(respuesta) || respuesta == "") {
      respuesta <- "ERROR_API"
    }
    
    respuesta_final[a] <- respuesta
  }
  
  # Agregar columna al data.frame
  datos$AREA_COMPUTACION <- respuesta_final
  
  # Reubicar entre TIPO_DOCUMENTO y CATEGORIAS
  if (!all(c("TIPO_DOCUMENTO", "CATEGORIAS") %in% names(datos))) {
    warning("No se pudo reubicar columna AREA_COMPUTACION. Columnas de referencia no encontradas.")
    return(datos)
  }
  
  pos_tipo_doc <- match("TIPO_DOCUMENTO", names(datos))
  pos_cat <- match("CATEGORIAS", names(datos))
  
  cols_antes <- names(datos)[1:pos_tipo_doc]
  cols_despues <- names(datos)[(pos_tipo_doc + 1):ncol(datos)]
  cols_despues <- setdiff(cols_despues, "AREA_COMPUTACION")
  
  nueva_orden <- c(cols_antes, "AREA_COMPUTACION", cols_despues)
  datos <- datos[, nueva_orden]
  
  return(datos)
}
