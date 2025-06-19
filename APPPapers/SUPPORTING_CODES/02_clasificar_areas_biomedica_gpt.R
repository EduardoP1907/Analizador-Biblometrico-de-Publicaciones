clasificar_areas_biomedica_gpt <- function(datos, api_key) {
  
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
        #model = "gpt-3.5-turbo",
        model = "gpt-4-turbo",
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
      "Clasifica el siguiente resumen en exactamente UNA de estas categorías del área de Bioingeniería.",
      "Si NO pertenece a ninguna, responde exactamente así: (18) It does not belong to the bioengineering field.",
      "SI pertenece a alguna, responde copiando exactamente UNA de las siguientes líneas, sin cambiar nada, incluyendo el número entre paréntesis.",
      "Usa (12) Bioinformatics **solo** si el resumen trata sobre software o algoritmos aplicados a problemas asociados con el DOGMA CENTRAL DE LA BIOLOGÍA MOLECULAR.",
      "NO uses (12) Bioinformatics si el trabajo es solo simulación, machine learning genérico, imágenes, fisiología, o procesos celulares sin relación explícita con el dogma (ADN → ARN → proteína).",
      "Evita dejar (15) Cellular and Tissue Engineering salvo que sea claramente sobre cultivos celulares, matrices extracelulares o desarrollo de tejidos. Si involucra ratones, fisiología, regulación homeostática o modelos animales, usa (10) Systems Physiology.",
      "Si el trabajo es sobre educación, aprendizaje, competencias, programas de estudio, o cualquier aspecto educativo, o si no tiene relación con tecnologías biomédicas o procesos fisiológicos, entonces clasifica como (18) It does not belong to the bioengineering field.",
      "Las siguientes definiciones son solo para ayudarte a decidir, pero NO DEBEN INCLUIRSE EN LA RESPUESTA:",
      "(1) Biomechanics – Fuerzas mecánicas en el cuerpo humano.",
      "(2) Biofluid – Fluidos biológicos (sangre, aire, etc.).",
      "(3) Bionics – Sistemas artificiales que imitan funciones biológicas.",
      "(4) Rehabilitation Engineering – Tecnologías para rehabilitación.",
      "(5) Orthopaedic Bioengineering – Ingeniería para huesos y articulaciones.",
      "(6) Biomedical Electronics – Electrónica aplicada a salud.",
      "(7) Biomechatronics – Integración de sistemas mecatrónicos con biología.",
      "(8) Bioinstrumentation – Sensores biomédicos y dispositivos de medición.",
      "(9) Medical Imaging – Técnicas de imagen médica.",
      "(10) Systems Physiology – Modelos y funciones fisiológicas.",
      "(11) Neural Engineering – Ingeniería del sistema nervioso.",
      "(12) Bioinformatics – Algoritmos para el dogma central molecular.",
      "(13) Clinical Engineering – Tecnologías en entornos clínicos.",
      "(14) Biomaterials – Materiales compatibles con el cuerpo.",
      "(15) Cellular and Tissue Engineering – Ingeniería celular y tisular.",
      "(16) Genetic Engineering – Modificación genética.",
      "(18) It does not belong to the bioengineering field",
      "RECUERDA: SOLO DEBES RESPONDER CON UNA DE LAS SIGUIENTES OPCIONES EXACTAMENTE COMO ESTÁ ESCRITA:",
      "(1) Biomechanics",
      "(2) Biofluid",
      "(3) Bionics",
      "(4) Rehabilitation Engineering",
      "(5) Orthopaedic Bioengineering",
      "(6) Biomedical Electronics",
      "(7) Biomechatronics",
      "(8) Bioinstrumentation",
      "(9) Medical Imaging",
      "(10) Systems Physiology",
      "(11) Neural Engineering",
      "(12) Bioinformatics",
      "(13) Clinical Engineering",
      "(14) Biomaterials",
      "(15) Cellular and Tissue Engineering",
      "(16) Genetic Engineering",
      "(18) It does not belong to the bioengineering field",
      "NO AÑADAS PALABRAS EXTRA. NO EXPLIQUES. NO CAMBIES EL FORMATO.",
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
