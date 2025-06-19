tab_resumen = function(input, output, session, datos_tmp) {

  #--------------------------------------------------
  # A) Total de publicaciones y autores únicos
  #--------------------------------------------------
  output$TEXTO_numero_publicaciones <- renderText({
    if (nrow(datos_tmp) > 0) {
      total_pubs <- nrow(datos_tmp)
      total_autores <- length(unique(datos_tmp$SCOPUS_ID))
      promedio <- round(total_pubs / total_autores, 1)
      HTML(paste0(
        "<span style='font-size: 14px; color: rgb(0, 164, 153);'><b>", total_pubs, "</b></span>, considerando ",
        "<span style='font-size: 14px; color: rgb(0, 164, 153);'><b>", total_autores, "</b></span> autores(as) cuya filiación es la institución seleccionada. ",
        "Esto da un promedio de <span style='font-size: 14px; color: rgb(0, 164, 153);'><b>", promedio, "</b></span> publicaciones por autor(a)."
      ))
    } else {
      HTML("<span style='font-size: 14px; color: rgb(0, 164, 153);'><b>0</b></span>, considerando <span style='font-size: 14px; color: rgb(0, 164, 153);'><b>0</b></span> autores(as) cuya filiación es la institución seleccionada.")
    }
  })

  #--------------------------------------------------
  # B) Tipos de documentos (Artículos, Actas, Otros)
  #--------------------------------------------------
  output$TEXTO_tipo_publicaciones <- renderText({
    if (nrow(datos_tmp) > 0) {
      conteos <- table(factor(datos_tmp$TIPO_DOCUMENTO, 
                              levels = c("Article", "Conference paper")))
      otros <- nrow(datos_tmp) - sum(conteos)
      total <- nrow(datos_tmp)
      etiquetas <- c("Revistas", "Actas de conferencia", "Otros tipos")
      cantidades <- c(conteos["Article"], conteos["Conference paper"], otros)
      porcentajes <- round(cantidades / total * 100, 1)
      HTML(paste0(etiquetas, ": &nbsp<span style='font-size: 14px; color: rgb(0, 164, 153);'><b>", 
                  cantidades, "</b></span> (", porcentajes, "%)<br/>", collapse = ""))
    } else {
      HTML("-: &nbsp<span style='font-size: 14px; color: rgb(0, 164, 153);'><b>-</b></span> (-%)<br/>")
    }
  })

  #--------------------------------------------------
  # C) Distribución de publicaciones por indexación (WoS / Scopus)
  #--------------------------------------------------
  output$TEXTO_promedio_publicaciones_autor <- renderText({
    if (nrow(datos_tmp) > 0) {
      wos <- sum(datos_tmp$WOS == "SI")
      scopus <- sum(datos_tmp$WOS == "NO")
      total <- nrow(datos_tmp)
      etiquetas <- c("Web of Science", "Scopus")
      cantidades <- c(wos, scopus)
      porcentajes <- round(cantidades / total * 100, 1)
      HTML(paste0(etiquetas, ": &nbsp<span style='font-size: 14px; color: rgb(0, 164, 153);'><b>",
                  cantidades, "</b></span> (", porcentajes, "%)<br/>", collapse = ""))
    } else {
      HTML("-: &nbsp<span style='font-size: 14px; color: rgb(0, 164, 153);'><b>-</b></span> (-%)<br/>")
    }
  })

  #--------------------------------------------------
  # D) Áreas de CS más frecuentes
  #--------------------------------------------------
  output$TEXTO_areas_cs_mas_frecuentes <- renderText({
    if (nrow(datos_tmp) > 0) {
      tabla <- table(datos_tmp$AREA_COMPUTACION)
      tabla <- sort(tabla[tabla > 0], decreasing = TRUE)
      max_freq <- max(tabla)
      seleccion <- names(tabla)[tabla == max_freq]
      texto <- paste0("<i>", seleccion, "</i> (<span style='font-size: 14px; color: rgb(0, 164, 153);'><b>", max_freq, "</b></span>)")
      HTML(paste(texto, collapse = if (length(texto) > 1) ", " else ""))
    } else {
      HTML("-")
    }
  })
  
  
  #--------------------------------------------------
  # E) Áreas de aplicación más frecuentes
  #--------------------------------------------------
  output$TEXTO_areas_aplicacion_mas_frecuentes <- renderText({
    if (nrow(datos_tmp) > 0) {
      
      # Separar las áreas por ";"
      areas <- unlist(strsplit(datos_tmp$AREAS, ";"))
      areas <- trimws(areas)  # Elimina espacios sobrantes
      areas <- areas[areas != "#N/A"]  # Eliminar valores faltantes
      
      if (length(areas) == 0) return(HTML("-"))
      
      # Contar todas las áreas, sin excluir aún ninguna
      tabla <- sort(table(areas), decreasing = TRUE)
      
      # Excluir "Computer Science" después de tener el conteo completo
      tabla_filtrada <- tabla[names(tabla) != "Computer Science"]
      
      # Si queda vacía la tabla, se retorna Computer Science como fallback
      if (length(tabla_filtrada) == 0) {
        seleccion <- names(tabla)[1]
        freq <- as.integer(tabla[1])
      } else {
        max_freq <- max(tabla_filtrada)
        seleccion <- names(tabla_filtrada)[tabla_filtrada == max_freq]
        freq <- max_freq
      }
      
      # Construcción del texto con HTML
      texto <- paste0("<i>", seleccion, "</i> (<span style='font-size: 14px; color: rgb(0, 164, 153);'><b>", freq, "</b></span>)")
      HTML(paste(texto, collapse = if (length(texto) > 1) ", " else ""))
      
    } else {
      HTML("-")
    }
  })

  #--------------------------------------------------
  # F) Revista más publicada
  #--------------------------------------------------
  output$TEXTO_revista_mas_publicada <- renderText({
    datos_filtrados <- filter(datos_tmp, TIPO_DOCUMENTO == "Article")
    if (nrow(datos_filtrados) > 0) {
      tabla <- sort(table(datos_filtrados$FUENTE), decreasing = TRUE)
      max_freq <- max(tabla)
      seleccion <- names(tabla)[tabla == max_freq]
      texto <- paste0("<i>", seleccion, "</i> (<span style='font-size: 14px; color: rgb(0, 164, 153);'><b>", max_freq, "</b></span>)")
      HTML(paste(texto, collapse = if (length(texto) > 1) ", " else ""))
    } else {
      HTML("-")
    }
  })

  #--------------------------------------------------
  # G) Áreas de CS menos frecuentes
  #--------------------------------------------------
  output$TEXTO_areas_cs_menos_frecuentes <- renderText({
    if (nrow(datos_tmp) > 0) {
      tabla <- table(datos_tmp$AREA_COMPUTACION)
      tabla <- sort(tabla[tabla > 0])
      min_freq <- min(tabla)
      seleccion <- names(tabla)[tabla == min_freq]
      texto <- paste0("<i>", seleccion, "</i> (<span style='font-size: 14px; color: rgb(0, 164, 153);'><b>", min_freq, "</b></span>)")
      HTML(paste(texto, collapse = if (length(texto) > 1) ", " else ""))
    } else {
      HTML("-")
    }
  })

  #--------------------------------------------------
  # H) Áreas de aplicación menos frecuentes
  #--------------------------------------------------
  output$TEXTO_areas_aplicacion_menos_frecuentes <- renderText({
    if (nrow(datos_tmp) > 0) {
      
      # Separar las áreas y limpiar
      areas <- unlist(strsplit(datos_tmp$AREAS, ";"))
      areas <- trimws(areas)
      areas <- areas[areas != "#N/A"]
      
      if (length(areas) == 0) return(HTML("-"))
      
      # Contar todas las áreas, sin excluir aún ninguna
      tabla <- sort(table(areas))
      
      # Excluir "Computer Science" después del conteo
      tabla_filtrada <- tabla[names(tabla) != "Computer Science"]
      
      # Si no quedan otras áreas, usar "Computer Science" como fallback
      if (length(tabla_filtrada) == 0) {
        seleccion <- names(tabla)[1]
        freq <- as.integer(tabla[1])
      } else {
        min_freq <- min(tabla_filtrada)
        seleccion <- names(tabla_filtrada)[tabla_filtrada == min_freq]
        freq <- min_freq
      }
      
      # Formato HTML para el texto
      texto <- paste0("<i>", seleccion, "</i> (<span style='font-size: 14px; color: rgb(0, 164, 153);'><b>", freq, "</b></span>)")
      HTML(paste(texto, collapse = if (length(texto) > 1) ", " else ""))
      
    } else {
      HTML("-")
    }
  })
  
  

  #--------------------------------------------------
  # I) Promedio de autores por publicación
  #--------------------------------------------------
  output$TEXTO_promedio_autores <- renderText({
    if (nrow(datos_tmp) > 0) {
      listas_autores <- strsplit(datos_tmp$AUTORES_ID, ";")
      promedio <- round(mean(sapply(listas_autores, length)), 1)
      HTML(paste0("<span style='font-size: 14px; color: rgb(0, 164, 153);'><b>", promedio, "</b></span>"))
    } else {
      HTML("<span style='font-size: 14px; color: rgb(0, 164, 153);'><b>-</b></span>")
    }
  })

  #--------------------------------------------------
  # J) Publicación más citada
  #--------------------------------------------------
  if (nrow(datos_tmp)>0) {
    # Si hay datos, calcular y mostrar la publicación más citada
    datos_filtrados = datos_tmp
    datos_filtrados$CITADO_POR[which(is.na(datos_filtrados$CITADO_POR))] = 0
    idx = which(datos_filtrados$CITADO_POR == max(datos_filtrados$CITADO_POR))
    
    
   salida_tmp = NULL
  for (a in idx)
    {
      autores = datos_tmp$NOMBRE_AUTOR[which(datos_filtrados$TITULO[a]==datos_filtrados$TITULO)]
      autores = strcat(autores,collapse = ",")
      salida_tmp = rbind(salida_tmp,c(datos_filtrados$TITULO[a],datos_filtrados$CITADO_POR[a],autores))
      
  }
 
   salida_tmp = unique(data.frame(salida_tmp))
   
    # Obtener la publicación más citada
    publicaciones_mas_citada = paste("'", salida_tmp$X1, "'&nbsp (<b><span style='font-size: 14px; color: rgb(0, 164, 153);'>",
                                     salida_tmp$X2, " </span></b> citas)",
                                     "&nbspdel(de la) profesor(a)&nbsp <b> <span style='font-size: 14px; color: rgb(0, 164, 153);'>",
                                     salida_tmp$X3, "</span></b>.", sep = "")

    output$TEXTO_publicacion_mas_citada = renderText({
      # Renderiza el texto con la publicación más citada
      HTML(publicaciones_mas_citada)
    }) # Cierre de renderText
  } else {
    # Si no hay datos, mostrar un guion para la publicación más citada
    output$TEXTO_publicacion_mas_citada = renderText({
      # Renderiza un guion para indicar la ausencia de datos en la publicación más citada
      HTML("-")
    }) # Cierre de renderText
  } # Cierre del else

  #--------------------------------------------------
  # K) Publicación con mayor factor de impacto (SJR)
  #--------------------------------------------------
  output$TEXTO_publicacion_mas_impacto <- renderText({
    if (nrow(datos_tmp) > 0) {
      datos_tmp$SJR[is.na(datos_tmp$SJR)] <- 0
      idx <- which(datos_tmp$SJR == max(datos_tmp$SJR, na.rm = TRUE))
      publicaciones <- unique(datos_tmp[idx, c("TITULO", "SJR", "NOMBRE_AUTOR")])
      
      publicaciones_html <- apply(publicaciones, 1, function(row) {
        paste0("'", row["TITULO"], "' (<b><span style='font-size: 14px; color: rgb(0, 164, 153);'>",
               row["SJR"], "</span></b> puntaje SJR) del(de la) profesor(a) <b><span style='font-size: 14px; color: rgb(0, 164, 153);'>",
               row["NOMBRE_AUTOR"], "</span></b>.")
      })
      
      HTML(paste(publicaciones_html, collapse = "<br/>"))
    } else {
      HTML("-")
    }
  })

  #--------------------------------------------------
  # L) Porcentaje de publicaciones con más de un autor institucional (SCOPUS_ID)
  #--------------------------------------------------
  output$TEXTO_promedio_autores_diinf <- renderText({
    if (nrow(datos_tmp) > 0) {
      autores_diinf <- unique(datos_tmp$SCOPUS_ID)
      listas_autores <- strsplit(datos_tmp$AUTORES_ID, ";")
      
      # Conteo de publicaciones con más de un autor institucional
      conteo_diinf <- sapply(listas_autores, function(lista) {
        sum(as.numeric(lista) %in% autores_diinf)
      })
      
      porcentaje <- round(sum(conteo_diinf > 1) / length(conteo_diinf) * 100)
      HTML(paste0("<span style='font-size: 14px; color: rgb(0, 164, 153);'><b>", porcentaje, "%</b></span>"))
    } else {
      HTML("<span style='font-size: 14px; color: rgb(0, 164, 153);'><b>-</b></span>")
    }
  })

}