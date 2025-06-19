#=========================================
# FUNCIONES DE VISUALIZACIÓN: PANEL PERFILES
# Archivo: server_funciones.R
#=========================================

#=========================================
# 1. Gráfico de barras (completando con 0s)
#=========================================
generar_grafico_puntos <- function(data_bar, periodo) {
  
  # Validación de datos
  if (is.null(data_bar) || nrow(data_bar) == 0) {
    return(
      ggplot() + 
        geom_blank() + 
        labs(title = "Frecuencia de publicaciones", x = "Años", y = "Publicaciones") +
        theme_minimal() +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 14)
        ) +
        annotate("text", x = 1, y = 1, label = "No hay datos disponibles para los filtros seleccionados.", size = 5, hjust = 0.5)
    )
  }
  
  # Asegura que el vector periodo sea numérico (por si viene de sliderTextInput)
  periodo <- as.integer(periodo)
  
  # Preparar base completa con todos los años del período
  todos_los_anos <- data.frame(Años = periodo[1]:periodo[2])
  
  # Contar ocurrencias por año
  data_bar <- data.frame(table(data_bar$ANO))
  names(data_bar) <- c("Años", "Publicaciones")
  data_bar$Años <- as.numeric(as.character(data_bar$Años))
  
  # Unir con la base completa de años y rellenar NA con 0
  data_bar <- merge(todos_los_anos, data_bar, by = "Años", all.x = TRUE)
  data_bar$Publicaciones[is.na(data_bar$Publicaciones)] <- 0
  
  # Calcular el máximo de publicaciones
  y_max <- max(data_bar$Publicaciones, na.rm = TRUE)
  
  # Generar gráfico de barras
  ggplot(data_bar, aes(x = Años, y = Publicaciones)) +
    geom_col(fill = "#00A499", alpha = 0.6, width = 0.7) +
    labs(title = "Frecuencia de publicaciones", x = "Años", y = "Publicaciones") +
    theme_minimal() +
    scale_y_continuous(
      breaks = function(x) unique(floor(pretty(x))),
      limits = c(0, y_max + 1),
      expand = c(0, 0)
    ) +
    scale_x_continuous(
      breaks = periodo[1]:periodo[2],
      limits = c(periodo[1] - 0.5, periodo[2] + 0.5)
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14)
    )
}

#=========================================
# 2. Gráfico Sankey usando plotly (alternativa a networkD3)
#=========================================
generar_grafico_sankey <- function(data_sanky) {
  
  # Validación inicial
  if (nrow(data_sanky) == 0) {
    shinybusy::show_modal_spinner(
      text = "Espere mientras se estiman los datos...",
      spin = "fading-circle",
      color = "#FF9800"
    )
    return("No hay datos disponibles para generar el gráfico Sankey.")
  }
  
  # Áreas a eliminar
  areas_a_eliminar <- c("Computer Science")
  
  # Procesamiento de datos
  new_data <- data_sanky %>%
    select(AREA_COMPUTACION, AREAS) %>%
    separate_rows(AREAS, sep = "; ") %>%
    filter(!AREAS %in% areas_a_eliminar)
  
  if (nrow(new_data) == 0) {
    return("No hay datos disponibles para generar el gráfico Sankey debido a que todas las áreas han sido eliminadas por los filtros.")
  }
  
  new_data <- new_data %>%
    mutate(
      AREAS = ifelse(AREAS == "#N/A", "SIN CLASIFICACIÓN", AREAS),
      AREA_COMPUTACION = gsub("\\(.*\\)", "", AREA_COMPUTACION),
      AREA_COMPUTACION = str_to_upper(AREA_COMPUTACION),
      AREAS = str_to_upper(AREAS)
    )
  
  # Crear enlaces
  links <- new_data %>%
    count(AREA_COMPUTACION, AREAS, name = "value") %>%
    rename(source = AREA_COMPUTACION, target = AREAS)
  
  # Crear nodos únicos
  node_names <- unique(c(links$source, links$target))
  nodes <- data.frame(name = node_names, id = 0:(length(node_names)-1))
  
  # Asignar índices a los enlaces
  links$source_id <- match(links$source, nodes$name) - 1
  links$target_id <- match(links$target, nodes$name) - 1
  
  # Construir el gráfico Sankey con plotly
  p <- plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = nodes$name,
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5)
    ),
    link = list(
      source = links$source_id,
      target = links$target_id,
      value = links$value,
      label = paste(links$source, "→", links$target, ": ", links$value)
    ),
    height = 500  # <-- AQUÍ CAMBIÁS LA ALTURA
  ) %>%
    layout(
      title = "Distribución entre área de especilidad y aplicación",
      font = list(size = 12)
    )
  
  shinybusy::remove_modal_spinner()
  return(p)
}

#=========================================
# 3.1. Gráfico de barras con palabras clave más frecuentes (Top 20, con plotly)
#=========================================

generar_barras_palabras <- function(data_cloud, max_words = 20) {
  
  # Preprocesamiento de las palabras clave
  word_freq <- data_cloud %>%
    separate_rows(INDEX_PALABRAS_CLAVES, sep = "; ") %>%
    mutate(
      INDEX_PALABRAS_CLAVES = gsub("\\(.*\\)", "", INDEX_PALABRAS_CLAVES),
      INDEX_PALABRAS_CLAVES = trimws(INDEX_PALABRAS_CLAVES),
      INDEX_PALABRAS_CLAVES = str_to_upper(INDEX_PALABRAS_CLAVES)
    ) %>%
    filter(INDEX_PALABRAS_CLAVES != "", INDEX_PALABRAS_CLAVES != "#N/A") %>%
    count(INDEX_PALABRAS_CLAVES, sort = TRUE) %>%
    slice_head(n = max_words) %>%
    mutate(INDEX_PALABRAS_CLAVES = fct_reorder(INDEX_PALABRAS_CLAVES, n))  # orden para ggplot
  
  # Crear gráfico con ggplot
  g <- ggplot(word_freq, aes(x = INDEX_PALABRAS_CLAVES, y = n, text = paste0(INDEX_PALABRAS_CLAVES, ": ", n))) +
    geom_col(fill = "#00A499",alpha = 0.6) +
    coord_flip() +
    labs(title = "",
         x = "",
         y = "Frecuencia de palabras claves (top 20)") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
    )
  
  # Hacer interactivo con plotly
  ggplotly(g, tooltip = "text")
}

#=========================================
# 3.2 Comparación de palabras clave entre un autor y el resto (Chi-cuadrado)
#=========================================
generar_comparacion_palabras <- function(datos_filtrados, datos_tmp, grupo_comparacion, grupo_variable = "Palabras claves", max_words = 10) {
  tryCatch({
    
    # Definir columna a procesar según grupo seleccionado
    variable_base <- case_when(
      grupo_variable == "Palabras claves" ~ "INDEX_PALABRAS_CLAVES",
      grupo_variable == "Áreas de especialidad" ~ "AREA_COMPUTACION",
      grupo_variable == "Áreas de aplicación" ~ "AREAS",
      grupo_variable == "Subáreas de aplicación" ~ "CATEGORIAS",
      TRUE ~ "INDEX_PALABRAS_CLAVES"
    )
    
    # Grupo de comparación
    datos_grupo <- if (grupo_comparacion == "EL RESTO DE ACADÉMICOS(AS)") {
      filter(datos_tmp, !(NOMBRE_AUTOR %in% datos_filtrados$NOMBRE_AUTOR))
    } else {
      filter(datos_tmp, NOMBRE_AUTOR == grupo_comparacion)
    }
    
    # Preprocesamiento de términos
    procesar_terminos <- function(df) {
      df %>%
        select(!!sym(variable_base)) %>%
        filter(!is.na(!!sym(variable_base))) %>%
        separate_rows(!!sym(variable_base), sep = "[;,|]") %>%
        mutate(palabra = str_to_upper(trimws(gsub("\\(.*\\)", "", !!sym(variable_base)))) ) %>%
        filter(palabra != "", palabra != "#N/A") %>%
        count(palabra, name = "frecuencia")
    }
    
    freq_autor  <- procesar_terminos(datos_filtrados)
    freq_grupo  <- procesar_terminos(datos_grupo)
    
    total_autor <- sum(freq_autor$frecuencia)
    total_grupo <- sum(freq_grupo$frecuencia)
    
    # Comparación con test de Chi-cuadrado
    comparacion <- full_join(freq_autor, freq_grupo, by = "palabra") %>%
      replace_na(list(frecuencia.x = 0, frecuencia.y = 0)) %>%
      mutate(
        chisq = map2_dbl(frecuencia.x, frecuencia.y, ~ chisq.test(matrix(c(.x, total_autor - .x, .y, total_grupo - .y), nrow = 2), correct = FALSE)$residuals[1]),
        categoria = ifelse(chisq > 0, "Sobre-representado", "Sub-representado"),
        label = paste0(palabra, " (", frecuencia.x, " / ", frecuencia.y, ")")
      )
    
    # Seleccionar top 20 positivos y negativos (sin empates)
    comparacion_top <- bind_rows(
      comparacion %>% filter(chisq > 0) %>% arrange(desc(chisq)) %>% head(max_words),
      comparacion %>% filter(chisq < 0) %>% arrange(chisq) %>% head(max_words)
    ) %>%
      mutate(label = fct_reorder(label, chisq))
    
    # Gráfico final
    g <- ggplot(comparacion_top, aes(x = chisq, y = label, fill = categoria, text = paste0(label, "\nResiduo: ", round(chisq, 2)))) +
      geom_col() +
      geom_vline(xintercept = 0, color = "black", linewidth = 0.3) +
      scale_fill_manual(
        values = c("Sobre-representado" = "#00A499", "Sub-representado" = "gray80"),
        name = NULL
      ) +
      labs(
        x = "Residuo de Chi-cuadrado",
        y = grupo_variable
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 10)
      )
    
    ggplotly(g, tooltip = "text")
    
  }, error = function(e) {
    ggplotly(
      ggplot() +
        geom_blank() +
        labs(title = "Sin datos disponibles", x = "", y = "") +
        theme_minimal() +
        theme(
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 14)
        )
    )
  })
}

#=========================================
# 4. Gráfico de barras: fuentes de publicación
#=========================================
crear_grafico_barras <- function(datos) {
  if (nrow(datos) != 0) {
    conteo_fuentes <- datos %>%
      group_by(FUENTE) %>%
      summarise(Frecuencia = n()) %>%
      arrange(desc(Frecuencia)) %>%
      slice(1:20) %>%
      mutate(Tooltip_Text = str_to_title(FUENTE),
             FUENTE = ifelse(nchar(FUENTE) > 20, paste0(substr(FUENTE, 1, 20), "..."), FUENTE),
             FUENTE = toupper(FUENTE)) %>%
      group_by(FUENTE) %>%
      summarise(Frecuencia = sum(Frecuencia), Tooltip_Text = first(Tooltip_Text)) %>%
      ungroup()
    
    conteo_fuentes$FUENTE <- factor(conteo_fuentes$FUENTE, levels = conteo_fuentes$FUENTE[order(conteo_fuentes$Frecuencia, decreasing = TRUE)])
    
    ggplot(conteo_fuentes, aes(x = FUENTE, y = Frecuencia, text = Tooltip_Text)) +
      geom_bar(stat = "identity", fill = "#8C4799",alpha = 0.6) +
      labs(x = "Fuente de publicaciones", y = "Cantidad de publicaciones") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 12, face = "normal"),
        axis.title.y = element_text(size = 12, face = "normal"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "normal")
      )
  } else {
    return(NULL) 
  }
}


#=========================================
# 5. Gráfico de relaciones entre autores (red de coautorías)
#=========================================

generar_grafico_relaciones <- function(data_relation, autor_destacado = NULL) {
  
  if (nrow(data_relation) == 0) {
    return(
      ggplot() +
        geom_blank() +
        labs(title = "Red de Colaboraciones", x = "", y = "") +
        theme_void() +
        annotate("text", x = 1, y = 1, label = "No hay datos disponibles para generar el gráfico de colaboración.", size = 5, hjust = 0.5)
    )
  }
  
  # Paso 1: Preparar datos base
  seleccionar_datos <- data_relation %>%
    mutate(NOMBRE_AUTOR = str_to_upper(NOMBRE_AUTOR)) %>%
    select(NOMBRE_AUTOR, SCOPUS_ID, AUTORES_ID)
  
  # Paso 2: Separar coautores
  separar_autores_id <- seleccionar_datos %>%
    separate_rows(AUTORES_ID, sep = "; ")
  
  # Paso 3: Generar ID por autor
  nuevo_id_scopus_id <- seleccionar_datos %>%
    group_by(NOMBRE_AUTOR, AUTORES_ID) %>%
    summarise(FRECUENCIA = n(), .groups = "drop") %>%
    group_by(NOMBRE_AUTOR) %>%
    summarise(TOTAL_PUBLICACION = sum(FRECUENCIA), .groups = "drop") %>%
    arrange(NOMBRE_AUTOR) %>%
    mutate(NEW_SCOPUS_ID = row_number())
  
  # Paso 4: Enlazar datos
  dato_tmp_academico <- separar_autores_id %>%
    left_join(nuevo_id_scopus_id, by = "NOMBRE_AUTOR")
  
  dato_tmp_academico$NEW_AUTORES_ID <- NA
  indices <- match(dato_tmp_academico$AUTORES_ID, dato_tmp_academico$SCOPUS_ID)
  dato_tmp_academico$NEW_AUTORES_ID <- dato_tmp_academico$NEW_SCOPUS_ID[indices]
  
  # Paso 5: Crear nodos
  vertices <- dato_tmp_academico %>%
    select(NEW_SCOPUS_ID, NOMBRE_AUTOR, TOTAL_PUBLICACION) %>%
    distinct(.keep_all = TRUE)
  
  names(vertices) <- c("id", "label", "width")
  vertices$title <- paste(vertices$label, "\nPublicaciones:", vertices$width)
  
  # Paso 6: Manejo seguro del autor destacado
  autor_destacado <- if (!is.null(autor_destacado)) str_to_upper(autor_destacado) else NA_character_
  existe_autor <- !is.na(autor_destacado) && autor_destacado %in% vertices$label
  
  vertices$color.background <- if (existe_autor) {
    ifelse(vertices$label == autor_destacado, "#00A499", "lightblue")
  } else {
    rep("lightblue", nrow(vertices))
  }
  
  vertices$color.border <- if (existe_autor) {
    ifelse(vertices$label == autor_destacado, "#8C9090", "lightblue")
  } else {
    rep("lightblue", nrow(vertices))
  }
  
  # Paso 7: Crear aristas
  aristas <- dato_tmp_academico %>%
    group_by(NEW_SCOPUS_ID, NEW_AUTORES_ID) %>%
    summarise(REPETICIONES = n(), .groups = 'drop') %>%
    filter(!is.na(NEW_AUTORES_ID), NEW_SCOPUS_ID != NEW_AUTORES_ID) %>%
    mutate(from_to = paste(pmin(NEW_SCOPUS_ID, NEW_AUTORES_ID), pmax(NEW_SCOPUS_ID, NEW_AUTORES_ID), sep = "_")) %>%
    distinct(from_to, .keep_all = TRUE) %>%
    group_by(NEW_SCOPUS_ID, NEW_AUTORES_ID) %>%
    summarise(width = sum(REPETICIONES), .groups = 'drop') %>%
    rename(from = NEW_SCOPUS_ID, to = NEW_AUTORES_ID)
  
  aristas$title <- paste("Colaboraciones:", aristas$width)
  
  # Paso 8: Graficar red
  visNetwork(nodes = vertices, edges = aristas) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visLayout(randomSeed = 123) %>%
    visPhysics(stabilization = FALSE) %>%
    visNodes(
      color = list(
        background = vertices$color.background,
        border = vertices$color.border
      )
    ) %>%
    visEdges(
      color = list(color = "gray"),
      width = "width"
    )
}

#=========================================
# Generar dentrogramas
#=========================================

#Generar dentrograma jerárquico
generar_dendrograma_jerarquico <- function(df, metodo_dist = "euclidean", autor_resaltado = NULL) {
  df_textos <- df %>%
    mutate(across(c(INDEX_PALABRAS_CLAVES, RESUMEN, CATEGORIAS, AREAS), ~replace_na(., ""))) %>%
    group_by(NOMBRE_AUTOR) %>%
    summarise(
      texto_completo = paste(INDEX_PALABRAS_CLAVES, RESUMEN, CATEGORIAS, AREAS, collapse = " "),
      .groups = "drop"
    )
  
  corpus <- VCorpus(VectorSource(df_textos$texto_completo))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  dtm <- DocumentTermMatrix(corpus)
  m <- as.matrix(dtm)
  rownames(m) <- df_textos$NOMBRE_AUTOR
  m <- m[, colSums(m) > 0, drop = FALSE]
  
  if (nrow(m) < 2) {
    return(plotly::plot_ly() %>%
             layout(annotations = list(text = "No hay suficientes académicos", x = 0.5, y = 0.5,
                                       showarrow = FALSE, font = list(size = 16))))
  }
  
  dist_matrix <- if (metodo_dist == "correlation") {
    as.dist(1 - cor(t(m), method = "pearson", use = "pairwise.complete.obs"))
  } else {
    dist(m, method = metodo_dist)
  }
  
  hc <- hclust(dist_matrix)
  dend <- as.dendrogram(hc)
  dend_data <- ggdendro::dendro_data(dend, type = "rectangle")
  labels_df <- ggdendro::label(dend_data)
  
  # Normalizar para comparación
  etiquetas_norm <- str_trim(str_to_lower(labels_df$label))
  autor_resaltado_norm <- str_trim(str_to_lower(autor_resaltado))
  
  labels_df$resaltado <- if (!is.null(autor_resaltado)) {
    etiquetas_norm == autor_resaltado_norm
  } else {
    rep(FALSE, nrow(labels_df))
  }
  
  # Posiciones para eje x
  posiciones_x <- labels_df$x
  etiquetas <- labels_df$label
  
  # Segmento barra solo si hay autor resaltado
  segmento_barra <- NULL
  if (!is.null(autor_resaltado) && any(labels_df$resaltado)) {
    segmento_barra <- labels_df %>%
      filter(resaltado) %>%
      mutate(
        xstart = x - 0.4,
        xend = x + 0.4,
        ybarra = -0.25 * max(dend_data$segments$y)
      )
  }
  
  p <- ggplot() +
    geom_segment(data = ggdendro::segment(dend_data),
                 aes(x = x, y = y, xend = xend, yend = yend)) +
    # Solo agrega barra si corresponde
    {
      if (!is.null(segmento_barra)) {
        geom_segment(data = segmento_barra,
                     aes(x = xstart, xend = xend, y = ybarra, yend = ybarra),
                     color = "#8C4799", linewidth = 1.2)
      }
    } +
    scale_x_continuous(breaks = posiciones_x, labels = etiquetas) +
    labs(x = "Académicos", y = "Distancia") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      plot.margin = unit(c(1, 1, 6, 1), "lines")
    )
  
  ggplotly(p)
}