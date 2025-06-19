perfil_variable_global <- reactiveVal(NULL)

# Función para crear la pestaña de análisis de palabras
tab_analisis_dominio = function(input, output, session, datos_tmp) {
  
  # Si no hay datos disponibles, mostrar un mensaje
  if (nrow(datos_tmp) == 0) {
    output$OUT_word_analysis <- renderPlot({
      plot(NA, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
      text(0.5, 0.5, "No hay datos disponibles para los filtros seleccionados.", cex = 1.5)
    })
    return(NULL)
  }
  
  dominio_campo <- reactive({
    switch(input$SIn_tipo_dominio,
           "Áreas de especialidad" = "AREA_COMPUTACION",
           "Áreas de aplicación" = "AREAS",
           "Subáreas de aplicación" = "CATEGORIAS",
           "Palabras claves" = "INDEX_PALABRAS_CLAVES")
  })
  
  
  processed_data <- reactive({
    req(input$SIn_tipo_documento)
    campo <- dominio_campo()
    
    datos_filtrados <- datos_tmp
    
    if (input$SIn_tipo_documento == "WOS") {
      datos_filtrados <- filter(datos_filtrados, WOS == "SI")
    } else if (input$SIn_tipo_documento == "SCOPUS") {
      datos_filtrados <- filter(datos_filtrados, WOS == "NO")
    }
    
    datos_filtrados %>%
      select(NOMBRE_AUTOR, WOS, ANO, dominio = all_of(campo)) %>%
      separate_rows(dominio, sep = ";") %>%
      mutate(
        dominio = gsub("\\(.*\\)", "", dominio),
        dominio = gsub("[0-9]", "", dominio),
        dominio = gsub("'", "", dominio),
        dominio = str_to_title(trimws(dominio)),
        NOMBRE_AUTOR = str_to_title(NOMBRE_AUTOR),
        WOS = ifelse(WOS == "SI", "Web of Science", "Scopus")
      ) %>%
      filter(dominio != "", dominio != "#N/A") %>%
      group_by(NOMBRE_AUTOR, WOS, dominio, ANO) %>%
      summarise(Frecuencia = n(), .groups = "drop") %>%
      rename(
        Autor = NOMBRE_AUTOR,
        Tipo_Indexacion = WOS,
        Palabra = dominio,
        Año = ANO
      )
  })
  
  output$In_dominio_clave <- renderUI({
    req(processed_data())
    claves <- sort(unique(processed_data()$Palabra[!grepl("^[-/]", processed_data()$Palabra)]))
    selectInput("In_dominio_clave",
                "Seleccione una palabra",
                choices = claves,
                selected = claves[1],
                width = "80%")
  })
  
  filtered_data <- reactive({
    req(processed_data(), input$In_dominio_clave)
    palabra <- input$In_dominio_clave
    
    processed_data() %>%
      filter(Palabra == palabra)
  })
  
  output$OUT_word_analysis <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(
        plot_ly(type = "scatter", mode = "markers") %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              x = 0.5, y = 0.5,
              text = "No hay datos disponibles para los filtros seleccionados.",
              showarrow = FALSE,
              font = list(size = 20)
            )
          )
      )
    }
    
    data$Autor <- factor(data$Autor, levels = unique(sort(data$Autor, decreasing = TRUE)))
    
    plot_app <- ggplot(data, aes(x = Año - 0.5, xend = Año + 0.5, y = Autor, yend = Autor,
                                 alpha = Frecuencia, color = Frecuencia)) +
      geom_segment(size = 4) +
      scale_alpha_continuous(range = c(0.3, 1), guide = "none") +
      scale_color_gradient(low = "#B2DFDB", high = "#007366") +
      theme_minimal() +
      xlim(min(data$Año) - 1, max(data$Año) + 1) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none",
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold")
      ) +
      xlab("Años") +
      ylab("Académicos o académicas")
    
    ggplotly(plot_app) %>%
      layout(xaxis = list(dtick = 1, tick0 = min(data$Año), tickmode = "linear")) %>%
      config(locale = "es")
  })
}