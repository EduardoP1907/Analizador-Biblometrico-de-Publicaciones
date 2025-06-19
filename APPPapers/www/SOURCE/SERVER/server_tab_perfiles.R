#=========================================
# Variable reactiva global: perfil seleccionado
#=========================================
perfil_variable_global <- reactiveVal(NULL)

#=========================================
# Funciones auxiliares
#=========================================
source("www/SOURCE/UTILS/server_funciones.R")

#=========================================
# Módulo principal: Análisis de Perfiles
#=========================================
tab_perfiles <- function(input, output, session, datos_tmp, datos_filtrados) {
  
  #=========================================
  # VISTA INSTITUCIONAL
  #=========================================
  if (input$In_Sel_Perfil == "Institucionales") {
    
    # Imagen del departamento (centrada y estilizada)
    output$OUT_ImageProfile <- renderUI({
      tags$div(
        style = "text-align: center;",
        tags$img(
          src = datos_tmp$URL_FOTO_DEPARTAMENTO[1],
          style = "width: 90%; max-width: 380px; border-radius: 10px; box-shadow: 0 4px 10px rgba(0,0,0,0.15);"
        )
      )
    })
    
    # Cálculo de publicaciones por tipo de indexación
    total_publicaciones <- nrow(datos_tmp)
    publicaciones_wos    <- sum(datos_tmp$WOS == "SI")
    publicaciones_scopus <- sum(datos_tmp$WOS != "SI")
    
    # Mostrar resumen de producción
    output$OUT_ProduccionProfile <- renderUI({
      etiquetas <- c("Total:", "Web of Science:", "Scopus:")
      valores   <- c(total_publicaciones, publicaciones_wos, publicaciones_scopus)
      estilo    <- "font-size: 14px; color: rgb(0, 164, 153); font-weight: bold;"
      
      lista <- tagList(lapply(1:3, function(i) {
        tags$li(HTML(paste(
          etiquetas[i], 
          span(valores[i], style = estilo),
          paste0(" (", round(valores[i] / total_publicaciones * 100, 1), "%)")
        )))
      }))
      
      HTML(paste(tags$p("El", tags$b("número de publicaciones"), "es:"), tags$ol(lista)))
    })
    
    # Render de gráficos generales para la unidad
    output$OUT_PlotbarProfile       <- renderPlotly({ generar_grafico_puntos(datos_tmp, input$SIn_Periodo) })
    output$OUT_sankey_diagram       <- renderUI({ generar_grafico_sankey(datos_tmp) })
    output$selector_grupo1          <- NULL
    output$selector_grupo2          <- NULL
    output$OUT_word_freq            <- renderPlotly({ generar_barras_palabras(datos_tmp) })
    output$OUT_graphic_fonts        <- renderPlotly({ crear_grafico_barras(datos_tmp) })
    output$OUT_relationship_graph   <- renderVisNetwork({ generar_grafico_relaciones(datos_tmp, NULL) })
    output$OUT_dendro_euclidiano    <- renderPlotly({ generar_dendrograma_jerarquico(datos_tmp, metodo_dist = "euclidean") })
    output$OUT_dendro_correlacion   <- renderPlotly({ generar_dendrograma_jerarquico(datos_tmp, metodo_dist = "correlation") })
    
  } else {
    
    #=========================================
    # VISTA POR AUTOR(A)
    #=========================================
    
    # Imagen del académico(a)
    output$OUT_ImageProfile <- renderUI({
      tags$div(
        style = "text-align: center;",
        tags$img(
          src = datos_filtrados$URL_FOTO_ACADEMICO[[1]],
          style = "width: 40%; max-width: 380px; border-radius: 10px; box-shadow: 0 4px 10px rgba(0,0,0,0.15);"
        )
      )
    })
    
    # Cálculo de producción e IDs Scopus
    total_publicaciones <- nrow(datos_filtrados)
    publicaciones_wos    <- sum(datos_filtrados$WOS == "SI")
    publicaciones_scopus <- sum(datos_filtrados$WOS != "SI")
    scopus_ids           <- unique(datos_filtrados$SCOPUS_ID)
    
    # Mostrar resumen de producción e identificadores
    output$OUT_ProduccionProfile <- renderUI({
      etiquetas <- c("Total:", "Web of Science:", "Scopus:")
      valores   <- c(total_publicaciones, publicaciones_wos, publicaciones_scopus)
      estilo    <- "font-size: 14px; color: rgb(0, 164, 153); font-weight: bold;"
      
      lista_valores <- tagList(lapply(1:3, function(i) {
        tags$li(HTML(paste(
          etiquetas[i],
          span(valores[i], style = estilo),
          paste0(" (", round((valores[i] / total_publicaciones) * 100, 1), "%)")
        )))
      }))
      
      lista_ids <- tagList(lapply(scopus_ids, function(id) {
        tags$li(HTML(paste("ID:", span(id, style = estilo))))
      }))
      
      HTML(paste(
        tags$p("El", tags$b("número de publicaciones"), "es:"), tags$ol(lista_valores),
        tags$p("El", tags$b("número de identificador Scopus"), "es:"), tags$ol(lista_ids)
      ))
    })
    
    # Render de gráficos individuales
    output$OUT_PlotbarProfile       <- renderPlotly({ generar_grafico_puntos(datos_filtrados, input$SIn_Periodo) })
    output$OUT_sankey_diagram       <- renderUI({ generar_grafico_sankey(datos_filtrados) })
    
    # Selectores para comparación
    output$selector_grupo1 <- renderUI({
      selectInput(
        inputId = "grupo1",
        label = "Seleccione el tipo de comparación que desea realizar",
        choices = c("Áreas de especialidad", "Áreas de aplicación", "Subáreas de aplicación", "Palabras claves"),
        selected = "Áreas de especialidad",
        width = "100%"
      )
    })
    
    output$selector_grupo2 <- renderUI({
      selectInput(
        inputId = "grupo2",
        label = paste0(unique(datos_filtrados$NOMBRE_AUTOR), " comparado(a) con "),
        choices = c(
          "EL RESTO DE ACADÉMICOS(AS)",
          setdiff(unique(datos_tmp$NOMBRE_AUTOR), unique(datos_filtrados$NOMBRE_AUTOR))
        ),
        selected = "EL RESTO DE ACADÉMICOS(AS)",
        width = "100%"
      )
    })
    
    # Gráficos comparativos y redes
    output$OUT_word_freq           <- renderPlotly({ generar_comparacion_palabras(datos_filtrados, datos_tmp, input$grupo2, input$grupo1, 10) })
    output$OUT_graphic_fonts       <- renderPlotly({ crear_grafico_barras(datos_filtrados) })
    output$OUT_relationship_graph  <- renderVisNetwork({ generar_grafico_relaciones(datos_tmp, unique(datos_filtrados$NOMBRE_AUTOR)) })
    output$OUT_dendro_euclidiano   <- renderPlotly({ generar_dendrograma_jerarquico(datos_tmp, metodo_dist = "euclidean", autor_resaltado = input$SIn_Perfil_autores) })
    output$OUT_dendro_correlacion  <- renderPlotly({ generar_dendrograma_jerarquico(datos_tmp, metodo_dist = "correlation", autor_resaltado = input$SIn_Perfil_autores) })
    
  } # Fin de condicional por tipo de perfil
  
} # Fin de función tab_perfiles
