#=======================================================================
# DEFINICIÓN DEL SERVIDOR DE LA APLICACIÓN SHINY
#=======================================================================

server <- function(input, output, session) {  # <- INICIO de la función server
  
  #=====================================================================
  # A) CARGA DE LÓGICA MODULAR E INICIALIZACIÓN
  #=====================================================================
  
  source("www/SOURCE/SERVER/server_conf_inicial.R")             # <- Carga configuración inicial y datos
  source("www/SOURCE/SERVER/server_ayuda.R")                    # <- Carga sistema de ayuda contextual
  source("www/SOURCE/SERVER/server_tab_resumen.R")              # <- Carga lógica del panel de resumen
  source("www/SOURCE/SERVER/server_tab_indicadores.R")          # <- Carga lógica del panel de indicadores
  source("www/SOURCE/SERVER/server_tab_perfiles.R")             # <- Carga lógica del panel de perfiles
  source("www/SOURCE/SERVER/server_tab_analisis_dominio.R")     # <- Carga lógica del panel de dominio
  source("SUPPORTING_CODES/014_buscar_palabra_archivos.R")
  source("www/SOURCE/SERVER/server_tab_busqueda.R")

  
  
  cat("\014")  # <- Limpieza de consola en entorno local
  print("Iniciando servidor...")  # <- Mensaje en consola
  #=====================================================================
  # B) VARIABLES REACTIVAS GLOBALES
  #=====================================================================
  
  datos_globales <- reactiveVal(NULL)  # <- Contenedor reactivo para los datos
  
  show_modal_spinner(  # <- Inicio del spinner de carga inicial
    text = "Cargando repositorio de datos...",
    spin = "fading-circle",
    color = "#FF9800"
  )  # <- Fin del spinner de carga inicial
  
  datos <- conf_inicial(input, output, session, datos_globales)  # <- Carga de datos
  datos_globales(datos)  # <- Guarda los datos cargados
  
  #=====================================================================
  # C) ACTUALIZACIÓN DINÁMICA DE INPUTS
  #=====================================================================
  
  #--- 1. Cambio de Universidad -------------------------------------------------
  observeEvent(input$SIn_Universidad, {  # <- INICIO observeEvent universidad
    datos <- datos_globales()
    secciones_disponibles <- unique(datos$SESION[datos$UNIVERSIDAD == input$SIn_Universidad])
    updateSelectInput(session, "SIn_Seccion",
                      choices = sort(secciones_disponibles),
                      selected = ifelse(input$SIn_Seccion %in% secciones_disponibles,
                                        input$SIn_Seccion,
                                        secciones_disponibles[1]
                      )  # <- Fin ifelse
    )  # <- Fin updateSelectInput
    datos_filtrados <- datos[datos$UNIVERSIDAD == input$SIn_Universidad & datos$SESION == input$SIn_Seccion, ]
    if (nrow(datos_filtrados) > 0) {  # <- INICIO if hay datos
      ano_min <- min(datos_filtrados$ANO, na.rm = TRUE)
      ano_max <- max(datos_filtrados$ANO, na.rm = TRUE)
      updateSliderTextInput(session, "SIn_Periodo",
                            choices = as.character(seq(ano_min, ano_max)),
                            selected = c(as.character(max(ano_min, ano_max - 5)),
                                         as.character(ano_max))
      )  # <- Fin updateSliderTextInput
    }  # <- FIN if hay datos
  })  # <- FIN observeEvent universidad
  
  #--- 2. Cambio de Sección ------------------------------------------------------
  observeEvent(input$SIn_Seccion, {  # <- INICIO observeEvent sección
    datos <- datos_globales()
    datos_filtrados <- datos[datos$UNIVERSIDAD == input$SIn_Universidad & datos$SESION == input$SIn_Seccion, ]
    if (nrow(datos_filtrados) > 0) {  # <- INICIO if hay datos
      ano_min <- min(datos_filtrados$ANO, na.rm = TRUE)
      ano_max <- max(datos_filtrados$ANO, na.rm = TRUE)
      updateSliderTextInput(session, "SIn_Periodo",
                            choices = as.character(seq(ano_min, ano_max)),
                            selected = c(as.character(max(ano_min, ano_max - 5)),
                                         as.character(ano_max))
      )  # <- Fin updateSliderTextInput
    }  # <- FIN if hay datos
  })  # <- FIN observeEvent sección
  
  # #--- 3. Cambio de Año ----------------------------------------------------------
  # observeEvent(input$SIn_Periodo, {  # <- INICIO observeEvent periodo
  #   datos <- datos_globales()
  #   datos_filtrados <- datos %>%
  #     filter(UNIVERSIDAD == input$SIn_Universidad,
  #            SESION == input$SIn_Seccion,
  #            ANO >= as.integer(input$SIn_Periodo[1]),
  #            ANO <= as.integer(input$SIn_Periodo[2]))
  #   output$In_perfil_variable <- renderUI({  # <- INICIO renderUI selector de autor
  #     selectInput("SIn_Perfil_autores", "Seleccione un(a) académico(a)",
  #                 choices = unique(datos_filtrados$NOMBRE_AUTOR),
  #                 selected = isolate(input$SIn_Perfil_autores),
  #                 width = "80%")
  #   })  # <- FIN renderUI selector de autor
  # })  # <- FIN observeEvent periodo
  
  #--- 4. Cambio de Tab Principal ------------------------------------------------
  observeEvent(input$tabs_principales, {  # <- INICIO observeEvent tabs
    cat("El usuario cambió a la pestaña:", input$tabs_principales, "\n")
    if (input$tabs_principales == "Perfiles" && input$In_Sel_Perfil == "Institucionales") {  # <- INICIO if pestaña Perfiles Institucional
      output$In_perfil_variable <- renderUI({  # <- INICIO renderUI texto institucional
        HTML(paste('<br>', h5(paste("UNIVERSIDAD DE SANTIAGO DE CHILE", "-", "DEPTO. DE INGENIERÍA INFORMÁTICA"),
                              class = "title-part text-center")))
      })  # <- FIN renderUI texto institucional
    }  # <- FIN if pestaña Perfiles Institucional
  })  # <- FIN observeEvent tabs
  
  #=====================================================================
  # D) BOTÓN ANALIZAR
  #=====================================================================
  observeEvent(input$btn_analizar, {  # <- INICIO observeEvent botón analizar
    show_modal_spinner(text = "Analizando datos...", spin = "fading-circle", color = "#FF9800")
    datos <- datos_globales()
    datos_filtrados <- datos %>%
      filter(UNIVERSIDAD == input$SIn_Universidad,
             SESION == input$SIn_Seccion,
             ANO >= as.integer(input$SIn_Periodo[1]),
             ANO <= as.integer(input$SIn_Periodo[2]))
    datos_tmp <- datos_filtrados
    
    #--- Filtrado por pestaña "Indicadores"
    if (input$tabs_principales == "Indicadores") {  # <- INICIO if pestaña Indicadores
      if (input$In_Sel_tipo_indexacion == "Web of Science") datos_tmp <- filter(datos_tmp, WOS == "SI")
      if (input$In_Sel_tipo_indexacion == "Scopus") datos_tmp <- filter(datos_tmp, WOS == "NO")
      updateAwesomeRadio(session, "In_Sel_tipo_indexacion_perfiles", selected = input$In_Sel_tipo_indexacion)
    }  # <- FIN if pestaña Indicadores
    
    #--- Filtrado por pestaña "Perfiles"
    if (input$tabs_principales == "Perfiles") {  # <- INICIO if pestaña Perfiles
      if (input$In_Sel_tipo_indexacion_perfiles == "Web of Science") datos_tmp <- filter(datos_tmp, WOS == "SI")
      if (input$In_Sel_tipo_indexacion_perfiles == "Scopus") datos_tmp <- filter(datos_tmp, WOS == "NO")
      updateAwesomeRadio(session, "In_Sel_tipo_indexacion", selected = input$In_Sel_tipo_indexacion_perfiles)
      if (input$In_Sel_Perfil != "Institucionales") {  # <- INICIO if no institucional
        datos_tmp <- filter(datos_tmp, NOMBRE_AUTOR == input$SIn_Perfil_autores)
      }  # <- FIN if no institucional
    }  # <- FIN if pestaña Perfiles
    
    #--- Verificación y ejecución del análisis
    if (nrow(datos_tmp) > 0) {  # <- INICIO if hay datos
      tab_resumen(input, output, session, datos_filtrados)
      tab_indicadores(input, output, session, datos_tmp)
      tab_perfiles(input, output, session, datos_filtrados, datos_tmp)
      tab_analisis_dominio(input, output, session, datos_filtrados)
      
      
    } else {  # <- ELSE no hay datos
      shinyalert(title = "Error",
                 text = "No hay datos disponibles para el análisis dada esta configuración de opciones.",
                 type = "error",
                 confirmButtonText = "Aceptar")
    }  # <- FIN if-else verificación datos
    remove_modal_spinner()
  })  # <- FIN observeEvent botón analizar
  
  #=====================================================================
  # E) CAMBIO DE PERFIL SELECCIONADO EN LA VISTA PERFILES
  #=====================================================================
  observeEvent(input$In_Sel_Perfil, {  # <- INICIO observeEvent cambio de perfil
    if (input$In_Sel_Perfil == "Institucionales") {  # <- INICIO if institucional
      perfil_variable_global(paste(input$SIn_Universidad, "-", input$SIn_Seccion))
      output$In_perfil_variable <- renderUI({  # <- INICIO renderUI institucional
        HTML(paste('<br>', h5(paste(input$SIn_Universidad, "-", input$SIn_Seccion),
                              class = "title-part text-center")))
      })  # <- FIN renderUI institucional
    } else {  # <- ELSE perfil específico
      datos <- datos_globales()
      datos_filtrados <- datos %>%
        filter(UNIVERSIDAD == input$SIn_Universidad,
               SESION == input$SIn_Seccion,
               ANO >= as.integer(input$SIn_Periodo[1]),
               ANO <= as.integer(input$SIn_Periodo[2]))
      output$In_perfil_variable <- renderUI({  # <- INICIO renderUI selector de autor
        selectInput("SIn_Perfil_autores", "Seleccione un(a) académico(a)",
                    choices = unique(datos_filtrados$NOMBRE_AUTOR),
                    selected = isolate(input$SIn_Perfil_autores),
                    width = "80%")
      })  # <- FIN renderUI selector de autor
    }  # <- FIN if-else perfil
  })  # <- FIN observeEvent cambio de perfil
  
  #=====================================================================
  # F) ACTIVACIÓN DEL SISTEMA DE AYUDA CONTEXTUAL
  #=====================================================================
  server_ayuda(input, output, session)  # <- Activa sistema de ayuda
  
  
  #=====================================================================
  # G) GUARDAR DATOS
  #=====================================================================
  datos_tmp <- read.csv("www/BD/BD_papers.csv", header = TRUE, sep = "|",quote = "")
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Base_Datos_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(datos_tmp, path = file)
    }
  )
  
  #=====================================================================
  # H) CIERRE DEL SPINNER INICIAL DE CARGA
  #=====================================================================
  remove_modal_spinner()  # <- Cierra el spinner inicial de carga

  server_tab_busqueda(input, output, session, datos)

}  # <- FIN de la función server

#=======================================================================
# FIN DEL SCRIPT DEL SERVIDOR
#=======================================================================
