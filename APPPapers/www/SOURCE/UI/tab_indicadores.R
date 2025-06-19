#==========================================
# Panel de pestaña "Indicadores" con icono y controles
#==========================================

tab_indicadores <- tabPanel(
  "Indicadores",
  icon = icon("chart-bar", lib = "font-awesome"),
  
  br(),
  
  # Título principal de la sección
  fluidRow(
    h4(class = "title-part text-center",
       bsButton("boton_ayuda14", label = img(src = "IMG/img_pregunta.png", height = "20px", width = "20px"), size = "small"),
       strong("INDICADORES BIBLIOMÉTRICOS")
    )
  ),
  
  br(),
  
  #==============================
  # Contenedor principal
  #==============================
  fluidRow(
    column(12, align = "center",
           
           #-----------------------------------
           # Controles de selección
           #-----------------------------------
           fluidRow(
             column(3, offset = 3, align = "left",
                    awesomeRadio(
                      inputId = "In_Sel_Indicadores",
                      label = "Mostrar indicadores",
                      choices = c("Institucionales", "Por autor(a)"),
                      selected = "Institucionales",
                      status = "info"
                    )
             ),
             
             column(4, offset = 1, align = "left",
                    awesomeRadio(
                      inputId = "In_Sel_tipo_indexacion",
                      label = "Tipo de indexación",
                      choices = c("Todas", "Web of Science", "Scopus"),
                      selected = "Todas",
                      status = "info"
                    )
             )
           ),
           
           br(),
           hr(),
           br(),
           
           #-----------------------------------
           # Tablas de rendimiento y colaboración
           #-----------------------------------
           h2(
             bsButton("boton_ayuda15", label = img(src = "IMG/img_pregunta.png", height = "20px", width = "20px"), size = "small"),
             "Rendimiento"
           ),
           withMathJax(dataTableOutput("table_Indexes_rendimiento")),
           
           h2(
             bsButton("boton_ayuda16", label = img(src = "IMG/img_pregunta.png", height = "20px", width = "20px"), size = "small"),
             "Colaboración"
           ),
           withMathJax(dataTableOutput("table_Indexes_colaboracion"))
    )
  )
)
