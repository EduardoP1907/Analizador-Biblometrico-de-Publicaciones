#=======================================
# UI: Interfaz de Usuario
#=======================================

#----------------------------------------------
# Cargar scripts de pestañas externas del UI
#----------------------------------------------
source("www/SOURCE/UTILS/cargar_bibliotecas.R")
source("www/SOURCE/UI/tab_resumen.R")
source("www/SOURCE/UI/tab_indicadores.R")
source("www/SOURCE/UI/tab_perfiles.R")
source("www/SOURCE/UI/tab_analisis_dominio.R")
source("www/SOURCE/UI/tab_busqueda.R")

#--------------------------------------------------
# A) Interfaz principal
#--------------------------------------------------

ui <- fluidPage(
  
  # Habilitar MathJax (fórmulas) y JS dinámico
  withMathJax(),
  useShinyjs(),
  useShinyalert(),
  
  # Meta datos HTML
  tags$head(
    # Google Analytics GA4
    tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=G-ZEHTGQZ74P"),
    tags$script(HTML("
    window.dataLayer = window.dataLayer || [];
    function gtag(){dataLayer.push(arguments);}
    gtag('js', new Date());
    gtag('config', 'G-ZEHTGQZ74P');
    ")),
    
    # Otros elementos ya presentes
    HTML('<link rel="icon", href="IMG/logo2.png", type="image/png" />'),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  titlePanel("", windowTitle = "Analizador Bibliométrico de Publicaciones"),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  
  #--------------------------------------------------
  # B) Panel principal con logo, controles y pestañas
  #--------------------------------------------------
  mainPanel(
    width = 12,
    column(10, offset = 1, class = "white-background",
           
           # Encabezado: logo + título
           fluidRow(
             br(),
             column(3, offset = 1, img(src = 'IMG/logo.png', align = "left", style = "width: 60%;")),
             column(7, offset = 0, h2(
               span(class = "title-part", strong("ANALIZADOR BIBLIOMÉTRICO DE PUBLICACIONES")),
               span(class = "version-part", "1.4.0v")
             ))
           ),
           
           br(),
           
           # Controles de entrada: Universidad, Sección, Periodo
           fluidRow(
             column(4, offset = 2, selectInput("SIn_Universidad", "Universidad",
                                               c("UNIVERSIDAD DE SANTIAGO DE CHILE"), width = "100%"),
                    selectInput("SIn_Seccion", "Sección",
                                c("DEPTO. DE INGENIERÍA INFORMÁTICA"), width = "100%")
             ),
             column(3, offset = 1, column(12, offset = 1,
                                          sliderTextInput(
                                           inputId = "SIn_Periodo",
                                            label = "Periodo (años)",
                                            choices = as.character(1990:2025),  # lista de años como texto
                                            selected = c("2000", "2025"),       # rango inicial como texto
                                            grid = TRUE,                        # muestra ticks uniformes
                                            dragRange = TRUE,                   # permite arrastrar el rango completo
                                            width = "100%"
                                          )
                                ),
                    
                    column(10, offset = 5, 
                           div(actionButton("btn_analizar", 
                                            label = tagList(icon("search"), "Analizar"), 
                                            style="color: #fff; background-color: #009688; border-color: #009688")
                           ))
                    
             ),
             
           ),
           
           br(),
           
           # Paneles/pestañas de análisis
           fluidRow(
              column(10, offset = 1, align = "left",
                     tabsetPanel(id="tabs_principales",type = "tabs", 
                                 tab_resumen, 
                                 tab_indicadores,
                                 tab_perfiles,
                                 tab_analisis_dominio,
                                 tab_busqueda
                     ),
                     br(), br(), br()
              )
            )
    )
  ),
  
  #--------------------------------------------------
  # C) Footer con descarga de base de datos
  #--------------------------------------------------
  fluidRow(
    column(12, tags$div(
      class = "footer-text",
      "Fecha: 08 de mayo de 2025 | Versión: 1.4.0",
      downloadButton("downloadData", "Descargar Base de Datos", 
                     style = "margin-left: 10px; display: inline-block; background-color: transparent; color: inherit; border: none;"),
      style = "display: flex; align-items: center; justify-content: center; color: #333; font-size: 14px;"
    ))
  )
)
