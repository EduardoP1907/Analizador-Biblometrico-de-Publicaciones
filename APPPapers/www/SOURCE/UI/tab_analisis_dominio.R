#=========================================
# UI: Panel de pestaña "Análisis de palabras"
#=========================================

tab_analisis_dominio = tabPanel("Ánalisis de dominio", icon = icon("file-word", class = "fa-regular"),
                                 
                                 br(),
                                 
                                 #-----------------------------------------
                                 # Título del panel
                                 #-----------------------------------------
                                 fluidRow(
                                   h4(class = "title-part text-center",
                                      bsButton("boton_ayuda23", label = img(src = "IMG/img_pregunta.png", height = "20px", width = "20px"), size = "small"),
                                      strong("ANÁLISIS DE DOMINIO")
                                   )
                                 ),
                                 
                                 #-----------------------------------------
                                 # Selectores centrados
                                 #-----------------------------------------
                                 fluidRow(
                                   column(12, align = "center",offset = 0,
                                          fluidRow(
                                            column(4, align = "center", selectInput("SIn_tipo_documento", "Seleccione tipo de documento:",
                                                                                    c("Todos", "WOS", 
                                                                                      "SCOPUS"), width = "100%")),
                                            column(4, offset = 0, selectInput("SIn_tipo_dominio", "Seleccione el dominio",
                                                                              c("Áreas de especialidad", "Áreas de aplicación", 
                                                                                "Subáreas de aplicación","Palabras claves"), width = "100%")),
                                            column(4, align = "center", uiOutput("In_dominio_clave"))
                                          )
                                   )
                                 ),
                                 
                                 br(),
                                 
                                 #-----------------------------------------
                                 # Gráfico central
                                 #-----------------------------------------
                                 fluidRow(
                                   column(8, align = "center",offset = 2,
                                          plotlyOutput("OUT_word_analysis")
                                   )
                                 )
)
