#===========================================
# Panel de pestaña: "Perfiles"
#===========================================

tab_perfiles = tabPanel("Perfiles", icon = icon("fa-solid fa-user", lib = "font-awesome"),
                        
                        br(),
                        
                        #===========================================
                        # Encabezado principal
                        #===========================================
                        fluidRow(
                          h4(class = "title-part text-center",
                             bsButton("boton_ayuda18", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                             strong("ANÁLISIS DE PERFILES")
                          )
                        ),
                        
                        #===========================================
                        # Filtros de selección
                        #===========================================
                        fluidRow(
                          column(2, offset = 1, align = "left",
                                 awesomeRadio(
                                   inputId = "In_Sel_Perfil",
                                   label = "Mostrar indicadores", 
                                   choices = c("Institucionales", "Por autor(a)"),
                                   selected = "Institucionales",
                                   status = "info"
                                 )
                          ),
                          column(6, align = "left",
                                 uiOutput("In_perfil_variable")
                          ),
                          column(3, align = "left",
                                 awesomeRadio(
                                   inputId = "In_Sel_tipo_indexacion_perfiles",
                                   label = "Tipo de indexación", 
                                   choices = c("Todas", "Web of Science", "Scopus"),
                                   selected = "Todas",
                                   status = "info"
                                 )
                          )
                        ),
                        
                        hr(), br(),
                        
                        #===========================================
                        # Sección: Datos generales
                        #===========================================
                        div(
                          style = "text-align: center;",
                          h2(
                            bsButton("boton_ayuda17", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                            "Datos generales"
                          )
                        ),
                        br(), br(),
                        
                        fluidRow(
                          column(5, offset = 1, align = "left",
                                 fluidRow(
                                   column(8, align = "center",
                                          br(), br(),
                                          br(), br(),
                                          uiOutput('OUT_ImageProfile')
                                   )
                                 ),
                                 fluidRow(
                                   column(10, offset = 2, align = "left",
                                          br(), br(),
                                          uiOutput('OUT_ProduccionProfile')
                                   )
                                 )
                          ),
                          column(6, align = "left",
                                 plotlyOutput('OUT_PlotbarProfile', height = "400px")
                          )
                        ),
                        
                        hr(), br(),
                        
                        #===========================================
                        # Sección: Gráfico sankey
                        #===========================================
                        div(
                          style = "text-align: center;",
                          h2(
                            bsButton("boton_ayuda19", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                            "Gráfico Sankey"
                          )
                        ),
                        br(), br(),
                        
                        fluidRow(
                          column(8, align = "center", offset = 2,
                                 uiOutput('OUT_sankey_diagram'),
                                 
                                 div(style = "display: flex; justify-content: space-between;",
                                     div(style = "flex: 1; font-size: left; font-weight: bold;", "Área de especialidad"),
                                     div(style = "flex: 1; font-size: right; font-weight: bold;", "Área de aplicación")
                                 ),
                                 
                          )
                        ),
                        
                        hr(), br(),
                        
                        #===========================================
                        # Sección: Frecuencia de palabras
                        #===========================================
                        div(
                          style = "text-align: center;",
                          h2(
                            bsButton("boton_ayuda20", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                            "Palabras claves más frecuentes"
                          )
                        ),
                        br(), br(),
                        
                        fluidRow(
                          column(10, offset = 1, align = "center",
                                 
                                 # Fila con selectores en paralelo
                                 fluidRow(
                                   column(6, uiOutput("selector_grupo1")),
                                   
                                   column(6, uiOutput("selector_grupo2")),
                                 ),
                                 
                                 # Gráfico interactivo
                                 plotlyOutput("OUT_word_freq", height = "700px")
                          )
                        ),
                        
                        hr(), br(),
                        
                        #===========================================
                        # Sección: Lugares de publicación
                        #===========================================
                        div(
                          style = "text-align: center;",
                          h2(
                            bsButton("boton_ayuda21", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                            "Lugares de publicación"
                          )
                        ),
                        br(), br(),
                        
                        fluidRow(
                          column(8, align = "center", offset = 2,
                                 plotlyOutput('OUT_graphic_fonts', height = "700px")
                          )
                        ),
                        
                        hr(), br(),
                        
                        #===========================================
                        # Sección: Grafo de relaciones
                        #===========================================
                        div(
                          style = "text-align: center;",
                          h2(
                            bsButton("boton_ayuda22", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                            "Grafo de relaciones"
                          )
                        ),
                        br(), br(),
                        
                        fluidRow(
                          column(12, align = "center",
                                 div(
                                   style = "border: 1px solid #ccc; border-radius: 8px; padding: 10px;",
                                   visNetworkOutput("OUT_relationship_graph", height = "700px")
                                 )
                          )
                        ),
                        
                        hr(), br(),
                        
                        #===========================================
                        # Sección: Clustering jerárquico
                        #===========================================
                        div(
                          style = "text-align: center;",
                          h2(
                            bsButton("boton_ayuda24", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                            "Clustering jerárquico de académicos(as)"
                          )
                        ),
                        br(),
                        div(
                          style = "text-align: center;",
                          p("Los dendrogramas muestran la distancia entre académicos(as) según similitud de palabras clave, resumen, categorías y áreas de investigación.")
                        ),                        br(),
                        
                        fluidRow(
                          column(6, align = "center",
                                 h5(strong("Distancia Euclidiana")),
                                 plotlyOutput("OUT_dendro_euclidiano", height = "600px")
                          ),
                          column(6, align = "center",
                                 h5(strong("Distancia por Correlación")),
                                 plotlyOutput("OUT_dendro_correlacion", height = "600px")
                          )
                        )
)
