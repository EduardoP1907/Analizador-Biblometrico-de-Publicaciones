#==========================================
# Panel de pestaña "Resumen" con ícono e indicadores visuales
#==========================================

tab_resumen <- tabPanel("Resumen", icon = icon("list-alt"),
                        
                        br(),
                        
                        #==============================
                        # Primera fila de indicadores
                        #==============================
                        fluidRow(
                          
                          h4(class = "title-part text-center",
                             bsButton("boton_ayuda1", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                             strong("TARJETAS DE RESUMEN")
                          ),
                          br(),
                          
                          # Total de publicaciones
                          column(3, align = "center", class = "green-background",
                                 br(),
                                 column(3,
                                        bsButton("boton_ayuda2", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                                        img(src = 'IMG/img_publicaciones.png', style = "width: 100%;"),
                                        HTML("&nbsp;")
                                 ),
                                 column(9,
                                        "El", strong("total de publicaciones"), "con indexación Web of Science (WoS) o SCOPUS para el periodo establecido es:",
                                        htmlOutput("TEXTO_numero_publicaciones"), br()
                                 )
                          ),
                          
                          # Tipos de publicaciones
                          column(3, offset = 1, align = "center", class = "green-background",
                                 br(),
                                 column(3,
                                        bsButton("boton_ayuda3", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                                        img(src = 'IMG/img_revista_conferencia.png', style = "width: 100%;"),
                                        HTML("&nbsp;")
                                 ),
                                 column(9,
                                        "Las publicaciones se dividen en los siguientes", strong("tipos de productos científicos:"),
                                        htmlOutput("TEXTO_tipo_publicaciones"), br()
                                 )
                          ),
                          
                          # Distribución de indexación
                          column(3, offset = 1, align = "center", class = "green-background",
                                 br(),
                                 column(3,
                                        bsButton("boton_ayuda4", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                                        img(src = 'IMG/img_promedio_publicaciones_por_autor.png', style = "width: 100%;"),
                                        HTML("&nbsp;")
                                 ),
                                 column(9,
                                        "La", strong("distribución de la indexación"), "de publicaciones en el periodo es:",
                                        htmlOutput("TEXTO_promedio_publicaciones_autor"), br()
                                 )
                          )
                        ),
                        
                        br(),
                        
                        #==============================
                        # Segunda fila de indicadores
                        #==============================
                        fluidRow(
                          
                          # Áreas de la especialidad más frecuentes
                          column(3, align = "center", class = "green-background",
                                 br(),
                                 column(3,
                                        bsButton("boton_ayuda5", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                                        img(src = 'IMG/img_incremento.png', style = "width: 100%;"),
                                        HTML("&nbsp;")
                                 ),
                                 column(9,
                                        "La(s)", strong("área(s) de especialidad más frecuente(s)"), "en las publicaciones es(son):",
                                        htmlOutput("TEXTO_areas_cs_mas_frecuentes"), br()
                                 )
                          ),
                          
                          # Áreas de aplicación más frecuentes
                          column(3, offset = 1, align = "center", class = "green-background",
                                 br(),
                                 column(3,
                                        bsButton("boton_ayuda6", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                                        img(src = 'IMG/img_mas_aplicacion.png', style = "width: 100%;"),
                                        HTML("&nbsp;")
                                 ),
                                 column(9,
                                        "La(s)", strong("área(s) de aplicación más frecuente(s)"), "en las publicaciones, además de Computer Science, es(son):",
                                        htmlOutput("TEXTO_areas_aplicacion_mas_frecuentes"), br()
                                 )
                          ),
                          
                          # Revista más publicada
                          column(3, offset = 1, align = "center", class = "green-background",
                                 br(),
                                 column(3,
                                        bsButton("boton_ayuda7", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                                        img(src = 'IMG/img_revista.png', style = "width: 100%;"),
                                        HTML("&nbsp;")
                                 ),
                                 column(9,
                                        "La(s)", strong("revista(s)"), "en que más se ha publicado es(son):",
                                        htmlOutput("TEXTO_revista_mas_publicada"), br()
                                 )
                          )
                        ),
                        
                        br(),
                        
                        #==============================
                        # Tercera fila de indicadores
                        #==============================
                        fluidRow(
                          
                          # Áreas de CS menos frecuentes
                          column(3, align = "center", class = "red-background",
                                 br(),
                                 column(3,
                                        bsButton("boton_ayuda8", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                                        img(src = 'IMG/img_decremento.png', style = "width: 100%;"),
                                        HTML("&nbsp;")
                                 ),
                                 column(9,
                                        "La(s)", strong("área(s) de la especialidad menos frecuente(s)"), "en las publicaciones es(son):",
                                        htmlOutput("TEXTO_areas_cs_menos_frecuentes"), br()
                                 )
                          ),
                          
                          # Áreas de aplicación menos frecuentes
                          column(3, offset = 1, align = "center", class = "red-background",
                                 br(),
                                 column(3,
                                        bsButton("boton_ayuda9", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                                        img(src = 'IMG/img_menos_aplicacion.png', style = "width: 100%;"),
                                        HTML("&nbsp;")
                                 ),
                                 column(9,
                                        "La(s)", strong("área(s) de aplicación menos frecuente(s)"), "en las publicaciones es(son):",
                                        htmlOutput("TEXTO_areas_aplicacion_menos_frecuentes"), br()
                                 )
                          ),
                          
                          # Promedio de autores por publicación
                          column(3, offset = 1, align = "center", class = "green-background",
                                 br(),
                                 column(3,
                                        bsButton("boton_ayuda10", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                                        img(src = 'IMG/img_colaboracion.png', style = "width: 100%;"),
                                        HTML("&nbsp;")
                                 ),
                                 column(9,
                                        "El", strong("promedio de autores(as)"), "por publicación es:",
                                        htmlOutput("TEXTO_promedio_autores"), br()
                                 )
                          )
                        ),
                        
                        br(),
                        
                        #==============================
                        # Cuarta fila de indicadores
                        #==============================
                        fluidRow(
                          
                          # Publicación más citada
                          column(3, align = "center", class = "green-background",
                                 br(),
                                 column(3,
                                        bsButton("boton_ayuda11", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                                        img(src = 'IMG/img_mas_citada.png', style = "width: 100%;"),
                                        HTML("&nbsp;")
                                 ),
                                 column(9,
                                        "La(s) publicación(es)", strong("más citadas"), "es(son):",
                                        htmlOutput("TEXTO_publicacion_mas_citada"), br()
                                 )
                          ),
                          
                          # Publicación con mayor SJR
                          column(3, offset = 1, align = "center", class = "green-background",
                                 br(),
                                 column(3,
                                        bsButton("boton_ayuda12", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                                        img(src = 'IMG/img_impacto.png', style = "width: 100%;"),
                                        HTML("&nbsp;")
                                 ),
                                 column(9,
                                        "La(s) publicación(es) con", strong("mayor factor de impacto SJR"), "es(son):",
                                        htmlOutput("TEXTO_publicacion_mas_impacto"), br()
                                 )
                          ),
                          
                          # Porcentaje de publicaciones con autores institucionales
                          column(3, offset = 1, align = "center", class = "green-background",
                                 br(),
                                 column(3,
                                        bsButton("boton_ayuda13", label = img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), size = "small"),
                                        img(src = 'IMG/img_hogar.png', style = "width: 100%;"),
                                        HTML("&nbsp;")
                                 ),
                                 column(9,
                                        "El porcentaje de publicaciones que comparte", strong("más de un autor(a) institucional"), "es:",
                                        htmlOutput("TEXTO_promedio_autores_diinf"), br()
                                 )
                          )
                        ),
                        
                        #==============================
                        # Tooltips de ayuda para todos los botones
                        #==============================
                        bsTooltip(
                          id = c("boton_ayuda1", "boton_ayuda2", "boton_ayuda3", "boton_ayuda4", "boton_ayuda5", "boton_ayuda6", "boton_ayuda7",
                                 "boton_ayuda8", "boton_ayuda9", "boton_ayuda10", "boton_ayuda11", "boton_ayuda12", "boton_ayuda13"),
                          title = "Clic para ver ayuda",
                          placement = "right",
                          trigger = "hover"
                        )
)
