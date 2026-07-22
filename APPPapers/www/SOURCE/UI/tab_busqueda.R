# www/SOURCE/UI/tab_busqueda.R

library(shiny)
library(fontawesome)
library(DT)
library(shinycssloaders)    # Para el spinner de carga

# CSS para animaciones de loading
loading_css <- tags$head(
  tags$style(HTML("
    @keyframes loading-bar {
      0% { transform: translateX(-100%); }
      50% { transform: translateX(0%); }
      100% { transform: translateX(100%); }
    }

    .loading-pulse {
      animation: pulse 2s ease-in-out infinite;
    }

    @keyframes pulse {
      0% { opacity: 1; }
      50% { opacity: 0.5; }
      100% { opacity: 1; }
    }

    #buscador-loading:hover {
      cursor: not-allowed !important;
      opacity: 0.7 !important;
    }
  "))
)

# Pestaña de búsqueda con chatbot NLP avanzado
tab_busqueda <- tabPanel(
  title = tagList(fa_i("robot"), "Chatbot NLP"),

  # Incluir CSS personalizado
  loading_css,
  
  # Encabezado del chatbot
  fluidRow(
    column(12,
      div(
        class = "chatbot-header",
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; margin-bottom: 20px; border-radius: 10px; text-align: center;",
        h4(tagList(fa_i("brain"), " Motor de Procesamiento de Lenguaje Natural"), style = "margin: 0; font-weight: 600;"),
        p("Realiza consultas inteligentes sobre papers académicos y obtén resúmenes generados automáticamente", 
          style = "margin: 10px 0 0 0; font-size: 14px; opacity: 0.9;")
      )
    )
  ),
  
  # Información del motor PLN
  fluidRow(
    column(12,
      div(
        style = "background: #e8f5e8; padding: 15px; border-radius: 8px; margin-bottom: 15px; border-left: 4px solid #28a745;",
        div(
          style = "display: flex; align-items: center;",
          tagList(
            fa_i("brain", style = "color: #28a745; margin-right: 10px; font-size: 18px;"),
            strong("Motor PLN Avanzado Activado", style = "color: #155724; font-size: 16px;")
          )
        ),
        div(
          style = "font-size: 14px; color: #155724; margin-top: 8px;",
          "✅ Procesamiento local de abstracts • ✅ Generación de resúmenes completamente nuevos • ✅ Análisis semántico avanzado"
        )
      )
    )
  ),
  
  # Área de conversación del chat
  fluidRow(
    column(12,
      div(
        id = "chat-container",
        style = "background: white; border: 1px solid #dee2e6; border-radius: 10px; min-height: 400px; max-height: 600px; overflow-y: auto; padding: 15px; margin-bottom: 15px;",
        div(
          id = "chat-welcome",
          style = "text-align: center; color: #6c757d; padding: 50px 20px;",
          tagList(
            fa_i("comments", "fa-3x", style = "color: #dee2e6; margin-bottom: 20px;"),
            h5("¡Hola! Soy tu asistente bibliométrico con IA"),
            p("Escribe consultas como:", style = "margin: 15px 0 10px 0; font-weight: 500;"),
            div(
              style = "background: #e9ecef; padding: 10px; border-radius: 5px; font-family: monospace; font-size: 13px;",
              '"machine learning"', br(),
              '"redes neuronales artificiales"', br(), 
              '"análisis de sentimientos en redes sociales"', br(),
              '"algoritmos genéticos optimización"'
            ),
            p("Analizaré los abstracts de todos los papers encontrados y generaré un resumen completamente nuevo.", 
              style = "margin-top: 15px; font-style: italic;")
          )
        ),
        # Área de loading en el chat
        div(
          id = "chat-loading-indicator",
          style = "display: none; text-align: center; padding: 40px 20px; background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 10px; margin: 10px; border: 1px dashed #007bff;",
          div(
            tags$i(class = "fas fa-brain fa-spin", style = "font-size: 28px; margin-bottom: 15px; color: #007bff; text-shadow: 0 0 10px rgba(0,123,255,0.3);"),
            br(),
            tags$strong("🔍 Procesando consulta con IA semántica...", style = "font-size: 16px; color: #495057; margin-bottom: 8px; display: block;"),
            br(),
            tags$small("Analizando términos • Aplicando filtros temporales • Buscando papers • Generando resumen", style = "color: #6c757d; font-size: 12px; line-height: 1.4;"),
            br(), br(),
            div(
              style = "width: 100%; height: 4px; background: #e9ecef; border-radius: 2px; overflow: hidden; margin-top: 15px;",
              div(
                style = "width: 100%; height: 100%; background: linear-gradient(90deg, #007bff, #0056b3, #007bff); animation: loading-bar 2s ease-in-out infinite;"
              )
            )
          )
        ),
        withSpinner(
          uiOutput("buscador-chat"),
          type = 6,
          color = "#007bff"
        )
      )
    )
  ),
  
  # Barra de entrada del chat
  fluidRow(
    column(12,
      div(
        style = "background: white; border: 1px solid #dee2e6; border-radius: 25px; padding: 8px;",
        fluidRow(
          column(10,
            textInput(
              inputId = "buscador-query",
              label = NULL,
              placeholder = "Escribe tu consulta sobre papers académicos... ej: 'machine learning en medicina'",
              width = "100%"
            )
          ),
          column(2,
            div(
              id = "search-button-container",
              actionButton(
                inputId = "buscador-go",
                label = tagList(fa_i("paper-plane"), "Enviar"),
                class = "btn-primary",
                width = "100%",
                style = "border-radius: 20px; height: 34px; font-weight: 500;"
              ),
              # Botón de loading oculto inicialmente
              actionButton(
                inputId = "buscador-loading",
                label = tagList(
                  tags$i(class = "fas fa-spinner fa-spin", style = "margin-right: 5px;"),
                  "Procesando..."
                ),
                class = "btn-secondary",
                width = "100%",
                style = "border-radius: 20px; height: 34px; font-weight: 500; display: none;"
              )
            )
          )
        )
      )
    )
  ),
  
  # Botones de acciones adicionales
  fluidRow(
    column(12,
      div(
        style = "text-align: center; margin-top: 15px;",
        actionButton(
          inputId = "limpiar-chat",
          label = tagList(fa_i("broom"), "Limpiar conversación"),
          class = "btn-outline-secondary btn-sm",
          style = "margin-right: 10px;"
        ),
        downloadButton(
          outputId = "descargar-chat",
          label = tagList(fa_i("download"), "Descargar chat"),
          class = "btn-outline-info btn-sm"
        )
      )
    )
  ),
  
  # Estadísticas de la búsqueda actual
  fluidRow(
    column(12,
      div(
        id = "stats-container",
        style = "margin-top: 20px;",
        uiOutput("estadisticas_busqueda")
      )
    )
  )
)
