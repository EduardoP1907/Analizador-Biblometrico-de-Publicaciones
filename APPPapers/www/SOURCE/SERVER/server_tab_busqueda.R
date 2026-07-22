library(shiny)
library(stringi)
library(dplyr)
library(DT)

# Cargar motor NLP semántico + embeddings multilingues
cat("🚀 Cargando motor NLP con embeddings multilingues...\n")
usar_motor_semantico <- FALSE

tryCatch({
  source("www/SOURCE/UTILS/nlp_chatbot_engine_semantic.R")
  usar_motor_semantico <- TRUE
  cat("✅ Motor semántico cargado\n")

  # Inicializar embeddings (una sola vez gracias al estado global .nlp_emb)
  cat("🔍 Inicializando motor de embeddings...\n")
  emb_ok <- inicializar_embeddings()
  if (emb_ok) {
    cat("✅ Embeddings multilingues ACTIVOS — búsqueda ES↔EN habilitada\n")
  } else {
    cat("ℹ️  Embeddings no disponibles — usando búsqueda semántica clásica\n")
    cat("   Para activar: python precompute_embeddings.py\n")
  }

}, error = function(e) {
  cat("⚠️ Error cargando motor semántico, usando versión básica...\n")
  cat("   Error:", conditionMessage(e), "\n")
  source("www/SOURCE/UTILS/nlp_chatbot_engine_simple_v2.R")
  usar_motor_semantico <- FALSE
})

server_tab_busqueda <- function(input, output, session, data) {
  
  # Variables reactivas para el chat
  chat_history <- reactiveVal(list())
  current_stats <- reactiveVal(NULL)
  processing <- reactiveVal(FALSE)
  
  # Procesar búsqueda con motor PLN avanzado
  observeEvent(input$`buscador-go`, {
    
    # Validar entrada
    query <- trimws(input$`buscador-query`)
    if(is.null(query) || query == "") {
      return()
    }
    
    # ===== ACTIVAR INDICADORES DE CARGA =====
    processing(TRUE)

    # Mostrar indicador de carga en el botón
    shinyjs::hide("buscador-go")
    shinyjs::show("buscador-loading")

    # Mostrar indicador de carga en el chat
    shinyjs::show("chat-loading-indicator")
    shinyjs::hide("chat-welcome")  # Ocultar mensaje de bienvenida

    # Limpiar el campo de entrada
    updateTextInput(session, "buscador-query", value = "")

    # Simular un pequeño delay para que el usuario vea el loading
    Sys.sleep(0.5)
    
    # No usar OpenAI - solo procesamiento local
    
    tryCatch({
      # Ejecutar motor PLN SEMÁNTICO MEJORADO (Fase 1)
      if(exists("usar_motor_semantico") && usar_motor_semantico) {
        cat("🧠 Ejecutando búsqueda semántica avanzada...\n")
        resultado <- proceso_nlp_chatbot_semantico(
          query = query,
          data = data,
          openai_key = NULL,
          usar_expansion_semantica = TRUE
        )
      } else {
        cat("🔧 Ejecutando motor básico...\n")
        resultado <- proceso_nlp_chatbot_simple(
          query = query,
          data = data,
          openai_key = NULL
        )
      }
      
      # Procesar resultado
      if(resultado$success) {
        
        # Crear respuesta del asistente
        respuesta_ui <- crear_respuesta_ui(resultado, query)
        
        # Actualizar estadísticas
        current_stats(list(
          num_papers = resultado$num_papers,
          terminos = resultado$terminos_busqueda,
          tiempo = Sys.time()
        ))
        
      } else {
        # Error en el procesamiento
        respuesta_ui <- tags$div(
          class = "alert alert-warning",
          style = "margin: 10px 0;",
          tagList(
            tags$i(class = "fas fa-exclamation-triangle", style = "margin-right: 8px;"),
            resultado$message
          )
        )
        
        current_stats(NULL)
      }
      
      # Agregar al historial de chat
      historia_actual <- chat_history()
      nueva_entrada <- list(
        timestamp = Sys.time(),
        query = query,
        response = respuesta_ui,
        num_papers = if(resultado$success) resultado$num_papers else 0
      )
      
      chat_history(append(historia_actual, list(nueva_entrada)))
      
    }, error = function(e) {
      # Manejo de errores
      error_ui <- tags$div(
        class = "alert alert-danger",
        style = "margin: 10px 0;",
        tagList(
          tags$i(class = "fas fa-exclamation-circle", style = "margin-right: 8px;"),
          "Ocurrió un error al procesar su consulta: ", as.character(e$message)
        )
      )
      
      historia_actual <- chat_history()
      nueva_entrada <- list(
        timestamp = Sys.time(),
        query = query,
        response = error_ui,
        num_papers = 0
      )
      
      chat_history(append(historia_actual, list(nueva_entrada)))
    })

    # ===== OCULTAR INDICADORES DE CARGA =====
    processing(FALSE)

    # Restaurar botón normal
    shinyjs::hide("buscador-loading")
    shinyjs::show("buscador-go")

    # Ocultar indicador de carga en el chat
    shinyjs::hide("chat-loading-indicator")
  })
  
  # Renderizar el chat
  output$`buscador-chat` <- renderUI({
    historia <- chat_history()
    
    if(length(historia) == 0) {
      return(NULL)  # Mostrar mensaje de bienvenida por defecto
    }
    
    # Crear elementos del chat
    chat_elements <- lapply(historia, function(entrada) {
      timestamp_str <- format(entrada$timestamp, "%H:%M")
      
      tagList(
        # Mensaje del usuario
        tags$div(
          class = "user-message-container",
          style = "display: flex; justify-content: flex-end; margin: 15px 0;",
          tags$div(
            class = "user-message",
            style = "background: linear-gradient(135deg, #007bff, #0056b3); color: white; padding: 12px 16px; border-radius: 18px 18px 4px 18px; max-width: 70%; box-shadow: 0 2px 8px rgba(0,123,255,0.3);",
            tags$div(
              style = "font-weight: 500; margin-bottom: 4px;",
              entrada$query
            ),
            tags$div(
              style = "font-size: 11px; opacity: 0.8; text-align: right;",
              timestamp_str
            )
          )
        ),
        
        # Respuesta del asistente
        tags$div(
          class = "bot-message-container",
          style = "display: flex; justify-content: flex-start; margin: 15px 0 25px 0;",
          tags$div(
            class = "bot-avatar",
            style = "width: 35px; height: 35px; background: linear-gradient(135deg, #28a745, #20c997); border-radius: 50%; display: flex; align-items: center; justify-content: center; margin-right: 12px; flex-shrink: 0;",
            tags$i(class = "fas fa-robot", style = "color: white; font-size: 16px;")
          ),
          tags$div(
            class = "bot-message",
            style = "background: #f8f9fa; border: 1px solid #e9ecef; padding: 15px; border-radius: 4px 18px 18px 18px; max-width: 75%; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            entrada$response,
            tags$div(
              style = "font-size: 11px; color: #6c757d; margin-top: 8px; text-align: right;",
              paste("🤖 Asistente IA •", timestamp_str)
            )
          )
        )
      )
    })
    
    do.call(tagList, chat_elements)
  })
  
  # Limpiar conversación
  observeEvent(input$`limpiar-chat`, {
    chat_history(list())
    current_stats(NULL)
    shinyjs::runjs("document.getElementById('chat-welcome').style.display = 'block';")
  })
  
  # Estadísticas de búsqueda
  output$estadisticas_busqueda <- renderUI({
    stats <- current_stats()
    
    if(is.null(stats)) {
      return(NULL)
    }
    
    tags$div(
      class = "search-stats",
      style = "background: #e3f2fd; border: 1px solid #bbdefb; border-radius: 8px; padding: 15px;",
      tags$h6(
        tagList(tags$i(class = "fas fa-chart-bar"), " Estadísticas de la última búsqueda"),
        style = "margin: 0 0 10px 0; color: #1976d2;"
      ),
      fluidRow(
        column(4,
          tags$div(
            style = "text-align: center;",
            tags$div(
              style = "font-size: 24px; font-weight: bold; color: #1976d2;",
              stats$num_papers
            ),
            tags$div(
              style = "font-size: 12px; color: #666;",
              "Papers encontrados"
            )
          )
        ),
        column(4,
          tags$div(
            style = "text-align: center;",
            tags$div(
              style = "font-size: 24px; font-weight: bold; color: #388e3c;",
              length(stats$terminos)
            ),
            tags$div(
              style = "font-size: 12px; color: #666;",
              "Términos analizados"
            )
          )
        ),
        column(4,
          tags$div(
            style = "text-align: center;",
            tags$div(
              style = "font-size: 24px; font-weight: bold; color: #f57c00;",
              "IA"
            ),
            tags$div(
              style = "font-size: 12px; color: #666;",
              "Procesamiento NLP"
            )
          )
        )
      ),
      if(length(stats$terminos) > 0) {
        tags$div(
          style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #bbdefb;",
          tags$div(
            style = "font-size: 13px; color: #666; margin-bottom: 8px;",
            "Términos clave identificados:"
          ),
          tags$div(
            style = "display: flex; flex-wrap: wrap; gap: 5px;",
            lapply(head(stats$terminos, 8), function(term) {
              tags$span(
                class = "badge badge-primary",
                style = "background: #2196f3; font-size: 11px; padding: 4px 8px;",
                term
              )
            })
          )
        )
      }
    )
  })
  
  # Descargar conversación
  output$`descargar-chat` <- downloadHandler(
    filename = function() {
      paste0("conversacion_chatbot_", format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      historia <- chat_history()
      
      if(length(historia) == 0) {
        writeLines("No hay conversaciones para exportar.", file)
        return()
      }
      
      # Generar HTML de la conversación
      html_content <- generate_chat_html(historia)
      writeLines(html_content, file, useBytes = TRUE)
    }
  )
  
  # Permitir envío con Enter y mejorar UX
  shinyjs::runjs('
    $(document).on("keypress", "#buscador-query", function(e) {
      if(e.which == 13) {
        // Verificar si no está procesando
        if($("#buscador-go").is(":visible")) {
          $("#buscador-go").click();
        }
      }
    });

    // Deshabilitar botón loading al hacer click
    $(document).on("click", "#buscador-loading", function(e) {
      e.preventDefault();
      return false;
    });

    // Añadir estilo visual de "deshabilitado" al botón loading
    $("#buscador-loading").css({
      "cursor": "not-allowed",
      "opacity": "0.7"
    });
  ')
}

# Función auxiliar para crear UI de respuesta
crear_respuesta_ui <- function(resultado, query_original) {
  
  # Resumen principal
  resumen_ui <- tags$div(
    class = "nlp-summary",
    style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-left: 4px solid #007bff; padding: 15px; margin: 10px 0; border-radius: 0 8px 8px 0;",
    tags$div(
      style = "display: flex; align-items: center; margin-bottom: 10px;",
      tags$i(class = "fas fa-brain", style = "color: #007bff; margin-right: 8px; font-size: 16px;"),
      tags$strong("Resumen generado por IA:", style = "color: #495057; font-size: 14px;")
    ),
    tags$div(
      style = "margin: 0; line-height: 1.6; color: #212529; text-align: justify; font-size: 13.5px;",
      HTML(resultado$resumen_generado)
    )
  )
  
  # Lista de papers encontrados
  if(resultado$num_papers > 0) {
    papers_ui <- tags$div(
      class = "papers-list",
      style = "margin-top: 15px;",
      tags$div(
        style = "display: flex; align-items: center; margin-bottom: 12px;",
        tags$i(class = "fas fa-file-alt", style = "color: #28a745; margin-right: 8px; font-size: 16px;"),
        tags$strong(
          paste("Papers encontrados:", resultado$num_papers),
          style = "color: #495057; font-size: 14px;"
        )
      ),
      
      # Tabla de papers
      if(nrow(resultado$papers) > 0) {
        crear_tabla_papers(resultado$papers)
      }
    )
    
    respuesta_completa <- tagList(resumen_ui, papers_ui)
  } else {
    respuesta_completa <- resumen_ui
  }
  
  return(respuesta_completa)
}

# Función auxiliar para crear tabla de papers - MOSTRAR TODOS
crear_tabla_papers <- function(papers_df) {
  
  # NO limitar - mostrar TODOS los papers encontrados
  mostrar_mas <- FALSE
  
  papers_list <- lapply(1:nrow(papers_df), function(i) {
    paper <- papers_df[i, ]
    
    tags$div(
      class = "paper-item",
      style = "background: white; border: 1px solid #dee2e6; border-radius: 6px; padding: 12px; margin-bottom: 8px; transition: all 0.2s;",
      
      # Título como enlace
      tags$div(
        style = "margin-bottom: 8px;",
        if(!is.na(paper$LINK) && paper$LINK != "") {
          tags$a(
            href = paper$LINK,
            target = "_blank",
            paper$TITULO,
            style = "color: #007bff; text-decoration: none; font-weight: 500; font-size: 14px; line-height: 1.3;",
            onmouseover = "this.style.textDecoration='underline'",
            onmouseout = "this.style.textDecoration='none'"
          )
        } else {
          tags$span(
            paper$TITULO,
            style = "color: #495057; font-weight: 500; font-size: 14px; line-height: 1.3;"
          )
        }
      ),
      
      # Metadatos
      tags$div(
        style = "display: flex; flex-wrap: wrap; gap: 15px; font-size: 12px; color: #6c757d;",
        tags$div(
          tagList(
            tags$i(class = "fas fa-user", style = "margin-right: 4px;"),
            paper$NOMBRE_AUTOR
          )
        ),
        tags$div(
          tagList(
            tags$i(class = "fas fa-calendar", style = "margin-right: 4px;"),
            paper$ANO
          )
        ),
        if(!is.na(paper$SJR) && as.numeric(paper$SJR) > 0) {
          tags$div(
            tagList(
              tags$i(class = "fas fa-star", style = "margin-right: 4px; color: #ffc107;"),
              paste("SJR:", round(as.numeric(paper$SJR), 3))
            )
          )
        },
        if(!is.na(paper$CITADO_POR) && as.numeric(paper$CITADO_POR) > 0) {
          tags$div(
            tagList(
              tags$i(class = "fas fa-quote-right", style = "margin-right: 4px; color: #17a2b8;"),
              paste("Citas:", paper$CITADO_POR)
            )
          )
        }
      )
    )
  })
  
  result <- tagList(papers_list)
  
  # Mostrar información de todos los papers
  if(nrow(papers_df) > 0) {
    result <- tagList(
      result,
      tags$div(
        style = "text-align: center; margin-top: 15px; padding: 10px; background: #f8f9fa; border-radius: 5px;",
        tags$strong(
          paste("📊 Mostrando todos los", nrow(papers_df), "papers encontrados"),
          style = "color: #495057; font-size: 14px;"
        )
      )
    )
  }
  
  return(result)
}

# Función auxiliar para generar HTML de exportación
generate_chat_html <- function(historia) {
  
  chat_html <- sapply(historia, function(entrada) {
    timestamp <- format(entrada$timestamp, "%Y-%m-%d %H:%M:%S")
    
    user_msg <- paste0(
      '<div style="margin: 20px 0; text-align: right;">',
      '<div style="background: #007bff; color: white; padding: 12px; border-radius: 15px; display: inline-block; max-width: 70%;">',
      '<strong>Usuario:</strong> ', htmltools::htmlEscape(entrada$query),
      '<br><small style="opacity: 0.8;">', timestamp, '</small>',
      '</div></div>'
    )
    
    # Convertir respuesta UI a HTML (simplificado)
    bot_response <- '<div style="margin: 20px 0;"><div style="background: #f8f9fa; border: 1px solid #e9ecef; padding: 15px; border-radius: 15px; display: inline-block; max-width: 80%;"><strong>🤖 Asistente IA:</strong><br>Respuesta procesada</div></div>'
    
    paste(user_msg, bot_response, sep = "\n")
  })
  
  full_html <- paste0(
    '<!DOCTYPE html>',
    '<html><head>',
    '<title>Conversación Chatbot NLP - ', format(Sys.Date(), "%Y-%m-%d"), '</title>',
    '<meta charset="UTF-8">',
    '<style>body { font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }</style>',
    '</head><body>',
    '<h1>Conversación con Chatbot NLP Bibliométrico</h1>',
    '<p><strong>Exportado:</strong> ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '</p>',
    '<hr>',
    paste(chat_html, collapse = "\n"),
    '</body></html>'
  )
  
  return(full_html)
}
