#=======================================================================
# SERVIDOR — Pestaña de Evaluación Cuantitativa (F1-Score)
# Compara búsqueda baseline vs sistema NLP sobre 15 consultas con
# ground truth construido del corpus USACH (3.914 papers)
#=======================================================================

# Cargar módulo de métricas una sola vez
source("www/SOURCE/UTILS/nlp_evaluacion_metricas.R")

server_tab_evaluacion <- function(input, output, session, data) {

  # Estado reactivo para guardar los resultados de la evaluación
  evaluacion_cache <- reactiveVal(NULL)

  # ── Botón "Ejecutar Evaluación" ─────────────────────────────────────────────
  observeEvent(input$btn_evaluar, {

    # Deshabilitar el botón durante el cálculo
    shinyjs::disable("btn_evaluar")

    output$evaluacion_resultados <- renderUI({
      div(
        style = "text-align: center; padding: 30px; color: #666;",
        tags$i(class = "fas fa-spinner fa-spin fa-2x", style = "color: #1a237e;"),
        br(), br(),
        p("Evaluando 15 consultas sobre el corpus de 3.914 papers...", style = "font-size: 14px;"),
        p(tags$small("Esto puede tomar entre 30 y 60 segundos según el motor activo."))
      )
    })

    # Limpiar KPIs mientras carga
    output$kpi_f1_baseline     <- renderUI(NULL)
    output$kpi_f1_nlp          <- renderUI(NULL)
    output$kpi_mejora          <- renderUI(NULL)
    output$evaluacion_detalle_tabla <- renderUI(NULL)

    # Ejecutar evaluación (puede ser lento — se corre dentro del observeEvent
    # para no bloquear el hilo de sesión principal)
    evaluacion <- tryCatch(
      ejecutar_evaluacion_completa(data, verbose = TRUE),
      error = function(e) {
        cat("❌ [EVAL] Error en evaluacion_completa:", conditionMessage(e), "\n")
        list(error = conditionMessage(e))
      }
    )

    # Guardar en caché reactivo
    evaluacion_cache(evaluacion)

    # Habilitar botón de nuevo
    shinyjs::enable("btn_evaluar")

    # Si hubo error, mostrar mensaje
    if (!is.null(evaluacion$error)) {
      output$evaluacion_resultados <- renderUI({
        div(
          class = "alert alert-danger",
          tags$strong("Error al ejecutar la evaluación: "),
          evaluacion$error
        )
      })
      return()
    }

    # ── Mostrar tabla de resumen ──────────────────────────────────────────────
    output$evaluacion_resultados <- renderUI({
      generar_tabla_evaluacion_html(evaluacion)
    })

    # ── KPIs individuales ────────────────────────────────────────────────────
    resumen <- evaluacion$resumen

    f1_base <- resumen$f1_macro[resumen$sistema == "Baseline (exacto)"]
    f1_nlp  <- resumen$f1_macro[resumen$sistema == "Sistema NLP"]
    p_base  <- resumen$precision_macro[resumen$sistema == "Baseline (exacto)"]
    p_nlp   <- resumen$precision_macro[resumen$sistema == "Sistema NLP"]
    r_base  <- resumen$recall_macro[resumen$sistema == "Baseline (exacto)"]
    r_nlp   <- resumen$recall_macro[resumen$sistema == "Sistema NLP"]
    mejora  <- if (length(f1_base) > 0 && f1_base > 0)
      round((f1_nlp - f1_base) / f1_base * 100, 1) else 0

    output$kpi_f1_baseline <- renderUI({
      div(
        style = "background: #e3f2fd; border-radius: 10px; padding: 20px; text-align: center; border-left: 5px solid #1565c0;",
        tags$i(class = "fas fa-search fa-2x", style = "color: #1565c0; margin-bottom: 10px;"),
        h5("Baseline (exacto)", style = "color: #1565c0; margin: 5px 0;"),
        tags$p(
          style = "font-size: 28px; font-weight: bold; color: #1a237e; margin: 5px 0;",
          sprintf("%.3f", f1_base)
        ),
        tags$small(
          style = "color: #555;",
          sprintf("P: %.3f | R: %.3f", p_base, r_base)
        )
      )
    })

    output$kpi_f1_nlp <- renderUI({
      div(
        style = "background: #e8f5e9; border-radius: 10px; padding: 20px; text-align: center; border-left: 5px solid #2e7d32;",
        tags$i(class = "fas fa-brain fa-2x", style = "color: #2e7d32; margin-bottom: 10px;"),
        h5("Sistema NLP", style = "color: #2e7d32; margin: 5px 0;"),
        tags$p(
          style = "font-size: 28px; font-weight: bold; color: #1b5e20; margin: 5px 0;",
          sprintf("%.3f", f1_nlp)
        ),
        tags$small(
          style = "color: #555;",
          sprintf("P: %.3f | R: %.3f", p_nlp, r_nlp)
        )
      )
    })

    color_mejora <- if (mejora > 0) "#e65100" else "#555"
    icono_mejora <- if (mejora > 0) "fa-arrow-up" else "fa-minus"

    output$kpi_mejora <- renderUI({
      div(
        style = "background: #fff3e0; border-radius: 10px; padding: 20px; text-align: center; border-left: 5px solid #e65100;",
        tags$i(class = paste("fas", icono_mejora, "fa-2x"), style = paste0("color: ", color_mejora, "; margin-bottom: 10px;")),
        h5("Mejora F1-Score", style = "color: #e65100; margin: 5px 0;"),
        tags$p(
          style = paste0("font-size: 28px; font-weight: bold; color: ", color_mejora, "; margin: 5px 0;"),
          sprintf("%+.1f%%", mejora)
        ),
        tags$small(
          style = "color: #555;",
          "NLP vs Baseline (macro)"
        )
      )
    })

    # ── Tabla detalle ────────────────────────────────────────────────────────
    output$evaluacion_detalle_tabla <- renderUI({
      tagList(
        tags$h5(
          tagList(tags$i(class = "fas fa-table"), " Detalle por consulta"),
          style = "color: #1a237e; margin-bottom: 10px;"
        ),
        DT::dataTableOutput("evaluacion_dt")
      )
    })

    output$evaluacion_dt <- DT::renderDataTable({
      detalle <- evaluacion$detalle
      detalle[, c("sistema", "query", "n_gt", "n_recuperados", "tp",
                  "precision", "recall", "f1")]
    },
    colnames = c("Sistema", "Consulta", "GT", "Recuperados",
                 "TP", "Precisión", "Recall", "F1"),
    options = list(
      pageLength = 15, scrollX = TRUE, dom = "tip",
      order = list(list(7, "desc"))
    ),
    rownames = FALSE,
    class = "table-condensed table-bordered table-hover",
    selection = "none"
    )

  })  # fin observeEvent btn_evaluar

}  # fin server_tab_evaluacion
