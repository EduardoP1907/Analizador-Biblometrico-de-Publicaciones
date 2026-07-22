# www/SOURCE/UI/tab_evaluacion.R
# Pestaña de evaluación cuantitativa: Precisión, Recall y F1-Score

tab_evaluacion <- tabPanel(
  title = tagList(tags$i(class = "fas fa-chart-line"), " Evaluación"),

  fluidRow(
    column(12,
      div(
        style = "background: linear-gradient(135deg, #1a237e 0%, #283593 100%); color: white; padding: 20px; margin-bottom: 20px; border-radius: 10px; text-align: center;",
        h4(
          tagList(tags$i(class = "fas fa-chart-bar"), " Evaluación Cuantitativa del Sistema NLP"),
          style = "margin: 0; font-weight: 600;"
        ),
        p(
          "Comparación de Precisión, Recall y F1-Score: Búsqueda exacta (Baseline) vs Sistema NLP",
          style = "margin: 10px 0 0 0; font-size: 14px; opacity: 0.9;"
        )
      )
    )
  ),

  # Panel explicativo de métricas
  fluidRow(
    column(4,
      div(
        style = "background: #e3f2fd; border-radius: 8px; padding: 15px; text-align: center; height: 110px;",
        tags$i(class = "fas fa-bullseye fa-2x", style = "color: #1565c0; margin-bottom: 8px;"),
        h6("Precisión", style = "color: #1565c0; margin: 5px 0;"),
        tags$small(
          style = "color: #555;",
          "¿Cuántos de los papers recuperados son realmente relevantes?",
          br(), "TP / (TP + FP)"
        )
      )
    ),
    column(4,
      div(
        style = "background: #e8f5e9; border-radius: 8px; padding: 15px; text-align: center; height: 110px;",
        tags$i(class = "fas fa-search fa-2x", style = "color: #2e7d32; margin-bottom: 8px;"),
        h6("Recall (Exhaustividad)", style = "color: #2e7d32; margin: 5px 0;"),
        tags$small(
          style = "color: #555;",
          "¿Qué fracción de los papers relevantes logró encontrar el sistema?",
          br(), "TP / (TP + FN)"
        )
      )
    ),
    column(4,
      div(
        style = "background: #fff3e0; border-radius: 8px; padding: 15px; text-align: center; height: 110px;",
        tags$i(class = "fas fa-balance-scale fa-2x", style = "color: #e65100; margin-bottom: 8px;"),
        h6("F1-Score", style = "color: #e65100; margin: 5px 0;"),
        tags$small(
          style = "color: #555;",
          "Equilibrio entre precisión y recall.",
          br(), "2 × (P × R) / (P + R)"
        )
      )
    )
  ),

  br(),

  # Botón de ejecutar evaluación
  fluidRow(
    column(12,
      div(
        style = "background: #f5f5f5; border-radius: 8px; padding: 15px; border: 1px dashed #9e9e9e;",
        fluidRow(
          column(8,
            div(
              style = "font-size: 13px; color: #555;",
              tags$strong("Ground truth: "),
              "15 consultas temáticas sobre el corpus de 3.914 papers de la USACH ",
              "(WoS + Scopus). Para cada consulta se identificaron manualmente los papers ",
              "relevantes. El sistema evalúa cuántos puede recuperar cada motor de búsqueda."
            )
          ),
          column(4,
            div(
              style = "text-align: center;",
              actionButton(
                "btn_evaluar",
                label = tagList(
                  tags$i(class = "fas fa-play-circle"),
                  " Ejecutar Evaluación"
                ),
                style = "background: #1a237e; color: white; border: none; border-radius: 6px; padding: 10px 20px; font-size: 14px; width: 100%;",
                width = "100%"
              ),
              tags$small(
                style = "color: #777; margin-top: 5px; display: block;",
                "Toma ~30-60 segundos"
              )
            )
          )
        )
      )
    )
  ),

  br(),

  # Área de resultados
  fluidRow(
    column(12,
      uiOutput("evaluacion_resultados")
    )
  ),

  br(),

  # Tarjetas de KPIs (se llenan después de evaluar)
  fluidRow(
    column(4,
      uiOutput("kpi_f1_baseline")
    ),
    column(4,
      uiOutput("kpi_f1_nlp")
    ),
    column(4,
      uiOutput("kpi_mejora")
    )
  ),

  br(),

  # Tabla detalle por query
  fluidRow(
    column(12,
      uiOutput("evaluacion_detalle_tabla")
    )
  )
)
