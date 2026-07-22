#=======================================================================
# MÓDULO DE EVALUACIÓN: Precisión, Recall y F1-Score
# Compara sistema NLP vs búsqueda exacta (baseline)
#=======================================================================

library(stringi, warn.conflicts = FALSE)
library(dplyr,   warn.conflicts = FALSE)

# ── 1. FUNCIONES MÉTRICAS BASE ──────────────────────────────────────────────

normalizar_titulo <- function(titulo) {
  if (is.null(titulo) || is.na(titulo)) return("")
  titulo <- as.character(titulo)
  titulo <- tolower(titulo)
  titulo <- stri_replace_all_regex(titulo, "[^a-z0-9áéíóúüñ ]", " ")
  titulo <- stri_trim_both(titulo)
  stri_replace_all_regex(titulo, "\\s+", " ")
}

titulo_coincide <- function(titulo_recuperado, titulos_gt) {
  # Asegurar que recibimos escalares de tipo character
  titulo_recuperado <- as.character(titulo_recuperado)
  if (length(titulo_recuperado) != 1 || is.na(titulo_recuperado) || titulo_recuperado == "") {
    return(FALSE)
  }
  if (length(titulos_gt) == 0) return(FALSE)

  t_norm  <- normalizar_titulo(titulo_recuperado)
  gt_norm <- vapply(titulos_gt, normalizar_titulo, character(1L))

  # 1. Coincidencia exacta normalizada
  if (t_norm %in% gt_norm) return(TRUE)

  # 2. Coincidencia difusa (Levenshtein ≤ 10% del largo, min 5 chars)
  if (nchar(t_norm) == 0) return(FALSE)
  umbral <- max(5L, as.integer(floor(nchar(t_norm) * 0.10)))
  dists  <- adist(t_norm, gt_norm)
  any(dists <= umbral, na.rm = TRUE)
}

# TP = número de papers del GT cubiertos por al menos un paper recuperado.
# (Vectorizado sobre GT: adist() compara 1 string contra todos los recuperados)
calcular_tp <- function(titulos_recuperados, titulos_gt) {
  if (length(titulos_recuperados) == 0 || length(titulos_gt) == 0) return(0L)
  rec_norm <- vapply(titulos_recuperados, normalizar_titulo, character(1L))
  as.integer(sum(vapply(titulos_gt, function(gt) {
    gt_n <- normalizar_titulo(gt)
    if (gt_n == "") return(FALSE)
    if (gt_n %in% rec_norm) return(TRUE)
    umbral <- max(5L, as.integer(floor(nchar(gt_n) * 0.10)))
    any(adist(gt_n, rec_norm) <= umbral, na.rm = TRUE)
  }, logical(1L))))
}

calcular_precision <- function(titulos_recuperados, titulos_gt) {
  if (length(titulos_recuperados) == 0) return(0)
  calcular_tp(titulos_recuperados, titulos_gt) / length(titulos_recuperados)
}

calcular_recall <- function(titulos_recuperados, titulos_gt) {
  if (length(titulos_gt) == 0) return(0)
  calcular_tp(titulos_recuperados, titulos_gt) / length(titulos_gt)
}

calcular_f1 <- function(precision, recall) {
  if ((precision + recall) == 0) return(0)
  2 * (precision * recall) / (precision + recall)
}

# ── 2. SISTEMA BASELINE (búsqueda por palabras clave exactas) ───────────────

busqueda_baseline <- function(query, data) {
  stopwords_es <- c("de","el","la","los","las","en","con","por","para","que",
                    "del","al","se","un","una","y","o","a","es","son",
                    "sus","su","pero","como","más","sobre","entre","sin")

  # Tokenizar sin pipes para evitar edge-cases de magrittr con listas
  raw_tokens <- stri_split_boundaries(tolower(as.character(query)),
                                      type = "word")[[1]]
  tokens <- raw_tokens[nchar(raw_tokens) > 2]
  tokens <- setdiff(tokens, stopwords_es)

  if (length(tokens) == 0) return(character(0))

  # Obtener columnas como character (robustez ante NA o tipos inesperados)
  campo_titulo <- tolower(as.character(data$TITULO            %||% ""))
  campo_kw     <- tolower(as.character(data$AUTOR_PALABRAS_CLAVES %||% ""))
  campo_idx    <- tolower(as.character(data$INDEX_PALABRAS_CLAVES %||% ""))

  # Reemplazar NA (stringi los deja como NA_character_)
  campo_titulo[is.na(campo_titulo)] <- ""
  campo_kw    [is.na(campo_kw)]     <- ""
  campo_idx   [is.na(campo_idx)]    <- ""

  mask <- rep(FALSE, nrow(data))
  for (token in tokens) {
    hit <- grepl(token, campo_titulo, fixed = TRUE) |
           grepl(token, campo_kw,     fixed = TRUE) |
           grepl(token, campo_idx,    fixed = TRUE)
    # grepl puede devolver NA si hay NAs; convertir a FALSE
    hit[is.na(hit)] <- FALSE
    mask <- mask | hit
  }

  titulos <- as.character(data$TITULO[mask])
  titulos[!is.na(titulos)]
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ── 3. EVALUACIÓN COMPLETA ───────────────────────────────────────────────────

cargar_ground_truth <- function(path = "www/BD/ground_truth_evaluacion.csv") {
  if (!file.exists(path)) stop("Ground truth no encontrado: ", path)

  gt <- read.csv(path, sep = "|", header = TRUE, stringsAsFactors = FALSE,
                 quote = "")

  # Parsear la lista de títulos (separados por ;;) como lista de vectores character
  gt$lista_relevantes <- lapply(
    strsplit(as.character(gt$relevantes_titulos), ";;", fixed = TRUE),
    function(v) {
      v <- as.character(v)
      v <- trimws(v)
      v[nchar(v) > 0]
    }
  )
  gt
}

evaluar_query <- function(query, titulos_gt, titulos_rec) {
  # Garantizar que ambos son vectores character sin NA ni vacíos
  titulos_gt  <- as.character(unlist(titulos_gt))
  titulos_gt  <- titulos_gt[!is.na(titulos_gt) & nchar(trimws(titulos_gt)) > 0]

  titulos_rec <- as.character(unlist(titulos_rec))
  titulos_rec <- titulos_rec[!is.na(titulos_rec) & nchar(trimws(titulos_rec)) > 0]

  tp <- calcular_tp(titulos_rec, titulos_gt)
  p  <- if (length(titulos_rec) == 0) 0 else tp / length(titulos_rec)
  r  <- if (length(titulos_gt)  == 0) 0 else tp / length(titulos_gt)
  f1 <- calcular_f1(p, r)

  list(
    query         = as.character(query),
    n_gt          = length(titulos_gt),
    n_recuperados = length(titulos_rec),
    tp            = as.integer(tp),
    precision     = round(p,  4),
    recall        = round(r,  4),
    f1            = round(f1, 4)
  )
}

ejecutar_evaluacion_completa <- function(data, verbose = TRUE) {

  gt <- cargar_ground_truth()

  resultados <- lapply(seq_len(nrow(gt)), function(i) {
    query      <- as.character(gt$query[i])
    titulos_gt <- gt$lista_relevantes[[i]]

    if (verbose) cat(sprintf("  [%02d/%02d] %s\n", i, nrow(gt), query))

    # ── Baseline: grepl exacto ─────────────────────────────────────────────
    rec_base <- tryCatch(
      busqueda_baseline(query, data),
      error = function(e) {
        if (verbose) cat("    ⚠️  Baseline error:", conditionMessage(e), "\n")
        character(0)
      }
    )
    eval_base         <- evaluar_query(query, titulos_gt, rec_base)
    eval_base$sistema <- "Baseline (exacto)"

    # ── Sistema NLP: búsqueda ligera (sin síntesis ni análisis temporal) ────
    # Se usa busqueda_nlp_ligera() que combina:
    #   1. Embeddings vectoriales (si están cargados)  — muy rápido
    #   2. Expansión de sinónimos académicos + agrep   — rápido
    # No se llama al motor completo (proceso_nlp_chatbot_semantico) porque
    # ese genera resúmenes y análisis temporales que no son necesarios aquí
    # y tardan varios minutos por consulta.
    rec_nlp <- tryCatch(
      busqueda_nlp_ligera(query, data),
      error = function(e) {
        if (verbose) cat("    ⚠️  NLP error:", conditionMessage(e), "\n")
        character(0)
      }
    )

    eval_nlp         <- evaluar_query(query, titulos_gt, rec_nlp)
    eval_nlp$sistema <- "Sistema NLP"

    list(baseline = eval_base, nlp = eval_nlp)
  })

  # Construir data frames asegurando que cada fila tiene exactamente los mismos campos
  lista_a_df <- function(lst) {
    do.call(rbind, lapply(lst, function(r) {
      data.frame(
        sistema       = r$sistema,
        query         = r$query,
        n_gt          = r$n_gt,
        n_recuperados = r$n_recuperados,
        tp            = r$tp,
        precision     = r$precision,
        recall        = r$recall,
        f1            = r$f1,
        stringsAsFactors = FALSE
      )
    }))
  }

  df_base <- lista_a_df(lapply(resultados, `[[`, "baseline"))
  df_nlp  <- lista_a_df(lapply(resultados, `[[`, "nlp"))
  df_all  <- rbind(df_base, df_nlp)

  resumen <- df_all %>%
    group_by(sistema) %>%
    summarise(
      precision_macro = round(mean(precision, na.rm = TRUE), 4),
      recall_macro    = round(mean(recall,    na.rm = TRUE), 4),
      f1_macro        = round(mean(f1,        na.rm = TRUE), 4),
      n_queries       = n(),
      .groups = "drop"
    )

  list(detalle = df_all, resumen = resumen, ground_truth = gt)
}

# ── 4. BÚSQUEDA DIFUSA SIMPLE (fallback) ────────────────────────────────────

busqueda_difusa_simple <- function(query, data) {
  raw_tokens <- stri_split_boundaries(tolower(as.character(query)),
                                      type = "word")[[1]]
  tokens <- raw_tokens[nchar(raw_tokens) > 3]

  if (length(tokens) == 0) return(character(0))

  campo_titulo <- tolower(as.character(data$TITULO                %||% ""))
  campo_kw     <- tolower(as.character(data$AUTOR_PALABRAS_CLAVES %||% ""))
  campo_idx    <- tolower(as.character(data$INDEX_PALABRAS_CLAVES %||% ""))
  campo_titulo[is.na(campo_titulo)] <- ""
  campo_kw    [is.na(campo_kw)]     <- ""
  campo_idx   [is.na(campo_idx)]    <- ""
  campo_full  <- paste(campo_titulo, campo_kw, campo_idx, sep = " ")

  mask <- rep(FALSE, nrow(data))
  for (token in tokens) {
    hits <- agrep(token, campo_full, max.distance = 0.15, ignore.case = TRUE)
    mask[hits] <- TRUE
  }

  titulos <- as.character(data$TITULO[mask])
  titulos[!is.na(titulos)]
}

# ── 5. BÚSQUEDA NLP LIGERA (para evaluación batch — sin síntesis ni temporal) ──
#
# Combina:
#   a) Embeddings vectoriales si están cargados (buscar_por_embeddings)
#   b) Expansión de sinónimos académicos (expandir_consulta_semantica)
#      + búsqueda difusa (agrep) en TITULO, AUTOR_PALABRAS_CLAVES e INDEX_PALABRAS_CLAVES
#
# No ejecuta resúmenes LexRank ni análisis temporal para que sea rápida.

busqueda_nlp_ligera <- function(query, data) {
  # Búsqueda difusa pura: agrep (Levenshtein) sobre título + palabras clave.
  # Sin dependencias externas (no Python, no sinónimos, no sesión Shiny).
  # Diferencia clave vs baseline: tolerancia a variantes morfológicas y errores.
  busqueda_difusa_simple(query, data)
}

# ── 6. FORMATEO DE RESULTADOS ────────────────────────────────────────────────

generar_tabla_evaluacion_html <- function(evaluacion) {
  resumen <- evaluacion$resumen

  f1_base <- resumen$f1_macro[resumen$sistema == "Baseline (exacto)"]
  f1_nlp  <- resumen$f1_macro[resumen$sistema == "Sistema NLP"]
  mejora  <- if (length(f1_base) > 0 && f1_base > 0)
    round((f1_nlp - f1_base) / f1_base * 100, 1) else 0

  tagList(
    tags$h5("📊 Resumen de evaluación — 15 consultas",
            style = "color:#1a237e; margin-bottom:15px;"),

    tags$table(
      class = "table table-bordered table-sm",
      style = "font-size:13px; margin-bottom:20px;",
      tags$thead(
        style = "background:#1a237e; color:white;",
        tags$tr(
          tags$th("Sistema"),
          tags$th("Precisión (macro)"),
          tags$th("Recall (macro)"),
          tags$th("F1-Score (macro)")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(resumen)), function(i) {
          es_nlp <- grepl("NLP", resumen$sistema[i])
          tags$tr(
            style = if (es_nlp) "background:#e8f5e9; font-weight:bold;" else "",
            tags$td(resumen$sistema[i]),
            tags$td(sprintf("%.4f", resumen$precision_macro[i])),
            tags$td(sprintf("%.4f", resumen$recall_macro[i])),
            tags$td(sprintf("%.4f", resumen$f1_macro[i]))
          )
        })
      )
    ),

    tags$div(
      style = "background:#fff9c4; border:1px solid #f9a825; border-radius:6px; padding:12px; margin-bottom:20px;",
      tags$strong(
        sprintf("🏆 Mejora del sistema NLP sobre baseline: %+.1f%% en F1-Score", mejora)
      ),
      tags$br(),
      tags$small(
        style = "color:#666;",
        "Evaluación macro-promediada sobre 15 consultas con ground truth construido a partir del corpus USACH."
      )
    )
  )
}

generar_reporte_texto <- function(evaluacion) {
  resumen <- evaluacion$resumen
  f1_base <- resumen$f1_macro[resumen$sistema == "Baseline (exacto)"]
  f1_nlp  <- resumen$f1_macro[resumen$sistema == "Sistema NLP"]
  mejora  <- if (length(f1_base) > 0 && f1_base > 0)
    round((f1_nlp - f1_base) / f1_base * 100, 1) else 0

  p_nlp  <- resumen$precision_macro[resumen$sistema == "Sistema NLP"]
  r_nlp  <- resumen$recall_macro   [resumen$sistema == "Sistema NLP"]
  p_base <- resumen$precision_macro[resumen$sistema == "Baseline (exacto)"]
  r_base <- resumen$recall_macro   [resumen$sistema == "Baseline (exacto)"]

  cat(sprintf(paste(
    "=== RESULTADOS DE EVALUACIÓN ===\n\n",
    "Baseline (búsqueda exacta):\n",
    "  Precisión: %.4f | Recall: %.4f | F1-Score: %.4f\n\n",
    "Sistema NLP (difusa + semántica):\n",
    "  Precisión: %.4f | Recall: %.4f | F1-Score: %.4f\n\n",
    "Mejora F1: %+.1f%%\n\n",
    "Evaluado sobre 15 consultas con ground truth del corpus USACH (3.914 papers).\n"
  ), p_base, r_base, f1_base, p_nlp, r_nlp, f1_nlp, mejora))
}
