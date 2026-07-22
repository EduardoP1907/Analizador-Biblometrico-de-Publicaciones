#=======================================================================
# SCRIPT DE EVALUACIÓN: Precisión, Recall y F1-Score
# Compara búsqueda baseline (exacta) vs sistema NLP del proyecto APPPapers
#
# USO: Abrir en RStudio y ejecutar con Ctrl+Shift+Enter (o Source)
#       setwd() al directorio raíz del proyecto antes de correr
#=======================================================================

cat("=================================================================\n")
cat(" APPPapers — Evaluación F1-Score: Baseline vs NLP\n")
cat("=================================================================\n\n")

# ── 0. Dependencias ────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(stringi)
  library(dplyr)
})

# ── 1. Cargar datos reales ─────────────────────────────────────────────────
cat("[1/4] Cargando base de datos bibliométrica...\n")
datos <- read.csv(
  "www/BD/BD_papers.csv",
  sep = "|", header = TRUE,
  stringsAsFactors = FALSE,
  encoding = "UTF-8",
  quote = ""
)
cat(sprintf("      %d papers cargados\n", nrow(datos)))

# ── 2. Cargar módulos ──────────────────────────────────────────────────────
cat("[2/4] Cargando módulos NLP...\n")
source("www/SOURCE/UTILS/nlp_evaluacion_metricas.R")

# Intentar cargar el motor semántico completo
motor_cargado <- tryCatch({
  source("www/SOURCE/UTILS/nlp_synonyms_academic.R")
  source("www/SOURCE/UTILS/nlp_semantic_scoring.R")
  source("www/SOURCE/UTILS/nlp_temporal_filters.R")
  source("www/SOURCE/UTILS/nlp_busqueda_por_autor.R")
  source("www/SOURCE/UTILS/nlp_embedding_search.R")
  source("www/SOURCE/UTILS/nlp_analisis_temporal_avanzado.R")
  source("www/SOURCE/UTILS/nlp_analisis_citaciones_temporal.R")
  source("www/SOURCE/UTILS/nlp_analisis_autores_temporal.R")
  source("www/SOURCE/UTILS/nlp_visualizaciones_temporales.R")
  source("www/SOURCE/UTILS/nlp_chatbot_engine_semantic.R")
  cat("      ✅ Motor semántico cargado\n")
  "semantico"
}, error = function(e) {
  cat("      ⚠️ Motor semántico no disponible, intentando motor simple...\n")
  tryCatch({
    source("www/SOURCE/UTILS/nlp_chatbot_engine_simple_v2.R")
    cat("      ✅ Motor simple cargado\n")
    "simple"
  }, error = function(e2) {
    cat("      ℹ️ Usando solo búsqueda difusa básica\n")
    "difuso"
  })
})

cat(sprintf("      Motor activo: %s\n", motor_cargado))

# ── 3. Ejecutar evaluación ─────────────────────────────────────────────────
cat("\n[3/4] Ejecutando evaluación sobre 15 consultas ground truth...\n\n")
evaluacion <- ejecutar_evaluacion_completa(datos, verbose = TRUE)

# ── 4. Mostrar resultados ──────────────────────────────────────────────────
cat("\n[4/4] RESULTADOS\n")
cat("=================================================================\n\n")

resumen <- evaluacion$resumen
cat("Sistema                  | Precisión | Recall   | F1-Score\n")
cat("-----------------------------------------------------------------\n")
for (i in seq_len(nrow(resumen))) {
  cat(sprintf("%-25s| %.4f    | %.4f   | %.4f\n",
    resumen$sistema[i],
    resumen$precision_macro[i],
    resumen$recall_macro[i],
    resumen$f1_macro[i]
  ))
}
cat("-----------------------------------------------------------------\n\n")

# Mejora porcentual
f1_base <- resumen$f1_macro[resumen$sistema == "Baseline (exacto)"]
f1_nlp  <- resumen$f1_macro[resumen$sistema == "Sistema NLP"]
if (length(f1_base) > 0 && f1_base > 0) {
  mejora <- (f1_nlp - f1_base) / f1_base * 100
  cat(sprintf("🏆 Mejora F1-Score NLP sobre Baseline: %+.1f%%\n\n", mejora))
}

# Detalle por query
cat("Detalle por consulta:\n")
detalle <- evaluacion$detalle
for (q in unique(detalle$query)) {
  sub <- detalle[detalle$query == q, ]
  cat(sprintf("  '%s'\n", q))
  for (j in seq_len(nrow(sub))) {
    cat(sprintf("    %-28s P=%.2f  R=%.2f  F1=%.2f  (GT=%d, rec=%d, TP=%d)\n",
      sub$sistema[j], sub$precision[j], sub$recall[j], sub$f1[j],
      sub$n_gt[j], sub$n_recuperados[j], sub$tp[j]
    ))
  }
}

# ── Guardar CSV con resultados ─────────────────────────────────────────────
output_csv <- paste0("evaluacion_resultados_", format(Sys.Date(), "%Y%m%d"), ".csv")
write.csv(evaluacion$detalle, output_csv, row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("\n✅ Resultados guardados en: %s\n", output_csv))
cat("=================================================================\n")
