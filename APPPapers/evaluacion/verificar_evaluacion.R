# Reproduce los números de la Sección 4.3 / Tabla 5 de la memoria.
#
# Uso:  Rscript verificar_evaluacion.R <dir APPPapers> [prefijo_salida]
#
# 1. Evaluación de la pestaña «Evaluación»: baseline, búsqueda difusa y
#    sistema híbrido sobre el mismo corpus que recibe la app (3.596 registros,
#    cargar_datos_app.R), que es el corpus sobre el que se juzgó el ground truth.
# 2. Análisis de componentes del híbrido: solo embeddings y modo degradado.
#
# Si cualquier número cambia respecto de la memoria, hay que actualizar TODOS
# los lugares donde aparece (ver SKILL.md, «Dónde aparecen las métricas»).

args <- commandArgs(trailingOnly = TRUE)
setwd(args[1])
prefijo <- if (length(args) >= 2) args[2] else NULL

dir_scripts <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(dir_scripts, "cargar_datos_app.R"))
suppressMessages({ library(dplyr); library(shiny) })
datos_tmp <- cargar_datos_app()
cat("Corpus de evaluación:", nrow(datos_tmp), "registros\n")

suppressMessages(source("www/SOURCE/UTILS/nlp_chatbot_engine_semantic.R"))
source("www/SOURCE/UTILS/nlp_evaluacion_metricas.R")
invisible(capture.output(ok <- inicializar_embeddings()))
cat("Embeddings activos:", ok, "\n\n")

invisible(capture.output(ev <- ejecutar_evaluacion_completa(datos_tmp, verbose = FALSE)))
cat("== Tabla 5 ==\n")
print(as.data.frame(ev$resumen), row.names = FALSE)

if (!is.null(prefijo)) {
  write.table(ev$detalle, paste0(prefijo, "_detalle.csv"), sep = ",", row.names = FALSE)
  write.table(ev$resumen, paste0(prefijo, "_resumen.csv"), sep = ",", row.names = FALSE)
}

# ── Análisis de componentes ─────────────────────────────────────────────────
gt <- cargar_ground_truth()
correr <- function(etiqueta) {
  r <- lapply(seq_len(nrow(gt)), function(i) {
    invisible(capture.output(rec <- busqueda_hibrida_eval(gt$query[i], datos_tmp)))
    evaluar_query(gt$query[i], gt$lista_relevantes[[i]], rec)
  })
  data.frame(configuracion = etiqueta,
             P  = round(mean(sapply(r, `[[`, "precision")), 4),
             R  = round(mean(sapply(r, `[[`, "recall")), 4),
             F1 = round(mean(sapply(r, `[[`, "f1")), 4))
}
pesos <- c(PESO_EMBEDDINGS, PESO_DIFUSA, PESO_SINONIMOS)
res <- list(correr(sprintf("Híbrido (%.2f/%.2f/%.2f)", pesos[1], pesos[2], pesos[3])))
PESO_EMBEDDINGS <- 1; PESO_DIFUSA <- 0; PESO_SINONIMOS <- 0
res[[2]] <- correr("Solo embeddings")
.emb$listo <- FALSE; PESO_DIFUSA <- pesos[2]; PESO_SINONIMOS <- pesos[3]
res[[3]] <- correr("Modo degradado (difusa + sinónimos)")
cat("\n== Análisis de componentes (párrafo 2 de la Sección 4.3) ==\n")
print(do.call(rbind, res), row.names = FALSE)
