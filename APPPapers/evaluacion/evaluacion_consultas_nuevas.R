# Evaluación con las consultas nuevas (observación O5): 10 consultas redactadas a partir de temas
# sorteados del corpus y juzgadas por dos evaluadores (planillas_consultas_nuevas.py gt).
#
# Uso (desde APPPapers/):
#   Rscript evaluacion/evaluacion_consultas_nuevas.R <ground_truth.csv> <pool_consultas_nuevas.csv> <salida.csv>
#
# Se juzgaron los 20 primeros del híbrido (fuente H) y la muestra de hasta 10 resultados del
# baseline (fuente B), así que solo se comparan esos dos sistemas, con el mismo criterio que el
# «mismo corte» de evaluacion_revision.R: el híbrido con su top-20 y el baseline con su muestra
# aleatoria ya juzgada. El recall es relativo a los relevantes encontrados en ese pool.

args <- commandArgs(trailingOnly = TRUE)
dir_scripts <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(dir_scripts, "cargar_datos_app.R"))
suppressMessages({ library(shiny); library(stringi) })
datos <- cargar_datos_app()
suppressMessages(source("www/SOURCE/UTILS/nlp_chatbot_engine_semantic.R"))
source("www/SOURCE/UTILS/nlp_evaluacion_metricas.R")
invisible(capture.output(ok <- inicializar_embeddings()))
if (!isTRUE(ok)) stop("Embeddings no disponibles: definir APPPAPERS_PYTHON")

gt   <- cargar_ground_truth(args[1])
pool <- read.table(args[2], sep = "|", header = TRUE, quote = "", comment.char = "",
                   stringsAsFactors = FALSE, encoding = "UTF-8")
pool <- pool[grepl("[HB]", pool$fuentes), ]

callar <- function(expr) { invisible(capture.output(x <- expr)); x }
metricas <- function(sistema, query, rel, rec, juzgados) {
  m <- evaluar_query(query, rel, rec)
  data.frame(sistema, query, n_rel = m$n_gt, n_rec = m$n_recuperados, tp = m$tp,
             n_juzgados = calcular_tp(unique(rec), juzgados), P = m$precision, R = m$recall, F1 = m$f1)
}

filas <- list()
for (i in seq_len(nrow(gt))) {
  q <- gt$query[i]; rel <- gt$lista_relevantes[[i]]
  juz <- pool$titulo[pool$query == q]
  hib <- callar(busqueda_hibrida_eval(q, datos))
  base <- pool$titulo[pool$query == q & grepl("B", pool$fuentes)]
  filas[[length(filas) + 1]] <- metricas("Sistema híbrido", q, rel, hib, juz)
  filas[[length(filas) + 1]] <- metricas("Baseline (exacto)", q, rel, base, juz)
}
det <- do.call(rbind, filas)
# Todo lo que devuelve el híbrido debe estar juzgado (si no, el pool quedó desfasado del código)
h <- det[det$sistema == "Sistema híbrido", ]
if (any(h$n_juzgados < h$n_rec)) warning("Hay resultados del híbrido sin juzgar")

set.seed(2026)
ic <- function(x) round(unname(quantile(replicate(10000, mean(sample(x, replace = TRUE))), c(.025, .975))), 4)
res <- list()
for (mt in c("P", "R", "F1")) {
  a <- det[[mt]][det$sistema == "Sistema híbrido"]; b <- det[[mt]][det$sistema == "Baseline (exacto)"]
  d <- a - b
  res[[length(res) + 1]] <- data.frame(
    metrica = mt, hibrido = round(mean(a), 4), ic_hibrido = paste(ic(a), collapse = " – "),
    baseline = round(mean(b), 4), ic_baseline = paste(ic(b), collapse = " – "),
    diferencia = round(mean(d), 4), ic_diferencia = paste(ic(d), collapse = " – "),
    p_wilcoxon = signif(suppressWarnings(wilcox.test(a, b, paired = TRUE, exact = FALSE)$p.value), 3),
    gana = sum(d > 0), empata = sum(d == 0), pierde = sum(d < 0))
}
res <- do.call(rbind, res)
write.csv(det, sub("\\.csv$", "_detalle.csv", args[3]), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(res, args[3], row.names = FALSE, fileEncoding = "UTF-8")
print(det[, c("sistema", "query", "n_rel", "n_rec", "tp", "n_juzgados", "P", "R", "F1")], row.names = FALSE)
print(res, row.names = FALSE)
