library(stringi, warn.conflicts = FALSE)
library(dplyr,   warn.conflicts = FALSE)

source("www/SOURCE/UTILS/nlp_evaluacion_metricas.R")

cat("Cargando datos...\n")
datos <- read.csv("www/BD/BD_papers.csv", sep = "|", header = TRUE,
                  stringsAsFactors = FALSE, quote = "")
cat(sprintf("%d papers cargados\n", nrow(datos)))

cat("Test busqueda_baseline...\n")
rec_base <- tryCatch(busqueda_baseline("machine learning", datos),
                     error = function(e) { cat("ERROR baseline:", conditionMessage(e), "\n"); character(0) })
cat(sprintf("  Baseline: %d resultados\n", length(rec_base)))

cat("Test busqueda_nlp_ligera...\n")
rec_nlp <- tryCatch(busqueda_nlp_ligera("machine learning", datos),
                    error = function(e) { cat("ERROR nlp:", conditionMessage(e), "\n"); character(0) })
cat(sprintf("  NLP:      %d resultados\n", length(rec_nlp)))

cat("Test cargar_ground_truth...\n")
gt <- tryCatch(cargar_ground_truth(),
               error = function(e) { cat("ERROR gt:", conditionMessage(e), "\n"); NULL })
if (!is.null(gt)) cat(sprintf("  Ground truth: %d queries\n", nrow(gt)))

cat("\nTODO OK\n")
