# Cobertura de código de los módulos de PLN con covr (la llama pruebas_y_cobertura.R).
# Uso, desde la raíz de APPPapers:  Rscript evaluacion/cobertura.R [salida]
# helper-cargar.R no se usa: volvería a cargar los módulos sin instrumentar.
suppressMessages(library(testthat))
salida <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(salida)) salida <- "evaluacion/resultados"
# Cobertura de los módulos que el módulo de PLN carga (www/SOURCE/UTILS)
fuentes <- c("nlp_synonyms_academic.R", "nlp_semantic_scoring.R", "nlp_busqueda_por_autor.R",
             "nlp_embedding_search.R", "nlp_chatbot_engine_semantic.R", "nlp_evaluacion_metricas.R")
fuentes <- file.path("www/SOURCE/UTILS", fuentes)
pruebas <- setdiff(list.files("tests/testthat", "^test-.*\\.R$", full.names = TRUE), "tests/testthat/test-app.R")
RAIZ_APP <- getwd()
suppressMessages({ library(dplyr); library(stringi) })
cob <- covr::file_coverage(fuentes, c("tests/testthat/helper-corpus.R", pruebas))
tab <- as.data.frame(cob)
por_archivo <- aggregate(value ~ filename, data = tab, FUN = function(v) round(100 * mean(v > 0), 1))
names(por_archivo)[2] <- "cobertura_lineas_pct"
print(por_archivo, row.names = FALSE)
cat(sprintf("Cobertura total de líneas: %.1f %%\n", covr::percent_coverage(cob)))
write.csv(por_archivo, file.path(salida, "cobertura.csv"), row.names = FALSE, fileEncoding = "UTF-8")
