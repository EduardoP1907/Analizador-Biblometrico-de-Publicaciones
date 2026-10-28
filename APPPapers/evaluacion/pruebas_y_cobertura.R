# Ejecuta la batería de pruebas con detalle por archivo y mide la cobertura de
# código de los módulos de PLN con covr (Anexo C de la memoria).
# Uso, desde la raíz de APPPapers:  Rscript evaluacion/pruebas_y_cobertura.R [salida]
suppressMessages(library(testthat))
salida <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(salida)) salida <- "evaluacion/resultados"
dir.create(salida, showWarnings = FALSE, recursive = TRUE)

t0 <- Sys.time()
res <- as.data.frame(test_dir("tests/testthat", reporter = "silent", stop_on_failure = FALSE))
dur <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
res$aprobada <- res$failed == 0 & !res$error & res$skipped == 0
det <- res[, c("file", "test", "nb", "failed", "skipped", "aprobada")]
write.csv(det, file.path(salida, "pruebas_detalle.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("Pruebas: %d | verificaciones: %d | fallidas: %d | omitidas: %d | %.0f s\n",
            nrow(res), sum(res$nb), sum(res$failed > 0 | res$error), sum(res$skipped), dur))
print(aggregate(cbind(pruebas = 1, verificaciones = nb) ~ file, data = res, FUN = sum))

# Cobertura: en un proceso de R aparte, porque test_dir() deja cargados los módulos
# sin instrumentar y covr falla al reutilizar la sesión
system2(file.path(R.home("bin"), "Rscript"), c("evaluacion/cobertura.R", salida))
