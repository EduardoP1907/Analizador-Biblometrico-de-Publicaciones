# Análisis del estudio de usabilidad (ver protocolo.md y la Sección 6.4 de la memoria)
#
# Uso:  Rscript estudio_usuarios/analizar_estudio.R estudio_usuarios ["<respuestas.xlsx>"]
# Lee las respuestas SUS directamente del archivo original exportado de Google Forms
# (por defecto «Formulario sin título (respuestas).xlsx» en APPPapers/: columnas 2-11 = ítems
# 1-10 del SUS, con valores del tipo «5 Totalmente de acuerdo»; 12-13 = preguntas abiertas).
# resultados_tareas.csv es opcional: la prueba del 20-oct-2026 fue de uso libre, sin tareas medidas.

args <- commandArgs(trailingOnly = TRUE)
dir_estudio <- args[1]
archivo <- if (length(args) >= 2) args[2] else
  file.path(dir_estudio, "..", "Formulario sin título (respuestas).xlsx")
resp   <- as.data.frame(readxl::read_excel(archivo))
tareas <- read.csv(file.path(dir_estudio, "resultados_tareas.csv"), stringsAsFactors = FALSE)
if (nrow(resp) == 0) stop("El archivo de respuestas no tiene filas.")

# ── SUS (Brooke, 1996): ítems impares (x - 1), pares (5 - x); suma × 2,5 ─────
q <- sapply(resp[, 2:11], function(x) as.integer(substr(x, 1, 1)))
q <- matrix(q, ncol = 10, dimnames = list(NULL, paste0("q", 1:10)))
if (any(q < 1 | q > 5, na.rm = TRUE) || anyNA(q)) stop("Respuestas SUS fuera de 1-5 o incompletas.")
puntaje <- (rowSums(q[, c(1, 3, 5, 7, 9), drop = FALSE] - 1) +
            rowSums(5 - q[, c(2, 4, 6, 8, 10), drop = FALSE])) * 2.5
cat("SUS por participante:\n")
print(data.frame(participante = paste0("P", seq_along(puntaje)), puntaje), row.names = FALSE)
cat(sprintf("\nSUS medio: %.1f (DE %.1f; mín. %.1f, máx. %.1f; n = %d)\n",
            mean(puntaje), sd(puntaje), min(puntaje), max(puntaje), length(puntaje)))
cat("\nRespuestas por ítem (1-5):\n")
print(apply(q, 2, function(x) paste(sort(x), collapse = " ")))
cat("\nPreguntas abiertas:\n")
for (i in seq_len(nrow(resp))) cat(sprintf("P%d | %s | %s\n", i, trimws(resp[i, 12]), trimws(resp[i, 13])))

# ── Tareas: éxito y tiempo (solo si se midieron) ────────────────────────────
if (nrow(tareas) > 0) {
  por_tarea <- do.call(rbind, lapply(split(tareas, tareas$tarea), function(t) data.frame(
    tarea = t$tarea[1], n = nrow(t), exitos = sum(t$exito), tasa_exito = mean(t$exito),
    mediana_s = median(t$tiempo_s), min_s = min(t$tiempo_s), max_s = max(t$tiempo_s),
    consultas_media = mean(t$n_consultas))))
  cat("\nTareas:\n"); print(por_tarea, row.names = FALSE)
  write.csv(por_tarea, file.path(dir_estudio, "resumen_tareas.csv"), row.names = FALSE)
} else {
  cat("\nSin tareas medidas (resultados_tareas.csv vacío).\n")
}
