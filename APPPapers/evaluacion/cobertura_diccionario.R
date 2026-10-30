# Cobertura del diccionario de sinónimos sobre el corpus y uso en las consultas de evaluación (O7, Sección 5.3).
#
# Uso (desde APPPapers/):  Rscript evaluacion/cobertura_diccionario.R
#
# Para cada término base cuenta cuántos de sus sinónimos aparecen como palabra completa en el título, las
# palabras clave o el resumen de algún registro (misma regla que score_sinonimos), y para cada consulta del
# ground truth cuántos términos agrega expandir_consulta_semantica().
source("evaluacion/cargar_datos_app.R")
suppressMessages({ library(shiny); library(stringi) })
datos <- cargar_datos_app()
suppressMessages(source("www/SOURCE/UTILS/nlp_chatbot_engine_semantic.R"))
source("www/SOURCE/UTILS/nlp_evaluacion_metricas.R")

dic <- get_diccionario_sinonimos()
# Una clave repetida nunca se usa: diccionario[[clave]] devuelve siempre la primera entrada
cat("Entradas:", length(dic), "| claves repetidas:", paste(names(dic)[duplicated(names(dic))], collapse = ", "), "\n")
dic <- dic[!duplicated(names(dic))]
resumen <- as.character(datos$RESUMEN); resumen[is.na(resumen)] <- ""
texto <- stri_trans_general(paste(texto_lexico(datos), tolower(resumen)), "Latin-ASCII")
n_registros <- function(t) {
  t <- limpiar_texto_para_sinonimos(t)
  sum(stri_detect_regex(texto, paste0("\\b", stri_replace_all_regex(t, "([.^$|()\\[\\]{}*+?\\\\-])", "\\\\$1"), "\\b")))
}

filas <- lapply(names(dic), function(b) {
  en_corpus <- vapply(dic[[b]], n_registros, numeric(1))
  data.frame(termino = b, n_sinonimos = length(dic[[b]]), registros_termino = n_registros(b),
             sinonimos_en_corpus = sum(en_corpus > 0))
})
cob <- do.call(rbind, filas)
print(cob, row.names = FALSE)
cat(sprintf("\nTérminos base: %d | presentes en el corpus: %d | sinónimos: %d, presentes: %d (%.1f %%)\n",
            nrow(cob), sum(cob$registros_termino > 0), sum(cob$n_sinonimos), sum(cob$sinonimos_en_corpus),
            100 * sum(cob$sinonimos_en_corpus) / sum(cob$n_sinonimos)))

gt <- cargar_ground_truth()
uso <- vapply(gt$query, function(q) {
  invisible(capture.output(e <- expandir_consulta_semantica(q)))
  length(e$terminos_adicionales)
}, numeric(1))
print(data.frame(query = gt$query, terminos_agregados = uso), row.names = FALSE)
cat(sprintf("\nConsultas en que el diccionario agrega términos: %d de %d\n", sum(uso > 0), length(uso)))
