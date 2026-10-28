# Mediciones citadas en la memoria: carga inicial, tiempo de búsqueda (RNF-03),
# ejemplo numérico de la fórmula (Sección 4.4), ejemplos del parser (Sección 5.2),
# tamaño del diccionario (Sección 5.3) y resúmenes LexRank (Sección 5.5).
# Uso, desde la raíz de APPPapers:  Rscript evaluacion/mediciones_memoria.R
suppressMessages({ library(dplyr); library(stringi); library(shiny) })
callar <- function(expr) { invisible(capture.output(x <- expr)); x }
source("evaluacion/cargar_datos_app.R")

# ── 1. Carga inicial: corpus, módulos y motor de embeddings ────────────────
t <- system.time({
  datos <- cargar_datos_app()
  callar(suppressMessages(source("www/SOURCE/UTILS/nlp_chatbot_engine_semantic.R")))
  callar(ok <- inicializar_embeddings())
})[["elapsed"]]
cat(sprintf("Carga inicial: %.1f s (embeddings activos: %s, %d registros)\n", t, ok, nrow(datos)))

buscar <- function(q, d = datos) {
  f <- callar(parsear_filtros_query(q))
  if (f$anos$tiene) d <- callar(aplicar_filtro_anos(d, f$anos))
  tema <- if (nchar(trimws(f$terminos_tema)) >= 2) f$terminos_tema else q
  callar(buscar_papers_hibrido(tema, d, expandir_consulta_semantica(tema)))
}

# ── 2. Tiempo de búsqueda (RNF-03) ─────────────────────────────────────────
consultas <- c("machine learning en medicina", "redes neuronales para clasificación de imágenes",
               "algoritmos genéticos", "seguridad informática", "procesamiento de lenguaje natural",
               "enseñanza de la programación", "computación en la nube", "bioinformática",
               "internet de las cosas", "minería de datos educacional")
invisible(buscar(consultas[1]))   # la primera codificación calienta el modelo
tiempos <- vapply(consultas, function(q) system.time(buscar(q))[["elapsed"]], numeric(1))
cat(sprintf("Búsqueda con embeddings (10 consultas): media %.2f s, máx. %.2f s\n", mean(tiempos), max(tiempos)))
emb <- .emb$listo; .emb$listo <- FALSE
tiempos_d <- vapply(consultas, function(q) system.time(buscar(q))[["elapsed"]], numeric(1))
.emb$listo <- emb
cat(sprintf("Búsqueda en modo degradado: media %.2f s, máx. %.2f s\n", mean(tiempos_d), max(tiempos_d)))

# ── 3. Ejemplo de la fórmula (Sección 4.4) ─────────────────────────────────
q <- "aprendizaje automático en educación"
f <- callar(parsear_filtros_query(q)); tema <- f$terminos_tema
e <- expandir_consulta_semantica(tema)
sims <- callar(similitudes_embeddings(tema, datos))
sc <- calcular_scores_hibridos(tema, datos, e, sims)
cat("\nEjemplo 4.4 — tema:", tema, "| sinónimos agregados:", length(e$terminos_adicionales), "\n")
cat("  ", paste(head(e$terminos_adicionales, 8), collapse = "; "), "\n")
cat("  sobre el umbral:", sum(sc$score_semantico >= UMBRAL_HIBRIDO), "de", nrow(datos), "\n")
r <- buscar(q)
print(head(r[, c("TITULO", "score_embeddings", "score_difuso", "score_sinonimos", "score_semantico")], 5), row.names = FALSE)
i <- grep("predict academic success in an introductory programming", datos$TITULO, ignore.case = TRUE)[1]
if (!is.na(i)) cat(sprintf("  «%s»: s_emb=%.3f s_dif=%.3f s_sin=%.3f score=%.3f (lugar %d)\n", datos$TITULO[i],
                           sc$score_embeddings[i], sc$score_difuso[i], sc$score_sinonimos[i], sc$score_semantico[i],
                           match(datos$TITULO[i], r$TITULO)))

# ── 4. Ejemplos del parser (Sección 5.2) ───────────────────────────────────
for (qq in c("papers de Pablo Pérez Lantero del 2018 al 2023", "¿Qué trabajos aplican ML desde 2020?",
             "papers de machine learning", "publicaciones de María González sobre redes neuronales")) {
  f <- callar(parsear_filtros_query(qq))
  cat(sprintf("\n«%s» → autor: %s | años: %s | tema: «%s»", qq,
              if (f$autor$tiene) f$autor$nombre else "—",
              if (f$anos$tiene) paste(f$anos$desde, f$anos$hasta, sep = "–") else "—", f$terminos_tema))
}
cat("\n")

# ── 5. Diccionario (Sección 5.3 y Anexo E) ─────────────────────────────────
dic <- get_diccionario_sinonimos()
n <- lengths(dic)
cat(sprintf("\nDiccionario: %d términos base, %d a %d sinónimos (media %.1f), %d siglas\n",
            length(dic), min(n), max(n), mean(n), length(siglas_diccionario())))
cat("Siglas:", paste(siglas_diccionario(), collapse = ", "), "\n")

# ── 6. Resúmenes LexRank (Sección 5.5) ─────────────────────────────────────
for (qq in c("redes neuronales para clasificación de imágenes", "algoritmos genéticos", "enseñanza de la programación")) {
  r <- buscar(qq)
  cat("\n== LexRank:", qq, "==\n")
  o <- resumen_lexrank(r)
  for (k in seq_along(o)) {
    j <- which(vapply(head(r$RESUMEN, 5), function(a) grepl(o[k], a, fixed = TRUE), logical(1)))[1]
    cat(sprintf("[%d] (publicación %s del ranking) %s\n", k, j, o[k]))
  }
  cat("Títulos 1-5:", paste(head(r$TITULO, 5), collapse = " || "), "\n")
}
