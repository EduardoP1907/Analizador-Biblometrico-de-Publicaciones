# Análisis pedidos por la revisión del 24-oct-2026 (O2, O6, O8, O12, O13, O14, O17).
#
# Uso:  Rscript evaluacion_revision.R <dir APPPapers> <dir salida>
#
# Escribe en <dir salida>:
#   detalle_sistemas.csv    métricas por consulta de los 5 sistemas (Tabla 8/9)
#   mismo_corte.csv         P@20 y R@20 de los 5 sistemas, precisión sobre lo
#                           juzgado y cobertura del pool (O2)
#   estadistica.csv         medias, IC 95 % bootstrap y Wilcoxon pareado (O12, O8)
#   sensibilidad.csv        umbral, límite, pesos y distancia de agrep (O13, O14)
#   variantes.csv           consultas en lenguaje natural, con errores, en el otro
#                           idioma y con período (O6)
#   errores_top20.csv / errores_fn.csv  material para el análisis de errores (O17)
#   no_juzgados.csv         resultados del top-20 que no están en el pool

args <- commandArgs(trailingOnly = TRUE)
setwd(args[1])
salida <- args[2]
dir.create(salida, showWarnings = FALSE, recursive = TRUE)

dir_scripts <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(dir_scripts, "cargar_datos_app.R"))
suppressMessages({ library(dplyr); library(shiny); library(stringi) })
datos <- cargar_datos_app()
suppressMessages(source("www/SOURCE/UTILS/nlp_chatbot_engine_semantic.R"))
source("www/SOURCE/UTILS/nlp_evaluacion_metricas.R")
invisible(capture.output(ok <- inicializar_embeddings()))
cat("Corpus:", nrow(datos), "| embeddings:", ok, "\n")

callar <- function(expr) { invisible(capture.output(x <- expr)); x }
gt   <- cargar_ground_truth()
pool <- read.csv("www/BD/ground_truth_pool.csv", sep = "|",
                 quote = "", stringsAsFactors = FALSE)
pool <- pool[pool$en_corpus_app %in% c(TRUE, "TRUE"), ]
vari <- read.csv("www/BD/consultas_variantes.csv", sep = "|", quote = "", stringsAsFactors = FALSE,
                 encoding = "UTF-8")

# ── Puntajes por consulta (se calculan una vez y se reordenan en memoria) ────
preparar <- function(query, data, dist = c(0.10, 0.15, 0.20)) {
  f <- callar(parsear_filtros_query(query))
  if (f$anos$tiene) data <- callar(aplicar_filtro_anos(data, f$anos))
  q <- if (nchar(trimws(f$terminos_tema)) >= 2) f$terminos_tema else query
  e <- expandir_consulta_semantica(q)
  sims <- callar(similitudes_embeddings(q, data))
  s_emb <- if (is.null(sims)) rep(0, nrow(data)) else { x <- pmax(0, sims); x[is.na(x)] <- 0; x }
  list(titulos = as.character(data$TITULO), s_emb = s_emb, s_sin = score_sinonimos(e, data),
       s_dif = setNames(lapply(dist, function(d) score_difuso(q, data, max_distance = d)), dist),
       data = data)
}

# Misma regla que buscar_papers_hibrido: umbral, orden por puntaje, un título una vez, top-k
ordenar <- function(p, w = c(PESO_EMBEDDINGS, PESO_DIFUSA, PESO_SINONIMOS), umbral = UMBRAL_HIBRIDO,
                    k = TOP_K_HIBRIDO, dist = "0.15", modo = c("hibrido", "embeddings", "degradado")) {
  modo <- match.arg(modo)
  s_dif <- p$s_dif[[dist]]
  score <- switch(modo,
    hibrido    = w[1] * p$s_emb + w[2] * s_dif + w[3] * p$s_sin,
    embeddings = p$s_emb,
    degradado  = (w[2] * s_dif + w[3] * p$s_sin) / (w[2] + w[3]))
  ok <- which(score >= umbral)
  o  <- ok[order(score[ok], decreasing = TRUE)]
  clave <- trimws(stri_replace_all_regex(stri_trans_general(tolower(p$titulos[o]), "Latin-ASCII"), "[^a-z0-9]+", " "))
  o <- o[!duplicated(clave)]
  list(titulos = head(p$titulos[o], k), score = score, orden = o)
}

metricas <- function(query, rel, rec, juzgados) {
  m <- evaluar_query(query, rel, rec)
  rec_u <- unique(rec)
  n_j <- calcular_tp(rec_u, juzgados)
  data.frame(n_rel = m$n_gt, n_rec = m$n_recuperados, tp = m$tp, P = m$precision, R = m$recall,
             F1 = m$f1, n_juzgados = n_j,
             cobertura = if (m$n_recuperados == 0) NA else round(n_j / m$n_recuperados, 4),
             P_juzgados = if (n_j == 0) NA else round(m$tp / n_j, 4))
}

cat("Preparando puntajes de las 15 consultas...\n")
prep <- lapply(gt$query, preparar, data = datos)

# Comprobación: la reimplementación coincide con el pipeline de la app
for (i in c(1, 9, 14)) {
  app <- callar(busqueda_hibrida_eval(gt$query[i], datos))
  stopifnot(identical(app, ordenar(prep[[i]])$titulos))
}
cat("Reimplementación del ranking = buscar_papers_hibrido: OK\n")

# ── 1. Sistemas sobre las listas completas (Tabla 8/9) y con el mismo corte (O2) ──
detalle <- list(); corte <- list(); no_juzg <- list()
for (i in seq_len(nrow(gt))) {
  q <- gt$query[i]; rel <- gt$lista_relevantes[[i]]
  juz <- pool$titulo[pool$query == q]
  base <- busqueda_baseline(q, datos)
  dif  <- busqueda_difusa_simple(q, datos)
  lst <- list("Baseline (exacto)" = base, "Búsqueda difusa" = dif,
              "Sistema híbrido" = ordenar(prep[[i]])$titulos,
              "Solo embeddings" = ordenar(prep[[i]], modo = "embeddings")$titulos,
              "Modo degradado" = ordenar(prep[[i]], modo = "degradado")$titulos)
  for (s in names(lst)) detalle[[length(detalle) + 1]] <- cbind(sistema = s, query = q, metricas(q, rel, lst[[s]], juz))

  # Mismo corte: top-20 de los sistemas que ordenan; para baseline y difusa, la
  # muestra aleatoria de hasta 20 resultados ya juzgada en el pool (fuentes B y D)
  muestra <- list("Baseline (exacto)" = pool$titulo[pool$query == q & grepl("B", pool$fuentes)],
                  "Búsqueda difusa"   = pool$titulo[pool$query == q & grepl("D", pool$fuentes)])
  for (s in names(lst)) {
    rec20 <- if (s %in% names(muestra)) muestra[[s]] else lst[[s]]
    corte[[length(corte) + 1]] <- cbind(sistema = s, query = q, metricas(q, rel, rec20, juz))
  }
  for (s in c("Sistema híbrido", "Solo embeddings")) {
    top <- lst[[s]]
    nj <- top[!vapply(top, function(t) calcular_tp(t, juz) > 0, logical(1))]
    if (length(nj)) {
      j <- match(nj, datos$TITULO)
      no_juzg[[length(no_juzg) + 1]] <- data.frame(query = q, sistema = s, titulo = nj, ano = datos$ANO[j],
        palabras_clave = as.character(datos$AUTOR_PALABRAS_CLAVES[j]), resumen = substr(as.character(datos$RESUMEN[j]), 1, 700))
    }
  }
}
detalle <- do.call(rbind, detalle); corte <- do.call(rbind, corte)
write.csv(detalle, file.path(salida, "detalle_sistemas.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(corte, file.path(salida, "mismo_corte.csv"), row.names = FALSE, fileEncoding = "UTF-8")
if (length(no_juzg)) write.csv(do.call(rbind, no_juzg), file.path(salida, "no_juzgados.csv"), row.names = FALSE, fileEncoding = "UTF-8")

resumir <- function(df) df %>% group_by(sistema) %>%
  summarise(P = round(mean(P), 4), R = round(mean(R), 4), F1 = round(mean(F1), 4),
            P_juzgados = round(mean(P_juzgados, na.rm = TRUE), 4),
            cobertura = round(mean(cobertura, na.rm = TRUE), 4), n_rec = round(mean(n_rec), 1), .groups = "drop")
cat("\n== Listas completas ==\n"); print(as.data.frame(resumir(detalle)), row.names = FALSE)
cat("\n== Mismo corte (20) ==\n"); print(as.data.frame(resumir(corte)), row.names = FALSE)

# ── 2. Estadística (O12, O8): IC 95 % bootstrap por consultas y Wilcoxon pareado ──
set.seed(2026)
B <- 10000
ic <- function(x) { m <- replicate(B, mean(sample(x, replace = TRUE))); round(quantile(m, c(0.025, 0.975)), 4) }
por <- function(df, s, col) df[[col]][df$sistema == s]
est <- list()
for (tabla in c("completa", "corte20")) {
  df <- if (tabla == "completa") detalle else corte
  for (s in unique(df$sistema)) for (col in c("P", "R", "F1")) {
    x <- por(df, s, col); ci <- ic(x)
    est[[length(est) + 1]] <- data.frame(tabla, comparacion = s, metrica = col, media = round(mean(x), 4),
                                         ic_inf = ci[1], ic_sup = ci[2], p_wilcoxon = NA, gana = NA, empata = NA, pierde = NA)
  }
  pares <- list(c("Sistema híbrido", "Baseline (exacto)"), c("Sistema híbrido", "Búsqueda difusa"),
                c("Sistema híbrido", "Solo embeddings"), c("Sistema híbrido", "Modo degradado"),
                c("Solo embeddings", "Baseline (exacto)"), c("Modo degradado", "Baseline (exacto)"))
  for (pp in pares) for (col in c("P", "R", "F1")) {
    d <- por(df, pp[1], col) - por(df, pp[2], col); ci <- ic(d)
    pv <- suppressWarnings(wilcox.test(por(df, pp[1], col), por(df, pp[2], col), paired = TRUE, exact = FALSE)$p.value)
    est[[length(est) + 1]] <- data.frame(tabla, comparacion = paste(pp, collapse = " - "), metrica = col,
                                         media = round(mean(d), 4), ic_inf = ci[1], ic_sup = ci[2],
                                         p_wilcoxon = signif(pv, 3), gana = sum(d > 0), empata = sum(d == 0), pierde = sum(d < 0))
  }
}
est <- do.call(rbind, est); rownames(est) <- NULL
# Corrección de Holm dentro de cada tabla y métrica (6 comparaciones)
est$p_holm <- NA
for (g in split(seq_len(nrow(est)), paste(est$tabla, est$metrica))) {
  k <- g[!is.na(est$p_wilcoxon[g])]
  est$p_holm[k] <- signif(p.adjust(est$p_wilcoxon[k], "holm"), 3)
}
write.csv(est, file.path(salida, "estadistica.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("\n== Comparaciones (F1) ==\n"); print(est[est$metrica == "F1" & !is.na(est$p_wilcoxon), ], row.names = FALSE)

# ── 3. Sensibilidad (O13) y recall sin límite (O14) ─────────────────────────
evaluar_config <- function(param, valor, ...) {
  r <- lapply(seq_len(nrow(gt)), function(i) {
    q <- gt$query[i]
    metricas(q, gt$lista_relevantes[[i]], ordenar(prep[[i]], ...)$titulos, pool$titulo[pool$query == q])
  })
  r <- do.call(rbind, r)
  data.frame(parametro = param, valor = valor, P = round(mean(r$P), 4), R = round(mean(r$R), 4),
             F1 = round(mean(r$F1), 4), P_juzgados = round(mean(r$P_juzgados, na.rm = TRUE), 4),
             cobertura = round(mean(r$cobertura, na.rm = TRUE), 4), n_rec = round(mean(r$n_rec), 1))
}
sens <- list()
for (u in c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40)) sens[[length(sens) + 1]] <- evaluar_config("umbral", u, umbral = u)
for (k in c(10, 20, 30, 50, 100, Inf)) sens[[length(sens) + 1]] <- evaluar_config("limite", k, k = k)
for (we in seq(0.3, 0.9, 0.1)) {
  w <- c(we, (1 - we) * 0.625, (1 - we) * 0.375)
  sens[[length(sens) + 1]] <- evaluar_config("peso_embeddings", we, w = w)
}
for (fd in c(0.3, 0.5, 0.7)) {
  w <- c(PESO_EMBEDDINGS, (1 - PESO_EMBEDDINGS) * fd, (1 - PESO_EMBEDDINGS) * (1 - fd))
  sens[[length(sens) + 1]] <- evaluar_config("fraccion_difusa_lexica", fd, w = w)
}
for (d in c("0.1", "0.15", "0.2")) sens[[length(sens) + 1]] <- evaluar_config("dist_agrep", d, dist = d)
sens <- do.call(rbind, sens)
write.csv(sens, file.path(salida, "sensibilidad.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("\n== Sensibilidad ==\n"); print(sens, row.names = FALSE)

# ── 4. Variantes de las consultas (O6) ──────────────────────────────────────
# Errores de escritura con una regla fija: en cada palabra de 5 letras o más se
# introduce un error en la letra central, alternando transposición, omisión y
# sustitución por la letra siguiente del alfabeto.
con_errores <- function(q) {
  pal <- strsplit(q, " ")[[1]]; j <- 0
  for (i in seq_along(pal)) {
    w <- pal[i]; n <- nchar(w)
    if (n < 5) next
    j <- j + 1; m <- n %/% 2; ch <- strsplit(w, "")[[1]]
    tipo <- (j - 1) %% 3
    if (tipo == 0) ch[c(m, m + 1)] <- ch[c(m + 1, m)]
    if (tipo == 1) ch <- ch[-m]
    if (tipo == 2) ch[m] <- if (ch[m] %in% letters) letters[(match(ch[m], letters) %% 26) + 1] else ch[m]
    pal[i] <- paste(ch, collapse = "")
  }
  paste(pal, collapse = " ")
}
ano <- suppressWarnings(as.integer(datos$ANO))
filas_var <- list()
for (i in seq_len(nrow(gt))) {
  q0 <- gt$query[i]; v <- vari[vari$query == q0, ]
  rel <- gt$lista_relevantes[[i]]
  # Relevantes publicados desde 2020 (la variante con período filtra por año)
  rel20 <- rel[vapply(rel, function(t) any(ano[datos$TITULO == t] >= 2020, na.rm = TRUE), logical(1))]
  consultas <- list(original = q0, lenguaje_natural = v$lenguaje_natural, errores = con_errores(q0),
                    otro_idioma = v$otro_idioma, periodo = paste(v$lenguaje_natural, "desde 2020"))
  for (tipo in names(consultas)) {
    qv <- consultas[[tipo]]; r <- if (tipo == "periodo") rel20 else rel
    if (length(r) == 0) next
    juz <- pool$titulo[pool$query == q0]
    p <- if (tipo == "original") prep[[i]] else preparar(qv, datos)
    lst <- list("Baseline (exacto)" = busqueda_baseline(qv, datos), "Búsqueda difusa" = busqueda_difusa_simple(qv, datos),
                "Sistema híbrido" = ordenar(p)$titulos, "Modo degradado" = ordenar(p, modo = "degradado")$titulos)
    for (s in names(lst)) filas_var[[length(filas_var) + 1]] <- cbind(query = q0, tipo = tipo, consulta = qv, sistema = s,
                                                                     metricas(qv, r, lst[[s]], juz))
  }
}
variantes <- do.call(rbind, filas_var)
write.csv(variantes, file.path(salida, "variantes.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("\n== Variantes (F1 medio) ==\n")
print(as.data.frame(variantes %>% group_by(tipo, sistema) %>%
  summarise(n = n(), P = round(mean(P), 4), R = round(mean(R), 4), F1 = round(mean(F1), 4),
            cobertura = round(mean(cobertura, na.rm = TRUE), 3), .groups = "drop")), row.names = FALSE)

# ── 5. Material para el análisis de errores (O17) ───────────────────────────
f1h <- detalle$F1[detalle$sistema == "Sistema híbrido"]
peores <- unique(c(gt$query[order(f1h)][1:3], "bases de datos mineria datos"))
top_rows <- list(); fn_rows <- list()
for (q in peores) {
  i <- match(q, gt$query); p <- prep[[i]]; o <- ordenar(p, k = Inf)
  rel <- gt$lista_relevantes[[i]]; juz <- pool$titulo[pool$query == q]
  idx <- head(o$orden, 20)
  top_rows[[length(top_rows) + 1]] <- data.frame(query = q, rango = seq_along(idx), titulo = p$titulos[idx],
    relevante = vapply(p$titulos[idx], function(t) calcular_tp(t, rel) > 0, logical(1)),
    juzgado = vapply(p$titulos[idx], function(t) calcular_tp(t, juz) > 0, logical(1)),
    score = round(o$score[idx], 3), s_emb = round(p$s_emb[idx], 3), s_dif = round(p$s_dif[["0.15"]][idx], 3),
    s_sin = round(p$s_sin[idx], 3), palabras_clave = substr(as.character(p$data$AUTOR_PALABRAS_CLAVES[idx]), 1, 150))
  tit_top <- p$titulos[idx]
  for (t in rel) {
    if (calcular_tp(t, tit_top) > 0) next
    j <- which(p$titulos == t)[1]; if (is.na(j)) next
    pos <- match(j, o$orden)
    fn_rows[[length(fn_rows) + 1]] <- data.frame(query = q, titulo = t, rango = pos, score = round(o$score[j], 3),
      s_emb = round(p$s_emb[j], 3), s_dif = round(p$s_dif[["0.15"]][j], 3), s_sin = round(p$s_sin[j], 3),
      palabras_clave = substr(as.character(p$data$AUTOR_PALABRAS_CLAVES[j]), 1, 150))
  }
}
write.csv(do.call(rbind, top_rows), file.path(salida, "errores_top20.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(do.call(rbind, fn_rows), file.path(salida, "errores_fn.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("\nListo:", salida, "\n")
