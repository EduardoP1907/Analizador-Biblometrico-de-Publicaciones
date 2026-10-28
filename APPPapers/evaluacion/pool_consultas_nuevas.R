# Candidatos (pooling) para juzgar las consultas nuevas de la evaluación (observación O5).
#
# Uso (desde APPPapers/):  Rscript evaluacion/pool_consultas_nuevas.R <Consultas_nuevas_O5.xlsx> <salida.csv>
#
# Toma las consultas aprobadas de la planilla y, para cada una, une sin duplicados:
#   H  20 primeros del sistema híbrido (ordenados por score_semantico)
#   E  20 primeros usando solo similitud por embeddings
#   L  20 primeros del modo degradado (difusa + sinónimos)
#   B  hasta 10 resultados del baseline (muestra aleatoria si devuelve más)
#   D  hasta 10 resultados de la búsqueda difusa aislada (muestra aleatoria)
#   P  hasta 15 publicaciones cuyo título o palabras clave calzan con un patrón temático escrito
#      a partir del criterio, sin mirar resultados de ningún sistema
# La profundidad 20 coincide con el corte de la evaluación (P@20, R@20).
# El corpus es el mismo que recibe la app (cargar_datos_app.R, 3.596 registros).

args <- commandArgs(trailingOnly = TRUE)
dir_scripts <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(dir_scripts, "cargar_datos_app.R"))
suppressMessages({ library(shiny); library(stringi); library(readxl) })
datos <- cargar_datos_app()
suppressMessages(source("www/SOURCE/UTILS/nlp_chatbot_engine_semantic.R"))
source("www/SOURCE/UTILS/nlp_evaluacion_metricas.R")
invisible(capture.output(ok <- inicializar_embeddings()))
if (!isTRUE(ok)) stop("Embeddings no disponibles: definir APPPAPERS_PYTHON")
set.seed(20261028)

consultas <- read_excel(args[1], sheet = "Consultas")
consultas <- consultas[!is.na(consultas$Consulta) & tolower(consultas$`¿Aprobada?`) %in% c("si", "sí"), ]

patrones <- c(
  N01 = "lorenz map|kneading|unimodal|interval maps?|maps of the interval|mapas? de lorenz",
  N02 = "higgs",
  N03 = "disaster|emergenc|desastre|earthquake|tsunami|evacuat|crisis management",
  N04 = "inverse problem|problema inverso|parameter (estimation|identification)|identification of (the )?(coefficient|parameter)",
  N05 = "fractional|fraccionari",
  N06 = "\\bbeams?\\b|\\bplates?\\b|\\bviga|\\bplaca|timoshenko|euler-bernoulli|extensible",
  N07 = "property \\(t\\)|kazhdan|automorphism group|\\bgroups? action|teoria de grupos",
  N08 = "configuration of (points|lines)|line arrangement|arrangements? of (lines|points)|combinatorial geometry|incidence|geometria combinatoria",
  N09 = "\\beeg\\b|electroencephalog|brain (signal|activity)|electroencefalog",
  N10 = "\\bmesh|meshing|finite element|malla|elementos finitos|octree"
)

norm <- function(x) normalizar_titulo(x)
texto <- stri_trans_general(tolower(paste(datos$TITULO, datos$AUTOR_PALABRAS_CLAVES, datos$INDEX_PALABRAS_CLAVES)), "Latin-ASCII")
norm_corpus <- vapply(datos$TITULO, norm, character(1), USE.NAMES = FALSE)
limpio <- function(x) trimws(gsub("[\r\n|]+", " ", x))

top_por <- function(q, pesos, usar_emb) {
  invisible(capture.output({
    f <- parsear_filtros_query(q); qt <- if (nchar(trimws(f$terminos_tema)) >= 2) f$terminos_tema else q
    e <- expandir_consulta_semantica(qt)
    sims <- if (usar_emb) similitudes_embeddings(qt, datos) else NULL
  }))
  PESO_EMBEDDINGS <<- pesos[1]; PESO_DIFUSA <<- pesos[2]; PESO_SINONIMOS <<- pesos[3]
  s <- calcular_scores_hibridos(qt, datos, e, sims)$score_semantico
  PESO_EMBEDDINGS <<- 0.60; PESO_DIFUSA <<- 0.25; PESO_SINONIMOS <<- 0.15
  datos$TITULO[order(s, decreasing = TRUE)]
}
muestra <- function(v, n) { v <- unique(v); if (length(v) > n) sample(v, n) else v }
primeros_unicos <- function(v, n) { v <- head(v, 300); head(v[!duplicated(vapply(v, norm, character(1)))], n) }

filas <- list()
for (i in seq_len(nrow(consultas))) {
  id <- consultas$ID[i]; q <- consultas$Consulta[i]
  fuentes <- list(
    H = primeros_unicos(top_por(q, c(.60, .25, .15), TRUE), 20),
    E = primeros_unicos(top_por(q, c(1, 0, 0), TRUE), 20),
    L = primeros_unicos(top_por(q, c(.60, .25, .15), FALSE), 20),
    B = muestra(busqueda_baseline(q, datos), 10),
    D = muestra(busqueda_difusa_simple(q, datos), 10),
    P = muestra(datos$TITULO[stri_detect_regex(texto, patrones[[id]])], 15)
  )
  todos <- unlist(fuentes)
  claves <- vapply(todos, norm, character(1))
  n0 <- length(filas)
  for (k in unique(claves)) {
    j <- match(k, norm_corpus)
    fu <- paste(names(fuentes)[vapply(fuentes, function(v) k %in% vapply(v, norm, character(1)), logical(1))], collapse = "")
    filas[[length(filas) + 1]] <- data.frame(
      id_consulta = id, query = q, criterio = consultas$`Criterio de relevancia`[i],
      titulo = limpio(todos[match(k, claves)]), fuentes = fu, ano = datos$ANO[j],
      palabras_clave = limpio(substr(paste(datos$AUTOR_PALABRAS_CLAVES[j], datos$INDEX_PALABRAS_CLAVES[j], sep = "; "), 1, 400)),
      resumen = limpio(substr(datos$RESUMEN[j], 1, 900)), stringsAsFactors = FALSE)
  }
  cat(sprintf("[%s] %-62s candidatos: %d\n", id, q, length(filas) - n0))
}
pool <- do.call(rbind, filas)
write.table(pool, args[2], sep = "|", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
cat("Total candidatos:", nrow(pool), "\n")
