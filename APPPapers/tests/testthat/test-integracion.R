# Pruebas de integración sobre el corpus real (RF-06, RNF-02 y RNF-03).
# Requieren www/BD/BD_papers.csv y, para los embeddings, el Python con
# sentence-transformers (variable APPPAPERS_PYTHON). Si faltan, se omiten.

cargar_corpus_real <- function() {
  ruta <- file.path(RAIZ_APP, "www/BD/BD_papers.csv")
  skip_if_not(file.exists(ruta), "No está BD_papers.csv")
  d <- read.csv(ruta, quote = "", header = TRUE, sep = "|", stringsAsFactors = FALSE)
  d[d$FIALIACION_SESION == "SI", ]
}

activar_embeddings <- function() {
  dir_anterior <- setwd(RAIZ_APP)
  on.exit(setwd(dir_anterior))
  ok <- tryCatch({ invisible(capture.output(x <- inicializar_embeddings())); isTRUE(x) },
                 error = function(e) FALSE)
  skip_if_not(ok, "Embeddings no disponibles (Python/sentence-transformers)")
}

test_that("Corpus: la app trabaja con las 3.596 publicaciones afiliadas", {
  expect_equal(nrow(cargar_corpus_real()), 3596)
})

test_that("RF-06: los embeddings dan una similitud por publicación en [-1, 1]", {
  d <- cargar_corpus_real()
  activar_embeddings()
  invisible(capture.output(s <- similitudes_embeddings("redes neuronales", d)))
  expect_length(s, nrow(d))
  expect_true(all(s[!is.na(s)] >= -1 - 1e-6 & s[!is.na(s)] <= 1 + 1e-6))
  expect_gt(mean(!is.na(s)), 0.95)
})

test_that("RNF-02: una consulta en español recupera publicaciones escritas en inglés", {
  d <- cargar_corpus_real()
  activar_embeddings()
  q <- "algoritmos genéticos para optimización"
  invisible(capture.output(r <- buscar_papers_hibrido(q, d, expandir_consulta_semantica(q))))
  expect_gt(nrow(r), 0)
  expect_true(any(grepl("genetic|evolutionary", r$TITULO, ignore.case = TRUE)))
})

test_that("RNF-03: una búsqueda híbrida sobre el corpus completo tarda menos de 5 segundos", {
  d <- cargar_corpus_real()
  activar_embeddings()
  q <- "procesamiento de lenguaje natural"
  e <- expandir_consulta_semantica(q)
  t <- system.time(invisible(capture.output(buscar_papers_hibrido(q, d, e))))[["elapsed"]]
  message(sprintf("tiempo de búsqueda: %.2f s", t))
  expect_lt(t, 5)
})

test_that("RNF-01: en modo degradado la búsqueda sobre el corpus real sigue respondiendo", {
  d <- cargar_corpus_real()
  q <- "redes neuronales deep learning"
  sin_embeddings(invisible(capture.output(r <- buscar_papers_hibrido(q, d, expandir_consulta_semantica(q)))))
  expect_gt(nrow(r), 0)
  expect_lte(nrow(r), TOP_K_HIBRIDO)
})
