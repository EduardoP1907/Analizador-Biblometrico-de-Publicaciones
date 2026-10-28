# RF-04 (sinónimos), RF-05 (búsqueda difusa), RF-07 (fusión, umbral y top-k) y RNF-01 (modo degradado)

test_that("RF-04: la expansión agrega sinónimos del diccionario (máximo 8)", {
  e <- expandir_consulta_semantica("aprendizaje automático")
  expect_gt(length(e$terminos_adicionales), 0)
  expect_lte(length(e$terminos_adicionales), 8)
  expect_true("machine learning" %in% e$terminos_adicionales)
})

test_that("RF-04: una consulta vacía no se expande", {
  expect_length(expandir_consulta_semantica("")$terminos_adicionales, 0)
})

test_that("RF-05: la búsqueda difusa tolera errores de tipeo", {
  d <- corpus_prueba()
  exacto <- score_difuso("genetic knapsack", d)
  con_error <- score_difuso("genetik knapsak", d)
  expect_equal(exacto[3], 1)
  expect_equal(con_error[3], 1)
  expect_equal(con_error[4], 0)
})

test_that("RF-05: s_dif es la fracción de términos encontrados", {
  d <- corpus_prueba()
  s <- score_difuso("genetic programming", d)
  expect_equal(s[3], 0.5)   # 'genetic' sí, 'programming' no
  expect_equal(s[4], 0.5)   # 'programming' sí, 'genetic' no
})

test_that("RF-05: los términos de 3 caracteres o menos se ignoran", {
  expect_equal(tokens_consulta_difusa("ML en la web"), character(0))
})

test_that("RF-04: s_sin cuenta sinónimos como palabra completa y satura en 1", {
  d <- corpus_prueba()
  s <- score_sinonimos(list(terminos_adicionales = c("neural networks", "deep learning", "image classification", "html")), d)
  expect_equal(s[1], 1)
  expect_equal(s[3], 0)
  expect_true(all(s >= 0 & s <= 1))
})

test_that("RF-07: la fusión aplica los pesos 0,60 / 0,25 / 0,15", {
  d <- corpus_prueba()
  e <- list(terminos_adicionales = character(0))
  sims <- c(0.5, 0.5, 0.1, -0.2, 0.3)
  s <- calcular_scores_hibridos("genetic knapsack", d, e, sims)
  esperado <- 0.60 * pmax(0, sims) + 0.25 * s$score_difuso + 0.15 * s$score_sinonimos
  expect_equal(s$score_semantico, esperado)
  expect_equal(s$score_embeddings[4], 0)   # similitudes negativas llevadas a 0
})

test_that("RNF-01: sin embeddings el puntaje se reescala a [0, 1]", {
  d <- corpus_prueba()
  e <- list(terminos_adicionales = character(0))
  s <- calcular_scores_hibridos("genetic knapsack", d, e, NULL)
  expect_true(all(is.na(s$score_embeddings)))
  expect_equal(s$score_semantico, (0.25 * s$score_difuso + 0.15 * s$score_sinonimos) / 0.40)
  expect_true(all(s$score_semantico >= 0 & s$score_semantico <= 1))
})

test_that("RF-07/RNF-01: buscar_papers_hibrido filtra por umbral, ordena y no repite títulos", {
  d <- corpus_prueba()
  sin_embeddings({
    e <- expandir_consulta_semantica("deep learning image classification")
    invisible(capture.output(r <- buscar_papers_hibrido("deep learning image classification", d, e)))
  })
  expect_gt(nrow(r), 0)
  expect_true(all(r$score_semantico >= UMBRAL_HIBRIDO))
  expect_false(is.unsorted(rev(r$score_semantico)))
  expect_equal(sum(r$TITULO == "Deep learning for image classification"), 1)
  expect_equal(r$TITULO[1], "Deep learning for image classification")
})

test_that("RF-07: buscar_papers_hibrido respeta el máximo de resultados", {
  d <- corpus_prueba()
  d <- d[rep(seq_len(nrow(d)), 10), ]
  d$TITULO <- paste(d$TITULO, seq_len(nrow(d)))
  sin_embeddings({
    invisible(capture.output(r <- buscar_papers_hibrido("neural networks", d, expandir_consulta_semantica("neural networks"), top_k = 7)))
  })
  expect_lte(nrow(r), 7)
})

test_that("RF-07: una consulta sin coincidencias devuelve un data.frame vacío", {
  sin_embeddings({
    invisible(capture.output(r <- buscar_papers_hibrido("astrofisica galaxias", corpus_prueba(), expandir_consulta_semantica("astrofisica galaxias"))))
  })
  expect_equal(nrow(r), 0)
})

test_that("RF-04: una sigla se expande y no coincide dentro de otra palabra", {
  e <- expandir_consulta_semantica("ml")
  expect_true("machine learning" %in% names(e$sinonimos_encontrados))
  expect_false(detectar_termino_en_consulta("blockchain consensus", "ai"))
  expect_false(detectar_termino_en_consulta("html parsing", "ml"))
})
