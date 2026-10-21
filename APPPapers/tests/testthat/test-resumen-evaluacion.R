# RF-08 (resumen extractivo LexRank) y RF-09 (evaluación con precisión, recall y F1)

test_that("RF-08: LexRank devuelve oraciones tomadas de los abstracts", {
  skip_if_not_installed("lexRankr")
  d <- corpus_prueba()
  o <- resumen_lexrank(d, n_papers = 5, n_oraciones = 3)
  expect_gt(length(o), 0)
  expect_lte(length(o), 3)
  todo <- paste(d$RESUMEN, collapse = " ")
  expect_true(all(vapply(o, function(x) grepl(x, todo, fixed = TRUE), logical(1))))
})

test_that("RF-08: sin abstracts no se genera resumen", {
  d <- corpus_prueba()
  d$RESUMEN <- ""
  expect_length(resumen_lexrank(d), 0)
  expect_equal(crear_seccion_lexrank_html(character(0)), "")
})

test_that("RF-09: precisión, recall y F1 con un caso calculado a mano", {
  gt  <- c("Paper A", "Paper B", "Paper C", "Paper D")
  rec <- c("Paper A", "Paper B", "Paper X")
  r <- evaluar_query("q", gt, rec)
  expect_equal(r$tp, 2L)
  expect_equal(r$precision, round(2 / 3, 4))
  expect_equal(r$recall, 0.5)
  expect_equal(r$f1, round(2 * (2/3) * 0.5 / (2/3 + 0.5), 4))
})

test_that("RF-09: TP nunca supera a los recuperados ni a los relevantes", {
  gt <- c("Fuzzy search for scientific papers in Spanish",
          "Fuzzy search for scientific papers in English")
  r <- evaluar_query("q", gt, c("Fuzzy search for scientific papers in Spanish"))
  expect_equal(r$tp, 1L)
  expect_lte(r$precision, 1)
})

test_that("RF-09: tolera diferencias menores en títulos largos", {
  gt <- "A hybrid retrieval system for institutional repositories using embeddings"
  expect_equal(calcular_tp("A hybrid retrieval system for institutional repository using embeddings", gt), 1L)
})

test_that("RF-09: un título repetido cuenta una sola vez como recuperado", {
  r <- evaluar_query("q", c("Paper A"), c("Paper A", "paper a.", "Paper Z"))
  expect_equal(r$n_recuperados, 2L)
  expect_equal(r$precision, 0.5)
})

test_that("RF-09: los títulos se comparan normalizados", {
  expect_true(titulo_coincide("Deep Learning: A Survey", c("deep learning a survey")))
  expect_false(titulo_coincide("Genetic algorithms", c("Neural networks for images")))
})

test_that("RF-09: sin resultados o sin relevantes las métricas son 0", {
  expect_equal(evaluar_query("q", c("Paper A"), character(0))$f1, 0)
  expect_equal(calcular_f1(0, 0), 0)
})

test_that("RF-09: el baseline solo encuentra coincidencias exactas", {
  d <- corpus_prueba()
  expect_true("Genetic algorithms for the knapsack problem" %in% busqueda_baseline("knapsack", d))
  expect_length(busqueda_baseline("knapsak", d), 0)
})
