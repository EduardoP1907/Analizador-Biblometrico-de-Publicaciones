# RF-02 (filtro por autor) y RF-03 (filtro por años)

test_that("RF-03: detecta un rango de años y lo quita de la consulta", {
  f <- extraer_filtro_anos("machine learning del 2018 al 2023")
  expect_true(f$tiene)
  expect_equal(c(f$desde, f$hasta), c(2018L, 2023L))
  expect_equal(f$query_sin_anos, "machine learning")
})

test_that("RF-03: detecta un año único y 'últimos N años'", {
  f <- extraer_filtro_anos("redes neuronales en 2021")
  expect_equal(c(f$desde, f$hasta), c(2021L, 2021L))
  g <- extraer_filtro_anos("seguridad últimos 3 años")
  ano <- as.integer(format(Sys.Date(), "%Y"))
  expect_equal(c(g$desde, g$hasta), c(ano - 3L, ano))
})

test_that("RF-03: una consulta sin años no se filtra", {
  f <- extraer_filtro_anos("bioinformatica genomica")
  expect_false(f$tiene)
  expect_equal(f$query_sin_anos, "bioinformatica genomica")
})

test_that("RF-03: aplicar_filtro_anos conserva solo el rango pedido", {
  d <- corpus_prueba()
  r <- invisible(capture.output(x <- aplicar_filtro_anos(d, list(tiene = TRUE, desde = 2020, hasta = 2022))))
  expect_setequal(x$ANO, c(2021, 2022))
})

test_that("RF-02: detecta el autor y separa el tema de la consulta", {
  f <- extraer_filtro_autor("papers de Juan Pérez sobre deep learning")
  expect_true(f$tiene)
  expect_equal(f$nombre, "juan pérez")
  expect_match(f$query_sin_autor, "deep learning")
})

test_that("RF-02: un tema académico no se confunde con un autor", {
  expect_false(extraer_filtro_autor("papers de machine learning")$tiene)
  expect_false(extraer_filtro_autor("publicaciones sobre redes neuronales")$tiene)
})

test_that("RF-02: aplicar_filtro_autor encuentra al autor sin importar tildes", {
  d <- corpus_prueba()
  invisible(capture.output(x <- aplicar_filtro_autor(d, "maria gonzalez")))
  expect_equal(nrow(x), 2)
  expect_true(all(x$NOMBRE_AUTOR == "María González"))
})

test_that("RF-02/RF-03: parsear_filtros_query combina autor, años y tema", {
  invisible(capture.output(f <- parsear_filtros_query("papers de Juan Pérez sobre deep learning del 2018 al 2020")))
  expect_true(f$autor$tiene)
  expect_true(f$anos$tiene)
  expect_equal(c(f$anos$desde, f$anos$hasta), c(2018L, 2020L))
  expect_match(f$terminos_tema, "deep learning")
})
