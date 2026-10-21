# Ejecutar desde la raíz de APPPapers:  Rscript tests/testthat.R
library(testthat)
test_dir("tests/testthat", stop_on_failure = TRUE)
