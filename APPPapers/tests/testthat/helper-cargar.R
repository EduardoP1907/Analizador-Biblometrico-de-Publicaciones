#=======================================
# Carga de los módulos de PLN para las pruebas
# Los source() de la app usan rutas relativas a la raíz de APPPapers
#=======================================

suppressMessages({ library(dplyr); library(stringi) })

RAIZ_APP <- normalizePath(file.path(testthat::test_path(), "..", ".."))

local({
  dir_anterior <- setwd(RAIZ_APP)
  on.exit(setwd(dir_anterior))
  invisible(capture.output(suppressMessages({
    source("www/SOURCE/UTILS/nlp_chatbot_engine_semantic.R", local = globalenv())
    source("www/SOURCE/UTILS/nlp_evaluacion_metricas.R", local = globalenv())
  })))
})
