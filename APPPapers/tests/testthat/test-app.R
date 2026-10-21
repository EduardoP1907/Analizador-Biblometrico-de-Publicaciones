# Prueba funcional de la app con una sesión simulada (shiny::testServer):
# RF-01 (consulta en el chat), RF-09 (pestaña Evaluación) y RF-10 (descarga de la conversación)

test_that("RF-01/RF-09/RF-10: la app responde una consulta, evalúa y exporta la conversación", {
  skip_if_not(file.exists(file.path(RAIZ_APP, "www/BD/BD_papers.csv")), "No está BD_papers.csv")
  dir_anterior <- setwd(RAIZ_APP)
  on.exit(setwd(dir_anterior))

  # ui.R carga los paquetes de la interfaz; los spinners modales requieren un navegador
  invisible(capture.output(suppressWarnings(suppressMessages(source("ui.R", encoding = "UTF-8")))))
  for (f in c("show_modal_spinner", "remove_modal_spinner")) {
    assign(f, function(...) invisible(NULL), envir = globalenv())
  }

  invisible(capture.output(suppressMessages(shiny::testServer(".", {
    session$setInputs(`buscador-query` = "redes neuronales para clasificación de imágenes")
    session$setInputs(`buscador-go` = 1)
    archivo <- output$`descargar-chat`
    html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_match(html, "redes neuronales para clasificación de imágenes", fixed = TRUE)
    expect_false(grepl("No hay conversaciones para exportar", html, fixed = TRUE))

    session$setInputs(btn_evaluar = 1)
  }))))
  succeed()
})
