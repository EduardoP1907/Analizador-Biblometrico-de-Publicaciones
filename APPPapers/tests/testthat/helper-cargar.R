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

#' Corpus mínimo con la estructura de BD_papers.csv
corpus_prueba <- function() {
  data.frame(
    TITULO = c("Deep learning for image classification",
               "Deep learning for image classification",
               "Genetic algorithms for the knapsack problem",
               "Teaching programming to first-year students",
               "Sentiment analysis of tweets with neural networks"),
    RESUMEN = c(
      "We propose a convolutional neural network for image classification. The network is trained on a large dataset. Results show high accuracy on benchmark data.",
      "Duplicated record of the same publication about convolutional neural networks for images.",
      "This paper presents a genetic algorithm for the knapsack problem. Crossover and mutation operators are compared. The algorithm finds near-optimal solutions.",
      "We report an experience teaching programming in a CS1 course. Students improved their performance. Active learning was used in every lecture.",
      "Tweets are classified by sentiment using recurrent neural networks. Natural language processing techniques are applied. The model outperforms lexicon baselines."),
    AUTOR_PALABRAS_CLAVES = c("deep learning; image classification", "deep learning",
                              "genetic algorithms; knapsack", "programming education; CS1",
                              "sentiment analysis; neural networks"),
    INDEX_PALABRAS_CLAVES = c("neural networks", "neural networks", "optimization",
                              "education", "natural language processing"),
    NOMBRE_AUTOR = c("Juan Pérez", "Juan Pérez", "María González", "Pedro Soto", "María González"),
    ANO = c(2019, 2019, 2015, 2022, 2021),
    SJR = 1, CITADO_POR = 0, LINK = "",
    stringsAsFactors = FALSE
  )
}

#' Desactiva los embeddings durante una prueba (modo degradado)
sin_embeddings <- function(codigo) {
  estado <- .emb$listo
  .emb$listo <- FALSE
  on.exit(.emb$listo <- estado)
  force(codigo)
}
