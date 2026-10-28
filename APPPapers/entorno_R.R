#=======================================
# Versiones de los paquetes de R de APPPapers
# Uso:  Rscript entorno_R.R          → escribe paquetes_R.txt
#       Rscript entorno_R.R instalar → instala los paquetes que falten
#=======================================

PAQUETES <- c(
  # Aplicación (www/SOURCE/UTILS/cargar_bibliotecas.R)
  "dplyr", "DT", "ggforce", "ggplot2", "ggsankey", "ggthemes", "ggwordcloud",
  "hrbrthemes", "htmlwidgets", "igraph", "networkD3", "plotly", "pracma",
  "randomForest", "readr", "RColorBrewer", "shiny", "shinyBS", "shinybusy",
  "shinyalert", "shinyjs", "shinyWidgets", "stringr", "tibble", "tidyr",
  "tidyverse", "tm", "threejs", "visNetwork", "viridis", "wordcloud",
  "wordcloud2", "writexl", "shinyFeedback", "fontawesome", "stringi",
  "stringdist", "textrank", "markdown", "reticulate", "httr", "jsonlite",
  # Módulo de PLN
  "lexRankr", "shinycssloaders", "htmltools",
  # Pruebas
  "testthat", "covr"
)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0 && args[1] == "instalar") {
  faltan <- PAQUETES[!vapply(PAQUETES, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltan) > 0) install.packages(faltan)
}

version_de <- function(p) tryCatch(as.character(packageVersion(p)), error = function(e) "no instalado")
lineas <- c(paste0("# ", R.version.string, " (", R.version$platform, ")"),
            sprintf("%s==%s", PAQUETES, vapply(PAQUETES, version_de, character(1))))
writeLines(lineas, "paquetes_R.txt")
cat(lineas, sep = "\n")
