#=======================================
# Temas para las consultas nuevas de la evaluación (observación O5)
# Toma publicaciones al azar del corpus de la app (semilla fija) y muestra su
# título, palabras clave y área. Cada consulta nueva se redacta a partir del
# tema de una publicación sorteada, sin ejecutar ningún sistema de búsqueda,
# para no elegir temas en que el sistema ya se sabe que funciona bien.
# Uso (desde APPPapers/):  Rscript evaluacion/temas_consultas_nuevas.R [n]
#=======================================
source("evaluacion/cargar_datos_app.R")

args <- commandArgs(trailingOnly = TRUE)
n <- if (length(args) > 0) as.integer(args[1]) else 40

datos <- cargar_datos_app()
datos <- datos[!duplicated(tolower(trimws(datos$TITULO))), ]

set.seed(20261027)
muestra <- datos[sample(nrow(datos), n), ]

for (i in seq_len(nrow(muestra))) {
  p <- muestra[i, ]
  cat(sprintf("[%02d] %s (%s)\n     Claves: %s\n     Área: %s\n\n", i, p$TITULO, p$ANO,
              substr(p$AUTOR_PALABRAS_CLAVES, 1, 160), p$AREAS))
}
