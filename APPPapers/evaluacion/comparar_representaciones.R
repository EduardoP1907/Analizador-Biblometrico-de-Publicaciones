# Compara dos salidas de evaluacion_revision.R obtenidas con distintas representaciones de
# los embeddings (O9): la vigente, por pasajes, y la anterior, truncada en 128 tokens.
# Para obtener la anterior se ejecuta evaluacion_revision.R con el paper_embeddings.npy
# antiguo en www/BD (respaldar antes el vigente).
#
# Uso:  Rscript comparar_representaciones.R <dir vigente> <dir anterior> <salida.csv>
args <- commandArgs(trailingOnly = TRUE)
n <- read.csv(file.path(args[1], "detalle_sistemas.csv"))
o <- read.csv(file.path(args[2], "detalle_sistemas.csv"))

set.seed(2026)
comparar <- function(a, b, sistema, metrica) {
  d <- a - b
  m <- replicate(10000, mean(sample(d, replace = TRUE)))
  data.frame(sistema = sistema, metrica = metrica, vigente = round(mean(a), 4), anterior = round(mean(b), 4),
             diferencia = round(mean(d), 4), ic_inf = round(unname(quantile(m, .025)), 4),
             ic_sup = round(unname(quantile(m, .975)), 4),
             p_wilcoxon = signif(suppressWarnings(wilcox.test(a, b, paired = TRUE, exact = FALSE)$p.value), 3),
             gana = sum(d > 0), empata = sum(d == 0), pierde = sum(d < 0))
}

filas <- list()
for (s in c("Sistema híbrido", "Solo embeddings")) for (mt in c("P", "R", "F1")) {
  a <- n[n$sistema == s, ]
  b <- o[o$sistema == s, ][match(a$query, o$query[o$sistema == s]), ]
  filas[[length(filas) + 1]] <- comparar(a[[mt]], b[[mt]], s, mt)
}
res <- do.call(rbind, filas)
write.csv(res, args[3], row.names = FALSE, fileEncoding = "UTF-8")
print(res)
