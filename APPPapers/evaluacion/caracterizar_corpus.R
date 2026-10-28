# Caracterización del corpus (observación O16 y error de forma F-04).
# Uso:  Rscript caracterizar_corpus.R <dir APPPapers>
args <- commandArgs(trailingOnly = TRUE)
setwd(args[1])
dir_scripts <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(dir_scripts, "cargar_datos_app.R"))
suppressMessages(library(stringi))

todo <- read.csv("www/BD/BD_papers.csv", quote = "", header = TRUE, sep = "|", stringsAsFactors = FALSE)
app  <- cargar_datos_app()
vacio <- function(x) is.na(x) | trimws(x) %in% c("", "NA", "nan", "[No abstract available]")
clave <- function(d) {
  doi <- tolower(trimws(d$DOI)); tit <- stri_replace_all_regex(stri_trans_general(tolower(d$TITULO), "Latin-ASCII"), "[^a-z0-9]+", " ")
  ifelse(vacio(doi), paste0("t:", trimws(tit)), paste0("d:", doi))
}

cat("== CSV completo ==\n")
cat("Registros:", nrow(todo), "\n")
print(table(todo$UNIVERSIDAD, todo$FIALIACION_SESION))
cat("\nPor universidad y sesión (todo el CSV):\n")
print(as.data.frame(table(paste(todo$UNIVERSIDAD, "|", todo$SESION))))

cat("\n== Corpus de la app (FIALIACION_SESION == 'SI') ==\n")
k <- clave(app)
cat("Registros:", nrow(app), "| publicaciones distintas (DOI o título):", length(unique(k)),
    "| títulos distintos:", length(unique(app$TITULO)), "| académicos:", length(unique(app$NOMBRE_AUTOR)), "\n")
pub <- app[!duplicated(k), ]
n <- nrow(pub)
pct <- function(x) sprintf("%d (%.1f %%)", sum(x), 100 * mean(x))
cat("Sobre", n, "publicaciones distintas:\n")
cat("  con resumen:", pct(!vacio(pub$RESUMEN)), "\n")
cat("  con palabras clave de autor:", pct(!vacio(pub$AUTOR_PALABRAS_CLAVES)), "\n")
cat("  con palabras clave de índice:", pct(!vacio(pub$INDEX_PALABRAS_CLAVES)), "\n")
cat("  con alguna palabra clave:", pct(!vacio(pub$AUTOR_PALABRAS_CLAVES) | !vacio(pub$INDEX_PALABRAS_CLAVES)), "\n")
cat("\nIdioma (campo IDIOMA):\n"); print(sort(table(pub$IDIOMA, useNA = "ifany"), decreasing = TRUE))
cat("\nTítulo con traducción entre corchetes (resumen original en otro idioma):", pct(grepl("\\[.*\\]", pub$TITULO)), "\n")
cat("\nUniversidad:\n"); print(table(pub$UNIVERSIDAD))
cat("\nTipo de documento:\n"); print(sort(table(pub$TIPO_DOCUMENTO), decreasing = TRUE))
ano <- suppressWarnings(as.integer(pub$ANO))
cat("\nAños:", min(ano, na.rm = TRUE), "-", max(ano, na.rm = TRUE), "\n")
print(table(cut(ano, c(-Inf, 1999, 2004, 2009, 2014, 2019, Inf),
                labels = c("<=1999", "2000-2004", "2005-2009", "2010-2014", "2015-2019", ">=2020"))))
