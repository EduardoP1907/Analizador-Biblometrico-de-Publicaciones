# Carga el corpus exactamente como lo recibe el módulo de búsqueda en la app
# (server_conf_inicial.R): 3.596 registros con FIALIACION_SESION == "SI".
cargar_datos_app <- function() {
  suppressMessages({ library(stringr); library(dplyr) })
  datos <- read.csv("www/BD/BD_papers.csv", quote = "", header = TRUE, sep = "|", stringsAsFactors = FALSE)
  datos$NOMBRE_AUTOR <- str_to_title(datos$NOMBRE_AUTOR)
  datos <- datos[order(datos$NOMBRE_AUTOR), ]
  datos$SJR[datos$SJR == "#N/A"] <- 0
  datos$SJR <- as.numeric(gsub(",", ".", datos$SJR))
  datos$SJR[is.na(datos$SJR)] <- 0
  datos %>% filter(FIALIACION_SESION == "SI")
}
