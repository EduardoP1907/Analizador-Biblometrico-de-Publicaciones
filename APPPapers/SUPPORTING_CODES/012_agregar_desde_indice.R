#' Agrega datos del índice de académicos al dataset bibliométrico
#'
#' @param datos Data.frame bibliométrico ya limpio.
#' @param archivo_actual Nombre del archivo original (debe contener SCOPUS ID).
#' @param indice Data.frame del índice de académicos.
#'
#' @return Data.frame con columnas agregadas y reordenadas.
agregar_desde_indice <- function(datos, archivo_actual, indice) {
  
  # 1. Extraer SCOPUS ID desde el nombre del archivo
  scopus_id_actual <- gsub(".* - ([0-9]+)\\.csv", "\\1", archivo_actual)
  
  # 2. Buscar la fila correspondiente en el índice
  fila <- indice[indice$SCOPUS.ID == scopus_id_actual, ]
  
  if (nrow(fila) == 0) {
    stop(paste("SCOPUS ID", scopus_id_actual, "no encontrado en el índice."))
  }
  
  # 3. Agregar columnas nuevas al data.frame
  datos$UNIVERSIDAD <- fila$UNIVERSIDAD
  datos$SESION <- fila$SESION
  datos$NOMBRE_AUTOR <- fila$NOMBRE_AUTOR
  datos$SCOPUS_ID <- fila$SCOPUS.ID
  datos$FIALIACION_SESION <- "SI-NO"
  datos$WOS <- "AGREGAR"
  datos$URL_FOTO_ACADEMICO <- fila$URL_FOTO_ACADEMICO
  datos$URL_FOTO_DEPARTAMENTO <- fila$URL_FOTO_DEPARTAMENTO
  
  # 4. Reordenar columnas:
  columnas_inicio <- c(
    "UNIVERSIDAD", "SESION", "NOMBRE_AUTOR", "SCOPUS_ID",
    "FIALIACION_SESION", "WOS"
  )
  
  columnas_final <- c("URL_FOTO_ACADEMICO", "URL_FOTO_DEPARTAMENTO")
  
  columnas_centro <- setdiff(names(datos), c(columnas_inicio, columnas_final))
  
  datos <- datos[, c(columnas_inicio, columnas_centro, columnas_final)]
  
  return(datos)
}
