clasificar_filiacion <- function(datos, lista_academicos) {
  
  # Normalizar columnas clave
  datos$SCOPUS_ID <- trimws(as.character(datos$SCOPUS_ID))
  lista_academicos$SCOPUS.ID <- trimws(as.character(lista_academicos$SCOPUS.ID))
  
  datos$ANO <- trimws(as.character(datos$ANO))
  lista_academicos$ANOS <- trimws(as.character(lista_academicos$ANOS))
  
  # Inicializar columna como "NO"
  datos$FIALIACION_SESION <- "NO"
  
  for (i in seq_len(nrow(datos))) {
    scopus_id <- datos$SCOPUS_ID[i]
    anio_pub <- datos$ANO[i]
    
    fila <- lista_academicos[lista_academicos$SCOPUS.ID == scopus_id, ]
    
    if (nrow(fila) > 0) {
      anios_discretos <- unlist(strsplit(gsub("–", "-", fila$ANOS[1]), "-"))
      anios_discretos <- trimws(anios_discretos)
      
      if (anio_pub %in% anios_discretos) {
        datos$FIALIACION_SESION[i] <- "SI"
      }
    }
  }
  
  return(datos)
}
