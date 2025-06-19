
##################################
### LIMPIAR COLUMNAS y RENOMBRAR
##################################
limpiar_columnas_y_renombrar <- function(datos) {
  
  #' Limpia un data.frame bibliométrico exportado desde Scopus.
  #'
  #' Esta función elimina columnas irrelevantes típicas del export Scopus
  #' y renombra las columnas útiles con nombres estandarizados en español.
  #'
  #' @param datos Un data.frame leído desde un archivo CSV de Scopus.
  #' @return Un data.frame reducido y con nombres de columnas renombrados.
  #' 
  # Definir las columnas innecesarias que se eliminarán
  
  columnas_a_eliminar <- c(
    "Author.full.names", "Issue", "Art..No.", "Page.count",
    "Molecular.Sequence.Numbers", "Chemicals.CAS", "Tradenames",
    "Manufacturers", "Funding.Details", "Funding.Texts", "References",
    "Correspondence.Address", "Editors", "Publisher", "Sponsors",
    "Conference.name", "Conference.date", "Conference.location",
    "Conference.code", "ISBN", "CODEN", "PubMed.ID",
    "Abbreviated.Source.Title", "Publication.Stage", "Source",
    "Open.Access", "EID"
  )
  
  # Eliminar las columnas listadas si existen en el dataset
  datos <- datos[ , !(names(datos) %in% columnas_a_eliminar)]
  
  # Renombrar las columnas restantes con nombres claros y consistentes
  names(datos) <- c(
    "AUTORES", "AUTORES_ID", "TITULO", "ANO", "FUENTE", "VOLUMEN", 
    "PAG_COMIENZO", "PAG_FINAL", "CITADO_POR", "DOI", "LINK", 
    "AFILIACIÓN_AUTOR", "AFILIACIÓN_AUTORES", "RESUMEN", 
    "AUTOR_PALABRAS_CLAVES", "INDEX_PALABRAS_CLAVES", "ISSN", 
    "IDIOMA", "TIPO_DOCUMENTO"
  )
  
  # Retornar el data.frame limpio y renombrado con modificaciones de variables y estandarización
  
  datos$VOLUMEN=as.character(datos$VOLUMEN)
  datos$PAG_COMIENZO=as.character(datos$PAG_COMIENZO)
  datos$PAG_FINAL=as.character(datos$PAG_FINAL)

  return(datos)
}