conf_inicial = function(input, output, session, datos_globales) {
  
  #--------------------------------------------------
  # A) Configuración general y carga de datos
  #--------------------------------------------------
  # Establecer codificación de caracteres
  options(encoding = "UTF-8")
  
  # Cargar base de datos desde archivo local (formato pipe-separado: '|')
  datos <- read.csv("www/BD/BD_papers.csv", quote = "", header = TRUE, sep = "|", stringsAsFactors = FALSE)

  #--------------------------------------------------
  # B) Limpieza y preprocesamiento inicial
  #--------------------------------------------------
  
  # Formatear nombres de autores a "Título Propio" y ordenar alfabéticamente
  datos$NOMBRE_AUTOR <- str_to_title(datos$NOMBRE_AUTOR)
  datos <- datos[order(datos$NOMBRE_AUTOR), ]
  
  # Conversión y limpieza del factor de impacto SJR
  datos$SJR[datos$SJR == "#N/A"] <- 0
  datos$SJR <- as.numeric(gsub(",", ".", datos$SJR))
  datos$SJR[is.na(datos$SJR)] <- 0
  
  #--------------------------------------------------
  # C) Filtro institucional y por sesión
  #--------------------------------------------------
  
  ano_min <- min(datos$ANO, na.rm = TRUE)
  ano_max <- max(datos$ANO, na.rm = TRUE)
  
  datos_filtrados <- datos %>% filter(FIALIACION_SESION == "SI")

  #--------------------------------------------------
  # D) Actualización dinámica de inputs en la UI
  #--------------------------------------------------
  
  universidades <- unique(datos_filtrados$UNIVERSIDAD)
  updateSelectInput(session, "SIn_Universidad", choices = universidades, selected = universidades[1])
  
  secciones <- unique(datos_filtrados$SESION)
  updateSelectInput(session, "SIn_Seccion", choices = secciones, selected = secciones[1])
  
  ano_min <- min(datos_filtrados$ANO, na.rm = TRUE)
  ano_max <- max(datos_filtrados$ANO, na.rm = TRUE)
  updateSliderInput(session, "SIn_Periodo", min = ano_min, max = ano_max, value = c(ano_max - 5, ano_max), step = 1)
  
  return(datos_filtrados)
}
