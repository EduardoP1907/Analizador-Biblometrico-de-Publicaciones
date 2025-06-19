anotar_con_sjr <- function(datos, revistas) {
  
  # 1. Expandir ISSN múltiples por coma
  issn_expandido <- strsplit(as.character(revistas$issn), ",\\s*")
  n <- lengths(issn_expandido)
  
  revistas_long <- revistas[rep(seq_len(nrow(revistas)), n), ]
  revistas_long$issn <- gsub("\\s+", "", unlist(issn_expandido))
  
  # 2. Filtrar al año más reciente por ISSN
  latest_rev <- do.call(rbind, lapply(split(revistas_long, revistas_long$issn), function(df) {
    df[df$year == max(df$year, na.rm = TRUE), , drop = FALSE]
  }))
  
  # 3. Limpiar ISSN en datos
  datos$ISSN <- as.character(datos$ISSN)
  datos$ISSN <- gsub("\\s+", "", datos$ISSN)
  
  # 4. Inicializar columnas
  datos$SJR <- NA
  datos$QUARTILE <- NA
  datos$CATEGORIAS <- NA
  datos$AREAS <- NA
  datos$WOS <- "NO"
  
  # 5. Match por ISSN y asignar valores
  for (i in seq_len(nrow(datos))) {
    issn_i <- datos$ISSN[i]
    match_row <- latest_rev[latest_rev$issn == issn_i, ]
    
    if (nrow(match_row) > 0) {
      datos$SJR[i] <- match_row$sjr[1]
      datos$QUARTILE[i] <- match_row$sjr_best_quartile[1]
      datos$CATEGORIAS[i] <- gsub("\\s*\\(Q[1-4]\\)", "", match_row$categories[1])
      datos$AREAS[i] <- match_row$areas[1]
      
      # Solo marcar WOS como "SI" si el cuartil es válido
      if (!is.na(datos$QUARTILE[i]) && datos$QUARTILE[i] != "-" && datos$QUARTILE[i] != "") {
        datos$WOS[i] <- "SI"
      }
    }
  }
  
  # 6. Reemplazos de NA y "-" por "Sin clasificación"
  datos$SJR[is.na(datos$SJR)] <- 0
  datos$QUARTILE[is.na(datos$QUARTILE) | datos$QUARTILE == "-"] <- "Sin clasificación"
  datos$CATEGORIAS[is.na(datos$CATEGORIAS)] <- "Sin clasificación"
  datos$AREAS[is.na(datos$AREAS)] <- "Sin clasificación"
  
  # Si no tiene cuartil, WOS debe ser "NO"
  datos$WOS[datos$QUARTILE == "Sin clasificación"] <- "NO"
  
  # 7. Reordenar columnas
  nombres_originales <- names(datos)
  otros <- setdiff(nombres_originales, c("SJR", "QUARTILE", "CATEGORIAS", "AREAS"))
  pos_fuente <- match("FUENTE", otros)
  parte_1 <- otros[1:pos_fuente]
  parte_2 <- otros[(pos_fuente + 1):length(otros)]
  
  url_cols <- c("URL_FOTO_DEPARTAMENTO", "URL_FOTO_ACADEMICO")
  pos_urls <- match(url_cols, parte_2)
  pos_inicio_urls <- min(pos_urls[!is.na(pos_urls)], na.rm = TRUE)
  parte_2a <- parte_2[1:(pos_inicio_urls - 1)]
  parte_2b <- parte_2[pos_inicio_urls:length(parte_2)]
  
  nuevo_orden <- c(parte_1, "SJR", "QUARTILE", parte_2a, "CATEGORIAS", "AREAS", parte_2b)
  nuevo_orden <- nuevo_orden[nuevo_orden %in% names(datos)]
  
  datos <- datos[, nuevo_orden]
  
  return(datos)
}
