library(tidyverse)
library(janitor)
library(readxl)
library(openxlsx)

# Función para obtener URL de revistas
journal_url <- function(year) {
  paste0("https://www.scimagojr.com/journalrank.php?year=", year, "&out=xls")
}

# Año límite
last_year <- 2024
years_j <- 1999:last_year
df_jr <- list()

for (i in seq_along(years_j)) {
  dfi <- suppressMessages(suppressWarnings(
    read_csv2(url(journal_url(years_j[i])))
  )) %>% clean_names()
  
  colnames(dfi)[9] <- colnames(dfi)[9] %>% str_replace("[0-9]+", "year")
  
  # Homogeneiza columnas: convierte percent_female si existe
  if ("percent_female" %in% names(dfi)) {
    dfi <- dfi %>%
      mutate(percent_female = str_replace_all(percent_female, "%", ""),
             percent_female = as.numeric(percent_female))
  }
  
  df_jr[[i]] <- dfi
  names(df_jr)[i] <- years_j[i]
}

df_jr <- df_jr %>% bind_rows(.id = "year")

# Guarda en Excel
wb <- createWorkbook()
addWorksheet(wb, "Revistas_1999_2024")
writeData(wb, "Revistas_1999_2024", df_jr)
saveWorkbook(wb, "SJR_revistas_1999_2024.xlsx", overwrite = TRUE)
