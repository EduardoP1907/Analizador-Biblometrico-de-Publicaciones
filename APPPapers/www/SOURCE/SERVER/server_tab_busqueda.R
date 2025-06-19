library(shiny)
library(stringi)
library(lexRankr)

server_tab_busqueda <- function(input, output, session, data) {
  rv <- reactiveValues(history = list())
  
  observeEvent(input$`buscador-go`, {
    req(q <- input$`buscador-query`)
    q_clean <- tolower(stri_trans_general(q, "Latin-ASCII"))
    
    # Columnas
    title_col   <- "TITULO"
    keyword_col <- "AUTOR_PALABRAS_CLAVES"
    summary_col <- "RESUMEN"
    link_col    <- "LINK"
    
    # Prepara corpus (solo para búsqueda, no para resumen)
    corpus_texts <- paste(
      data[[title_col]], data[[keyword_col]], data[[summary_col]], sep = " "
    )
    corpus_clean <- tolower(stri_trans_general(corpus_texts, "Latin-ASCII"))
    
    # ---- Nueva lógica de búsqueda ----
    # 1) Tokeniza el input y filtra "stopwords" comunes (puedes expandir esta lista)
    stopwords <- c("de", "el", "la", "los", "las", "en", "por", "con", "y", "o", "del", "para", "a", "un", "una", "que", "sobre", "al")
    words <- unique(unlist(stri_split_regex(q_clean, "\\s+")))
    words <- words[nchar(words) > 2 & !(words %in% stopwords)]
    
    # 2) Para cada documento, cuenta cuántas palabras clave aparecen (por regex exacta o difusa)
    hits_idx <- which(
      sapply(corpus_clean, function(txt) {
        sum(sapply(words, function(w) grepl(w, txt, ignore.case=TRUE))) >= ceiling(0.6 * length(words))
      })
    )
    # Fallback si no hay coincidencias: prueba con "OR" (al menos una palabra)
    if (length(hits_idx) == 0 && length(words) > 0) {
      hits_idx <- which(
        sapply(corpus_clean, function(txt) {
          sum(sapply(words, function(w) grepl(w, txt, ignore.case=TRUE))) >= 1
        })
      )
    }
    
    matched <- data[hits_idx, , drop=FALSE]
    n <- nrow(matched)
    
    if (n > 0) {
      # 2) Sample aleatorio de hits para el resumen
      matched <- matched[!duplicated(matched[[summary_col]]), , drop=FALSE]
      n <- nrow(matched)
      summary_n <- min(3, n)           # número de hits que quiero usar en el resumen
      idx_samp <- sample(seq_len(n), size = min(n, summary_n))
      matched_s <- matched[idx_samp, , drop=FALSE]
      
      # 3) Concatenar y resumir extractivamente con lexRankr
      textos_samp <- paste(matched_s[[summary_col]], collapse="\n\n")
      lr <- lexRank(
        text            = textos_samp,
        docId           = "create",
        n               = 5,
        continuous      = TRUE,
        sentencesAsDocs = TRUE
      )
      summary_en <- paste(unique(lr$sentence), collapse=" ")
      
      # 4) Traducir offline con Apertium (WSL)
      raw_trad <- system2(
        "wsl",
        args   = c("apertium", "-u", "en-es"),
        input  = summary_en,
        stdout = TRUE, stderr = TRUE
      )
      summary_txt <- stri_trim_both(paste(raw_trad, collapse=" "))
      
      # 5) Construir UI de enlaces con **todos** los hits
      links_ui <- lapply(seq_len(n), function(i) {
        tags$div(
          tags$a(
            href   = matched[[link_col]][i],
            target = "_blank",
            matched[[title_col]][i]
          )
        )
      })
      
      resp_ui <- tagList(
        tags$strong("Resumen:"),
        tags$p(summary_txt),
        tags$strong("Papers encontrados (", n, "):"),
        tags$div(links_ui)
      )
      
    } else {
      resp_ui <- tags$em("No se encontraron coincidencias para tu búsqueda.")
    }
    
    rv$history <- append(rv$history, list(list(query=q, response=resp_ui)))
  })
  
  output$`buscador-chat` <- renderUI({
    req(rv$history)
    do.call(tagList, lapply(rv$history, function(m) {
      tagList(
        tags$div(style="display:flex;justify-content:flex-end;margin:5px;",
                 tags$div(style="background:#DCF8C6;padding:10px;border-radius:10px;max-width:60%;",
                          tags$p(strong("Tú: "), m$query))
        ),
        tags$div(style="display:flex;justify-content:flex-start;margin:5px;",
                 tags$div(style="background:#ECECEC;padding:10px;border-radius:10px;max-width:60%;",
                          m$response))
      )
    }))
  })
}
