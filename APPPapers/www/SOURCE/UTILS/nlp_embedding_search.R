#=======================================
# Motor de Búsqueda por Embeddings Multilingues
# Modelo: paraphrase-multilingual-MiniLM-L12-v2
# Alineación: por TITULO (no por índice de fila)
#=======================================

# Apuntar reticulate al Python de Miniconda
PYTHON_PATH <- "C:/Users/eduar/miniconda3/python.exe"
if (file.exists(PYTHON_PATH)) {
  tryCatch(
    suppressWarnings(reticulate::use_python(PYTHON_PATH, required = FALSE)),
    error = function(e) NULL
  )
}

# Estado global persistente entre sesiones Shiny
if (!exists(".nlp_emb", envir = .GlobalEnv)) {
  .GlobalEnv$.nlp_emb <- new.env(parent = emptyenv())
  .GlobalEnv$.nlp_emb$listo      <- FALSE
  .GlobalEnv$.nlp_emb$intentado  <- FALSE
  .GlobalEnv$.nlp_emb$modelo     <- NULL
  .GlobalEnv$.nlp_emb$matrix     <- NULL   # matriz completa (n_csv x 384)
  .GlobalEnv$.nlp_emb$titulos    <- NULL   # vector de títulos alineado a matrix
}
.emb <- .GlobalEnv$.nlp_emb

EMBEDDING_MODEL <- "paraphrase-multilingual-MiniLM-L12-v2"
EMBEDDINGS_FILE <- "www/BD/paper_embeddings.npy"
TITULO_IDX_FILE <- "www/BD/paper_titulo_index.json"
THRESHOLD_BASE  <- 0.25

# ── Inicialización ────────────────────────────────────────────────────────────

inicializar_embeddings <- function() {
  if (.emb$intentado) return(.emb$listo)
  .emb$intentado <- TRUE

  if (!file.exists(EMBEDDINGS_FILE) || !file.exists(TITULO_IDX_FILE)) {
    cat("⚠️  [EMBED] Archivos no encontrados. Ejecutar: python precompute_embeddings.py\n")
    return(FALSE)
  }

  tryCatch({
    cat("🐍 [EMBED] Inicializando motor de embeddings multilingues...\n")

    np <- reticulate::import("numpy", convert = FALSE)
    st <- reticulate::import("sentence_transformers", convert = FALSE)

    # Cargar matriz pre-computada
    emb_r <- reticulate::py_to_r(np$load(EMBEDDINGS_FILE))
    cat(paste0("   Embeddings: ", nrow(emb_r), " papers x ", ncol(emb_r), " dims\n"))

    # Cargar índice de títulos para alineación por TITULO
    titulos <- jsonlite::fromJSON(TITULO_IDX_FILE)
    cat(paste0("   Índice de títulos: ", length(titulos), " entradas\n"))

    # Cargar modelo (para codificar queries en runtime)
    cat("   Cargando modelo (puede tardar ~20 s la primera vez)...\n")
    modelo <- st$SentenceTransformer(EMBEDDING_MODEL)

    .emb$matrix  <- emb_r
    .emb$titulos <- titulos
    .emb$modelo  <- modelo
    .emb$listo   <- TRUE

    cat(paste0("✅ [EMBED] Motor activo | modelo: ", EMBEDDING_MODEL, "\n"))
    return(TRUE)

  }, error = function(e) {
    cat(paste0("❌ [EMBED] Error: ", conditionMessage(e), "\n"))
    .emb$listo <- FALSE
    return(FALSE)
  })
}

# ── Búsqueda principal ────────────────────────────────────────────────────────

#' Búsqueda semántica multilingue ES↔EN
#'
#' Alinea cada fila de `data` con su embedding usando el campo TITULO,
#' lo que es robusto frente a filtros o reordenamientos del dataset.
#'
#' @param query       Consulta en cualquier idioma
#' @param data        Data frame con los papers (debe tener columna TITULO)
#' @param top_k       Máximo de resultados
#' @param threshold   Similitud mínima (0-1)
buscar_por_embeddings <- function(query, data, top_k = 20, threshold = THRESHOLD_BASE) {

  if (!.emb$listo) return(NULL)
  if (!"TITULO" %in% colnames(data)) return(NULL)

  tryCatch({
    # Codificar query → vector 384-dim normalizado
    qv <- as.numeric(reticulate::py_to_r(
      .emb$modelo$encode(list(query), convert_to_numpy = TRUE, normalize_embeddings = TRUE)
    )[1L, ])

    # Alinear data con embeddings via TITULO
    # (robusto ante filtros, reordenamientos o versiones distintas del dataset)
    titulo_a_emb_idx <- setNames(seq_along(.emb$titulos) - 1L, .emb$titulos)

    data_titulos <- as.character(data$TITULO)
    emb_indices  <- titulo_a_emb_idx[data_titulos]   # NA si el título no está en el índice

    validos <- which(!is.na(emb_indices))
    if (length(validos) == 0) {
      cat("⚠️  [EMBED] Ningún título del data coincide con el índice. Regenerar embeddings.\n")
      return(NULL)
    }

    idx_r <- emb_indices[validos] + 1L   # convertir 0-based → 1-based para R
    sub_matrix <- .emb$matrix[idx_r, , drop = FALSE]   # submatriz alineada

    # Similitud coseno (embeddings normalizados → dot product)
    sims_sub <- as.numeric(sub_matrix %*% qv)

    cat(paste0("   [EMBED] ", length(validos), " papers alineados | sim_max=",
               round(max(sims_sub), 3), "\n"))

    # Filtrar por threshold (relajar si no hay resultados)
    mask <- sims_sub >= threshold
    if (!any(mask)) {
      th2  <- threshold * 0.65
      mask <- sims_sub >= th2
      if (any(mask)) cat(paste0("   [EMBED] Threshold relajado a ", round(th2, 3), "\n"))
    }

    if (!any(mask)) {
      cat("   [EMBED] Sin resultados\n")
      return(data.frame())
    }

    # Top-K ordenado por similitud
    orden      <- order(sims_sub[mask], decreasing = TRUE)
    top_validos <- validos[mask][orden]
    top_sims    <- sims_sub[mask][orden]
    top_k_idx   <- head(seq_along(top_validos), top_k)

    resultado <- data[top_validos[top_k_idx], ]
    resultado$score_semantico   <- top_sims[top_k_idx]
    resultado$score_normalizado <- normalizar_emb(top_sims[top_k_idx])

    cat(paste0("   [EMBED] ", nrow(resultado), " papers encontrados\n"))
    return(resultado)

  }, error = function(e) {
    cat(paste0("❌ [EMBED] Error en búsqueda: ", conditionMessage(e), "\n"))
    return(NULL)
  })
}

normalizar_emb <- function(s) {
  mn <- min(s, na.rm = TRUE); mx <- max(s, na.rm = TRUE)
  if (mx == mn) return(rep(1, length(s)))
  (s - mn) / (mx - mn)
}

embeddings_listos <- function() .emb$listo
