#=======================================
# Motor de Búsqueda por Embeddings Multilingues
# Modelo: paraphrase-multilingual-MiniLM-L12-v2
# Alineación: por TITULO (no por índice de fila)
#=======================================

# Apuntar reticulate al Python de Miniconda.
# La ruta se lee de la variable de entorno APPPAPERS_PYTHON; si no está
# definida se usa la del equipo de desarrollo.
PYTHON_PATH <- Sys.getenv("APPPAPERS_PYTHON", "C:/Users/eduar/miniconda3/python.exe")
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

# ── Similitud por embeddings ─────────────────────────────────────────────────

#' Similitud coseno entre la consulta y cada publicación (ES↔EN)
#'
#' Alinea cada fila de `data` con su embedding usando el campo TITULO,
#' lo que es robusto frente a filtros o reordenamientos del dataset.
#' No filtra ni ordena: entrega un puntaje por fila para que la fusión
#' híbrida (calcular_scores_hibridos) lo combine con los puntajes léxicos.
#'
#' @param query Consulta en cualquier idioma
#' @param data  Data frame con los papers (debe tener columna TITULO)
#' @return Vector numérico de largo nrow(data) (NA si la fila no tiene
#'   embedding), o NULL si el motor no está disponible
similitudes_embeddings <- function(query, data) {

  if (!.emb$listo) return(NULL)
  if (!"TITULO" %in% colnames(data)) return(NULL)

  tryCatch({
    # Codificar query → vector 384-dim normalizado
    qv <- as.numeric(reticulate::py_to_r(
      .emb$modelo$encode(list(query), convert_to_numpy = TRUE, normalize_embeddings = TRUE)
    )[1L, ])

    # Alinear data con embeddings via TITULO
    titulo_a_emb_idx <- setNames(seq_along(.emb$titulos), .emb$titulos)
    emb_indices <- titulo_a_emb_idx[as.character(data$TITULO)]   # NA si el título no está en el índice

    validos <- which(!is.na(emb_indices))
    if (length(validos) == 0) {
      cat("⚠️  [EMBED] Ningún título del data coincide con el índice. Regenerar embeddings.
")
      return(NULL)
    }

    # Similitud coseno (embeddings normalizados → dot product)
    sims <- rep(NA_real_, nrow(data))
    sims[validos] <- as.numeric(.emb$matrix[emb_indices[validos], , drop = FALSE] %*% qv)

    cat(paste0("   [EMBED] ", length(validos), " papers alineados | sim_max=",
               round(max(sims, na.rm = TRUE), 3), "
"))
    sims

  }, error = function(e) {
    cat(paste0("❌ [EMBED] Error en búsqueda: ", conditionMessage(e), "
"))
    NULL
  })
}

embeddings_listos <- function() .emb$listo
