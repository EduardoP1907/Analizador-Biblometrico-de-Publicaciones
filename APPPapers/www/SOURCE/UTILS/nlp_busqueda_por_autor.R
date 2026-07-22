#=======================================
# DETECCIÓN DE INTENCIÓN Y FILTROS DE BÚSQUEDA
# Soporta: autor, año, rango de años, tema, y combinaciones
#=======================================

# ── Vocabulario ───────────────────────────────────────────────────────────────

.TOPIC_PREPS <- c(
  "sobre", "acerca", "referente", "relacionado", "relativo", "enfocado",
  "about", "regarding", "related", "concerning", "on", "in"
)

.TEMAS_ACADEMICOS <- tolower(c(
  "matematica","matematicas","algebra","calculo","estadistica","geometria",
  "probabilidad","analisis","ecuaciones","trigonometria",
  "fisica","quimica","biologia","ciencia","ciencias","geologia","astronomia",
  "computacion","informatica","tecnologia","ingenieria","electronica","mecanica",
  "salud","medicina","enfermeria","farmacia","odontologia","nutricion","clinico",
  "educacion","pedagogia","didactica","ensenanza","formacion","capacitacion","curriculum",
  "economia","finanzas","contabilidad","administracion","gestion","negocios","marketing",
  "derecho","leyes","legislacion","historia","filosofia","sociologia",
  "psicologia","linguistica","idioma","idiomas","lenguaje","literatura",
  "arte","musica","arquitectura","diseno","comunicacion","periodismo",
  # CS temas (español)
  "inteligencia","artificial","automatico","automatica","automatizacion",
  "aprendizaje","profundo","maquina","neuronal","neuronales","cognicion",
  "vision","computacional","reconocimiento","clasificacion","deteccion","segmentacion",
  "datos","mineria","analisis","visualizacion","procesamiento","extraccion",
  "optimizacion","heuristica","metaheuristica","evolutivo","genetico","combinatoria",
  "software","hardware","sistemas","redes","bases","seguridad","privacidad",
  "ciberseguridad","criptografia","biometria","protocolos","vulnerabilidad",
  "robotica","simulacion","modelado","modelamiento","prediccion","pronostico",
  "programacion","algoritmos","estructuras","compiladores","paradigmas",
  "movil","web","cloud","nube","iot","sensores","embedded","tiempo real",
  "biomedico","genomica","proteina","bioingenieria","bioinformatica","clinica",
  "grafo","grafos","red","redes","busqueda","indexacion","recuperacion",
  "senal","senales","imagen","imagenes","audio","video","multimedia",
  # CS temas (inglés)
  "mathematics","physics","chemistry","biology","science","geology",
  "health","medicine","education","learning","teaching","training","pedagogy",
  "machine","deep","neural","network","networks","vision","language","text",
  "data","mining","analysis","visualization","processing","extraction",
  "optimization","heuristic","evolutionary","genetic","algorithm","algorithms",
  "software","hardware","systems","security","cryptography","privacy",
  "robotics","simulation","modeling","prediction","classification","detection",
  "programming","computing","artificial","intelligence","automation",
  "mobile","wireless","cloud","embedded","sensor","sensors","real-time",
  "biomedical","genomics","protein","bioinformatics","clinical",
  "graph","graphs","search","indexing","retrieval","recommendation",
  "signal","signals","image","images","audio","video","multimedia",
  "quantum","nanotechnology","photonics","plasma","thermodynamics",
  "econometrics","finance","accounting","management","marketing"
))

.NOMBRES_PROPIOS <- tolower(c(
  # Nombres masculinos
  "juan","carlos","jose","luis","miguel","pedro","manuel","pablo","andres",
  "roberto","mario","eduardo","rodrigo","marcelo","jorge","gabriel","daniel",
  "francisco","sergio","alejandro","victor","diego","nicolas","rafael","david",
  "hector","oscar","gonzalo","cristian","sebastian","mauricio","claudio",
  "alberto","antonio","felipe","javier","fernando","ignacio","patricio",
  "gustavo","raul","ernesto","cesar","hugo","omar","ivan","alan",
  "ariel","axel","boris","camilo","dante","emilio","fabio","gaston","hernan",
  "isaias","jaime","kevin","leandro","marcos","nahuel","oliver",
  "quintero","renato","tomas","ulises","valentino","walter","alvaro",
  "adriano","agustin","augusto","beniamino","benito","bernardo","blas",
  "cayetano","celso","constantino","cornelio","cosme","crescencio",
  "dario","demetrio","desiderio","domingo","donato",
  "elias","eliseo","emigdio","emiliano","enrique","esteban","eugenio",
  "ezequiel","fabian","feliciano","fidel","florencio","florentino",
  "fulgencio","genaro","gerardo","gilberto","gregorio","guillermo",
  "guadalupe","lamberto","laureano","lazaro","leocadio","leon","leonel",
  "leopoldo","liborio","luciano","lucio","martin","mateo","matias",
  "maximo","nicanor","octavio","odilio","oliverio","onofre",
  "primitivo","prospero","prudencio","ramon","reinaldo","remigio",
  "reyes","rogelio","rolando","roman","rosendo","ruben","rufino",
  "samuel","sandalio","saturnino","simplicio","sixto","timoteo",
  "tobias","urbano","ursicino","valentin","venancio","zoilo",
  # Nombres femeninos
  "maria","ana","carmen","rosa","laura","claudia","patricia","andrea","monica",
  "sandra","carolina","veronica","alejandra","cristina","paula","natalia",
  "lorena","diana","elena","isabel","julia","luz","marcela","norma","olga",
  "pilar","raquel","sofia","teresa","valentina","viviana","yasmin","zaida",
  "fernanda","gabriela","daniela","camila","beatriz","alicia","adriana",
  "angela","barbara","cecilia","dolores","evelyn","fabiola","gloria",
  "ines","jacqueline","karen","leslie","liliana","magdalena","nadia",
  "alejandra","amelia","amparo","asuncion","aurora","azucena",
  "blanca","brigida","candida","caridad","celeste","celia","clorinda",
  "concepcion","consuelo","corina","delia","dominga","dulce",
  "edith","efigenia","elba","elisa","elizabet","encarnacion","epifania",
  "esperanza","eulalia","evangelina","felisa","filomena","flor",
  "florencia","francisca","guadalupe","herminia","hortensia","hortencia",
  "juana","leonor","leticia","lilia","lourdes","lucia","luisa",
  "magdalena","manuela","margarita","margot","marta","mercedes",
  "milagros","miriam","narcisa","nieves","nora","ofelia",
  "olimpia","paloma","pamela","perpetua","petra","prudencia",
  "remedios","rocio","rosalia","rosario","rufina","sabrina",
  "soledad","susana","trinidad","veronica","victoria","virginia","visitacion",
  # Apellidos comunes en contexto chileno/latinoamericano
  "garcia","gonzalez","martinez","lopez","rodriguez","perez","sanchez",
  "romero","flores","diaz","morales","torres","ramirez","vargas","medina",
  "castro","espinoza","contreras","moreno","silva","gutierrez","ramos",
  "villanueva","rojas","alvarez","herrera","mendoza","nunez","vega","reyes",
  "ruiz","blanco","fuentes","ortiz","chacon","montero","riff","ojeda",
  "inostroza","parada","villalobos","carcamo","baladrón","kri",
  "rannou","leiva","hidalgo","roman","marin","solar","chourio","cockbaine",
  "jara","araya","munoz","calanche","villavicencio","acuna","lobos","vasquez",
  "navas","labarca","pezoa","faouzi","grimm","lizama","sotelo","quispe",
  "abarzua","barbieri","barrera","cerda","chang","chourio","espinoza",
  "elorrieta","guerra","han","herrero","iturriaga","miranda","palominos",
  "salinas","zamorano","asenjo","caro","contreras","cid","daza","ibáñez",
  "acuña","muñoz","infante","soto","vidal","quezada","bravo","campos",
  "tapia","paredes","saavedra","sepulveda","valenzuela","toledo","carrasco"
))

# ── Utilidades ────────────────────────────────────────────────────────────────

.normalizar_texto <- function(x) {
  x <- tolower(trimws(x))
  x <- chartr("áéíóúüñ", "aeiouun", x)
  x
}

.es_nombre_persona <- function(texto) {
  tokens <- unlist(strsplit(.normalizar_texto(texto), "\\s+"))
  tokens <- tokens[nchar(tokens) >= 2]
  if (length(tokens) == 0) return(FALSE)
  # Señal positiva: primer token es nombre propio conocido
  if (tokens[1] %in% .NOMBRES_PROPIOS) return(TRUE)
  # Señal negativa: primer token es tema académico
  if (tokens[1] %in% .TEMAS_ACADEMICOS) return(FALSE)
  # Señal negativa: algún token es tema académico
  if (any(tokens %in% .TEMAS_ACADEMICOS)) return(FALSE)
  # Sin señal clara: solo asumir persona si tiene 2+ tokens (un solo token ambiguo = tema)
  length(tokens) >= 2
}

# ── Extractor de año(s) ───────────────────────────────────────────────────────

#' Extraer restricciones de año de la query
#' @return list(tiene=FALSE) o list(tiene=TRUE, desde=YYYY, hasta=YYYY, query_sin_anos=...)
extraer_filtro_anos <- function(query) {
  q <- query

  # Rango: "del 2018 al 2023", "entre 2018 y 2023", "de 2018 a 2023", "2018-2023"
  patron_rango <- "(\\d{4})\\s*[-–]\\s*(\\d{4})|(?:del?|desde|entre)\\s+(\\d{4})\\s+(?:al?|hasta|y|a)\\s+(\\d{4})"
  m <- regmatches(q, regexpr(patron_rango, q, perl = TRUE, ignore.case = TRUE))
  if (length(m) > 0 && nchar(m) > 0) {
    anos <- regmatches(m, gregexpr("\\d{4}", m))[[1]]
    if (length(anos) >= 2) {
      q_limpia <- gsub(patron_rango, "", q, perl = TRUE, ignore.case = TRUE)
      return(list(tiene = TRUE, desde = as.integer(anos[1]),
                  hasta = as.integer(anos[2]),
                  query_sin_anos = trimws(gsub("\\s+", " ", q_limpia))))
    }
  }

  # Últimos N años: "últimos 5 años", "last 3 years"
  patron_ultimos <- "(?:últimos|ultimos|last|pasados?)\\s+(\\d+)\\s+a[ñn]os?"
  m2 <- regmatches(q, regexpr(patron_ultimos, q, perl = TRUE, ignore.case = TRUE))
  if (length(m2) > 0 && nchar(m2) > 0) {
    n <- as.integer(regmatches(m2, regexpr("\\d+", m2)))
    ano_hasta <- as.integer(format(Sys.Date(), "%Y"))
    q_limpia  <- gsub(patron_ultimos, "", q, perl = TRUE, ignore.case = TRUE)
    return(list(tiene = TRUE, desde = ano_hasta - n, hasta = ano_hasta,
                query_sin_anos = trimws(gsub("\\s+", " ", q_limpia))))
  }

  # Año único: "en 2022", "del año 2021", "de 2020", "year 2019", "2023"
  patron_unico <- "(?:en|del?\\s+año|del?|from|year|in|año)\\s*(\\d{4})|\\b(\\d{4})\\b"
  m3 <- regmatches(q, regexpr(patron_unico, q, perl = TRUE, ignore.case = TRUE))
  if (length(m3) > 0 && nchar(m3) > 0) {
    ano_str <- regmatches(m3, regexpr("\\d{4}", m3))
    if (length(ano_str) > 0) {
      ano <- as.integer(ano_str)
      if (ano >= 1990 && ano <= 2030) {
        q_limpia <- gsub(patron_unico, "", q, perl = TRUE, ignore.case = TRUE)
        return(list(tiene = TRUE, desde = ano, hasta = ano,
                    query_sin_anos = trimws(gsub("\\s+", " ", q_limpia))))
      }
    }
  }

  list(tiene = FALSE, desde = NA, hasta = NA, query_sin_anos = query)
}

# ── Extractor de autor ────────────────────────────────────────────────────────

#' Extraer nombre de autor de la query si está presente
#' @return list(tiene=FALSE) o list(tiene=TRUE, nombre=..., query_sin_autor=...)
extraer_filtro_autor <- function(query) {
  q_lower <- tolower(trimws(query))
  q_norm  <- .normalizar_texto(query)

  # Preposiciones temáticas → definitivamente NO es autor
  palabras <- unlist(strsplit(q_norm, "\\s+"))
  if (any(palabras %in% .normalizar_texto(paste(.TOPIC_PREPS, collapse = " ")))) {
    # revisión más cuidadosa: si contiene "sobre", "acerca", etc. → no es búsqueda de autor
    if (any(palabras %in% c("sobre","acerca","referente","relacionado","relativo",
                             "enfocado","about","regarding","related","concerning"))) {
      return(list(tiene = FALSE, nombre = "", query_sin_autor = query))
    }
  }

  # Patrones con prefijo explícito de autor
  patrones <- list(
    list(re = "(?:todos\\s+los\\s+)?(?:publicaciones|papers|art[ií]culos|trabajos|investigaciones|estudios)\\s+de\\s+([^,\\.]+)",
         prefijos = c("publicaciones de","papers de","artículos de","articulos de",
                      "trabajos de","investigaciones de","estudios de",
                      "todos los papers de","todos los publicaciones de")),
    list(re = "(?:papers|publications|articles|works|research)\\s+by\\s+([^,\\.]+)",
         prefijos = c("papers by","publications by","articles by","works by","research by")),
    list(re = "(?:del?\\s+)?(?:autor|investigador|profesor|academico)\\s+([^,\\.]+)",
         prefijos = c("del autor","del investigador","del profesor","autor","investigador","profesor")),
    list(re = "(?:authored|written)\\s+by\\s+([^,\\.]+)",
         prefijos = c("authored by","written by")),
    list(re = "dra?\\.?\\s+([a-záéíóúñ]+(?:\\s+[a-záéíóúñ]+)+)",
         prefijos = c("dr.","dr","dra.","dra"))
  )

  for (p in patrones) {
    m <- regexpr(p$re, q_lower, perl = TRUE, ignore.case = TRUE)
    if (m != -1) {
      capturado <- trimws(regmatches(q_lower, m))
      # Limpiar prefijo
      nombre <- capturado
      for (pref in p$prefijos) {
        nombre <- gsub(paste0("^", pref, "\\s*"), "", nombre, ignore.case = TRUE)
      }
      nombre <- trimws(gsub("[,\\.;:]+$", "", nombre))
      if (nchar(nombre) >= 3 && .es_nombre_persona(nombre)) {
        # Remover el fragmento de autor de la query para dejar solo el tema
        q_sin <- trimws(gsub(p$re, "", query, perl = TRUE, ignore.case = TRUE))
        q_sin <- trimws(gsub("\\s+", " ", q_sin))
        return(list(tiene = TRUE, nombre = nombre, query_sin_autor = q_sin))
      }
    }
  }

  # Nombre directo (sin prefijo): 2-3 palabras, primera con mayúscula, parece nombre
  palabras_orig <- unlist(strsplit(trimws(query), "\\s+"))
  if (length(palabras_orig) >= 2 && length(palabras_orig) <= 3 &&
      grepl("^[A-ZÁÉÍÓÚÑ]", palabras_orig[1]) &&
      .es_nombre_persona(query)) {
    return(list(tiene = TRUE, nombre = q_lower, query_sin_autor = ""))
  }

  list(tiene = FALSE, nombre = "", query_sin_autor = query)
}

# ── Función principal: parsear TODOS los filtros de una query ─────────────────

#' Extraer todos los filtros presentes en la query
#'
#' Soporta combinaciones: "papers de García sobre ML del 2020 al 2023"
#' @param query Texto del usuario
#' @return list con: autor (list), anos (list), terminos_tema (character)
parsear_filtros_query <- function(query) {
  q <- query

  # 1. Extraer filtro de años (y limpiar query)
  filtro_anos <- extraer_filtro_anos(q)
  q <- filtro_anos$query_sin_anos

  # 2. Extraer filtro de autor (sobre la query ya sin años)
  filtro_autor <- extraer_filtro_autor(q)
  q_tema <- if (filtro_autor$tiene) filtro_autor$query_sin_autor else q

  # 3. Limpiar stop-words de instrucción de lo que queda → términos de tema
  stop_instruccion <- c("dame","muestra","busca","encuentra","listame","dime",
                        "quiero","necesito","obten","trae","presenta","lista",
                        "show","find","give","get","list","search",
                        "todos","todas","los","las","el","la","un","una",
                        "papers","publicaciones","articulos","trabajos","estudios",
                        "investigaciones","de","del","por","en","con","sin",
                        "mas","recientes","ultimos","nuevos","sobre","acerca")
  tokens_tema <- unlist(strsplit(tolower(q_tema), "\\s+"))
  tokens_tema <- tokens_tema[nchar(tokens_tema) >= 3 & !tokens_tema %in% stop_instruccion]
  terminos_tema <- paste(unique(tokens_tema), collapse = " ")

  cat(paste0("📋 [INTENT] autor=", if(filtro_autor$tiene) filtro_autor$nombre else "—",
             " | años=", if(filtro_anos$tiene) paste0(filtro_anos$desde,"-",filtro_anos$hasta) else "—",
             " | tema='", terminos_tema, "'\n"))

  list(
    autor = filtro_autor,
    anos  = filtro_anos,
    terminos_tema = terminos_tema,
    query_original = query
  )
}

# ── Aplicar filtro de años ────────────────────────────────────────────────────

aplicar_filtro_anos <- function(data, filtro_anos) {
  if (!filtro_anos$tiene || !"ANO" %in% colnames(data)) return(data)
  anos_data <- suppressWarnings(as.integer(data$ANO))
  ok <- !is.na(anos_data) & anos_data >= filtro_anos$desde & anos_data <= filtro_anos$hasta
  cat(paste0("   [AÑOS] ", filtro_anos$desde, "-", filtro_anos$hasta,
             " → ", sum(ok), "/", nrow(data), " papers\n"))
  data[ok, ]
}

# ── Aplicar filtro de autor ───────────────────────────────────────────────────

aplicar_filtro_autor <- function(data, nombre_autor) {
  if (!"NOMBRE_AUTOR" %in% colnames(data)) return(data.frame())
  nombre_norm <- .normalizar_texto(nombre_autor)
  palabras    <- unlist(strsplit(nombre_norm, "\\s+"))
  palabras    <- palabras[nchar(palabras) >= 2]
  if (length(palabras) == 0) return(data.frame())

  nombres_data <- sapply(as.character(data$NOMBRE_AUTOR), .normalizar_texto)
  scores <- sapply(nombres_data, function(n) {
    sum(palabras %in% unlist(strsplit(n, "\\s+"))) / length(palabras)
  })
  umbral <- if (length(palabras) == 1) 0.9 else 0.5
  encontrados <- data[scores >= umbral, ]

  if (nrow(encontrados) > 0) {
    encontrados$score_autor <- scores[scores >= umbral]
    encontrados <- encontrados[order(encontrados$score_autor, decreasing = TRUE), ]
  }

  cat(paste0("   [AUTOR] '", nombre_autor, "' → ", nrow(encontrados), " papers\n"))
  encontrados
}

# ── Generar resumen para búsqueda por autor ───────────────────────────────────

generar_resumen_busqueda_autor <- function(papers, nombre_autor, query_original) {
  n <- nrow(papers)
  if (n == 0) {
    return(paste0("No se encontraron publicaciones del autor '", nombre_autor,
                  "'. Verifique el nombre o intente con variaciones."))
  }

  resumen <- paste0("Se encontraron ", n, " publicaci", if(n==1) "ón" else "ones",
                    " del autor '", nombre_autor, "'. ")

  if ("ANO" %in% colnames(papers)) {
    anos <- suppressWarnings(as.integer(papers$ANO))
    anos <- anos[!is.na(anos)]
    if (length(anos) > 1)
      resumen <- paste0(resumen, "Rango temporal: ", min(anos), " – ", max(anos), ". ")
  }

  if ("CITADO_POR" %in% colnames(papers)) {
    citas <- suppressWarnings(as.numeric(papers$CITADO_POR))
    citas <- citas[!is.na(citas)]
    if (length(citas) > 0 && sum(citas) > 0)
      resumen <- paste0(resumen, "Total de citas: ", sum(citas),
                        " (promedio: ", round(mean(citas), 1), " por paper). ")
  }

  if ("AUTOR_PALABRAS_CLAVES" %in% colnames(papers)) {
    kws <- paste(papers$AUTOR_PALABRAS_CLAVES, collapse = " ; ")
    top <- head(names(sort(table(trimws(unlist(strsplit(kws, "[,;]")))), decreasing = TRUE)), 4)
    top <- top[nchar(top) > 2]
    if (length(top) > 0)
      resumen <- paste0(resumen, "Áreas principales: ", paste(top, collapse = ", "), ".")
  }

  resumen
}

# ── Función principal de búsqueda por autor (compatibilidad con motor) ────────

proceso_busqueda_por_autor <- function(query, data) {
  filtro_autor <- extraer_filtro_autor(query)
  if (!filtro_autor$tiene) {
    return(list(es_busqueda_autor = FALSE, mensaje = "No se detectó búsqueda por autor"))
  }

  papers <- aplicar_filtro_autor(data, filtro_autor$nombre)
  resumen <- generar_resumen_busqueda_autor(papers, filtro_autor$nombre, query)

  list(
    es_busqueda_autor  = TRUE,
    success            = TRUE,
    nombre_autor       = filtro_autor$nombre,
    confianza_deteccion = 0.9,
    num_papers         = nrow(papers),
    papers             = papers,
    resumen_generado   = resumen,
    query_original     = query
  )
}

# Compatibilidad con llamadas existentes en el motor semántico
detectar_busqueda_por_autor <- function(query) {
  r <- extraer_filtro_autor(query)
  list(
    es_busqueda_autor      = r$tiene,
    nombre_autor_detectado = r$nombre,
    patron_usado           = "parsear_filtros",
    query_original         = query,
    confianza              = if (r$tiene) 0.9 else 0
  )
}
