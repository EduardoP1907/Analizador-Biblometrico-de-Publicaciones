tab_indicadores = function(input, output, session, datos_tmp) {
  

  #=========================================
  # Universidad
  #=========================================
  if (input$In_Sel_Indicadores=="Institucionales")
  {
    
    #-------------------------------------
    #1. Indicadores de Rendimiento
    #-------------------------------------

    #--------------------------
    #1.1) Total de publicaciones
    #--------------------------
    TP = nrow(datos_tmp)
    
    #-------------------------
    #1.2) Total de publicaciones en revistas
    #-------------------------
    TPj = 0
    if (TP!=0) {TPj = nrow(datos_tmp[which(datos_tmp$TIPO_DOCUMENTO=="Article"),])}
    
    #-------------------------
    #1.3) Total de publicaciones en actas de conferencia
    #-------------------------
    TPp = 0
    if (TP!=0) {TPp = nrow(datos_tmp[which(datos_tmp$TIPO_DOCUMENTO=="Conference paper"),])}
    
    #-------------------------
    #1.4) % de publicaciones en revistas
    #-------------------------
    PTPj = 0
    if (TP!=0) {PTPj = round(TPj/TP*100,3)}
    
    #-------------------------
    #1.5) % de publicaciones en revistas
    #-------------------------
    PTPp = 0
    if (TP!=0) {PTPp = round(TPp/TP*100,3)}
    
    #-------------------------
    #1.6) Promedio de publicaciones por académico
    #-------------------------
    PPA = 0
    n_academicos = length(unique(datos_tmp$NOMBRE_AUTOR))
    if (n_academicos!=0){PPA = round(TP/n_academicos,3)}
    
    #-------------------------
    #1.7) Promedio de publicaciones en revistas por académico
    #-------------------------
    PPAj = 0
    if (n_academicos!=0){PPAj = round(TPj/n_academicos,3)}
    
    #-------------------------
    #1.8) Promedio de publicaciones en conferencias por académico
    #-------------------------
    PPAp = 0
    if (n_academicos!=0){PPAp = round(TPp/n_academicos,3)}
    
    #-------------------------
    #1.9) Promedio de publicaciones por año
    #-------------------------
    PPAA = 0
    periodo = 1
    if (TP!=0){periodo = max(datos_tmp$ANO)-min(datos_tmp$ANO)+1}
    if (periodo!=0){PPAA = round(TP/periodo,3)}
    
    #-------------------------
    #1.10) Promedio de publicaciones en revista por año
    #-------------------------
    PPAjA = 0
    if (periodo!=0){PPAjA = round(TPj/periodo,3)}
    
    #-------------------------
    #1.11) Promedio de publicaciones en conferencias por año
    #-------------------------
    PPApA = 0
    if (periodo!=0){PPApA = round(TPp/periodo,3)}
    
    #-------------------------
    #1.12) Total de años activos de publicaciones
    #-------------------------
    TPA = length(unique(datos_tmp$ANO))
    
    #-------------------------
    #1.13) Productividad por año activo de publicación
    #-------------------------
    PAPP = 0
    if (TPA!=0){PAPP = round(TP/TPA,3)}
    
    #-------------------------
    #1.14) Total de cita
    #-------------------------
    TC = sum(datos_tmp$CITADO_POR)
    
    #-------------------------
    #1.15) Promedio de citas por publicación
    #-------------------------
    PCPP = 0
    if (TP!=0){PCPP = round(TC/TP,3)}
    
    #-------------------------
    #1.16) Total de publicaciones citadas
    #-------------------------
    TCPP = 0
    if (TP!=0){TCPP = length(which(datos_tmp$CITADO_POR>0))}
    
    #-------------------------
    #1.17) Proporción de publicaciones citadas
    #-------------------------
    PCP = 0
    if (TP!=0){PCP = round(TCPP/TP,3)}
    
    #-------------------------
    #1.18) Citas por publicación citada
    #-------------------------
    CPP = 0
    if (TCPP!=0){CPP = round(TC/TCPP,3)}
    
    #-------------------------
    #1.19) SJR IF promedio
    #------------------------
    PSJR_IF = 0
    if (length(datos_tmp$SJR[which(datos_tmp$SJR>=0)]>0)){
      PSJR_IF = round(mean(datos_tmp$SJR[which(datos_tmp$SJR>=0)]),3)}
    
    #-------------------------
    #1.20) SJR IF Máximo
    #------------------------
    maxSJR_IF = 0
    if (length(datos_tmp$SJR[which(datos_tmp$SJR>=0)]>0)){
      maxSJR_IF = round(max(datos_tmp$SJR[which(datos_tmp$SJR>=0)]),3)}
    
    #-------------------------
    #1.21) SJR IF Mínimo
    #------------------------
    minSJR_IF= 0
    if (length(datos_tmp$SJR[which(datos_tmp$SJR>=0)]>0)){
      minSJR_IF = round(min(datos_tmp$SJR[which(datos_tmp$SJR>=0)]),3)}
    
    #--------------------------
    # Creación de tabla
    #-------------------------
    
    output$table_Indexes_rendimiento = renderDataTable({
      nombre = c("Total de publicaciones","Total de publicaciones en revistas","Total de publicaciones en actas de conferencias",
                 "Porcentaje de publicaciones en revistas","Porcentaje de publicaciones en actas de conferencias",
                 "Promedio de las publicaciones por académico(a)","Promedio de las publicaciones en revista por académico(a)",
                 "Promedio de publicaciones de acta de conferencia por académico(a)","Promedio de publicaciones por año",
                 "Promedio de publicaciones en revistas por año","Promedio de publicaciones en acta de conferencia por año",
                 "Total de años activos de publicaciones","Productividad por año activo de publicación","Total de citas",
                 "Promedio de citas por publicación","Total de publicaciones citadas","Proporción de publicaciones citadas",
                 "Citas por publicación citada","Promedio de factor de impacto SJR","Máximo de factor de impacto SJR",
                 "Mínimo de factor de impacto SJR")
      abreviacion = c("TP","TPj","TPp","PTPj","PTPp","PPA","PPAj","PPAp","PPAA","PPAjA","PPApA","TPA","PPAP","TC",
                      "PCPP","TCPP","PCP","CPP","PSJR","maxSJR","minSJR")
      
      descripcion=c("Total de publicaciones", 
                    "Total de publicaciones en revistas",
                    "Total de publicaciones en actas de conferencias", 
                    "\\( \\frac{TP_j}{TP} \\times 100 \\)",
                    "\\( \\frac{TP_p}{TP} \\times 100 \\)",
                    "Promedio de artículos publicados por académico(a)", 
                    "Promedio de artículos publicados en revistas por académico(a)",                                     
                    "Promedio de artículos publicados en actas de conferencias por académico(a)",                                      
                    "Promedio de artículos publicados por año", 
                    "Promedio de artículos publicados en revistas por año",
                    "Promedio de artículos publicados en actas de conferencias por año",
                    "Número de años que los(as) académicos(as) registran publicaciones",     
                    "\\( \\frac{TP}{TPA} \\)",           
                    "Total de citas",                           
                    "\\( \\frac{TC}{TP} \\)",
                    "Cantidad de artículos citados",
                    "\\( \\frac{TCPP}{TP} \\)",
                    "\\( \\frac{TC}{TCPP} \\)",
                    "Promedio de factor de impacto SJR",
                    "Máximo de factor de impacto SJR",
                    "Mínimo de factor de impacto SJR")
      valores = c(TP,TPj,TPp,PTPj,PTPp,PPA,PPAj,PPAp,PPAA,PPAjA,PPApA,TPA,PAPP,TC,PCPP,TCPP,PCP,CPP,PSJR_IF,maxSJR_IF,minSJR_IF)
      valores = formatC(round(valores,3),3,format="f")
      
      
      tabla = data.frame(Indicador = nombre, Abreviación = abreviacion, Descripción = descripcion, Valores = valores)
      tabla = as_tibble(tabla)
      
      # se retorna una tabla, se le dan propiedades
      datatable(tabla, class = 'display', options = list(pageLength = 21, dom = 'Brt', autoWidth = F, ordering = FALSE,scrollX = TRUE,
                                                         initComplete = JS(
                                                           "function(settings, json) {",
                                                           "  MathJax.Hub.Queue(['Typeset', MathJax.Hub]);",
                                                           "}"
                                                         ),
                                                         language = list(
                                                           url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
                                                         ),
                                                         columnDefs = list(list(className = 'dt-body-right', targets = 4),
                                                                           list(width = '30%', targets = c(1,3)))
      ))
    }) #Fin datatable
    
    #-------------------------------------
    #2. Indicadores de colaboración
    #-------------------------------------
    
    #-------------------------------------
    #2.1 Total de autores
    #-------------------------------------
    TA = 0
    if (TP!=0){TA = length(unique(datos_tmp$NOMBRE_AUTOR))}
    
    #-------------------------------------
    #2.2 Total de autores colaboradores
    #-------------------------------------
    NCA = 0
    if (TP!=0){
      autores_publicaciones = strsplit(datos_tmp$AUTORES_ID, ";")
      NCA = round(length(unique(unlist(autores_publicaciones))), 3)
    }
    
    #-------------------------------------
    #2.3 Promedio de autores colaboradores
    #-------------------------------------
    ANCA = 0
    if (TP!=0){ANCA = round(NCA/TP,3)}
    
    #-------------------------------------
    #2.4 Publicaciones en coautoría
    #-------------------------------------
    PC = 0
    if (TP!=0){
      PC = strsplit(datos_tmp$AUTORES_ID, ";")
      PC = sapply(PC, function(x) length(x))
      PC = length(which(PC>1))
    }
    
    #-------------------------------------
    #2.5 Porcentaje de publicaciones en coautoria
    #-------------------------------------
    PPCA = 0
    if (TP!=0){
      PPCA = round((PC/TP)*100,3)
    }
    
    #-------------------------------------
    #2.6 Publicaciones en coautoria institucional
    #-------------------------------------
    PCAI=0
    if (TP!=0){
      autores = unique(datos_tmp$SCOPUS_ID)
      conteo_autores = NULL
      for (a in 1:length(autores_publicaciones)) {
        interseccion = autores %in% as.numeric(autores_publicaciones[[a]])
        conteo_autores = c(conteo_autores, length(which(interseccion)))
      }
      PCAI = length(which(conteo_autores > 1))
    }
    
    #-------------------------------------
    #2.7 Porcentaje de publicaciones en coautoria institucional
    #-------------------------------------
    PPCAI=0
    if (TP!=0){PPCAI=round((PCAI/TP)*100,3)}
    
    
    #-------------------------------------
    #2.8 # Indicadores 8, 9 y 10: Distribución de autoría institucional
    #-------------------------------------
    # Vector de autores institucionales
    # Vector de autores institucionales (todo como carácter)
    autores_institucionales <- as.character(unique(datos_tmp$SCOPUS_ID))
    
    # Inicializar contadores
    primer_autor <- ultimo_autor <- intermedio_autor <- 0
    
    if (TP > 0) {
      for (i in 1:TP) {
        ids <- unlist(strsplit(as.character(datos_tmp$AUTORES_ID[i]), ";"))
        ids <- trimws(ids)  # por si hay espacios extra
        
        if (length(ids) >= 1) {
          if (ids[1] %in% autores_institucionales) {
            primer_autor <- primer_autor + 1
          }
          if (length(ids) > 2 && any(ids[-c(1, length(ids))] %in% autores_institucionales)) {
            intermedio_autor <- intermedio_autor + 1
          }
          if (length(ids) > 1 && ids[length(ids)] %in% autores_institucionales) {
            ultimo_autor <- ultimo_autor + 1
          }
        }
      }
      
      PPA1 <- round(primer_autor / TP * 100, 3)
      PPA2 <- round(intermedio_autor / TP * 100, 3)
      PPA3 <- round(ultimo_autor / TP * 100, 3)
    } else {
      PPA1 <- PPA2 <- PPA3 <- 0
    }
    
    #--------------------------
    # Creación de tabla
    #-------------------------
    output$table_Indexes_colaboracion = renderDataTable({
      nombre = c(
        "Total de académicos(as)",
        "Total de autores(as) colaboradores(as)",
        "Promedio de autores(as) colaboradores(as)",
        "Publicaciones con coautoría",
        "Porcentaje de publicaciones con coautoría",
        "Publicaciones en coautoría institucional",
        "Porcentaje de publicaciones en coautoría institucional",
        "Porcentaje de publicaciones donde un(a) académico(a) fue primer autor",
        "Porcentaje de publicaciones donde un(a) académico(a) fue autor intermedio",
        "Porcentaje de publicaciones donde un(a) académico(a) fue último autor"
      )
      
      abreviacion = c("TA", "NCA", "ANCA", "PC", "PPCA", "PCAI", "PPCAI", "PPA1", "PPA2", "PPA3")
      
      descripcion = c(
        "Total de académicos(as)",
        "Total de autores(as) colaboradores(as)",
        "\\( \\frac{NAC}{TP} \\)",
        "Total de publicaciones con coautoría",
        "\\( \\frac{PC}{TP} \\times 100 \\)",
        "Publicaciones en coautoría institucional",
        "\\( \\frac{PCAI}{TP} \\times 100 \\)",
        "\\( \\frac{Publicaciones\\ primer\\ autor}{TP} \\times 100 \\)",
        "\\( \\frac{Publicaciones\\ autor\\ intermedio}{TP} \\times 100 \\)",
        "\\( \\frac{Publicaciones\\ último\\ autor}{TP} \\times 100 \\)"
      )
      
      valores = c(TA, NCA, ANCA, PC, PPCA, PCAI, PPCAI, PPA1, PPA2, PPA3)
      valores = formatC(round(valores, 3), 3, format = "f")
      
      tabla = data.frame(Indicador = nombre, Abreviación = abreviacion, Descripción = descripcion, Valores = valores)
      tabla = as_tibble(tabla)
      
      # se retorna una tabla, se le dan propiedades
      datatable(tabla, class = 'display', options = list(pageLength = 21, dom = 'Brt', autoWidth = F, ordering = FALSE,scrollX = TRUE,
                                                         initComplete = JS(
                                                           "function(settings, json) {",
                                                           "  MathJax.Hub.Queue(['Typeset', MathJax.Hub]);",
                                                           "}"
                                                         ),
                                                         language = list(
                                                           url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
                                                         ),
                                                         columnDefs = list(list(className = 'dt-body-right', targets = 4),
                                                                           list(width = '30%', targets = c(1,3)))
      )) #fin data table
    }) #Fin output datatable
    
    
    
    
  } 
  else
    
  #=========================================
  #Académicos
  #=========================================
  {
    if (length(datos_tmp$NOMBRE_AUTOR)>0) {academicos = unique(datos_tmp$NOMBRE_AUTOR)} else
    {academicos="No existen publicaciones en el periodo seleccionado"}
    
    TP = TPj = TPp = PTPj = PTPp = PPPA = PPAjA = PPApA = TPA = PAPP = TC = PCPP = TCPP = PCP = CPP = PSJR_IF = maxSJR_IF =
      minSJR_IF = NCA = ANCA = PAPA = PC = PPCA = PCAI = PPCAI = PPA1 = PPA2 = PPA3 = PPA1_3 = (1:length(academicos))*0
  
    
    for (a in 1:length(academicos))
    {
      datos_academico = datos_tmp[which(datos_tmp$NOMBRE_AUTOR==academicos[a]),]
      
      
      #-------------------------
      #3. Indicadores de Rendimiento
      #-------------------------
      
      #--------------------------
      #3.1) Total de publicaciones
      #--------------------------
      TP[a] = nrow(datos_academico)
      
      #-------------------------
      #3.2) Total de publicaciones en revistas
      #-------------------------
      if (TP[a]!=0 & !is.na(TP[a])) {TPj[a] = nrow(datos_academico[which(datos_academico$TIPO_DOCUMENTO=="Article"),])}
      
      #-------------------------
      #3.3) Total de publicaciones en actas de conferencia
      #-------------------------
      if (TP[a]!=0 & !is.na(TP[a])) {TPp[a] = nrow(datos_academico[which(datos_academico$TIPO_DOCUMENTO=="Conference paper"),])}
      
      #-------------------------
      #3.4) % de publicaciones en revistas
      #-------------------------
      if (TP[a]!=0 & !is.na(TP[a])) {PTPj[a] = round(TPj[a]/TP[a]*100,3)}
      
      #-------------------------
      #3.5) % de publicaciones en actas de conferencia
      #-------------------------
      if (TP[a]!=0 & !is.na(TP[a])) {PTPp[a] = round(TPp[a]/TP[a]*100,3)}
      
      
      #-------------------------
      #3.6) Promedio de publicaciones por académico - PPA
      #-------------------------
      # No tiene sentido
      
      #-------------------------
      #3.7) Promedio de publicaciones en revista por académico - PPAj
      #-------------------------
      # No tiene sentido
      
      #-------------------------
      #3.8) Promedio de publicaciones en conferencias por académico - PPAp
      #-------------------------
      # No tiene sentido
      
      #-------------------------
      #3.9) Promedio de publicaciones por año
      #-------------------------
      if (TP[a]!=0 & !is.na(TP[a])){
        anos = max(datos_academico$ANO)-min(datos_academico$ANO)+1
        PPPA[a] = round(TP[a]/anos,3)
      }
      
      #-------------------------
      #3.10) Promedio de publicaciones en revistas por año
      #-------------------------
      if (TP[a]!=0 & !is.na(TP[a])){
        PPAjA[a] = round(TPj[a]/anos,3)
      }
      
      #-------------------------
      #3.11) Promedio de publicaciones en conferencias por año
      #-------------------------
      if (TP[a]!=0 & !is.na(TP[a])){
        PPApA[a] = round(TPp[a]/anos,3)
      }
      
      #-------------------------
      #3.12) Total de años activos de publicaciones
      #-------------------------
      TPA[a] = length(unique(datos_academico$ANO))
      
      
      #-------------------------
      #3.13) Productividad por año activo de publicación
      #-------------------------
      if (TPA[a]!=0){PAPP = round(TP[a]/TPA[a],3)}
      
      #-------------------------
      #3.14) Total de citas
      #-------------------------
      TC[a] = sum(datos_academico$CITADO_POR)
      
      #-------------------------
      #3.15) Promedio de citas por publicación
      #-------------------------
      if (TP[a]!=0 & !is.na(TP[a])){PCPP[a]= round(TC[a]/TP[a],3)}
      
      #-------------------------
      #3.16) Total de publicaciones citadas
      #-------------------------
      if (TP[a]!=0 & !is.na(TP[a])){TCPP[a] = length(which(datos_academico$CITADO_POR>0))}
      
      #-------------------------
      #3.17) Proporción de publicaciones citadas
      #-------------------------
      if (TP[a]!=0 & !is.na(TP[a])){PCP[a] = round(TCPP[a]/TP[a],3)}
      
      #-------------------------
      #3.18) Citas por publicación citada
      #-------------------------
      if (TCPP[a]!=0){CPP[a] = round(TC[a]/TCPP[a],3)}
      
      #-------------------------
      #3.19) SJR IF promedio
      #------------------------
      if (length(datos_academico$SJR[which(datos_academico$SJR>=0)]>0)){
        PSJR_IF[a] = round(mean(datos_academico$SJR[which(datos_academico$SJR>=0)]),3)}
      
      #-------------------------
      #3.20) SJR IF Máximo
      #------------------------
      if (length(datos_academico$SJR[which(datos_academico$SJR>=0)]>0)){
        maxSJR_IF[a] = round(max(datos_academico$SJR[which(datos_academico$SJR>=0)]),3)}
      
      #-------------------------
      #3.21) SJR IF Mínimo
      #------------------------
      if (length(datos_academico$SJR[which(datos_academico$SJR>=0)]>0)){
        minSJR_IF[a] = round(min(datos_academico$SJR[which(datos_academico$SJR>=0)]),3)}
      
      #------------------------
      # Indicadores de Colaboración
      #------------------------
      
      #-------------------------------------
      #4. Indicadores de colaboración
      #-------------------------------------
      
      #-------------------------------------
      #4.1 Promedio de autores por artículo
      #-------------------------------------
      if (TP[a] != 0 & !is.na(TP[a])) {
        autores_publicaciones = strsplit(datos_academico$AUTORES_ID, ";")
        total_autores = sapply(autores_publicaciones, length)
        PAPA[a] = round(mean(total_autores), 3)
      }
      
      #-------------------------------------
      #4.2 Total de autores colaboradores
      #-------------------------------------
      if (TP[a]!=0 & !is.na(TP[a])){
        autores_publicaciones = strsplit(datos_academico$AUTORES_ID, ";")
        NCA[a] = round(length(unique(unlist(autores_publicaciones))), 3)
      }
      
      #-------------------------------------
      #4.3 Promedio de autores colaboradores
      #-------------------------------------
      if (TP[a]!=0 & !is.na(TP[a])){ANCA[a] = round(NCA[a]/TP[a],3)}
      
      #-------------------------------------
      #4.4 Publicaciones en coautoría
      #-------------------------------------
      if (TP[a]!=0 & !is.na(TP[a])){
        PC_tmp = strsplit(datos_academico$AUTORES_ID, ";")
        PC_tmp = sapply(PC_tmp, function(x) length(x))
        PC[a] = length(which(PC_tmp>1))
      }
      
      #-------------------------------------
      #4.5 Porcentaje de publicaciones en coautoria
      #-------------------------------------
      if (TP[a]!=0 & !is.na(TP[a])){
        PPCA[a] = round((PC[a]/TP[a])*100,3)
      }
      
      #-------------------------------------
      #4.6 Publicaciones en coautoria institucional
      #-------------------------------------
      if (TP[a]!=0 & !is.na(TP[a])){
        autores = unique(datos_tmp$SCOPUS_ID)
        conteo_autores = NULL
        for (b in 1:length(autores_publicaciones)) {
          interseccion = autores %in% as.numeric(autores_publicaciones[[b]])
          conteo_autores = c(conteo_autores, length(which(interseccion)))
        }
        PCAI[a] = length(which(conteo_autores > 1))
      }
      
      #-------------------------------------
      #4.7 Porcentaje de publicaciones en coautoria institucional
      #-------------------------------------
      if (TP[a]!=0 & !is.na(TP[a])){PPCAI[a]=round((PCAI[a]/TP[a])*100,3)}
      
      #-------------------------------------
      #4.8 Posiciones por académico
      #-------------------------------------
      primer_autor <- intermedio_autor <- ultimo_autor <- 0
      
      for (idx in 1:nrow(datos_academico)) {
        local_scopus_autor <- as.character(datos_academico$SCOPUS_ID[idx])
        local_scopus_autores <- strsplit(as.character(datos_academico$AUTORES_ID[idx]), ";")[[1]]
        local_scopus_autores <- trimws(local_scopus_autores)
        
        if (length(local_scopus_autores) == 0 || is.na(local_scopus_autor) || !(local_scopus_autor %in% local_scopus_autores)) {
          next
        }
        
        if (local_scopus_autores[1] == local_scopus_autor) primer_autor <- primer_autor + 1
        if (length(local_scopus_autores) > 2 && local_scopus_autor %in% local_scopus_autores[-c(1, length(local_scopus_autores))]) intermedio_autor <- intermedio_autor + 1
        if (length(local_scopus_autores) > 1 && local_scopus_autores[length(local_scopus_autores)] == local_scopus_autor) ultimo_autor <- ultimo_autor + 1
      
      }
      
      if (TP[a] != 0) {
        PPA1[a] <- round((primer_autor / TP[a]) * 100, 3)
        PPA2[a] <- round((intermedio_autor / TP[a]) * 100, 3)
        PPA3[a] <- round((ultimo_autor / TP[a]) * 100, 3)
        PPA1_3[a] <-round(PPA1[a]+PPA3[a],3)
      }
      
    } # Fin FOR ACADéMICOS
    
    
    
    tabla = data.frame(Profesional=academicos,TP,TPj,TPp,PTPj,PTPp,PPPA,PPAjA,PPApA,TPA,PAPP,TC,PCPP,TCPP,PCP,
                       CPP,PSJR_IF,maxSJR_IF,minSJR_IF)
    tabla = as_tibble(tabla)
    
    nombres = c("","","Total de publicaciones","Total de publicaciones en revistas","Total de publicaciones en actas de conferencia",
                "Porcentaje de publicaciones en revistas","Porcentaje de publicaciones en actas de conferencia", "Promedio de publicaciones por año",
                "Promedio de publicaciones en revista por año", "Promedio de publicaciones en actas de confernecia por año",
                "Total de años activos de publicaciones","Productividad por año activo de publicación","Total de citas",
                "Total de citas por publicación","Total de publicaciones citadas","Proporción de publicaciones citadas",
                "Citas por publicación citada","Factor de impacto según promedio del índice SJR","Factor de impacto máximo según el índice SJR",
                "Factor de impacto mínimo según el índice SJR")
    
    
    
    output$table_Indexes_rendimiento = renderDataTable({
      
      # se retorna una tabla, se le dan propiedades
      datatable(tabla,
                
                callback = JS(paste0("
                var tips = ['",paste0(nombres,collapse = "','"),"'],
                    header = table.columns().header();
                for (var i = 0; i < tips.length; i++) {
                  $(header[i]).attr('title', tips[i]);
                }
                ")),
                
                class = 'display nowrap', options = list(pageLength = 20, dom = 'Bfrtp', autoWidth = TRUE, ordering = TRUE,
                                                         scrollX = TRUE,
                                                         initComplete = JS(
                                                           "function(settings, json) {",
                                                           "  MathJax.Hub.Queue(['Typeset', MathJax.Hub]);",
                                                           "}"
                                                         ),
                                                         language = list(
                                                           url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
                                                         ),
                                                         columnDefs = list(list(className = 'dt-body-right', targets = c(2:19)),
                                                                           list(width = '100%', targets = 1))
                )) #fin data table
    }) #Fin output datatable
    
    
    tabla2 <- data.frame(Profesional = academicos, NCA, ANCA, PAPA, PC, PPCA, PCAI, PPCAI, PPA1, PPA2, PPA3,PPA1_3)
    tabla2 <- as_tibble(tabla2)
    
    nombres2 <- c(
      "", "", "Total de autores(as) colaboradores(as)", 
      "Promedio de autores(as) colaboradores(as)",
      "Promedio de autores por artículo",
      "Publicaciones con coautoría",
      "Porcentaje de publicaciones con coautoría",
      "Publicaciones en coautoría institucional",
      "Porcentaje de publicaciones en coautoría institucional",
      "Porcentaje de publicaciones donde fue primer autor",
      "Porcentaje de publicaciones donde fue autor intermedio",
      "Porcentaje de publicaciones donde fue último autor",
      "Porcentaje de publicaciones donde fue primer o último autor"
    )
    
    output$table_Indexes_colaboracion = renderDataTable({
      #se retorna una tabla, se le dan propiedades
      datatable(tabla2,
                
                callback = JS(paste0("
                var tips = ['",paste0(nombres2,collapse = "','"),"'],
                    header = table.columns().header();
                for (var i = 0; i < tips.length; i++) {
                  $(header[i]).attr('title', tips[i]);
                }
                ")),
                
                class = 'display nowrap', options = list(pageLength = 20, dom = 'Bfrtp', autoWidth = TRUE, ordering = TRUE,
                                                         scrollX = TRUE,
                                                         initComplete = JS(
                                                           "function(settings, json) {",
                                                           "  MathJax.Hub.Queue(['Typeset', MathJax.Hub]);",
                                                           "}"
                                                         ),
                                                         language = list(
                                                           url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
                                                         ),
                                                         columnDefs = list(list(className = 'dt-body-right', targets = c(2:6)),
                                                                           list(width = '100%', targets = 1))
                )) #fin data table
    }) #Fin output datatable
    
  } # FIn if académicos
  
} # Cierre de la función tab_resumen