server_ayuda=function(input,output,session){
  
  #==========================================
  # Resumen
  #==========================================
  useShinyjs()
  
  #Botón - Ayuda - Título Resumen
  observeEvent(input$boton_ayuda1, {
    sendSweetAlert(
      session = session,
      title = NULL,
      text = tags$span(
        tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
        tags$p("Esta sección te ofrece una visión general y simplificada de métricas bibliométricas, resaltando aspectos clave como el ",
               tags$span(style = "color:#00A499;", "número de publicaciones indexadas"), ", la ",
               tags$span(style = "color:#00A499;", "diversidad de áreas de investigación"), ", los ",
               tags$span(style = "color:#00A499;", "tipos de publicaciones"), " y las ",
               tags$span(style = "color:#00A499;", "revistas más destacadas"), "."),
        tags$p("También incluye información sobre la ",
               tags$span(style = "color:#00A499;", "colaboración interna"), " y el ",
               tags$span(style = "color:#00A499;", "impacto de las publicaciones"), 
               " mediante indicadores como el ",
               tags$span(style = "color:#00A499;", "número de citas"), " y el ",
               tags$span(style = "color:#00A499;", "factor de impacto SJR"), "."),
        tags$p("Todo esto te ayuda a tener una mirada completa sobre la ",
               tags$span(style = "color:#00A499;", "producción científica"), 
               " de la institución o unidad que hayas seleccionado.")
      ),
      type = NULL,
      btn_labels = "Cerrar",
      html = TRUE
    )
  }) #Fin observeEvent
  
  #Botón - Ayuda - Número de publicaciones y autores
  observeEvent(input$boton_ayuda2, {
    sendSweetAlert(
      session = session,
      title = NULL,
      text = tags$span(
        tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
        tags$p("Este indicador muestra el ",
               tags$span(style = "color:#00A499;", "total de publicaciones indexadas"), " en ",
               tags$span(style = "color:#00A499;", "Web of Science (WoS)"), " o ",
               tags$span(style = "color:#00A499;", "SCOPUS"), ", durante el período seleccionado."),
        tags$p("Se consideran solo aquellas publicaciones donde los(as) investigadores(as) han indicado como filiación ",
               tags$span(style = "color:#00A499;", "la unidad y universidad seleccionadas"), 
               " en la parte superior del sitio.")
      ),
      type = NULL,
      btn_labels = "Cerrar",
      html = TRUE
    )
  }) #Fin observeEvent
  
  #Botón - Ayuda - Tipo de productos
  observeEvent(input$boton_ayuda3, {
    sendSweetAlert(
      session = session,
      title =  NULL,
      text = tags$span(
        tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
        tags$p("Este indicador muestra la ", 
               tags$span(style = "color:#00A499;", "diversidad de publicaciones"), 
               " realizadas durante el período seleccionado, según el ",
               tags$span(style = "color:#00A499;", "tipo de producto científico"), "."),
        tags$p("Se consideran publicaciones en ", 
               tags$span(style = "color:#00A499;", "revistas"), ", ", 
               tags$span(style = "color:#00A499;", "actas de conferencias"), 
               " y otras como ", 
               tags$span(style = "color:#00A499;", "capítulos de libros"), ", ", 
               tags$span(style = "color:#00A499;", "editoriales"), ", ", 
               tags$span(style = "color:#00A499;", "artículos de datos"), ", ", 
               tags$span(style = "color:#00A499;", "notas") , " o ", 
               tags$span(style = "color:#00A499;", "encuestas"), ".")
      ),
      type = NULL,
      btn_labels = "Cerrar",
      html = TRUE
    )
  }) #Fin observeEvent
  
  # Botón - Ayuda para la Distribución de Indexación de Publicaciones
  observeEvent(input$boton_ayuda4, {
    sendSweetAlert(
      session = session,
      title = NULL,
      text = tags$span(
        tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
        tags$p("Este indicador muestra cómo están distribuidas las publicaciones según su ",
               tags$span(style = "color:#00A499;", "indexación"), 
               " en el período seleccionado."),
        tags$p("Se diferencian las publicaciones que están en ",
               tags$span(style = "color:#00A499;", "Web of Science"), " y ", 
               tags$span(style = "color:#00A499;", "Scopus"), 
               ", ayudando a entender en qué plataformas académicas se difunde más la producción científica.")
      ),
      type = NULL,
      btn_labels = "Cerrar",
      html = TRUE
    )
  }) #Fin observeEvent
  
  # Botón - Ayuda para el Indicador de Áreas de la Ciencia de Computación más publicadas
  observeEvent(input$boton_ayuda5, {
    sendSweetAlert(
      session = session,
      title = NULL,
      text = tags$span(
        tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
        tags$p("Este indicador muestra las ", 
               tags$span(style = "color:#00A499;", "áreas técnicas o de especialidad"), 
               " más comunes en las publicaciones del período seleccionado."),
        tags$p("Por defecto se utilizan las áreas definidas en el currículo de la ", 
               tags$a("Association for Computing Machinery", 
                      href = "https://csed.acm.org/wp-content/uploads/2023/09/Version-Gamma.pdf", target = "_blank"), 
               ", aplicado a cada artículo mediante un modelo de lenguaje."),
        tags$p("Las posibles áreas incluyen:"),
        tags$ol(
          tags$li(tags$span(style = "color:#00A499;", "Algorithms and Complexity")),
          tags$li(tags$span(style = "color:#00A499;", "Architecture and Organization")),
          tags$li(tags$span(style = "color:#00A499;", "Artificial Intelligence")),
          tags$li(tags$span(style = "color:#00A499;", "Data Management")),
          tags$li(tags$span(style = "color:#00A499;", "Foundations of Programming Languages")),
          tags$li(tags$span(style = "color:#00A499;", "Graphics and Interactive Techniques")),
          tags$li(tags$span(style = "color:#00A499;", "Human-Computer Interaction")),
          tags$li(tags$span(style = "color:#00A499;", "Mathematical and Statistical Foundations")),
          tags$li(tags$span(style = "color:#00A499;", "Networking and Communication")),
          tags$li(tags$span(style = "color:#00A499;", "Operating Systems")),
          tags$li(tags$span(style = "color:#00A499;", "Parallel and Distributed Computing")),
          tags$li(tags$span(style = "color:#00A499;", "Security")),
          tags$li(tags$span(style = "color:#00A499;", "Society, Ethics, and Professionalism")),
          tags$li(tags$span(style = "color:#00A499;", "Software Development Fundamentals")),
          tags$li(tags$span(style = "color:#00A499;", "Software Engineering")),
          tags$li(tags$span(style = "color:#00A499;", "Specialized Platform Development")),
          tags$li(tags$span(style = "color:#00A499;", "Systems Fundamentals")),
          tags$li(tags$span(style = "color:#00A499;", "No se puede clasificar como parte del campo de la ciencia de la computación."))
        )
      ),
      type = "NULL",
      btn_labels = "Cerrar",
      html = TRUE
    )
  }) #Fin observeEvent
  
  # Botón - Ayuda para el Indicador de Áreas de Aplicación más publicadas
  observeEvent(input$boton_ayuda6, {
    sendSweetAlert(
      session = session,
      title = NULL,
      text = tags$span(
        tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
        tags$p("Este indicador muestra las ", 
               tags$span(style = "color:#00A499;", "áreas de aplicación más frecuentes"), 
               " en las publicaciones del período seleccionado."),
        tags$p("Las áreas se definen a partir del sistema de clasificación de ", 
               tags$a("SCIMAGO Journal Rank (SJR)", href = "https://www.scimagojr.com/", target = "_blank"), 
               ", aplicado a cada revista o conferencia."),
        tags$p("Estas áreas incluyen:"),
        tags$ol(
          tags$li(tags$span(style = "color:#00A499;", "Agricultural and Biological Sciences")),
          tags$li(tags$span(style = "color:#00A499;", "Arts and Humanities")),
          tags$li(tags$span(style = "color:#00A499;", "Biochemistry, Genetics and Molecular Biology")),
          tags$li(tags$span(style = "color:#00A499;", "Business, Management and Accounting")),
          tags$li(tags$span(style = "color:#00A499;", "Chemical Engineering")),
          tags$li(tags$span(style = "color:#00A499;", "Chemistry")),
          tags$li(tags$span(style = "color:#00A499;", "Computer Science")),
          tags$li(tags$span(style = "color:#00A499;", "Decision Sciences")),
          tags$li(tags$span(style = "color:#00A499;", "Dentistry")),
          tags$li(tags$span(style = "color:#00A499;", "Earth and Planetary Sciences")),
          tags$li(tags$span(style = "color:#00A499;", "Economics, Econometrics and Finance")),
          tags$li(tags$span(style = "color:#00A499;", "Energy")),
          tags$li(tags$span(style = "color:#00A499;", "Engineering")),
          tags$li(tags$span(style = "color:#00A499;", "Environmental Science")),
          tags$li(tags$span(style = "color:#00A499;", "Health Professions")),
          tags$li(tags$span(style = "color:#00A499;", "Immunology and Microbiology")),
          tags$li(tags$span(style = "color:#00A499;", "Materials Science")),
          tags$li(tags$span(style = "color:#00A499;", "Mathematics")),
          tags$li(tags$span(style = "color:#00A499;", "Medicine")),
          tags$li(tags$span(style = "color:#00A499;", "Multidisciplinary")),
          tags$li(tags$span(style = "color:#00A499;", "Neuroscience")),
          tags$li(tags$span(style = "color:#00A499;", "Nursing")),
          tags$li(tags$span(style = "color:#00A499;", "Pharmacology, Toxicology and Pharmaceutics")),
          tags$li(tags$span(style = "color:#00A499;", "Physics and Astronomy")),
          tags$li(tags$span(style = "color:#00A499;", "Psychology")),
          tags$li(tags$span(style = "color:#00A499;", "Social Sciences")),
          tags$li(tags$span(style = "color:#00A499;", "Veterinary"))
        )
      ),
      type = "NULL",
      btn_labels = "Cerrar",
      html = TRUE
    )
  }) #Fin observeEvent
  
  
  
  #Botón - Ayuda - Revista más publicada
  observeEvent(input$boton_ayuda7, {
    sendSweetAlert(
      session = session,
      title =  NULL,
      text = tags$span(
        tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
        tags$p("Este indicador muestra la ", 
               tags$span(style = "color:#00A499;", "revista o conferencia"), 
               " donde los(as) investigadores(as) han publicado con mayor frecuencia en el período seleccionado.")
      ),
      type = NULL,
      btn_labels = "Cerrar",
      html = TRUE
    )
  }) #Fin observeEvent
  
  
  # Botón - Ayuda para el Indicador de Áreas de la Ciencia de Computación menos publicadas
  observeEvent(input$boton_ayuda8, {
    sendSweetAlert(
      session = session,
      title = NULL,
      text = tags$span(
        tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
        tags$p("Este indicador muestra las ", 
               tags$span(style = "color:#00A499;", "áreas de especialidad menos frecuentes"), 
               " en las publicaciones del período seleccionado."),
        tags$p("Por defecto se utilizan las áreas de la ", 
               tags$span(style = "color:#00A499;", "Ciencia de la Computación"), 
               " definidas por el currículo de la ",
               tags$a("Association for Computing Machinery", 
                      href = "https://csed.acm.org/wp-content/uploads/2023/09/Version-Gamma.pdf", 
                      target = "_blank"), "."),
        tags$p("Cada publicación fue clasificada automáticamente en una de las siguientes áreas:"),
        tags$ol(
          tags$li("Algorithms and Complexity"),
          tags$li("Architecture and Organization"),
          tags$li("Artificial Intelligence"),
          tags$li("Data Management"),
          tags$li("Foundations of Programming Languages"),
          tags$li("Graphics and Interactive Techniques"),
          tags$li("Human-Computer Interaction"),
          tags$li("Mathematical and Statistical Foundations"),
          tags$li("Networking and Communication"),
          tags$li("Operating Systems"),
          tags$li("Parallel and Distributed Computing"),
          tags$li("Security"),
          tags$li("Society, Ethics, and Professionalism"),
          tags$li("Software Development Fundamentals"),
          tags$li("Software Engineering"),
          tags$li("Specialized Platform Development"),
          tags$li("Systems Fundamentals"),
          tags$li("No se puede clasificar como parte del campo de la ciencia de la computación.")
        ),
        tags$p(tags$em("Nota: Si estás usando el analizador con otra carrera, estas áreas podrían no reflejar su especialidad."))
      ),
      type = "NULL",
      btn_labels = "Cerrar",
      html = TRUE
    )
  }) #Fin observeEvent
  
  
  # Botón - Ayuda para el Indicador de Áreas de la Ciencia de Computación menos publicadas
  observeEvent(input$boton_ayuda8, {
    sendSweetAlert(
      session = session,
      title = NULL,
      text = tags$span(
        tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
        tags$p("Este indicador muestra las ", 
               tags$span(style = "color:#00A499;", "áreas de especialidad menos frecuentes"), 
               " en las publicaciones del período seleccionado."),
        tags$p("Por defecto se utilizan las áreas de la ", 
               tags$span(style = "color:#00A499;", "Ciencia de la Computación"), 
               " definidas en el currículo de la ",
               tags$a("Association for Computing Machinery", 
                      href = "https://csed.acm.org/wp-content/uploads/2023/09/Version-Gamma.pdf", 
                      target = "_blank"), "."),
        tags$p("Cada publicación fue clasificada automáticamente en una de las siguientes áreas:"),
        tags$ol(
          tags$li("Algorithms and Complexity"),
          tags$li("Architecture and Organization"),
          tags$li("Artificial Intelligence"),
          tags$li("Data Management"),
          tags$li("Foundations of Programming Languages"),
          tags$li("Graphics and Interactive Techniques"),
          tags$li("Human-Computer Interaction"),
          tags$li("Mathematical and Statistical Foundations"),
          tags$li("Networking and Communication"),
          tags$li("Operating Systems"),
          tags$li("Parallel and Distributed Computing"),
          tags$li("Security"),
          tags$li("Society, Ethics, and Professionalism"),
          tags$li("Software Development Fundamentals"),
          tags$li("Software Engineering"),
          tags$li("Specialized Platform Development"),
          tags$li("Systems Fundamentals"),
          tags$li("No se puede clasificar como parte del campo de la ciencia de la computación.")
        ),
        tags$p(tags$em("Nota: Si estás usando el analizador con otra carrera, estas áreas podrían no reflejar su especialidad."))
      ),
      type = "NULL",
      btn_labels = "Cerrar",
      html = TRUE
    )
  }) #Fin observeEvent
  

  
#Botón - Ayuda - Número de autores por publicación
observeEvent(input$boton_ayuda10, {
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
      tags$p("Este indicador muestra el ", 
             tags$span(style = "color:#00A499;", "número promedio de autores por publicación"), 
             ", lo que permite estimar el nivel de colaboración entre investigadores(as)."),
      tags$p("Un promedio más alto refleja una ", 
             tags$span(style = "color:#00A499;", "mayor colaboración"), 
             " en la producción científica, mientras que valores bajos pueden indicar trabajos individuales o con menor interacción entre autores.")
    ),
    type = NULL,
    btn_labels = "Cerrar",
    html = TRUE
  )
}) #Fin observeEvent

  
#Botón - Ayuda - Publicación más citada
observeEvent(input$boton_ayuda11, {
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
      tags$p("Este indicador destaca la ", 
             tags$span(style = "color:#00A499;", "publicación con más citas"), 
             " dentro del período seleccionado, según los registros de ", 
             tags$span(style = "color:#00A499;", "SCOPUS"), "."),
      tags$p("Una alta cantidad de citas refleja el ", 
             tags$span(style = "color:#00A499;", "impacto e influencia"), 
             " de un trabajo en la comunidad científica, mostrando qué investigaciones han sido más reconocidas por otros(as) investigadores(as).")
    ),
    type = NULL,
    btn_labels = "Cerrar",
    html = TRUE
  )
}) #Fin observeEvent

# Botón - Ayuda - Publicación con más impacto SJR
observeEvent(input$boton_ayuda12, {
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
      tags$p("Este indicador muestra la ", 
             tags$span(style = "color:#00A499;", "publicación con mayor puntaje SJR"), 
             " durante el período seleccionado."),
      tags$p("El ", 
             tags$span(style = "color:#00A499;", "SJR (SCImago Journal Rank)"), 
             " mide el impacto de una publicación científica según la ", 
             tags$span(style = "color:#00A499;", "calidad y prestigio de las revistas"), 
             " que la citan. Citas de revistas mejor posicionadas aportan más al puntaje.")
    ),
    type = NULL,
    btn_labels = "Cerrar",
    html = TRUE
  )
}) #Fin observeEvent

  
# Botón - Ayuda - Porcentaje de publicaciones que comparte más de un autor(a) institucional
observeEvent(input$boton_ayuda13, {
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
      tags$p("Este indicador muestra el ", 
             tags$span(style = "color:#00A499;", "porcentaje de publicaciones que incluyen a más de un(a) investigador(a) de la misma institución"), 
             "."),
      tags$p("Un valor más alto indica un mayor nivel de ", 
             tags$span(style = "color:#00A499;", "colaboración interna"), 
             " y trabajo conjunto entre miembros de la unidad seleccionada.")
    ),
    type = NULL,
    btn_labels = "Cerrar",
    html = TRUE
  )
}) #Fin observeEvent
  
  #==========================================
  # Indicadores
  #==========================================
# Botón - Ayuda - Indicadores bibliométricos
observeEvent(input$boton_ayuda14, {
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
      tags$p("Esta sección presenta diferentes ",
             tags$span(style = "color:#00A499;", "indicadores bibliométricos"), 
             " que ayudan a entender mejor la producción científica de una institución o de sus investigadores(as)."),
      tags$p("Incluye indicadores de ", 
             tags$span(style = "color:#00A499;", "rendimiento"), 
             " (como número de publicaciones y citas) y de ", 
             tags$span(style = "color:#00A499;", "colaboración"), 
             " (como cantidad de coautores o publicaciones en conjunto)."),
      tags$p("Puedes revisar los resultados según el tipo de indexación: ", 
             tags$span(style = "color:#00A499;", "Web of Science"), 
             " o ", 
             tags$span(style = "color:#00A499;", "Scopus"), 
             ".")
    ),
    type = NULL,
    btn_labels = "Cerrar",
    html = TRUE
  )
}) #Fin observeEvent
  
# Botón - Ayuda - Indicadores bibliométricos de rendimiento
observeEvent(input$boton_ayuda15, {
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
      tags$p("Esta sección muestra los ", 
             tags$span(style = "color:#00A499;", "indicadores de rendimiento"), 
             " más relevantes para evaluar la productividad científica."),
      tags$p("Incluye datos como el ", 
             tags$span(style = "color:#00A499;", "número de publicaciones"), ", la ",
             tags$span(style = "color:#00A499;", "cantidad de citas"), " recibidas, el ",
             tags$span(style = "color:#00A499;", "promedio de publicaciones por año"), 
             " y el ", 
             tags$span(style = "color:#00A499;", "factor de impacto SJR"), "."),
      tags$p("Permite observar cómo ha sido la producción de los(as) investigadores(as) en el tiempo y el alcance que han tenido sus trabajos.")
    ),
    type = NULL,
    btn_labels = "Cerrar",
    html = TRUE
  )
}) #Fin observeEvent
  
  
# Botón - Ayuda - Indicadores de Colaboración
observeEvent(input$boton_ayuda16, {
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
      tags$p("Esta sección muestra los ",
             tags$span(style = "color:#00A499;", "indicadores de colaboración"), 
             " científica entre los(as) investigadores(as)."),
      tags$p("Incluye métricas como el ",
             tags$span(style = "color:#00A499;", "número promedio de autores por publicación"), ", el ",
             tags$span(style = "color:#00A499;", "porcentaje de publicaciones en coautoría"), 
             " y el nivel de ",
             tags$span(style = "color:#00A499;", "colaboración institucional"), 
             " entre miembros de la misma universidad."),
      tags$p("Estos indicadores permiten entender cómo se construyen redes de trabajo y cuán frecuente es la colaboración en las publicaciones.")
    ),
    type = NULL,
    btn_labels = "Cerrar",
    html = TRUE
  )
}) #Fin observeEvent
  
#==========================================
# PERFILES
#==========================================

  #--------------------------------
  # Botón - Ayuda - Frecuencia de palabras clave
  #--------------------------------
observeEvent(input$boton_ayuda17, {
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
      tags$p("Este indicador muestra las ", 
             tags$span(style = "color:#00A499;", "palabras clave más frecuentes"), 
             " utilizadas en las publicaciones del perfil seleccionado."),
      tags$p("Las barras reflejan cuántas veces aparece cada término, permitiendo identificar los ",
             tags$span(style = "color:#00A499;", "conceptos más recurrentes"), 
             " en la producción científica de los(as) investigadores(as).")
    ),
    type = NULL,
    btn_labels = "Cerrar",
    html = TRUE
  )
}) #Fin observeEvent
  
#Botón - Ayuda - Perfiles
observeEvent(input$boton_ayuda18, {
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
      tags$p("Esta sección ofrece una visión general sobre el perfil académico del cuerpo docente, mostrando datos como el ",
             tags$span(style = "color:#00A499;", "número de publicaciones"), ", el ",
             tags$span(style = "color:#00A499;", "tipo de indexación"), " y la ",
             tags$span(style = "color:#00A499;", "distribución por áreas de investigación"), "."),
      tags$p("Permite explorar y comparar la producción científica de los(as) investigadores(as) de la universidad.")
    ),
    type = NULL,
    btn_labels = "Cerrar",
    html = TRUE
  )
}) #Fin observeEvent
  
# Botón - Ayuda - Perfiles
observeEvent(input$boton_ayuda19, {
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
      tags$p("Esta sección utiliza un ", 
             tags$span(style = "color:#00A499;", "gráfico de Sankey"), 
             " para mostrar cómo se relacionan las ",
             tags$span(style = "color:#00A499;", "áreas de investigación"), ", las ",
             tags$span(style = "color:#00A499;", "palabras clave"), " y las ",
             tags$span(style = "color:#00A499;", "publicaciones"), "."),
      tags$p("Cada conexión representa una combinación frecuente de temas en las publicaciones. Las líneas más gruesas indican relaciones más comunes, lo que permite identificar ",
             tags$span(style = "color:#00A499;", "patrones temáticos"), " en la producción científica.")
    ),
    type = NULL,
    btn_labels = "Cerrar",
    html = TRUE
  )
}) #Fin observeEvent

# Botón - Ayuda - Palabras clave (nube / comparación)
observeEvent(input$boton_ayuda20, {
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
      
      tags$p("Esta sección permite analizar las ", 
             tags$span(style = "color:#00A499;", "palabras clave"), 
             " más relevantes asociadas a las publicaciones académicas."),
      
      tags$p("Si se selecciona un ", tags$span(style = "color:#00A499;", "perfil institucional"), 
             ", se muestran las 20 palabras clave más frecuentes, ordenadas por su aparición en el total de publicaciones."),
      
      tags$p("Si se elige un ", tags$span(style = "color:#00A499;", "perfil por autor(a)"), 
             ", se activa una comparación con otro(a) académico(a) seleccionado(a). Las diferencias se calculan usando el ",
             tags$span(style = "color:#00A499;", "test de Chi-cuadrado"), 
             ", destacando en ", tags$span(style = "color:green;", "verde"), 
             " las palabras sobre-representadas y en ", tags$span(style = "color:gray;", "gris"), 
             " las sub-representadas."),
      
      tags$p("Esta herramienta permite detectar rápidamente los temas más característicos y distintivos de cada perfil.")
    ),
    type = NULL,
    btn_labels = "Cerrar",
    html = TRUE
  )
}) #Fin observeEvent
  
# Botón - Ayuda - Perfiles
observeEvent(input$boton_ayuda21, {
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
      
      tags$p("Este indicador muestra los ", 
             tags$span(style = "color:#00A499;", "lugares de publicación más frecuentes"), 
             " utilizados por los(as) investigadores(as) de la universidad."),
      
      tags$p("Se destacan las ", 
             tags$span(style = "color:#00A499;", "revistas"), " y ", 
             tags$span(style = "color:#00A499;", "actas de conferencias"), 
             " donde se ha publicado más durante el período seleccionado."),
      
      tags$p("Permite identificar los ", 
             tags$span(style = "color:#00A499;", "canales de difusión más utilizados"), 
             " y comprender mejor los espacios donde circula la producción científica.")
    ),
    type = NULL,
    btn_labels = "Cerrar",
    html = TRUE
  )
}) #Fin observeEvent

  
  
#Botón - Ayuda - Perfiles
observeEvent(input$boton_ayuda22, {
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
      
      tags$p("Este indicador muestra un ", 
             tags$span(style = "color:#00A499;", "gráfico de relaciones"), 
             " que conecta a los(as) investigadores(as) según sus áreas de estudio y colaboraciones."),
      
      tags$p("Permite visualizar de forma clara las ", 
             tags$span(style = "color:#00A499;", "colaboraciones y coautorías"), 
             " más relevantes dentro de la universidad."),
      
      tags$p("Cada ", tags$span(style = "color:#00A499;", "nodo"), " representa a un(a) autor(a), y los enlaces indican vínculos por publicaciones compartidas."),
      
      tags$p("Puedes usar los ", 
             tags$span(style = "color:#00A499;", "selectores interactivos"), 
             " para filtrar el gráfico y explorar distintas configuraciones según tus intereses.")
    ),
    type = NULL,
    btn_labels = "Cerrar",
    html = TRUE
  )
}) #Fin observeEvent

  
#Botón - Ayuda - Perfiles
observeEvent(input$boton_ayuda23, {
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
      
      tags$p("Esta sección permite analizar la ", 
             tags$span(style = "color:#00A499;", "presencia de temas de investigación"), 
             " en las publicaciones, dentro de un rango de años determinado."),
      
      tags$p("El gráfico muestra qué ", 
             tags$span(style = "color:#00A499;", "académicos(as)"), 
             " han trabajado en los distintos ", 
             tags$span(style = "color:#00A499;", "dominios seleccionados"),
             ", como áreas de especialidad, áreas de aplicación, subáreas o palabras clave."),
      
      tags$p("Esto entrega una visión clara del ", 
             tags$span(style = "color:#00A499;", "enfoque investigativo"), 
             " de cada autor(a), y cómo ha evolucionado su trabajo en ese ámbito a lo largo del tiempo.")
    ),
    type = NULL,
    btn_labels = "Cerrar",
    html = TRUE
  )
}) #Fin observeEvent
# Botón - Ayuda - Dendrogramas jerárquicos
observeEvent(input$boton_ayuda24, {
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$h3(img(src = 'IMG/img_pregunta.png', height = '20px', width = '20px'), " Información"), tags$br(),
      
      tags$p("Esta sección muestra dos ", 
             tags$span(style = "color:#00A499;", "dendrogramas jerárquicos"), 
             " que agrupan a los(as) académicos(as) según la similitud de su producción científica."),
      
      tags$p("El primero usa ", 
             tags$span(style = "color:#00A499;", "distancia euclidiana"), 
             " y el segundo una métrica basada en ", 
             tags$span(style = "color:#00A499;", "correlación"), 
             ". Ambos se construyen a partir de un vector combinado de ",
             tags$span(style = "color:#00A499;", "palabras clave, resúmenes, categorías y áreas temáticas"), "."),
      
      tags$p("Esta visualización permite identificar ", 
             tags$span(style = "color:#00A499;", "grupos temáticos"), 
             " y explorar relaciones de cercanía entre investigadores(as), en función de su línea de trabajo.")
    ),
    type = NULL,
    btn_labels = "Cerrar",
    html = TRUE
  )
}) #Fin observeEvent

  
  
}