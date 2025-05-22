# Crear el nuevo dataframe con nombres cortos
estwdf <- estodf %>%
  rename(
    # Variables de identificación y contexto
    year = AGNO,
    id = ID,
    dep = COD_DEPE2,
    est = ESTRATO_ANALITICO,
    rural = RURAL_RBD,
    gen = Q001,
    
    # Percepción sobre establecimiento
    esc01 = Q004SQ001,
    esc02 = Q004SQ002,
    
    # Autopercepción forma de pensar
    pns01 = Q005SQ001,
    pns02 = Q005SQ002,
    pns03 = Q005SQ003,
    pns04 = Q005SQ004,
    
    # Percepción sobre clases
    cls01 = Q006SQ001,
    cls02 = Q006SQ002,
    cls03 = Q006SQ003,
    cls04 = Q006SQ004,
    cls05 = Q006SQ005,
    cls06 = Q006SQ006,
    cls07 = Q006SQ007,
    
    # Frecuencia de actividades en clases
    fac01 = Q007SQ001,
    fac02 = Q007SQ002,
    fac03 = Q007SQ003,
    fac04 = Q007SQ004,
    fac05 = Q007SQ005,
    
    # Percepción sobre profesores
    pro01 = Q008SQ001,
    pro02 = Q008SQ002,
    pro03 = Q008SQ003,
    
    # Equipos en el hogar
    eqh01 = Q009SQ001,
    eqh02 = Q009SQ002,
    eqh03 = Q009SQ003,
    eqh04 = Q009SQ004,
    eqh05 = Q009SQ005,
    eqh06 = Q009SQ006,
    
    # Edad inicio uso tecnologías
    edpc = Q010,
    celdat = Q011P001,
    edcel = Q011P002,
    
    # Uso de celular en clases
    cel01 = Q011P003SQ001,
    cel02 = Q011P003SQ002,
    cel03 = Q011P003SQ003,
    
    # Lugares de uso de internet
    net01 = Q012SQ001,
    net02 = Q012SQ002,
    net03 = Q012SQ003,
    net04 = Q012SQ004,
    net05 = Q012SQ005,
    
    # Actividades en internet
    web01 = Q013SQ001,
    web02 = Q013SQ002,
    web03 = Q013SQ003,
    web04 = Q013SQ004,
    web05 = Q013SQ005,
    web06 = Q013SQ006,
    web07 = Q013SQ007,
    
    # Actividades tecnológicas en la escuela
    ese01 = Q014SQ001,
    ese02 = Q014SQ002,
    ese03 = Q014SQ004,
    ese04 = Q014SQ005,
    
    # Uso de dispositivos por asignatura
    asg01 = Q015SQ001,
    asg02 = Q015SQ002,
    asg03 = Q015SQ003,
    asg04 = Q015SQ009,
    asg05 = Q015SQ005,
    asg06 = Q015SQ006,
    asg07 = Q015SQ007,
    asg08 = Q015SQ010,
    
    # Formación en herramientas digitales
    for01 = Q016SQ001,
    for02 = Q016SQ002,
    for03 = Q016SQ003,
    for04 = Q016SQ004,
    
    # Participación en actividades tecnológicas específicas
    tec01 = Q017P001SQ001,
    tec02 = Q017P001SQ002,
    tec03 = Q017P001SQ003,
    tec04 = Q017P001SQ004,
    tec05 = Q017P001SQ005,
    
    # Uso de programas específicos
    prg01 = Q017P002SQ001,
    prg02 = Q017P002SQ002,
    prg03 = Q017P002SQ003,
    prg04 = Q017P002SQ004,
    prg05 = Q017P002SQ005,
    prg06 = Q017P002SQ006,
    prg07 = Q017P002SQ007,
    prg08 = Q017P002SQ008,
    prg09 = Q017P002SQ009,
    prg10 = Q017P002SQ010,
    
    # Conductas digitales problemáticas
    cib01 = Q018SQ001,
    cib02 = Q018SQ002,
    cib03 = Q018SQ003,
    cib04 = Q018SQ004,
    cib05 = Q018SQ005,
    cib06 = Q018SQ006,
    
    # Formación en ciudadanía digital
    ciu01 = Q019SQ001,
    ciu02 = Q019SQ002,
    ciu03 = Q019SQ003,
    ciu04 = Q019SQ004,
    
    # Autopercepción de habilidades digitales
    hab01 = Q020SQ001,
    hab02 = Q020SQ002,
    hab03 = Q020SQ003,
    hab04 = Q020SQ004,
    hab05 = Q020SQ005,
    hab06 = Q020SQ006,
    hab07 = Q020SQ007,
    hab08 = Q020SQ008,
    hab09 = Q020SQ009,
    hab10 = Q020SQ010,
    hab11 = Q020SQ011,
    
    # Actitudes hacia la tecnología
    att01 = Q021SQ001,
    att02 = Q021SQ002,
    att03 = Q021SQ003,
    att04 = Q021SQ004,
    att05 = Q021SQ005,
    att06 = Q021SQ006,
    att07 = Q021SQ007,
    
    # Interés en temas tecnológicos
    int01 = Q022SQ001,
    int02 = Q022SQ002,
    int03 = Q022SQ003,
    int04 = Q022SQ004,
    int05 = Q022SQ005,
    int06 = Q022SQ006,
    int07 = Q022SQ007,
    int08 = Q022SQ008,
    int09 = Q022SQ009,
    
    # Valoración de tecnologías
    val01 = Q023SQ001,
    val02 = Q023SQ002,
    val03 = Q023SQ003,
    val04 = Q023SQ004,
    val05 = Q023SQ005,
    val06 = Q023SQ006,
    val07 = Q023SQ007,
    val08 = Q023SQ008,
    
    # Variables derivadas/índices
    wgt = WGT_EST_FINAL,
    ift = IND_ACCNES_FORM_TD,
    ifc = IND_FREC_FORM_CD,
    ifa = IND_FREC_ACTS_TD,
    ifui = IND_FREC_USO_INT,
    ifud = IND_FREC_USO_DD,
    ihd1 = IND_AUTP_HAB_DIG1,
    ihd2 = IND_AUTP_HAB_DIG2,
    imt1 = IND_NIVEL_MOT_TIC1,
    imt2 = IND_NIVEL_MOT_TIC2,
    ivte = IND_VAL_TD_EST,
    iafp = IND_AUTP_FORM_PENSAR,
    ippe = IND_PERC_PROT_EST,
    ifaa = IND_FREC_APR_ACT,
    iaac = IND_AUTP_APR_CLASES,
    ipitd = IND_PERC_IMP_TD
  )


# Crear dataframe con los nombres y descripciones
var_names_df <- data.frame(
  VARIABLE_NUEVA = c(
    "year", "id", "dep", "est", "rural", "gen",
    "esc01", "esc02",
    "pns01", "pns02", "pns03", "pns04",
    "cls01", "cls02", "cls03", "cls04", "cls05", "cls06", "cls07",
    "fac01", "fac02", "fac03", "fac04", "fac05",
    "pro01", "pro02", "pro03",
    "eqh01", "eqh02", "eqh03", "eqh04", "eqh05", "eqh06",
    "edpc", "celdat", "edcel",
    "cel01", "cel02", "cel03",
    "net01", "net02", "net03", "net04", "net05",
    "web01", "web02", "web03", "web04", "web05", "web06", "web07",
    "ese01", "ese02", "ese03", "ese04",
    "asg01", "asg02", "asg03", "asg04", "asg05", "asg06", "asg07", "asg08",
    "for01", "for02", "for03", "for04",
    "tec01", "tec02", "tec03", "tec04", "tec05",
    "prg01", "prg02", "prg03", "prg04", "prg05", "prg06", "prg07", "prg08", "prg09", "prg10",
    "cib01", "cib02", "cib03", "cib04", "cib05", "cib06",
    "ciu01", "ciu02", "ciu03", "ciu04",
    "hab01", "hab02", "hab03", "hab04", "hab05", "hab06", "hab07", "hab08", "hab09", "hab10", "hab11",
    "att01", "att02", "att03", "att04", "att05", "att06", "att07",
    "int01", "int02", "int03", "int04", "int05", "int06", "int07", "int08", "int09",
    "val01", "val02", "val03", "val04", "val05", "val06", "val07", "val08",
    "wgt", "ift", "ifc", "ifa", "ifui", "ifud", "ihd1", "ihd2", "imt1", "imt2", "ivte", "iafp", "ippe", "ifaa", "iaac", "ipitd"
    
  ),
  VARIABLE_ORIGINAL = c(
    "AGNO", "ID", "COD_DEPE2", "ESTRATO_ANALITICO", "RURAL_RBD", "Q001",
    "Q004SQ001", "Q004SQ002", 
    "Q005SQ001", "Q005SQ002", "Q005SQ003", "Q005SQ004",
    "Q006SQ001", "Q006SQ002", "Q006SQ003", "Q006SQ004", "Q006SQ005", "Q006SQ006", "Q006SQ007",
    "Q007SQ001", "Q007SQ002", "Q007SQ003", "Q007SQ004", "Q007SQ005",
    "Q008SQ001", "Q008SQ002", "Q008SQ003",
    "Q009SQ001", "Q009SQ002", "Q009SQ003", "Q009SQ004", "Q009SQ005", "Q009SQ006",
    "Q010", "Q011P001", "Q011P002",
    "Q011P003SQ001", "Q011P003SQ002", "Q011P003SQ003",
    "Q012SQ001", "Q012SQ002", "Q012SQ003", "Q012SQ004", "Q012SQ005",
    "Q013SQ001", "Q013SQ002", "Q013SQ003", "Q013SQ004", "Q013SQ005", "Q013SQ006", "Q013SQ007",
    "Q014SQ001", "Q014SQ002", "Q014SQ004", "Q014SQ005",
    "Q015SQ001", "Q015SQ002", "Q015SQ003", "Q015SQ009", "Q015SQ005", "Q015SQ006", "Q015SQ007", "Q015SQ010",
    "Q016SQ001", "Q016SQ002", "Q016SQ003", "Q016SQ004",
    "Q017P001SQ001", "Q017P001SQ002", "Q017P001SQ003", "Q017P001SQ004", "Q017P001SQ005",
    "Q017P002SQ001", "Q017P002SQ002", "Q017P002SQ003", "Q017P002SQ004", "Q017P002SQ005",
    "Q017P002SQ006", "Q017P002SQ007", "Q017P002SQ008", "Q017P002SQ009", "Q017P002SQ010",
    "Q018SQ001", "Q018SQ002", "Q018SQ003", "Q018SQ004", "Q018SQ005", "Q018SQ006",
    "Q019SQ001", "Q019SQ002", "Q019SQ003", "Q019SQ004",
    "Q020SQ001", "Q020SQ002", "Q020SQ003", "Q020SQ004", "Q020SQ005", "Q020SQ006", 
    "Q020SQ007", "Q020SQ008", "Q020SQ009", "Q020SQ010", "Q020SQ011",
    "Q021SQ001", "Q021SQ002", "Q021SQ003", "Q021SQ004", "Q021SQ005", "Q021SQ006", "Q021SQ007",
    "Q022SQ001", "Q022SQ002", "Q022SQ003", "Q022SQ004", "Q022SQ005", "Q022SQ006", 
    "Q022SQ007", "Q022SQ008", "Q022SQ009",
    "Q023SQ001", "Q023SQ002", "Q023SQ003", "Q023SQ004", "Q023SQ005", "Q023SQ006", "Q023SQ007", "Q023SQ008",
    "WGT_EST_FINAL", "IND_ACCNES_FORM_TD", "IND_FREC_FORM_CD", "IND_FREC_ACTS_TD", 
    "IND_FREC_USO_INT", "IND_FREC_USO_DD", "IND_AUTP_HAB_DIG1", "IND_AUTP_HAB_DIG2",
    "IND_NIVEL_MOT_TIC1", "IND_NIVEL_MOT_TIC2", "IND_VAL_TD_EST", "IND_AUTP_FORM_PENSAR",
    "IND_PERC_PROT_EST", "IND_FREC_APR_ACT", "IND_AUTP_APR_CLASES", "IND_PERC_IMP_TD"
  ),
  DESCRIPCION = c(
    "Año escolar", "Identificador único", "Dependencia administrativa", "Estrato analítico", 
    "Indicador de ruralidad", "Género del estudiante",
    "Interés del equipo directivo en opinión de estudiantes", "Participación estudiantil en espacios con profesores/directivos",
    "Ideas novedosas para resolver problemas", "Persistencia ante dificultades", 
    "Valoración de ideas de compañeros", "Aprendizaje de errores",
    "Tiempo pasando materia sin participación", "Apoyo de profesores", "Diversidad de métodos de enseñanza", 
    "Aprendizaje de creatividad", "Aprendizaje de pensamiento crítico", "Aprendizaje de expresión de ideas", 
    "Aprendizaje de trabajo en equipo",
    "Frecuencia de trabajo en equipo", "Frecuencia de trabajos de investigación", "Frecuencia de debates", 
    "Frecuencia de fabricación de maquetas/prototipos", "Frecuencia de resolución de problemas reales",
    "Profesores acogen sugerencias", "Profesores incorporan temas de interés", "Proyectos interdisciplinarios",
    "Uso de computador de escritorio", "Uso de notebook", "Uso de tablet", "Conexión fija a internet", 
    "Uso de teléfono celular", "Uso de consola de videojuegos",
    "Edad inicio uso computadores/tablets", "Tiene internet en celular", "Edad primer celular con internet",
    "Uso de celular en clases para fines recreativos", "Uso de celular en clases para actividades educativas", 
    "Uso de celular para registro de clase",
    "Frecuencia uso internet en casa", "Frecuencia uso internet en casa de amigos/familiares", 
    "Frecuencia uso internet en escuela/liceo", "Frecuencia uso internet en centro público", 
    "Frecuencia uso internet en vía pública",
    "Frecuencia uso redes sociales", "Frecuencia llamadas/videoconferencias", "Frecuencia ver videos tutoriales", 
    "Frecuencia búsqueda de información", "Frecuencia estudio/tareas con internet", 
    "Frecuencia coordinación tareas con compañeros", "Frecuencia participación en grupos de opinión",
    "Frecuencia búsqueda/selección información", "Frecuencia uso Word/Excel/PowerPoint", 
    "Frecuencia uso aplicaciones educativas", "Frecuencia pruebas/quizzes digitales",
    "Frecuencia uso dispositivos en Lenguaje", "Frecuencia uso dispositivos en Matemática", 
    "Frecuencia uso dispositivos en Ciencias", "Frecuencia uso dispositivos en Historia", 
    "Frecuencia uso dispositivos en Inglés", "Frecuencia uso dispositivos en Tecnología", 
    "Frecuencia uso dispositivos en Artes/Música", "Frecuencia uso dispositivos en Ed. Física",
    "Formación en procesadores de texto/planillas", "Formación en uso de internet", 
    "Formación en uso de correo electrónico", "Formación en herramientas colaboración",
    "Participación en talleres de programación", "Participación en talleres de robótica", 
    "Participación con realidad aumentada", "Uso de IA (ChatGPT, etc.)", "Uso de impresoras 3D",
    "Uso de Scratch", "Uso de Code Studio", "Uso de La Hora del Código", "Uso de App Inventor", 
    "Uso de Snap", "Uso de Python", "Uso de JavaScript", "Uso de CSS", "Uso de HTML", "Uso de C o C++",
    "Compartir fotos/memes para ridiculizar", "Mensajes ofensivos en redes sociales", 
    "Mensajes ofensivos por correo/chat", "Copiar trabajos desde internet", 
    "Uso de celular para copiar en pruebas", "Amenazas a través de internet",
    "Formación en administración datos/privacidad", "Formación en ciberbullying/convivencia", 
    "Formación en uso responsable de información", "Formación en identificar fake news",
    "Habilidad para crear/editar documentos", "Habilidad para enviar archivos por correo", 
    "Habilidad para crear planillas con fórmulas", "Habilidad para crear presentaciones", 
    "Habilidad para crear/editar videos", "Habilidad para subir contenido a sitios", 
    "Habilidad para buscar información escolar", "Habilidad para evaluar confiabilidad información", 
    "Habilidad para editar documentos compartidos", "Habilidad para coordinar trabajos online", "Habilidad para programar",
    "Búsqueda de nuevos usos de PC/internet", "Autoaprendizaje en computación", 
    "Preferencia por tareas en computador", "Mayor aprendizaje con tecnologías", 
    "Búsqueda de nuevos programas/apps", "Autopercepción como solucionador problemas técnicos", 
    "Percepción conocimiento estudiantes vs profesores",
    "Interés en ilustración digital/fotografía", "Interés en producción audiovisual", 
    "Interés en animación/videojuegos", "Interés en diseño web", "Interés en producción musical digital", 
    "Interés en robótica", "Interés en análisis científico", "Interés en astronomía/astrofísica", 
    "Interés en inteligencia artificial",
    "Tecnología ayuda a aprender más", "Tecnología ayuda a mejorar notas", "Redes sociales no distraen de tareas", 
    "Tecnología mejora trabajo con compañeros", "Tareas más entretenidas con dispositivos", 
    "Tecnología motiva aprendizaje", "Tecnología útil para futuro laboral", "Tecnología facilita entender contenido",
    "Factor de expansión", "Indicador formación en tecnologías digitales", 
    "Indicador frecuencia formación ciudadanía digital", "Indicador frecuencia actividades tecnológicas", 
    "Indicador frecuencia uso internet escolar", "Indicador frecuencia uso dispositivos en asignaturas", 
    "Indicador autopercepción habilidades digitales generales", "Indicador autopercepción habilidades digitales avanzadas", 
    "Indicador motivación TIC (creación contenido)", "Indicador motivación TIC (aplicaciones científicas)", 
    "Indicador valoración tecnologías digitales", "Indicador autopercepción forma de pensar", 
    "Indicador percepción protagonismo estudiantes", "Indicador frecuencia aprendizaje activo", 
    "Indicador autopercepción aprendizaje en clases", "Indicador percepción impacto tecnologías digitales"
  )
)

# Guardar el archivo de documentación
write.table(var_names_df, "estwdfvn.txt", sep="\t", row.names=FALSE, quote=FALSE)

# Eliminar todos los objetos excepto estwdf
rm(list = setdiff(ls(), "estwdf"))

# comenzamos con n= 10326

# Filtrar casos según los criterios especificados, manteniendo NAs
estwdf <- estwdf %>%
  filter(
    gen != 0,                                     # Omitir gen=0
    is.na(edpc) | between(edpc, 1, 16),          # Mantener NAs y valores entre 1-16
    is.na(edcel) | between(edcel, 1, 16),        # Mantener NAs y valores entre 1-16
    rural == 0                                    # Omitir rural=1
  )

# quedamos con n=9403, 923 filtrados, aunque ojo con NAs

