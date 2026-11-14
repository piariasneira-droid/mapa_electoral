# Auxiliar ----
## Limpieza ----
.limpiar_texto <- function(x) {
  x |>
    as.character() |>
    str_trim() |>
    str_squish()
}

.a_entero <- function(x) {
  parse_integer(
    as.character(x),
    na = c("", "NA")
  )
}

.tipar_columnas <- function(df,   
  cols_enteras = c("anio", "mes", "fecha_eleccion", "ccaa", "prov",
  "mun", "distrito", "seccion")) {
  
  cols_presentes <- intersect(
    cols_enteras,
    names(df)
  )
  
  df |>
    mutate(
      across(
        all_of(cols_presentes),
        as.integer
      )
    )
}

# .reparar_mojibake <- function(x) {
#   y <- iconv(x, from = "UTF-8", to = "latin1")
#   Encoding(y) <- "UTF-8"
#   dplyr::if_else(!is.na(y) & validUTF8(y), y, x)
# }


#Lectura elecciones ----
.leer_eleccion_congreso <- function(periodo){
  base <- file.path(
    "interim",
    "generales",
    periodo
  )
  
  list(
    mesas = read_parquet(
      file.path(base, "mesas.parquet")
    ),
    votos = read_parquet(
      file.path(base, "votos.parquet")
    ),
    candidaturas = read_parquet(
      file.path(base, "metadata", "candidaturas.parquet")
    ),
    municipios = read_parquet(
      file.path(base, "metadata", "municipios.parquet")
    )
  )
}

# Preparar elecciones ----
## Filtro territorial ----
# Si un código es NULL, no se filtra por ese nivel (se admiten todos).
# Admite también vectores (p. ej. cod_prov = c("28", "45")).
.filtrar_territorio <- function(
    df,
    cod_ccaa = NULL,
    cod_prov = NULL,
    cod_mun = NULL,
    cod_dis = NULL,
    cod_sec = NULL,
    cod_mesa = NULL) {
  filtros <- c(
    ccaa = !is.null(cod_ccaa),
    prov = !is.null(cod_prov),
    mun = !is.null(cod_mun),
    distrito = !is.null(cod_dis),
    seccion = !is.null(cod_sec),
    mesa = !is.null(cod_mesa)
  )
  faltan <- setdiff(names(filtros)[filtros], names(df))
  if (length(faltan) > 0) {
    stop(
      "No existe la columna para filtrar: ",
      paste(faltan, collapse = ", "),
      ". Revisa que el dataset de entrada la incluya.",
      call. = FALSE
    )
  }
  
  if (!is.null(cod_ccaa)) df <- filter(df, ccaa %in% cod_ccaa)
  if (!is.null(cod_prov)) df <- filter(df, prov %in% cod_prov)
  if (!is.null(cod_mun))  df <- filter(df, mun %in% cod_mun)
  if (!is.null(cod_dis))  df <- filter(df, distrito %in% cod_dis)
  if (!is.null(cod_sec))  df <- filter(df, seccion %in% cod_sec)
  if (!is.null(cod_mesa)) df <- filter(df, mesa %in% cod_mesa)
  df
}

.preparar_eleccion_congreso <- function(
    codigo_eleccion,
    cod_ccaa = NULL,
    cod_prov = NULL,
    cod_mun = NULL,
    cod_dis = NULL,
    cod_sec = NULL,
    cod_mesa = NULL) {
  
  mensaje <- paste("Procesando elección:", codigo_eleccion)
  message(mensaje)
  
  elec <- .leer_eleccion_congreso(codigo_eleccion)
  
  mesas <- elec$mesas |>
    mutate(
      across(
        c(
          eleccion, anio, mes, vuelta,
          ccaa, prov, mun, distrito,
          seccion, mesa, oficial
        ),
        .limpiar_texto
      ),
      across(
        c(
          censo_ine,
          censo_escrutinio,
          censo_cere,
          votantes_cere,
          avance1,
          avance2,
          blanco,
          nulo,
          candidaturas,
          si_ref,
          no_ref
        ),
        .a_entero
      )
    ) |>
    .filtrar_territorio(
      cod_ccaa, cod_prov, cod_mun,
      cod_dis, cod_sec, cod_mesa
    )
  
  votos <- elec$votos |>
    mutate(
      across(
        c(
          eleccion, anio, mes, vuelta,
          ccaa, prov, mun, distrito,
          seccion, mesa, cod_cand
        ),
        .limpiar_texto
      ),
      votos = .a_entero(votos)
    ) |>
    .filtrar_territorio(
      cod_ccaa, cod_prov, cod_mun,
      cod_dis, cod_sec, cod_mesa
    )
  
  candidaturas <- elec$candidaturas |>
    mutate(
      across(
        c(
          eleccion, anio, mes,
          cod_cand, siglas, denominacion
        ),
        .limpiar_texto
      )
    ) |>
    select(
      eleccion,
      anio,
      mes,
      cod_cand,
      siglas,
      denominacion
    ) |>
    distinct()
  
  votos_detalle <- votos |>
    left_join(
      candidaturas,
      by = c(
        "eleccion",
        "anio",
        "mes",
        "cod_cand"
      ),
      relationship = "many-to-one"
    )
  
  list(
    mesas = mesas,
    votos = votos_detalle,
    candidaturas = candidaturas
  )
}

# Homogeneización partidos ----
## Congreso ----
.clasificar_partido_congreso <- function(
    siglas,
    denominacion
) {
  
  etiqueta <- stringr::str_c(
    dplyr::coalesce(siglas, ""),
    dplyr::coalesce(denominacion, ""),
    sep = " "
  ) |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_upper() |>
    stringr::str_squish()
  
  dplyr::case_when(
    
    # PSOE ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])PSOE([^A-Z]|$)|",
        "P\\.S\\.O\\.E\\.|",
        "PARTIDO SOCIALISTA OBRERO|",
        "PARTIT SOCIALISTA OBRER|",
        "PARTIT DELS SOCIALISTES DE CATALUNYA|",
        "(^|[^A-Z])PSC([^A-Z]|$)|",
        "PSC-PSOE|",
        "PSE-PSOE|",
        "PSE-EE|",
        "PSDEG-PSOE|",
        "PSDG-PSOE|",
        "PSG-PSOE|",
        "PSN-PSOE|",
        "PSIB-PSOE|",
        "PSOE-A|",
        "PSOE DE A"
      )
    ) ~ "PSOE",
    
    # PP ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])PP([^A-Z]|$)|",
        "P\\.P\\.|",
        "PARTIDO POPULAR|",
        "PARTIT POPULAR|",
        "^AP-PDP|",
        "^AP-PL|",
        "COALICION POPULAR"
      )
    ) ~ "PP",
    
    # VOX ----
    stringr::str_detect(
      etiqueta,
      "(^|[^A-Z])VOX([^A-Z]|$)"
    ) ~ "VOX",
    
    # SUMAR / MAS PAIS / MAS MADRID ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])SUMAR([^A-Z]|$)|",
        "SUMAR ANDALUC|",
        "SUMAR GALICIA|",
        "SUMAR CANARIAS|",
        "SUMAR ARAG|",
        "SUMAR - ECP|",
        "COMPROM.*SUMAR|",
        "SUMAR M|",
        "MAS PAIS|",
        "MAS MADRID"
      )
    ) ~ "SUMAR",
    
    # PODEMOS ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])PODEMOS([^A-Z]|$)|",
        "UNIDOS PODEMOS|",
        "UNIDAS PODEMOS|",
        "ELKARREKIN PODEMOS|",
        "EN COMU PODEM|",
        "(^|[^A-Z])ECP([^A-Z]|$)|",
        "PODEMOS-AHAL|",
        "PODEMOS/AHAL"
      )
    ) ~ "PODEMOS",
    
    # IU ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "IZQUIERDA UNIDA|",
        "IZQUIERDA PLURAL|",
        "ESQUERRA UNIDA|",
        "EZKER BATUA|",
        "EZKER ANITZA|",
        "(^|[^A-Z])IU([^A-Z]|$)|",
        "(^|[^A-Z])I\\.U\\.([^A-Z]|$)|",
        "IULV|",
        "IUC|",
        "IUN-NEB|",
        "IUCL|",
        "IUCYL|",
        "EUPV|",
        "ICV-EUIA|",
        "UNIDAD POPULAR"
      )
    ) ~ "IU",
    
    # UPYD ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])UPYD([^A-Z]|$)|",
        "(^|[^A-Z])UPD([^A-Z]|$)|",
        "UNION PROGRESO Y DEMOCRACIA"
      )
    ) ~ "UPYD",
    
    # CIUDADANOS ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "CIUDADANOS.PARTIDO DE LA CIUDADAN|",
        "CIUTADANS.PARTIDO DE LA CIUDADAN|",
        "(^|[^A-Z])C'S([^A-Z]|$)|",
        "(^|[^A-Z])CS([^A-Z]|$)"
      )
    ) ~ "CIUDADANOS",
    
    # CIU / CDC / PDECAT / JUNTS ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])CIU([^A-Z]|$)|",
        "CONVERGENCIA I UNIO|",
        "DEMOCRACIA I LLIBERTAT|",
        "(^|[^A-Z])CDC([^A-Z]|$)|",
        "CONVERGENCIA DEMOCRATICA DE CATALUNYA|",
        "JXCAT|",
        "JUNTS PER CATALUNYA|",
        "PDECAT"
      )
    ) ~ "CIU_JUNTS",
    
    # ERC ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])ERC([^A-Z]|$)|",
        "ESQUERRA REPUBLICANA"
      )
    ) ~ "ERC",
    
    # PNV ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "EAJ-PNV|",
        "E\\.A\\.J\\.-P\\.N\\.V\\.|",
        "PARTIDO NACIONALISTA VASCO|",
        "EUZKO ALDERDI JELTZALEA"
      )
    ) ~ "PNV",
    
    # HB / EH / AMAIUR / EH BILDU ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])HB([^A-Z]|$)|",
        "HERRI BATASUNA|",
        "EUSKAL HERRITARROK|",
        "(^|[^A-Z])AMAIUR([^A-Z]|$)|",
        "EH BILDU|",
        "EUSKAL HERRIA BILDU"
      )
    ) ~ "HB_EH_BILDU",
    
    # BNG ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])BNG([^A-Z]|$)|",
        "B\\.N\\.G\\.|",
        "BLOQUE NACIONALISTA GALEGO|",
        "BNG-N"
      )
    ) ~ "BNG",
    
    # CC ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "COALICION CANARIA|",
        "(^|[^A-Z])CC-PNC([^A-Z]|$)|",
        "(^|[^A-Z])CCA-PNC([^A-Z]|$)|",
        "(^|[^A-Z])CCA([^A-Z]|$)"
      )
    ) ~ "CC",
    
    # UPN ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])UPN([^A-Z]|$)|",
        "U\\.P\\.N\\.|",
        "UNION DEL PUEBLO NAVARRO"
      )
    ) ~ "UPN",
    
    # CDS ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])CDS([^A-Z]|$)|",
        "C\\.D\\.S\\.|",
        "CENTRO DEMOCRATICO Y SOCIAL|",
        "CENTRE DEMOCRATIC I SOCIAL"
      )
    ) ~ "CDS",
    
    # PA ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])PA([^A-Z]|$)|",
        "P\\.A\\.|",
        "PARTIDO ANDALUCISTA|",
        "PARTIDO SOCIALISTA DE ANDALUCIA - PARTIDO ANDALUZ"
      )
    ) ~ "PA",
    
    # PAR ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])PAR([^A-Z]|$)|",
        "PARTIDO ARAGONES"
      )
    ) ~ "PAR",
    
    # CHA ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])CHA([^A-Z]|$)|",
        "CHUNTA ARAGONESISTA"
      )
    ) ~ "CHA",
    
    # EA ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])EA([^A-Z]|$)|",
        "EUSKO ALKARTASUNA"
      )
    ) ~ "EA",
    
    # EE ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])EE([^A-Z]|$)|",
        "EUSKADIKO EZKERRA"
      )
    ) ~ "EE",
    
    # NAFARROA BAI / GEROA BAI ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "NAFARROA BAI|",
        "NA-BAI|",
        "GEROA BAI|",
        "(^|[^A-Z])GBAI([^A-Z]|$)"
      )
    ) ~ "NA_BAI_GEROA_BAI",
    
    # COMPROMIS ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "COMPROMIS|",
        "COALICIO COMPROMIS|",
        "MES COMPROM"
      )
    ) ~ "COMPROMIS",
    
    # PRC ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])PRC([^A-Z]|$)|",
        "P\\.R\\.C\\.|",
        "PARTIDO REGIONALISTA DE CANTABRIA"
      )
    ) ~ "PRC",
    
    # FORO ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "FORO DE CIUDADANOS|",
        "(^|[^A-Z])FAC([^A-Z]|$)|",
        "PP-FORO"
      )
    ) ~ "FORO",
    
    # CUP ----
    stringr::str_detect(
      etiqueta,
      paste0(
        "(^|[^A-Z])CUP-PR([^A-Z]|$)|",
        "CANDIDATURA D'UNITAT POPULAR"
      )
    ) ~ "CUP",
    
    # TERUEL EXISTE ----
    stringr::str_detect(
      etiqueta,
      "TERUEL EXISTE|TERUEL EXI"
    ) ~ "TERUEL_EXISTE",
    
    TRUE ~ "OTROS"
  )
}

# Agregación territorial de resultados ----
# Funciones para pasar de resultados por mesa (mesas_resultados) a cualquier
# nivel territorial superior (sección, distrito, municipio...).

## Niveles disponibles ----
# Claves que identifican cada unidad territorial dentro de una elección.
# El código de sección/distrito/municipio se repite entre territorios, por eso
# hay que agrupar siempre con todas las claves jerárquicas.
.niveles_territoriales <- list(
  seccion   = c("ccaa", "prov", "mun", "provmun", "distrito", "seccion"),
  distrito  = c("ccaa", "prov", "mun", "provmun", "distrito"),
  municipio = c("ccaa", "prov", "mun", "provmun"),
  provincia = c("ccaa", "prov")
)

## Agregación ----
.agregar_resultados <- function(
    mesas,
    nivel = "seccion",
    partidos,
    id_col = paste0(nivel, "_id")) {
  
  mesas <- mesas |>
    mutate(
      provmun = prov * 1000L + mun
    )
  
  claves <- .niveles_territoriales[[nivel]]
  
  if (is.null(claves)) {
    stop(
      "Nivel no válido: ", nivel,
      ". Opciones: ", paste(names(.niveles_territoriales), collapse = ", ")
    )
  }
  
  cols_partidos <- str_c("votos_", partidos)
  
  cols_suma <- c(
    "censo_ine",
    "censo_escrutinio",
    "votos_emitidos",
    "votos_validos",
    "votos_candidaturas",
    "abstencion",
    "blanco",
    "nulo",
    cols_partidos
  )
  
  faltan <- setdiff(c(claves, cols_suma), names(mesas))
  if (length(faltan) > 0) {
    stop("Faltan columnas en mesas: ", paste(faltan, collapse = ", "))
  }
  
  mesas |>
    group_by(
      across(all_of(claves)),
      fecha_eleccion,
      anio,
      mes
    ) |>
    summarise(
      n_mesas = n(),
      n_secciones = n_distinct(str_c(mun, distrito, seccion)),
      across(all_of(cols_suma), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    mutate(
      "{id_col}" := dplyr::case_when(
        
        nivel == "seccion" ~ stringr::str_c(
          stringr::str_pad(prov, 2, pad = "0"),
          stringr::str_pad(mun, 3, pad = "0"),
          stringr::str_pad(distrito, 2, pad = "0"),
          stringr::str_pad(seccion, 3, pad = "0"),
          sep = "-"
        ),
        
        nivel == "distrito" ~ stringr::str_c(
          stringr::str_pad(prov, 2, pad = "0"),
          stringr::str_pad(mun, 3, pad = "0"),
          stringr::str_pad(distrito, 2, pad = "0"),
          sep = "-"
        ),
        
        nivel == "municipio" ~ stringr::str_c(
          stringr::str_pad(prov, 2, pad = "0"),
          stringr::str_pad(mun, 3, pad = "0"),
          sep = "-"
        ),
        
        nivel == "provincia" ~ stringr::str_pad(
          prov,
          2,
          pad = "0"
        )
      ),
      .before = 1
    ) |>
    mutate(
      participacion_pct = if_else(
        censo_escrutinio > 0,
        100 * votos_emitidos / censo_escrutinio,
        NA_real_
      ),
      abstencion_pct = if_else(
        censo_escrutinio > 0,
        100 * abstencion / censo_escrutinio,
        NA_real_
      ),
      blanco_pct_validos = if_else(
        votos_validos > 0,
        100 * blanco / votos_validos,
        NA_real_
      ),
      nulo_pct_emitidos = if_else(
        votos_emitidos > 0,
        100 * nulo / votos_emitidos,
        NA_real_
      ),
      across(
        all_of(cols_partidos),
        list(
          pct_validos = ~ if_else(
            votos_validos > 0,
            100 * .x / votos_validos,
            NA_real_
          ),
          pct_censo = ~ if_else(
            censo_escrutinio > 0,
            100 * .x / censo_escrutinio,
            NA_real_
          )
        ),
        .names = "{.col}_{.fn}"
      )
    ) |>
    arrange(
      fecha_eleccion,
      across(all_of(claves))
    )
}

## Evolución del censo ----
# Detecta cambios bruscos de censo o de número de mesas entre elecciones
# consecutivas (posibles cambios de límites o de configuración).
.evolucion_territorio <- function(
    resultados,
    id_col,
    var_censo_problem = 30) {
  
  resultados |>
    arrange(
      across(all_of(id_col)),
      fecha_eleccion
    ) |>
    group_by(across(all_of(id_col))) |>
    mutate(
      censo_anterior = lag(censo_escrutinio),
      n_mesas_anterior = lag(n_mesas),
      variacion_censo = censo_escrutinio - censo_anterior,
      variacion_censo_pct = if_else(
        censo_anterior > 0,
        100 * variacion_censo / censo_anterior,
        NA_real_
      ),
      cambio_n_mesas = n_mesas != n_mesas_anterior,
      posible_cambio_territorio =
        abs(variacion_censo_pct) >= var_censo_problem
    ) |>
    ungroup()
}

# Pipeline de agregación ----
# Funciones compartidas por los ETL que agregan resultados (secciones,
# distritos...), para que cada script solo contenga su configuración.

## Lectura de resultados por mesa ----
# Lee el output del ETL de mesas y aplica los filtros territoriales.
# NULL en un filtro = sin filtrar por ese nivel.
.leer_mesas_resultados <- function(
    dir_input,
    archivo = "mesas_resultados.parquet",
    cod_ccaa = NULL,
    cod_prov = NULL,
    cod_mun = NULL,
    cod_dis = NULL,
    cod_sec = NULL) {
  
  datos <- read_parquet(file.path(dir_input, archivo)) |>
    .filtrar_territorio(
      cod_ccaa = cod_ccaa,
      cod_prov = cod_prov,
      cod_mun = cod_mun,
      cod_dis = cod_dis,
      cod_sec = cod_sec
    )
  
  if (nrow(datos) == 0) {
    stop(
      "Los filtros territoriales no devuelven ninguna mesa. ",
      "Revisa los filtros (ccaa/prov/mun/dis/sec) o regenera el output de mesas.",
      call. = FALSE
    )
  }
  
  datos
}

## Agregación + evolución ----
# Devuelve una lista con resultados_<nivel> y evolucion_<nivel>.
.procesar_agregacion <- function(
    mesas,
    nivel,
    partidos,
    var_censo_problem = 30) {
  
  id_col <- paste0(nivel, "_id")
  
  resultados <- .agregar_resultados(
    mesas = mesas,
    nivel = nivel,
    partidos = partidos,
    id_col = id_col
  )
  
  evolucion <- .evolucion_territorio(
    resultados,
    id_col = id_col,
    var_censo_problem = var_censo_problem
  )
  
  salidas <- list(resultados, evolucion)
  names(salidas) <- c(
    paste0("resultados_", nivel),
    paste0("evolucion_", nivel)
  )
  salidas
}

## Guardado ----
# Guarda cada elemento de la lista en CSV y parquet.
.guardar_salidas <- function(salidas, dir_output) {
  dir.create(
    dir_output,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  iwalk(
    salidas,
    function(datos, nombre) {
      message("Guardando: ", nombre)
      
      write_csv(
        datos,
        file = file.path(dir_output, paste0(nombre, ".csv")),
        na = ""
      )
      
      write_parquet(
        datos,
        sink = file.path(dir_output, paste0(nombre, ".parquet"))
      )
    }
  )
  
  message(
    "Proceso finalizado. Archivos guardados en: ",
    normalizePath(dir_output)
  )
  
  invisible(salidas)
}