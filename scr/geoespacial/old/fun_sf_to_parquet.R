# Funciones auxiliares ----
.normalizar_nombres <- function(datos) {
  
  columna_geometria <- attr(
    datos,
    "sf_column"
  )
  
  nombres_originales <- names(datos)
  
  columnas_atributos <- setdiff(
    nombres_originales,
    columna_geometria
  )
  
  nombres_nuevos <- nombres_originales
  
  posiciones <- nombres_originales %in% columnas_atributos
  
  nombres_nuevos[posiciones] <- nombres_originales[posiciones] |>
    tolower() |>
    stringr::str_replace_all(
      "[^a-z0-9_]",
      "_"
    ) |>
    stringr::str_replace_all(
      "_+",
      "_"
    ) |>
    stringr::str_remove(
      "^_"
    ) |>
    stringr::str_remove(
      "_$"
    )
  
  names(datos) <- make.unique(
    nombres_nuevos,
    sep = "_"
  )
  
  datos
}

.comprobar_componentes_shapefile <- function(shp) {
  
  base <- tools::file_path_sans_ext(shp)
  
  necesarios <- paste0(
    base,
    c(
      ".shp",
      ".shx",
      ".dbf"
    )
  )
  
  existentes <- file.exists(necesarios)
  
  if (!all(existentes)) {
    
    faltan <- basename(
      necesarios[!existentes]
    )
    
    stop(
      paste0(
        "El shapefile está incompleto: ",
        basename(shp),
        ". Faltan: ",
        paste(
          faltan,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  prj <- paste0(
    base,
    ".prj"
  )
  
  if (!file.exists(prj)) {
    warning(
      "No se ha encontrado el PRJ de ",
      basename(shp),
      ". Se comprobará el CRS después de la lectura.",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

.leer_bloque_geografico <- function(
    shp,
    anho
) {
  
  .comprobar_componentes_shapefile(shp)
  
  message(
    "  Leyendo: ",
    basename(shp)
  )
  
  bloque <- sf::st_read(
    shp,
    quiet = TRUE,
    stringsAsFactors = FALSE
  )
  
  if (nrow(bloque) == 0) {
    stop(
      "El shapefile no contiene registros: ",
      shp,
      call. = FALSE
    )
  }
  
  if (is.na(sf::st_crs(bloque))) {
    stop(
      paste0(
        "El shapefile no tiene un CRS reconocido: ",
        shp,
        ". Comprueba el fichero PRJ."
      ),
      call. = FALSE
    )
  }
  
  message(
    "    Filas: ",
    format(
      nrow(bloque),
      big.mark = ".",
      scientific = FALSE
    )
  )
  
  message(
    "    CRS: ",
    sf::st_crs(bloque)$input
  )
  
  # Cada bloque se transforma por separado porque Canarias y Península/Baleares pueden tener proyecciones diferentes.
  bloque <- bloque |>
    sf::st_make_valid() |>
    sf::st_transform(4326) |>
    .normalizar_nombres()
  
  bb <- sf::st_bbox(bloque)
  
  coordenadas_validas <- (
    bb["xmin"] >= -180 &&
      bb["xmax"] <= 180 &&
      bb["ymin"] >= -90 &&
      bb["ymax"] <= 90
  )
  
  if (!coordenadas_validas) {
    stop(
      paste0(
        "La transformación de ",
        basename(shp),
        " no ha producido coordenadas EPSG:4326. ",
        "Extensión: xmin=",
        round(bb["xmin"], 4),
        ", ymin=",
        round(bb["ymin"], 4),
        ", xmax=",
        round(bb["xmax"], 4),
        ", ymax=",
        round(bb["ymax"], 4),
        "."
      ),
      call. = FALSE
    )
  }
  
  bloque |>
    dplyr::mutate(
      anho = as.integer(anho),
      origen_shp = basename(shp)
    )
}

.convertir_atributos_a_texto <- function(datos) {
  
  columna_geometria <- attr(
    datos,
    "sf_column"
  )
  
  columnas_atributos <- setdiff(
    names(datos),
    columna_geometria
  )
  
  datos |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(columnas_atributos),
        as.character
      )
    )
}

.limpiar_codigo <- function(x) {
  
  x <- as.character(x)
  x <- stringr::str_trim(x)
  
  # Corrige valores numéricos leídos desde DBF, por ejemplo 28079.0.
  x <- stringr::str_remove(
    x,
    "\\.0+$"
  )
  
  x <- stringr::str_remove_all(
    x,
    "[^0-9]"
  )
  
  dplyr::na_if(
    x,
    ""
  )
}

.renombrar_primera_coincidencia <- function(
    datos,
    nombre_destino,
    candidatos
) {
  
  if (nombre_destino %in% names(datos)) {
    return(datos)
  }
  
  encontrados <- intersect(
    candidatos,
    names(datos)
  )
  
  if (length(encontrados) == 0) {
    return(datos)
  }
  
  names(datos)[
    names(datos) == encontrados[1]
  ] <- nombre_destino
  
  datos
}

.eliminar_registros_sin_cusec <- function(
    datos,
    anho
) {
  
  cusec_valido <- (
    !is.na(datos$cusec) &
      stringr::str_detect(
        datos$cusec,
        "^[0-9]{10}$"
      )
  )
  
  numero_invalidos <- sum(
    !cusec_valido
  )
  
  if (numero_invalidos == 0) {
    
    message(
      "No hay registros territoriales sin CUSEC."
    )
    
    return(datos)
  }
  
  message(
    "Registros sin CUSEC que se excluirán: ",
    format(
      numero_invalidos,
      big.mark = ".",
      scientific = FALSE
    )
  )
  
  # Conserva todos los atributos originales para poder investigar posteriormente qué representan esos polígonos.
  diagnostico <- datos[
    !cusec_valido,
    ,
    drop = FALSE
  ] |>
    sf::st_drop_geometry() |>
    as.data.frame()
  
  fichero_diagnostico <- file.path(
    output_log,
    paste0(
      "diagnostico_registros_excluidos_",
      anho,
      ".csv"
    )
  )
  
  write.csv(
    diagnostico,
    fichero_diagnostico,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
  message(
    "Diagnóstico completo guardado en: ",
    normalizePath(
      fichero_diagnostico,
      mustWork = FALSE
    )
  )
  
  message(
    "Registros conservados: ",
    format(
      sum(cusec_valido),
      big.mark = ".",
      scientific = FALSE
    )
  )
  
  datos[
    cusec_valido,
    ,
    drop = FALSE
  ]
}

.construir_cusec <- function(datos) {
  
  datos <- .renombrar_primera_coincidencia(
    datos = datos,
    nombre_destino = "cusec",
    candidatos = c(
      "cusec",
      "cuseccion",
      "cod_cusec",
      "codigo_cusec"
    )
  )
  
  datos <- .renombrar_primera_coincidencia(
    datos = datos,
    nombre_destino = "provmun",
    candidatos = c(
      "provmun",
      "prov_mun",
      "cpro_cmun",
      "cpro_cmuni",
      "cod_provmun",
      "codprovmun"
    )
  )
  
  datos <- .renombrar_primera_coincidencia(
    datos = datos,
    nombre_destino = "cpro",
    candidatos = c(
      "cpro",
      "cod_prov",
      "codprov",
      "codigo_provincia"
    )
  )
  
  datos <- .renombrar_primera_coincidencia(
    datos = datos,
    nombre_destino = "cmun",
    candidatos = c(
      "cmun",
      "cod_mun",
      "codmun",
      "codigo_municipio"
    )
  )
  
  datos <- .renombrar_primera_coincidencia(
    datos = datos,
    nombre_destino = "cdis",
    candidatos = c(
      "cdis",
      "distrito",
      "cod_dis",
      "coddis",
      "codigo_distrito"
    )
  )
  
  datos <- .renombrar_primera_coincidencia(
    datos = datos,
    nombre_destino = "csec",
    candidatos = c(
      "csec",
      "seccion",
      "cod_sec",
      "codsec",
      "codigo_seccion"
    )
  )
  
  if ("cusec" %in% names(datos)) {
    
    message(
      "Construyendo claves territoriales a partir de cusec."
    )
    
    datos <- datos |>
      dplyr::mutate(
        cusec = .limpiar_codigo(cusec),
        cusec = stringr::str_pad(
          cusec,
          width = 10,
          side = "left",
          pad = "0"
        )
      )
    
  } else if (
    all(
      c(
        "provmun",
        "cdis",
        "csec"
      ) %in% names(datos)
    )
  ) {
    
    message(
      "Construyendo CUSEC con provmun + cdis + csec."
    )
    
    datos <- datos |>
      dplyr::mutate(
        provmun = .limpiar_codigo(provmun),
        cdis = .limpiar_codigo(cdis),
        csec = .limpiar_codigo(csec),
        provmun = stringr::str_pad(
          provmun,
          width = 5,
          side = "left",
          pad = "0"
        ),
        cdis = stringr::str_pad(
          cdis,
          width = 2,
          side = "left",
          pad = "0"
        ),
        csec = stringr::str_pad(
          csec,
          width = 3,
          side = "left",
          pad = "0"
        ),
        cusec = stringr::str_c(
          provmun,
          cdis,
          csec
        )
      )
    
  } else if (
    all(
      c(
        "cpro",
        "cmun",
        "cdis",
        "csec"
      ) %in% names(datos)
    )
  ) {
    
    message(
      "Construyendo CUSEC con cpro + cmun + cdis + csec."
    )
    
    datos <- datos |>
      dplyr::mutate(
        cpro = .limpiar_codigo(cpro),
        cmun = .limpiar_codigo(cmun),
        cdis = .limpiar_codigo(cdis),
        csec = .limpiar_codigo(csec),
        cpro = stringr::str_pad(
          cpro,
          width = 2,
          side = "left",
          pad = "0"
        ),
        cmun = stringr::str_pad(
          cmun,
          width = 3,
          side = "left",
          pad = "0"
        ),
        cdis = stringr::str_pad(
          cdis,
          width = 2,
          side = "left",
          pad = "0"
        ),
        csec = stringr::str_pad(
          csec,
          width = 3,
          side = "left",
          pad = "0"
        ),
        cusec = stringr::str_c(
          cpro,
          cmun,
          cdis,
          csec
        )
      )
    
  } else {
    
    stop(
      paste0(
        "No se puede construir CUSEC.",
        "\nSe esperaba una de estas estructuras:",
        "\n  1. cusec",
        "\n  2. provmun + cdis/distrito + csec/seccion",
        "\n  3. cpro + cmun + cdis/distrito + csec/seccion",
        "\nColumnas disponibles: ",
        paste(
          names(datos),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  # No se detiene aquí por códigos nulos. La validación posterior
  # guarda primero un CSV con las filas problemáticas.
  datos <- datos |>
    dplyr::mutate(
      cusec = .limpiar_codigo(cusec),
      cusec = stringr::str_pad(
        cusec,
        width = 10,
        side = "left",
        pad = "0"
      ),
      prov = dplyr::if_else(
        is.na(cusec),
        NA_character_,
        stringr::str_sub(cusec, 1, 2)
      ),
      mun = dplyr::if_else(
        is.na(cusec),
        NA_character_,
        stringr::str_sub(cusec, 3, 5)
      ),
      distrito = dplyr::if_else(
        is.na(cusec),
        NA_character_,
        stringr::str_sub(cusec, 6, 7)
      ),
      seccion = dplyr::if_else(
        is.na(cusec),
        NA_character_,
        stringr::str_sub(cusec, 8, 10)
      )
    )
  
  datos
}

.guardar_diagnostico_cusec_invalidos <- function(
    datos,
    anho
) {
  
  cusec_invalido <- (
    is.na(datos$cusec) |
      !stringr::str_detect(
        dplyr::coalesce(
          datos$cusec,
          ""
        ),
        "^[0-9]{10}$"
      )
  )
  
  if (!any(cusec_invalido)) {
    return(invisible(FALSE))
  }
  
  columnas_diagnostico <- intersect(
    c(
      "anho",
      "origen_shp",
      "provmun",
      "cpro",
      "cmun",
      "cdis",
      "csec",
      "letra",
      "codigo",
      "prov_original",
      "cusec"
    ),
    names(datos)
  )
  
  diagnostico <- datos[
    cusec_invalido,
    ,
    drop = FALSE
  ] |>
    sf::st_drop_geometry() |>
    dplyr::select(
      dplyr::all_of(columnas_diagnostico)
    )
  
  fichero_diagnostico <- file.path(
    output_log,
    paste0(
      "diagnostico_cusec_invalidos_",
      anho,
      ".csv"
    )
  )
  
  write.csv(
    diagnostico,
    fichero_diagnostico,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
  message("")
  message(
    "Filas con CUSEC inválido: ",
    nrow(diagnostico)
  )
  
  message(
    "Diagnóstico guardado en: ",
    normalizePath(
      fichero_diagnostico,
      mustWork = FALSE
    )
  )
  
  print(
    utils::head(
      diagnostico,
      20
    ),
    row.names = FALSE
  )
  
  stop(
    paste0(
      "Hay ",
      nrow(diagnostico),
      " filas sin un CUSEC válido en ",
      anho,
      ". Revisa diagnostico_cusec_invalidos_",
      anho,
      ".csv."
    ),
    call. = FALSE
  )
}

.consolidar_cusec_duplicados <- function(
    datos,
    anho
) {
  
  conteo <- datos |>
    sf::st_drop_geometry() |>
    dplyr::count(
      cusec,
      name = "numero_partes"
    )
  
  duplicados <- conteo |>
    dplyr::filter(
      numero_partes > 1
    )
  
  if (nrow(duplicados) > 0) {
    
    message(
      "CUSEC con varias geometrías: ",
      format(
        nrow(duplicados),
        big.mark = ".",
        scientific = FALSE
      )
    )
    
    message(
      "Filas adicionales detectadas: ",
      format(
        sum(duplicados$numero_partes - 1),
        big.mark = ".",
        scientific = FALSE
      )
    )
    
    fichero_diagnostico <- file.path(
      output_log,
      paste0(
        "diagnostico_cusec_multiples_",
        anho,
        ".csv"
      )
    )
    
    diagnostico <- datos |>
      sf::st_drop_geometry() |>
      dplyr::semi_join(
        duplicados,
        by = "cusec"
      )
    
    write.csv(
      diagnostico,
      fichero_diagnostico,
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
    
    message(
      "Diagnóstico guardado en: ",
      normalizePath(
        fichero_diagnostico,
        mustWork = FALSE
      )
    )
  }
  
  datos <- datos |>
    dplyr::left_join(
      conteo,
      by = "cusec"
    )
  
  datos
}

.validar_cusec <- function(
    datos,
    anho
) {
  
  if (anyNA(datos$cusec)) {
    stop(
      "Siguen existiendo CUSEC nulos en ",
      anho,
      ".",
      call. = FALSE
    )
  }
  
  formato_incorrecto <- !stringr::str_detect(
    datos$cusec,
    "^[0-9]{10}$"
  )
  
  if (any(formato_incorrecto)) {
    stop(
      "Siguen existiendo CUSEC con formato incorrecto en ",
      anho,
      ".",
      call. = FALSE
    )
  }
  
  duplicados <- sum(
    duplicated(datos$cusec)
  )
  
  if (duplicados > 0) {
    
    message(
      "Aviso: ",
      duplicados,
      " filas pertenecen a CUSEC multipartes."
    )
    
  }
  
  invisible(TRUE)
}

.procesar_anho <- function(anho) {
  
  message("")
  message(
    paste(
      rep(
        "=",
        70
      ),
      collapse = ""
    )
  )
  
  message(
    "Procesando año ",
    anho
  )
  
  message(
    paste(
      rep(
        "=",
        70
      ),
      collapse = ""
    )
  )
  
  carpeta_anho <- file.path(
    input_dir,
    as.character(anho)
  )
  
  if (!dir.exists(carpeta_anho)) {
    stop(
      "No existe la carpeta: ",
      normalizePath(
        carpeta_anho,
        mustWork = FALSE
      ),
      call. = FALSE
    )
  }
  
  ficheros_shp <- list.files(
    carpeta_anho,
    pattern = "\\.shp$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )
  
  ficheros_shp <- ficheros_shp[
    !stringr::str_detect(
      basename(ficheros_shp),
      stringr::regex(
        "\\.lock$|\\.sr\\.lock$",
        ignore_case = TRUE
      )
    )
  ]
  
  ficheros_shp <- sort(ficheros_shp)
  
  if (length(ficheros_shp) == 0) {
    stop(
      "No se ha encontrado ningún SHP en: ",
      carpeta_anho,
      call. = FALSE
    )
  }
  
  message(
    "Shapefiles encontrados: ",
    length(ficheros_shp)
  )
  
  message(
    paste0(
      "  - ",
      basename(ficheros_shp),
      collapse = "\n"
    )
  )
  
  bloques <- lapply(
    ficheros_shp,
    .leer_bloque_geografico,
    anho = anho
  )
  
  if (length(bloques) > 1) {
    bloques <- lapply(
      bloques,
      .convertir_atributos_a_texto
    )
  }
  
  message(
    "Uniendo bloques geográficos..."
  )
  
  secciones_4326 <- dplyr::bind_rows(bloques)
  
  message(
    "Total de secciones tras la unión: ",
    format(
      nrow(secciones_4326),
      big.mark = ".",
      scientific = FALSE
    )
  )
  
  # Conserva el campo PROV histórico antes de crear PROV desde CUSEC.
  if ("prov" %in% names(secciones_4326)) {
    
    if ("prov_original" %in% names(secciones_4326)) {
      secciones_4326 <- secciones_4326 |>
        dplyr::select(-prov_original)
    }
    
    secciones_4326 <- secciones_4326 |>
      dplyr::rename(
        prov_original = prov
      )
  }
  
  secciones_4326 <- .construir_cusec(
    secciones_4326
  )
  
  # Los registros que carecen de municipio, distrito o sección no
  # permiten construir un CUSEC. Se guardan completos en un CSV de
  # diagnóstico y se excluyen del Parquet.
  secciones_4326 <- .eliminar_registros_sin_cusec(
    datos = secciones_4326,
    anho = anho
  )
  
  if (nrow(secciones_4326) == 0) {
    stop(
      "No queda ninguna sección válida después de excluir los registros ",
      "sin CUSEC del año ",
      anho,
      ".",
      call. = FALSE
    )
  }
  
  # Comprobación defensiva. En principio no debería encontrar errores porque los registros no identificables ya han sido excluidos.
  .guardar_diagnostico_cusec_invalidos(
    datos = secciones_4326,
    anho = anho
  )
  
  # No se disuelven las geometrías que comparten CUSEC. Simplemente se añade el número de piezas de cada sección.
  secciones_4326 <- .consolidar_cusec_duplicados(
    datos = secciones_4326,
    anho = anho
  )
  
  .validar_cusec(
    datos = secciones_4326,
    anho = anho
  )
  
  secciones_4326 <- secciones_4326 |>
    dplyr::arrange(cusec)
  
  bb <- sf::st_bbox(secciones_4326)
  
  message(
    "Extensión conjunta EPSG:4326: ",
    paste(
      round(
        bb,
        4
      ),
      collapse = ", "
    )
  )
  
  if (as.numeric(bb["xmin"]) > -17) {
    warning(
      paste0(
        "La extensión del año ",
        anho,
        " no parece incluir completamente Canarias. xmin=",
        round(bb["xmin"], 4),
        "."
      ),
      call. = FALSE
    )
  }
  
  message(
    "Serializando features GeoJSON..."
  )
  
  tiempo_serializacion <- system.time({
    feature_json <- geojsonsf::sf_geojson(
      secciones_4326,
      atomise = TRUE,
      simplify = FALSE,
      digits = 7
    )
  })
  
  message(
    "Serialización terminada en ",
    round(
      tiempo_serializacion[["elapsed"]],
      1
    ),
    " segundos."
  )
  
  if (length(feature_json) != nrow(secciones_4326)) {
    stop(
      paste0(
        "No coincide el número de features serializadas. ",
        "Secciones: ",
        nrow(secciones_4326),
        "; features: ",
        length(feature_json),
        "."
      ),
      call. = FALSE
    )
  }
  
  atributos <- secciones_4326 |>
    sf::st_drop_geometry() |>
    as.data.frame() |>
    dplyr::mutate(
      anho = as.integer(anho)
    ) |>
    dplyr::select(
      cusec,
      prov,
      mun,
      distrito,
      seccion,
      anho,
      origen_shp,
      numero_partes,
      dplyr::everything()
    )
  
  secciones_parquet <- atributos |>
    dplyr::mutate(
      feature_json = as.character(feature_json)
    )
  
  fecha <- paste0(
    anho,
    "0101"
  )
  
  output_parquet <- file.path(
    output_dir,
    paste0(
      "SECC_CE_",
      fecha,
      ".parquet"
    )
  )
  
  message(
    "Escribiendo Parquet..."
  )
  
  arrow::write_parquet(
    secciones_parquet,
    output_parquet,
    compression = "snappy",
    use_dictionary = TRUE
  )
  
  tamanho_mb <- file.info(output_parquet)$size / 1024^2
  
  resultado <- data.frame(
    anho = as.integer(anho),
    shapefiles = length(ficheros_shp),
    filas = nrow(secciones_parquet),
    segundos_serializacion = unname(
      tiempo_serializacion[["elapsed"]]
    ),
    tamanho_mb = tamanho_mb,
    fichero = normalizePath(
      output_parquet,
      mustWork = FALSE
    ),
    estado = "OK",
    stringsAsFactors = FALSE
  )
  
  message(
    "Parquet generado: ",
    resultado$fichero
  )
  
  message(
    "Filas: ",
    format(
      resultado$filas,
      big.mark = ".",
      scientific = FALSE
    )
  )
  
  message(
    "Tamaño: ",
    round(
      resultado$tamanho_mb,
      1
    ),
    " MB"
  )
  
  resultado
}