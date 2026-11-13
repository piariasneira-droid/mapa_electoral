# Lectura directa de GeoJSON provinciales ----

.normalizar_codigo_geo <- function(
    x,
    ancho = NULL
) {
  
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- stringr::str_remove(x, "\\.0+$")
  x <- stringr::str_remove_all(x, "[^0-9]")
  x <- dplyr::na_if(x, "")
  
  if (!is.null(ancho)) {
    
    x <- stringr::str_pad(
      x,
      width = ancho,
      side = "left",
      pad = "0"
    )
  }
  
  x
}


.extraer_cusec_feature <- function(feature) {
  
  propiedades <- feature$properties
  
  if (is.null(propiedades)) {
    return(NA_character_)
  }
  
  candidatos <- c(
    "cusec",
    "CUSEC",
    "cuseccion",
    "CUSECCION",
    "cod_cusec",
    "codigo_cusec",
    "geo_id"
  )
  
  encontrados <- intersect(
    candidatos,
    names(propiedades)
  )
  
  if (length(encontrados) == 0) {
    return(NA_character_)
  }
  
  valor <- propiedades[[encontrados[1]]]
  
  if (
    is.null(valor) ||
    length(valor) == 0
  ) {
    return(NA_character_)
  }
  
  .normalizar_codigo_geo(
    x = valor[1],
    ancho = 10
  )
}


.asignar_geo_id_feature <- function(
    feature,
    cusec
) {
  
  if (is.null(feature$properties)) {
    feature$properties <- list()
  }
  
  feature$properties$geo_id <- as.character(cusec)
  
  feature
}


.leer_geojson_provincia <- function(
    ruta_geo,
    anho,
    provincia
) {
  
  fichero <- file.path(
    ruta_geo,
    as.character(anho),
    paste0(
      "prov_",
      sprintf(
        "%02d",
        as.integer(provincia)
      ),
      ".geojson"
    )
  )
  
  if (!file.exists(fichero)) {
    
    stop(
      "No existe el GeoJSON provincial: ",
      normalizePath(
        fichero,
        mustWork = FALSE
      ),
      call. = FALSE
    )
  }
  
  message(
    "Leyendo: ",
    normalizePath(
      fichero,
      mustWork = FALSE
    )
  )
  
  tiempo <- system.time({
    
    geojson <- jsonlite::fromJSON(
      fichero,
      simplifyVector = FALSE
    )
  })
  
  if (
    is.null(geojson$type) ||
    !identical(
      as.character(geojson$type),
      "FeatureCollection"
    )
  ) {
    
    stop(
      "El fichero no contiene un FeatureCollection válido: ",
      fichero,
      call. = FALSE
    )
  }
  
  if (
    is.null(geojson$features) ||
    length(geojson$features) == 0
  ) {
    
    stop(
      "El GeoJSON no contiene features: ",
      fichero,
      call. = FALSE
    )
  }
  
  message(
    "  Features: ",
    format(
      length(geojson$features),
      big.mark = ".",
      scientific = FALSE
    ),
    ". Tiempo: ",
    round(
      tiempo[["elapsed"]],
      2
    ),
    " segundos."
  )
  
  geojson
}


.leer_geojson_provincias <- function(
    ruta_geo,
    anho,
    provincias
) {
  
  if (
    is.null(provincias) ||
    length(provincias) == 0
  ) {
    
    stop(
      paste0(
        "Es necesario seleccionar al menos una provincia porque ",
        "los GeoJSON están particionados por provincia."
      ),
      call. = FALSE
    )
  }
  
  provincias <- unique(
    as.integer(provincias)
  )
  
  provincias <- provincias[
    !is.na(provincias)
  ]
  
  geojsons <- purrr::map(
    provincias,
    function(provincia) {
      
      .leer_geojson_provincia(
        ruta_geo = ruta_geo,
        anho = anho,
        provincia = provincia
      )
    }
  )
  
  features <- unlist(
    purrr::map(
      geojsons,
      "features"
    ),
    recursive = FALSE,
    use.names = FALSE
  )
  
  if (length(features) == 0) {
    
    stop(
      "No se ha leído ninguna feature de los GeoJSON provinciales.",
      call. = FALSE
    )
  }
  
  list(
    type = "FeatureCollection",
    features = features
  )
}


.preparar_geojson <- function(
    geojson,
    filtros
) {
  
  features <- geojson$features
  
  cusec <- purrr::map_chr(
    features,
    .extraer_cusec_feature
  )
  
  validos <- (
    !is.na(cusec) &
      stringr::str_detect(
        cusec,
        "^[0-9]{10}$"
      )
  )
  
  if (any(!validos)) {
    
    warning(
      "Se excluirán ",
      sum(!validos),
      " features sin CUSEC válido.",
      call. = FALSE
    )
    
    features <- features[validos]
    cusec <- cusec[validos]
  }
  
  if (length(features) == 0) {
    
    stop(
      "No quedan features con CUSEC válido.",
      call. = FALSE
    )
  }
  
  indice <- data.frame(
    posicion = seq_along(features),
    cusec = cusec,
    prov = as.integer(
      stringr::str_sub(
        cusec,
        1,
        2
      )
    ),
    mun = as.integer(
      stringr::str_sub(
        cusec,
        3,
        5
      )
    ),
    distrito = as.integer(
      stringr::str_sub(
        cusec,
        6,
        7
      )
    ),
    seccion = as.integer(
      stringr::str_sub(
        cusec,
        8,
        10
      )
    ),
    stringsAsFactors = FALSE
  )
  
  indice <- indice |>
    dplyr::mutate(
      provmun = prov * 1000L + mun
    )
  
  seleccion <- rep(
    TRUE,
    nrow(indice)
  )
  
  if (
    !is.null(filtros$provincias) &&
    length(filtros$provincias) > 0
  ) {
    
    seleccion <- (
      seleccion &
        indice$prov %in%
        as.integer(filtros$provincias)
    )
  }
  
  if (
    !is.null(filtros$municipios) &&
    length(filtros$municipios) > 0
  ) {
    
    seleccion <- (
      seleccion &
        indice$provmun %in%
        as.integer(filtros$municipios)
    )
  }
  
  if (
    !is.null(filtros$distritos) &&
    length(filtros$distritos) > 0
  ) {
    
    seleccion <- (
      seleccion &
        indice$distrito %in%
        as.integer(filtros$distritos)
    )
  }
  
  if (
    !is.null(filtros$secciones) &&
    length(filtros$secciones) > 0
  ) {
    
    seleccion <- (
      seleccion &
        indice$seccion %in%
        as.integer(filtros$secciones)
    )
  }
  
  indice <- indice[
    seleccion,
    ,
    drop = FALSE
  ]
  
  features <- features[
    indice$posicion
  ]
  
  if (length(features) == 0) {
    
    stop(
      "Los filtros territoriales no dejan ninguna feature.",
      call. = FALSE
    )
  }
  
  features <- purrr::map2(
    features,
    indice$cusec,
    .asignar_geo_id_feature
  )
  
  indice$posicion <- NULL
  rownames(indice) <- NULL
  
  message(
    "Features después del filtro territorial: ",
    format(
      length(features),
      big.mark = ".",
      scientific = FALSE
    )
  )
  
  list(
    geojson = list(
      type = "FeatureCollection",
      features = features
    ),
    indice = indice
  )
}


.obtener_datos_mapa <- function(
    ruta_resultados,
    ruta_geo,
    ruta_relmunicipios,
    filtros,
    partidos = NULL
) {
  
  if (is.null(filtros$eleccion)) {
    
    stop(
      "Es necesario indicar 'filtros$eleccion'.",
      call. = FALSE
    )
  }
  
  if (
    is.null(filtros$provincias) ||
    length(filtros$provincias) == 0
  ) {
    
    stop(
      paste0(
        "Es necesario indicar al menos una provincia en ",
        "'filtros$provincias'."
      ),
      call. = FALSE
    )
  }
  
  # Relación de municipios ----
  
  relmun <- openxlsx::read.xlsx(
    ruta_relmunicipios
  ) |>
    dplyr::mutate(
      COD = as.integer(COD)
    )
  
  # Resultados electorales ----
  
  ds_res <- arrow::open_dataset(
    ruta_resultados
  ) |>
    dplyr::filter(
      fecha_eleccion == filtros$eleccion
    )
  
  if (
    !is.null(filtros$provincias) &&
    length(filtros$provincias) > 0
  ) {
    
    ds_res <- ds_res |>
      dplyr::filter(
        prov %in% filtros$provincias
      )
  }
  
  if (
    !is.null(filtros$municipios) &&
    length(filtros$municipios) > 0
  ) {
    
    ds_res <- ds_res |>
      dplyr::filter(
        provmun %in% filtros$municipios
      )
  }
  
  if (
    !is.null(filtros$distritos) &&
    length(filtros$distritos) > 0
  ) {
    
    ds_res <- ds_res |>
      dplyr::filter(
        distrito %in% filtros$distritos
      )
  }
  
  if (
    !is.null(filtros$secciones) &&
    length(filtros$secciones) > 0
  ) {
    
    ds_res <- ds_res |>
      dplyr::filter(
        seccion %in% filtros$secciones
      )
  }
  
  variables_generales <- c(
    "seccion_id",
    "provmun",
    "censo_escrutinio",
    "participacion_pct",
    "votos_validos",
    "votos_blancos",
    "votos_nulos"
  )
  
  if (
    !is.null(partidos) &&
    length(partidos) > 0
  ) {
    
    variables_votos_partidos <- paste0(
      "votos_",
      partidos
    )
    
    variables_porcentaje_partidos <- paste0(
      "votos_",
      partidos,
      "_pct_validos"
    )
    
  } else {
    
    variables_votos_partidos <- character(0)
    variables_porcentaje_partidos <- character(0)
  }
  
  columnas_solicitadas <- unique(
    c(
      variables_generales,
      variables_votos_partidos,
      variables_porcentaje_partidos
    )
  )
  
  columnas_resultados <- intersect(
    columnas_solicitadas,
    names(ds_res)
  )
  
  if (!"seccion_id" %in% columnas_resultados) {
    
    stop(
      "No existe la columna 'seccion_id' en los resultados.",
      call. = FALSE
    )
  }
  
  if (!"provmun" %in% columnas_resultados) {
    
    stop(
      "No existe la columna 'provmun' en los resultados.",
      call. = FALSE
    )
  }
  
  resultados <- ds_res |>
    dplyr::select(
      dplyr::all_of(
        columnas_resultados
      )
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      cusec = gsub(
        "-",
        "",
        as.character(seccion_id)
      ),
      cusec = .normalizar_codigo_geo(
        cusec,
        ancho = 10
      ),
      provmun = as.integer(provmun)
    ) |>
    dplyr::select(
      -seccion_id
    )
  
  message(
    "Filas de resultados: ",
    format(
      nrow(resultados),
      big.mark = ".",
      scientific = FALSE
    )
  )
  
  # GeoJSON provincial ----
  
  anho_geo <- as.integer(
    substr(
      as.character(filtros$eleccion),
      1,
      4
    )
  )
  
  geojson_completo <- .leer_geojson_provincias(
    ruta_geo = ruta_geo,
    anho = anho_geo,
    provincias = filtros$provincias
  )
  
  preparacion_geo <- .preparar_geojson(
    geojson = geojson_completo,
    filtros = filtros
  )
  
  geojson_filtrado <- preparacion_geo$geojson
  geo <- preparacion_geo$indice
  
  # Join ----
  
  datos <- geo |>
    dplyr::left_join(
      resultados,
      by = c(
        "cusec",
        "provmun"
      )
    ) |>
    dplyr::left_join(
      relmun |>
        dplyr::select(
          dplyr::any_of(
            c(
              "COD",
              "NOMBRE",
              "DENOMINACION"
            )
          )
        ),
      by = c(
        "provmun" = "COD"
      )
    )
  
  # Partido ganador ----
  
  if (
    !is.null(partidos) &&
    length(partidos) > 0
  ) {
    
    variables_partidos <- paste0(
      "votos_",
      partidos,
      "_pct_validos"
    )
    
    variables_partidos <- intersect(
      variables_partidos,
      names(datos)
    )
    
    if (length(variables_partidos) > 0) {
      
      matriz <- as.matrix(
        datos[
          ,
          variables_partidos,
          drop = FALSE
        ]
      )
      
      storage.mode(matriz) <- "numeric"
      
      sin_resultados <- apply(
        matriz,
        1,
        function(x) {
          all(is.na(x))
        }
      )
      
      matriz_calculo <- matriz
      matriz_calculo[is.na(matriz_calculo)] <- -Inf
      
      etiquetas <- sub(
        "^votos_(.*)_pct_validos$",
        "\\1",
        variables_partidos
      )
      
      ganador <- etiquetas[
        max.col(
          matriz_calculo,
          ties.method = "first"
        )
      ]
      
      ganador[sin_resultados] <- NA_character_
      
      datos$ganador <- ganador
      
    } else {
      
      datos$ganador <- NA_character_
    }
  }
  
  # El GeoJSON se guarda como atributo y se reutiliza en todos los mapas.
  
  attr(
    datos,
    "geojson"
  ) <- geojson_filtrado
  
  datos
}


.construir_geojson <- function(datos) {
  
  geojson <- attr(
    datos,
    "geojson",
    exact = TRUE
  )
  
  if (is.null(geojson)) {
    
    stop(
      paste0(
        "datos no contiene el atributo 'geojson'. ",
        "Debe generarse con .obtener_datos_mapa()."
      ),
      call. = FALSE
    )
  }
  
  if (
    is.null(geojson$features) ||
    length(geojson$features) == 0
  ) {
    
    stop(
      "El GeoJSON no contiene features.",
      call. = FALSE
    )
  }
  
  geojson
}