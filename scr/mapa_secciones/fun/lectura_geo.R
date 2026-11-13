# Funciones de lectura y preparación geográfica sin sf ----


# Normalización de códigos ----

.normalizar_codigo <- function(
    x,
    ancho) {
  
  x <- as.character(x)
  
  x[is.na(x)] <- NA_character_
  
  # Evita valores como "28.0"
  x <- stringr::str_remove(
    x,
    "\\.0+$"
  )
  
  stringr::str_pad(
    x,
    width = ancho,
    side = "left",
    pad = "0"
  )
}


# Extraer coordenadas GeoJSON ----

.extraer_coordenadas <- function(x) {
  
  if (
    is.list(x) &&
    length(x) >= 2 &&
    is.numeric(x[[1]]) &&
    length(x[[1]]) == 1 &&
    is.numeric(x[[2]]) &&
    length(x[[2]]) == 1
  ) {
    return(
      matrix(
        c(
          as.numeric(x[[1]]),
          as.numeric(x[[2]])
        ),
        nrow = 1,
        dimnames = list(
          NULL,
          c("lon", "lat")
        )
      )
    )
  }
  
  if (!is.list(x)) {
    return(NULL)
  }
  
  partes <- lapply(
    x,
    .extraer_coordenadas
  )
  
  partes <- Filter(
    Negate(is.null),
    partes
  )
  
  if (length(partes) == 0) {
    return(NULL)
  }
  
  do.call(
    rbind,
    partes
  )
}


# Bounding box de una feature ----

.bbox_feature <- function(feature) {
  
  coordenadas <- .extraer_coordenadas(
    feature$geometry$coordinates
  )
  
  if (
    is.null(coordenadas) ||
    nrow(coordenadas) == 0
  ) {
    return(NULL)
  }
  
  coordenadas <- coordenadas[
    stats::complete.cases(coordenadas),
    ,
    drop = FALSE
  ]
  
  if (nrow(coordenadas) == 0) {
    return(NULL)
  }
  
  c(
    xmin = min(coordenadas[, "lon"]),
    ymin = min(coordenadas[, "lat"]),
    xmax = max(coordenadas[, "lon"]),
    ymax = max(coordenadas[, "lat"])
  )
}


# Bounding box de un FeatureCollection ----

.bbox_geojson <- function(geojson) {
  
  if (
    is.null(geojson$features) ||
    length(geojson$features) == 0
  ) {
    stop(
      "El GeoJSON no contiene features.",
      call. = FALSE
    )
  }
  
  cajas <- lapply(
    geojson$features,
    .bbox_feature
  )
  
  cajas <- Filter(
    Negate(is.null),
    cajas
  )
  
  if (length(cajas) == 0) {
    stop(
      "No se han podido extraer coordenadas del GeoJSON.",
      call. = FALSE
    )
  }
  
  cajas <- do.call(
    rbind,
    cajas
  )
  
  c(
    xmin = min(cajas[, "xmin"]),
    ymin = min(cajas[, "ymin"]),
    xmax = max(cajas[, "xmax"]),
    ymax = max(cajas[, "ymax"])
  )
}


# Lectura de secciones y mesas desde Parquet ----
.leer_secciones_geo <- function(
    ruta,
    aaaamm,
    cod_prov = NULL,
    cod_mun = NULL,
    cod_dis = NULL,
    cod_sec = NULL) {
  
  if (!dir.exists(ruta)) {
    stop(
      "No existe el dataset geográfico: ",
      normalizePath(
        ruta,
        mustWork = FALSE
      ),
      call. = FALSE
    )
  }
  
  if (
    length(aaaamm) != 1 ||
    is.na(aaaamm)
  ) {
    stop(
      "Debe indicarse un único periodo geográfico AAAAMM.",
      call. = FALSE
    )
  }
  
  aaaamm <- as.integer(aaaamm)
  
  
  # Abrir dataset particionado
  tiempo_arrow <- system.time({
    
    consulta <- arrow::open_dataset(
      ruta,
      format = "parquet"
    ) |>
      dplyr::filter(
        aaaamm == !!aaaamm
      )
    
    if (!is.null(cod_prov)) {
      
      valor <- .normalizar_codigo(
        cod_prov,
        2
      )
      
      consulta <- consulta |>
        dplyr::filter(
          prov == !!valor
        )
    }
    
    if (!is.null(cod_mun)) {
      
      valor <- .normalizar_codigo(
        cod_mun,
        3
      )
      
      consulta <- consulta |>
        dplyr::filter(
          mun == !!valor
        )
    }
    
    if (!is.null(cod_dis)) {
      
      valor <- .normalizar_codigo(
        cod_dis,
        2
      )
      
      consulta <- consulta |>
        dplyr::filter(
          distrito == !!valor
        )
    }
    
    if (!is.null(cod_sec)) {
      
      valor <- .normalizar_codigo(
        cod_sec,
        3
      )
      
      consulta <- consulta |>
        dplyr::filter(
          seccion == !!valor
        )
    }
    
    datos_completos <- consulta |>
      dplyr::collect()
  })
  
  message(
    "Lectura y filtrado Arrow: ",
    round(
      tiempo_arrow[["elapsed"]],
      2
    ),
    " segundos."
  )
  
  if (nrow(datos_completos) == 0) {
    stop(
      paste0(
        "No se han encontrado geometrías para aaaamm = ",
        aaaamm,
        ". Provincia: ",
        if (is.null(cod_prov)) "NULL" else cod_prov,
        "; municipio: ",
        if (is.null(cod_mun)) "NULL" else cod_mun,
        "; distrito: ",
        if (is.null(cod_dis)) "NULL" else cod_dis,
        "; sección: ",
        if (is.null(cod_sec)) "NULL" else cod_sec,
        "."
      ),
      call. = FALSE
    )
  }
  
  columnas_necesarias <- c(
    "cusec",
    "prov",
    "mun",
    "distrito",
    "seccion",
    "feature_json"
  )
  
  columnas_faltantes <- setdiff(
    columnas_necesarias,
    names(datos_completos)
  )
  
  if (length(columnas_faltantes) > 0) {
    stop(
      "Faltan columnas en el dataset geográfico: ",
      paste(
        columnas_faltantes,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  datos_completos <- datos_completos |>
    dplyr::mutate(
      cusec = .normalizar_codigo(
        cusec,
        10
      ),
      prov = .normalizar_codigo(
        prov,
        2
      ),
      mun = .normalizar_codigo(
        mun,
        3
      ),
      distrito = .normalizar_codigo(
        distrito,
        2
      ),
      seccion = .normalizar_codigo(
        seccion,
        3
      )
    )
  
  if (anyNA(datos_completos$cusec)) {
    stop(
      "Hay filas del dataset geográfico sin CUSEC.",
      call. = FALSE
    )
  }
  
  # Se admiten CUSEC multipartes.
  # Cada feature recibe un identificador geográfico único.
  datos_completos <- datos_completos |>
    dplyr::group_by(
      cusec
    ) |>
    dplyr::mutate(
      indice_geometria = dplyr::row_number(),
      geo_id = stringr::str_c(
        cusec,
        indice_geometria,
        sep = "__"
      )
    ) |>
    dplyr::ungroup()
  
  tiempo_geojson <- system.time({
    
    features <- lapply(
      seq_len(nrow(datos_completos)),
      function(i) {
        
        cadena <- datos_completos$feature_json[[i]]
        
        if (
          is.na(cadena) ||
          !nzchar(cadena)
        ) {
          stop(
            "La sección ",
            datos_completos$cusec[[i]],
            " no tiene feature_json.",
            call. = FALSE
          )
        }
        
        feature <- jsonlite::fromJSON(
          cadena,
          simplifyVector = FALSE
        )
        
        if (
          identical(
            feature$type,
            "FeatureCollection"
          ) &&
          length(feature$features) == 1
        ) {
          feature <- feature$features[[1]]
        }
        
        if (!identical(feature$type, "Feature")) {
          stop(
            "feature_json no contiene una Feature válida para CUSEC ",
            datos_completos$cusec[[i]],
            ".",
            call. = FALSE
          )
        }
        
        if (is.null(feature$properties)) {
          feature$properties <- list()
        }
        
        feature$properties$cusec <-
          datos_completos$cusec[[i]]
        
        feature$properties$geo_id <-
          datos_completos$geo_id[[i]]
        
        feature
      }
    )
  })
  
  message(
    "Reconstrucción de ",
    length(features),
    " features: ",
    round(
      tiempo_geojson[["elapsed"]],
      2
    ),
    " segundos."
  )
  
  geojson <- list(
    type = "FeatureCollection",
    features = features
  )
  
  bb <- .bbox_geojson(
    geojson
  )
  
  coordenadas_validas <- (
    bb["xmin"] >= -180 &&
      bb["xmax"] <= 180 &&
      bb["ymin"] >= -90 &&
      bb["ymax"] <= 90
  )
  
  if (!coordenadas_validas) {
    stop(
      paste0(
        "Las geometrías no están en EPSG:4326. ",
        "Extensión detectada: xmin=",
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
  
  datos <- datos_completos |>
    dplyr::select(
      -feature_json
    ) |>
    dplyr::select(
      geo_id,
      cusec,
      prov,
      mun,
      distrito,
      seccion,
      dplyr::everything()
    )
  
  message(
    "Periodo geográfico: ",
    aaaamm,
    " | filas geográficas: ",
    format(
      nrow(datos),
      big.mark = ".",
      decimal.mark = ",",
      scientific = FALSE
    ),
    " | CUSEC distintos: ",
    format(
      dplyr::n_distinct(datos$cusec),
      big.mark = ".",
      decimal.mark = ",",
      scientific = FALSE
    )
  )
  
  list(
    datos = datos,
    geojson = geojson
  )
}

.leer_resultados_seccion <- function(
    ruta,
    eleccion = NULL,
    cod_prov = NULL,
    cod_mun = NULL,
    cod_dis = NULL,
    cod_sec = NULL
) {
  
  if (!file.exists(ruta) && !dir.exists(ruta)) {
    stop(
      "No existe el fichero o dataset de resultados: ",
      normalizePath(
        ruta,
        mustWork = FALSE
      ),
      call. = FALSE
    )
  }
  
  
  # Abrir sin cargar en memoria ----
  consulta <- arrow::open_dataset(
    ruta,
    format = "parquet"
  )
  
  
  # Comprobar columnas disponibles ----
  columnas_disponibles <- names(
    consulta$schema
  )
  
  columnas_clave <- c(
    "fecha_eleccion",
    "prov",
    "mun",
    "distrito",
    "seccion"
  )
  
  columnas_faltantes <- setdiff(
    columnas_clave,
    columnas_disponibles
  )
  
  if (length(columnas_faltantes) > 0) {
    stop(
      "Faltan columnas territoriales en los resultados: ",
      paste(
        columnas_faltantes,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  
  # Filtro por elección ----
  if (!is.null(eleccion)) {
    
    eleccion_filtrada <- as.character(
      eleccion
    )
    
    consulta <- consulta |>
      dplyr::filter(
        fecha_eleccion == eleccion_filtrada
      )
  }
  
  
  # Filtro por provincia ----
  if (!is.null(cod_prov)) {
    
    prov_filtrada <- .normalizar_codigo(
      cod_prov,
      2
    )
    
    consulta <- consulta |>
      dplyr::filter(
        prov == prov_filtrada
      )
  }
  
  
  # Filtro por municipio ----
  if (!is.null(cod_mun)) {
    
    mun_filtrado <- .normalizar_codigo(
      cod_mun,
      3
    )
    
    consulta <- consulta |>
      dplyr::filter(
        mun == mun_filtrado
      )
  }
  
  
  # Filtro por distrito ----
  if (!is.null(cod_dis)) {
    
    distrito_filtrado <- .normalizar_codigo(
      cod_dis,
      2
    )
    
    consulta <- consulta |>
      dplyr::filter(
        distrito == distrito_filtrado
      )
  }
  
  
  # Filtro por sección ----
  if (!is.null(cod_sec)) {
    
    seccion_filtrada <- .normalizar_codigo(
      cod_sec,
      3
    )
    
    consulta <- consulta |>
      dplyr::filter(
        seccion == seccion_filtrada
      )
  }
  
  
  # Ejecutar la consulta ----
  tiempo_lectura <- system.time({
    
    resultados <- consulta |>
      dplyr::collect()
    
  })
  
  
  # Validar resultado ----
  if (nrow(resultados) == 0) {
    stop(
      paste0(
        "Los filtros no han seleccionado ningún resultado. ",
        "Elección: ",
        if (is.null(eleccion)) "NULL" else eleccion,
        "; provincia: ",
        if (is.null(cod_prov)) "NULL" else cod_prov,
        "; municipio: ",
        if (is.null(cod_mun)) "NULL" else cod_mun,
        "; distrito: ",
        if (is.null(cod_dis)) "NULL" else cod_dis,
        "; sección: ",
        if (is.null(cod_sec)) "NULL" else cod_sec,
        "."
      ),
      call. = FALSE
    )
  }
  
  
  message(
    "Resultados leídos: ",
    format(
      nrow(resultados),
      big.mark = ".",
      decimal.mark = ",",
      scientific = FALSE
    ),
    " filas en ",
    round(
      tiempo_lectura[["elapsed"]],
      2
    ),
    " segundos."
  )
  
  message(
    "Elecciones cargadas: ",
    paste(
      sort(
        unique(
          resultados$fecha_eleccion
        )
      ),
      collapse = ", "
    )
  )
  
  
  resultados
}

# Preparación de resultados para el mapa ----
.preparar_resultados_mapa <- function(
    resultados,
    partidos,
    eleccion = NULL,
    id_col = "seccion_id") {
  
  if (!"fecha_eleccion" %in% names(resultados)) {
    stop(
      "No existe la columna fecha_eleccion en resultados.",
      call. = FALSE
    )
  }
  
  if (is.null(eleccion)) {
    
    elecciones_disponibles <- resultados$fecha_eleccion[
      !is.na(resultados$fecha_eleccion)
    ]
    
    if (length(elecciones_disponibles) == 0) {
      stop(
        "No hay elecciones disponibles en resultados.",
        call. = FALSE
      )
    }
    
    eleccion <- max(
      elecciones_disponibles
    )
    
    message(
      "Elección no indicada. Se usa la última: ",
      eleccion
    )
  }
  
  cols_pct <- stringr::str_c(
    "votos_",
    partidos,
    "_pct_validos"
  )
  
  if (!id_col %in% names(resultados)) {
    stop(
      "No existe la columna identificadora ",
      id_col,
      " en resultados.",
      call. = FALSE
    )
  }
  
  columnas_partidos_faltantes <- setdiff(
    cols_pct,
    names(resultados)
  )
  
  if (length(columnas_partidos_faltantes) > 0) {
    
    warning(
      "Se crearán con valor cero las siguientes columnas ausentes: ",
      paste(
        columnas_partidos_faltantes,
        collapse = ", "
      ),
      call. = FALSE
    )
    
    resultados[
      columnas_partidos_faltantes
    ] <- 0
  }
  
  res <- resultados |>
    dplyr::filter(
      fecha_eleccion == eleccion
    )
  
  if (nrow(res) == 0) {
    stop(
      "No hay resultados para la elección ",
      eleccion,
      ". Disponibles: ",
      paste(
        sort(
          unique(
            resultados$fecha_eleccion
          )
        ),
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  m <- as.matrix(
    res[, cols_pct]
  )
  
  storage.mode(m) <- "numeric"
  
  m[is.na(m)] <- -Inf
  
  maximo <- apply(
    m,
    1,
    max
  )
  
  idx <- max.col(
    m,
    ties.method = "first"
  )
  
  margen <- apply(
    m,
    1,
    function(x) {
      
      x <- x[
        is.finite(x)
      ]
      
      if (length(x) < 2) {
        return(NA_real_)
      }
      
      s <- sort(
        x,
        decreasing = TRUE
      )
      
      s[1] - s[2]
    }
  )
  
  ganador <- partidos[idx]
  
  ganador[
    !is.finite(maximo)
  ] <- NA_character_
  
  res |>
    dplyr::mutate(
      cusec = stringr::str_remove_all(
        as.character(.data[[id_col]]),
        "-"
      ),
      cusec = .normalizar_codigo(
        cusec,
        10
      ),
      ganador = ganador,
      margen_ganador = margen
    )
}


# Unión entre atributos geográficos y resultados ----

.unir_mapa_resultados <- function(
    geo,
    res) {
  
  if (
    is.null(geo$datos) ||
    is.null(geo$geojson)
  ) {
    stop(
      "El objeto geo debe contener datos y geojson.",
      call. = FALSE
    )
  }
  
  datos_geo <- geo$datos
  
  sin_resultado <- sum(
    !datos_geo$cusec %in% res$cusec
  )
  
  sin_geometria <- sum(
    !res$cusec %in% datos_geo$cusec
  )
  
  message(
    "Secciones en geometría: ",
    nrow(datos_geo),
    " | con resultado: ",
    nrow(datos_geo) - sin_resultado,
    " | sin resultado: ",
    sin_resultado,
    " | resultados sin geometría: ",
    sin_geometria
  )
  
  res <- res |>
    dplyr::select(
      -dplyr::any_of(
        c(
          "ccaa",
          "prov",
          "mun",
          "distrito",
          "seccion"
        )
      )
    )
  
  datos_unidos <- datos_geo |>
    dplyr::left_join(
      res,
      by = "cusec"
    )
  
  list(
    datos = datos_unidos,
    geojson = geo$geojson
  )
}