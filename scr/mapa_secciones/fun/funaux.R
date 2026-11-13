.formatear_numero <- function(
    x,
    decimales = 1,
    sufijo = "") {
  
  resultado <- ifelse(
    is.na(x),
    "Sin dato",
    paste0(
      format(
        round(
          x,
          decimales
        ),
        nsmall = decimales,
        decimal.mark = ",",
        big.mark = "."
      ),
      sufijo
    )
  )
  
  resultado
}

.obtener_datos_mapa <- function(
    ruta_resultados,
    ruta_geo,
    ruta_relmunicipios,
    filtros,
    partidos = NULL
) {
  
  # Relación municipios
  relmun <- openxlsx::read.xlsx(
    ruta_relmunicipios
  )
  
  # Resultados electorales
  ds_res <- arrow::open_dataset(
    ruta_resultados
  )
  
  # Aplicar filtros antes de seleccionar y recopilar
  if (!is.null(filtros$eleccion)) {
    
    ds_res <- ds_res |>
      dplyr::filter(
        fecha_eleccion == filtros$eleccion
      )
  }
  
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
  
  # Selección de columnas necesarias
  
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
      "No existe la columna 'seccion_id' en el dataset de resultados.",
      call. = FALSE
    )
  }
  
  resultados <- ds_res |>
    dplyr::select(
      dplyr::all_of(
        columnas_resultados
      )
    ) |>
    dplyr::collect()
  
  resultados <- resultados |>
    dplyr::mutate(
      cusec = gsub(
        "-",
        "",
        as.character(seccion_id)
      )
    ) |>
    dplyr::select(
      -seccion_id
    )
  
  # Geografía
  if (is.null(filtros$eleccion)) {
    
    stop(
      "Es necesario indicar 'filtros$eleccion' para seleccionar la geografía.",
      call. = FALSE
    )
  }
  
  aaaamm_geo <- as.integer(
    paste0(
      substr(
        as.character(filtros$eleccion),
        1,
        4
      ),
      "01"
    )
  )
  
  ds_geo <- arrow::open_dataset(
    ruta_geo
  ) |>
    dplyr::mutate(
      provmun = prov * 1000L + mun
    ) |>
    dplyr::filter(
      aaaamm == aaaamm_geo
    )
  
  if (
    !is.null(filtros$provincias) &&
    length(filtros$provincias) > 0
  ) {
    
    ds_geo <- ds_geo |>
      dplyr::filter(
        prov %in% filtros$provincias
      )
  }
  
  if (
    !is.null(filtros$municipios) &&
    length(filtros$municipios) > 0
  ) {
    
    ds_geo <- ds_geo |>
      dplyr::filter(
        provmun %in% filtros$municipios
      )
  }
  
  if (
    !is.null(filtros$distritos) &&
    length(filtros$distritos) > 0
  ) {
    
    ds_geo <- ds_geo |>
      dplyr::filter(
        distrito %in% filtros$distritos
      )
  }
  
  if (
    !is.null(filtros$secciones) &&
    length(filtros$secciones) > 0
  ) {
    
    ds_geo <- ds_geo |>
      dplyr::filter(
        seccion %in% filtros$secciones
      )
  }
  
  geo <- ds_geo |>
    dplyr::select(
      cusec,
      provmun,
      feature_json
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      cusec = as.character(cusec)
    )
  
  # Join
  
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
          COD,
          NOMBRE,
          DENOMINACION
        ),
      by = c(
        "provmun" = "COD"
      )
    )
  
  # Cálculo del partido ganador
  
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
  
  datos
}

.construir_geojson <- function(datos) {
  
  features <- lapply(
    seq_len(nrow(datos)),
    function(i) {
      
      feature <- jsonlite::fromJSON(
        datos$feature_json[i],
        simplifyVector = FALSE
      )
      
      feature$properties$geo_id <- datos$cusec[i]
      
      feature
    }
  )
  
  list(
    type = "FeatureCollection",
    features = features
  )
}

.mapa_plotly <- function(
    datos,
    variable,
    partidos,
    titulo = variable,
    estilo = "open-street-map",
    paletacolpartidos = PALETA
) {
  
  if (nrow(datos) == 0) {
    stop(
      "No hay datos para representar.",
      call. = FALSE
    )
  }
  
  if (!variable %in% names(datos)) {
    stop(
      "No existe la variable '",
      variable,
      "' en los datos.",
      call. = FALSE
    )
  }
  
  geojson <- .construir_geojson(
    datos
  )
  
  if (
    is.null(geojson$features) ||
    length(geojson$features) == 0
  ) {
    stop(
      "El GeoJSON no contiene features.",
      call. = FALSE
    )
  }
  
  ids_geojson <- purrr::map_chr(
    geojson$features,
    function(feature) {
      as.character(
        feature$properties$geo_id
      )
    }
  )
  
  coincidentes <- datos$cusec %in% ids_geojson
  
  if (!any(coincidentes)) {
    
    stop(
      paste0(
        "Ningún CUSEC coincide con el GeoJSON. ",
        "Ejemplo datos: ",
        paste(
          head(datos$cusec, 3),
          collapse = ", "
        ),
        ". Ejemplo GeoJSON: ",
        paste(
          head(ids_geojson, 3),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  bbox <- .bbox_geojson(
    geojson
  )
  
  centro <- list(
    lon = unname(
      (
        bbox["xmin"] +
          bbox["xmax"]
      ) / 2
    ),
    lat = unname(
      (
        bbox["ymin"] +
          bbox["ymax"]
      ) / 2
    )
  )
  
  dx <- unname(
    bbox["xmax"] -
      bbox["xmin"]
  )
  
  dy <- unname(
    bbox["ymax"] -
      bbox["ymin"]
  )
  
  span <- max(
    dx,
    dy / cos(
      centro$lat * pi / 180
    ),
    1e-6
  )
  
  zoom <- log2(
    360 / span
  ) - 1
  
  zoom <- max(
    0,
    min(
      zoom,
      20
    )
  )
  
  valores <- datos[[variable]]
  
  hover <- .popup_seccion(
    datos,
    partidos
  )
  
  linea <- list(
    color = "white",
    width = 0.7
  )
  
  fig <- plotly::plot_ly()
  
  if (is.numeric(valores)) {
    
    sel <- (
      !is.na(valores) &
        datos$cusec %in% ids_geojson
    )
    
    if (!any(sel)) {
      
      stop(
        "La variable numérica '",
        variable,
        "' no tiene valores representables.",
        call. = FALSE
      )
    }
    
    fig <- plotly::add_trace(
      fig,
      type = "choroplethmapbox",
      geojson = geojson,
      featureidkey = "properties.geo_id",
      locations = as.character(
        datos$cusec[sel]
      ),
      z = as.numeric(
        valores[sel]
      ),
      text = hover[sel],
      hoverinfo = "text",
      colorscale = "Viridis",
      zmin = min(
        valores[sel],
        na.rm = TRUE
      ),
      zmax = max(
        valores[sel],
        na.rm = TRUE
      ),
      marker = list(
        opacity = 0.85,
        line = linea
      ),
      colorbar = list(
        title = list(
          text = titulo
        )
      ),
      showscale = TRUE
    )
    
  } else {
    
    categorias <- unique(
      stats::na.omit(
        as.character(valores)
      )
    )
    
    if (length(categorias) == 0) {
      
      stop(
        "La variable categórica '",
        variable,
        "' no tiene valores representables.",
        call. = FALSE
      )
    }
    
    for (categoria in categorias) {
      
      sel <- (
        !is.na(valores) &
          as.character(valores) == categoria &
          datos$cusec %in% ids_geojson
      )
      
      if (!any(sel)) {
        next
      }
      
      color <- paletacolpartidos[
        categoria
      ]
      
      if (
        length(color) == 0 ||
        is.na(color)
      ) {
        color <- "#9E9E9E"
      }
      
      fig <- plotly::add_trace(
        fig,
        type = "choroplethmapbox",
        geojson = geojson,
        featureidkey = "properties.geo_id",
        locations = as.character(
          datos$cusec[sel]
        ),
        z = rep(
          1,
          sum(sel)
        ),
        text = hover[sel],
        hoverinfo = "text",
        colorscale = list(
          list(
            0,
            unname(color)
          ),
          list(
            1,
            unname(color)
          )
        ),
        zmin = 0,
        zmax = 1,
        showscale = FALSE,
        showlegend = TRUE,
        name = categoria,
        marker = list(
          opacity = 0.85,
          line = linea
        )
      )
    }
  }
  
  plotly::layout(
    fig,
    title = list(
      text = titulo,
      x = 0.5,
      xanchor = "center"
    ),
    margin = list(
      l = 0,
      r = 0,
      t = 50,
      b = 0
    ),
    mapbox = list(
      style = estilo,
      center = centro,
      zoom = zoom
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = 0
    )
  )
}

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

.popup_seccion <- function(
    datos,
    partidos) {
  
  lineas <- purrr::map(
    partidos,
    function(p) {
      
      columna <- stringr::str_c(
        "votos_",
        p,
        "_pct_validos"
      )
      
      if (!columna %in% names(datos)) {
        return(
          rep(
            paste0(
              p,
              ": sin dato"
            ),
            nrow(datos)
          )
        )
      }
      
      stringr::str_c(
        p,
        ": ",
        .formatear_numero(
          datos[[columna]],
          decimales = 1,
          sufijo = "%"
        )
      )
    }
  )
  
  cuerpo <- purrr::reduce(
    lineas,
    function(x, y) {
      stringr::str_c(
        x,
        "<br>",
        y
      )
    }
  )
  
  censo <- if (
    "censo_escrutinio" %in% names(datos)
  ) {
    ifelse(
      is.na(datos$censo_escrutinio),
      "Sin dato",
      format(
        datos$censo_escrutinio,
        big.mark = ".",
        scientific = FALSE
      )
    )
  } else {
    rep(
      "Sin dato",
      nrow(datos)
    )
  }
  
  participacion <- if (
    "participacion_pct" %in% names(datos)
  ) {
    .formatear_numero(
      datos$participacion_pct,
      decimales = 1,
      sufijo = "%"
    )
  } else {
    rep(
      "Sin dato",
      nrow(datos)
    )
  }
  
  stringr::str_c(
    "<b>Sección ",
    datos$cusec,
    "</b><br>",
    "Censo: ",
    censo,
    "<br>",
    "Participación: ",
    participacion,
    "<br>",
    cuerpo
  )
}
