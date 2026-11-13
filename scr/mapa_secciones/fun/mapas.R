# Mapas de resultados por sección sin sf
# Paleta de partidos ----
.paleta_partidos <- c(
  PSOE = "#E53935",
  PP = "#1E88E5",
  VOX = "#7CB342",
  IU = "#B71C1C",
  PODEMOS = "#6A1B9A",
  SUMAR = "#E91E63",
  UPYD = "#D81B60",
  CIUDADANOS = "#FB8C00",
  CIU_JUNTS = "#00A6A6",
  ERC = "#F9A825",
  PNV = "#00897B",
  HB_EH_BILDU = "#66A61E",
  BNG = "#1976D2",
  CC = "#FDD835",
  UPN = "#0D47A1",
  CDS = "#2E7D32",
  PA = "#43A047",
  PAR = "#FF8F00",
  CHA = "#8E0000",
  EA = "#C0CA33",
  EE = "#D4E157",
  NA_BAI_GEROA_BAI = "#7CB342",
  COMPROMIS = "#F57C00",
  PRC = "#6D4C41",
  FORO = "#3949AB",
  CUP = "#FBC02D",
  TERUEL_EXISTE = "#8D6E63",
  OTROS = "#9E9E9E"
)

# Conversión de anillos GeoJSON a data frame ----

.anillo_a_df <- function(
    anillo,
    cusec,
    grupo) {
  
  if (
    is.null(anillo) ||
    length(anillo) == 0
  ) {
    return(NULL)
  }
  
  coordenadas <- purrr::map_dfr(
    seq_along(anillo),
    function(i) {
      
      punto <- anillo[[i]]
      
      if (
        !is.list(punto) ||
        length(punto) < 2
      ) {
        return(NULL)
      }
      
      tibble::tibble(
        lon = as.numeric(punto[[1]]),
        lat = as.numeric(punto[[2]]),
        orden = i
      )
    }
  )
  
  if (nrow(coordenadas) == 0) {
    return(NULL)
  }
  
  coordenadas |>
    dplyr::mutate(
      cusec = cusec,
      grupo = grupo
    )
}


# Conversión de Polygon a data frame ----

.polygon_a_df <- function(
    coordinates,
    cusec,
    indice_feature,
    indice_poligono = 1) {
  
  if (
    is.null(coordinates) ||
    length(coordinates) == 0
  ) {
    return(NULL)
  }
  
  # Cada elemento es un anillo.
  # El primero es el contorno exterior.
  # Los restantes son huecos.
  purrr::map_dfr(
    seq_along(coordinates),
    function(indice_anillo) {
      
      grupo <- paste(
        indice_feature,
        indice_poligono,
        indice_anillo,
        sep = "_"
      )
      
      .anillo_a_df(
        coordinates[[indice_anillo]],
        cusec = cusec,
        grupo = grupo
      ) |>
        dplyr::mutate(
          anillo = indice_anillo,
          exterior = indice_anillo == 1
        )
    }
  )
}


# Conversión de una feature a data frame ----

.feature_a_df <- function(
    feature,
    indice_feature) {
  
  tipo <- feature$geometry$type
  coordinates <- feature$geometry$coordinates
  cusec <- as.character(
    feature$properties$cusec
  )
  
  if (identical(tipo, "Polygon")) {
    
    return(
      .polygon_a_df(
        coordinates,
        cusec = cusec,
        indice_feature = indice_feature,
        indice_poligono = 1
      )
    )
  }
  
  if (identical(tipo, "MultiPolygon")) {
    
    return(
      purrr::map_dfr(
        seq_along(coordinates),
        function(indice_poligono) {
          .polygon_a_df(
            coordinates[[indice_poligono]],
            cusec = cusec,
            indice_feature = indice_feature,
            indice_poligono = indice_poligono
          )
        }
      )
    )
  }
  
  warning(
    "Geometría no admitida para mapa estático: ",
    tipo,
    ". CUSEC: ",
    cusec,
    call. = FALSE
  )
  
  NULL
}


# Convertir GeoJSON completo a tabla de polígonos ----

.geojson_a_df <- function(geojson) {
  
  poligonos <- purrr::map_dfr(
    seq_along(geojson$features),
    function(i) {
      .feature_a_df(
        geojson$features[[i]],
        indice_feature = i
      )
    }
  )
  
  if (nrow(poligonos) == 0) {
    stop(
      "No se han podido convertir las geometrías a polígonos.",
      call. = FALSE
    )
  }
  
  poligonos
}


# Centroides aproximados para etiquetas ----
#
# No son centroides espaciales exactos. Se calculan como la media
# de las coordenadas del anillo exterior, suficiente para etiquetar
# estas secciones sin utilizar sf.

.centros_poligonos <- function(poligonos) {
  
  poligonos |>
    dplyr::filter(exterior) |>
    dplyr::group_by(cusec) |>
    dplyr::summarise(
      lon = mean(
        lon,
        na.rm = TRUE
      ),
      lat = mean(
        lat,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}


# Mapa estático sin sf ----

.mapa_estatico <- function(
    mapa,
    variable,
    titulo = NULL,
    leyenda = variable,
    etiquetas = FALSE,
    tam_etiqueta = 2.5,
    margen = 0.03) {
  
  if (
    is.null(mapa$datos) ||
    is.null(mapa$geojson)
  ) {
    stop(
      "El objeto mapa debe contener datos y geojson.",
      call. = FALSE
    )
  }
  
  datos <- mapa$datos
  
  if (!variable %in% names(datos)) {
    stop(
      "No existe la variable '",
      variable,
      "' en los datos.",
      call. = FALSE
    )
  }
  
  poligonos <- .geojson_a_df(
    mapa$geojson
  )
  
  poligonos <- poligonos |>
    dplyr::left_join(
      datos |>
        dplyr::select(
          cusec,
          valor = dplyr::all_of(variable)
        ),
      by = "cusec"
    )
  
  xmin <- min(
    poligonos$lon,
    na.rm = TRUE
  )
  
  xmax <- max(
    poligonos$lon,
    na.rm = TRUE
  )
  
  ymin <- min(
    poligonos$lat,
    na.rm = TRUE
  )
  
  ymax <- max(
    poligonos$lat,
    na.rm = TRUE
  )
  
  dx <- xmax - xmin
  dy <- ymax - ymin
  
  xlim <- c(
    xmin - margen * dx,
    xmax + margen * dx
  )
  
  ylim <- c(
    ymin - margen * dy,
    ymax + margen * dy
  )
  
  p <- ggplot2::ggplot(
    poligonos,
    ggplot2::aes(
      x = lon,
      y = lat,
      group = grupo,
      fill = valor
    )
  ) +
    ggplot2::geom_polygon(
      colour = "white",
      linewidth = 0.15
    ) +
    ggplot2::coord_fixed(
      ratio = 1 / cos(
        mean(
          c(ymin, ymax)
        ) * pi / 180
      ),
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    ) +
    ggplot2::labs(
      title = titulo,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        size = 14,
        margin = ggplot2::margin(
          b = 8
        )
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(
        size = 9
      ),
      legend.text = ggplot2::element_text(
        size = 8
      ),
      plot.margin = ggplot2::margin(
        8,
        8,
        8,
        8
      )
    )
  
  if (etiquetas) {
    
    centros <- .centros_poligonos(
      poligonos
    ) |>
      dplyr::left_join(
        datos |>
          dplyr::select(
            cusec,
            seccion
          ),
        by = "cusec"
      )
    
    p <- p +
      ggplot2::geom_text(
        data = centros,
        ggplot2::aes(
          x = lon,
          y = lat,
          label = seccion
        ),
        inherit.aes = FALSE,
        size = tam_etiqueta,
        colour = "grey15",
        check_overlap = TRUE
      )
  }
  
  if (is.numeric(datos[[variable]])) {
    
    p <- p +
      ggplot2::scale_fill_viridis_c(
        name = leyenda,
        na.value = "grey85",
        guide = ggplot2::guide_colourbar(
          title.position = "top",
          title.hjust = 0.5,
          barwidth = 14,
          barheight = 0.6
        )
      )
    
  } else {
    
    valores_presentes <- unique(
      stats::na.omit(
        as.character(
          datos[[variable]]
        )
      )
    )
    
    colores <- .paleta_partidos[
      intersect(
        names(.paleta_partidos),
        valores_presentes
      )
    ]
    
    # Para cualquier categoría no incluida en la paleta
    faltan_colores <- setdiff(
      valores_presentes,
      names(colores)
    )
    
    if (length(faltan_colores) > 0) {
      
      colores_adicionales <- grDevices::hcl.colors(
        length(faltan_colores),
        palette = "Dark 3"
      )
      
      names(colores_adicionales) <- faltan_colores
      
      colores <- c(
        colores,
        colores_adicionales
      )
    }
    
    p <- p +
      ggplot2::scale_fill_manual(
        name = leyenda,
        values = colores,
        na.value = "grey85",
        guide = ggplot2::guide_legend(
          title.position = "top",
          title.hjust = 0.5,
          nrow = 1
        )
      )
  }
  
  p
}


# Popup de cada sección ----

.popup_seccion <- function(
    datos,
    partidos) {
  
  formatear_numero <- function(
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
        formatear_numero(
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
    formatear_numero(
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


# Mapa interactivo Plotly sin sf ----

.mapa_plotly <- function(
    mapa,
    variable,
    partidos,
    titulo = variable,
    estilo = "open-street-map") {
  
  if (
    is.null(mapa$datos) ||
    is.null(mapa$geojson)
  ) {
    stop(
      "El objeto mapa debe contener datos y geojson.",
      call. = FALSE
    )
  }
  
  datos <- mapa$datos
  geojson <- mapa$geojson
  
  if (!variable %in% names(datos)) {
    stop(
      "No existe la variable '",
      variable,
      "' en los datos.",
      call. = FALSE
    )
  }
  
  if (
    is.null(geojson$features) ||
    length(geojson$features) == 0
  ) {
    stop(
      "El GeoJSON filtrado no contiene features.",
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
  
  if (!"geo_id" %in% names(datos)) {
    stop(
      "Los datos geográficos no contienen la columna geo_id.",
      call. = FALSE
    )
  }
  
  coincidentes <- datos$geo_id %in% ids_geojson
  
  if (!any(coincidentes)) {
    stop(
      paste0(
        "Ningún CUSEC de los resultados coincide con properties.cusec ",
        "del GeoJSON. Ejemplo en datos: ",
        paste(
          utils::head(
            datos$cusec,
            3
          ),
          collapse = ", "
        ),
        ". Ejemplo en GeoJSON: ",
        paste(
          utils::head(
            ids_geojson,
            3
          ),
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  message(
    "Coincidencias Plotly: ",
    sum(coincidentes),
    " de ",
    nrow(datos),
    " secciones"
  )
  
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
      centro$lat *
        pi /
        180
    ),
    1e-6
  )
  
  zoom <- log2(
    360 / span
  ) - 1
  
  # Evita valores de zoom fuera del rango práctico de Mapbox.
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
        datos$geo_id %in% ids_geojson
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
        datos$geo_id[sel]
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
          datos$geo_id %in% ids_geojson
      )
      
      if (!any(sel)) {
        next
      }
      
      color <- .paleta_partidos[
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
          datos$geo_id[sel]
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