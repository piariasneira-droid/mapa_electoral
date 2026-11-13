# Dependencias: arrow, geoarrow, sf, dplyr, stringr, openxlsx, leaflet

# Utilidades ----

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
    x <- stringr::str_pad(x, width = ancho, side = "left", pad = "0")
  }
  
  x
}


# Lectura de secciones desde GeoParquet ----

# Elige el fichero secciones_<año>.parquet. Si no existe el año exacto
# de la elección, usa el último año disponible anterior.
.ruta_geo_secciones <- function(
    ruta_geo,
    anho
) {
  
  exacta <- file.path(
    ruta_geo,
    sprintf("secciones_%s.parquet", anho)
  )
  
  if (file.exists(exacta)) {
    return(exacta)
  }
  
  ruta_abs <- normalizePath(ruta_geo, mustWork = FALSE)
  
  if (!dir.exists(ruta_geo)) {
    stop(
      "La carpeta de geometrías no existe: ", ruta_abs,
      "\nDirectorio de trabajo actual: ", getwd(),
      "\nRevisa 'ruta_geo' (mejor una ruta absoluta).",
      call. = FALSE
    )
  }
  
  disponibles <- list.files(
    ruta_geo,
    pattern = "^secciones_[0-9]{4}\\.parquet$"
  )
  
  if (length(disponibles) == 0) {
    stop(
      "No hay ficheros 'secciones_<año>.parquet' en: ", ruta_abs,
      "\nContenido de la carpeta: ",
      paste(utils::head(list.files(ruta_geo), 10), collapse = ", "),
      call. = FALSE
    )
  }
  
  anhos <- as.integer(
    stringr::str_extract(disponibles, "[0-9]{4}")
  )
  
  anhos_validos <- anhos[anhos <= anho]
  
  if (length(anhos_validos) == 0) {
    stop(
      "No hay geometrías para el año ", anho, " ni anteriores.",
      "\nAños disponibles en ", ruta_abs, ": ",
      paste(sort(anhos), collapse = ", "),
      call. = FALSE
    )
  }
  
  anhos <- anhos_validos
  
  message(
    "No existe secciones_", anho, ".parquet; se usa el año ", max(anhos), "."
  )
  
  file.path(
    ruta_geo,
    sprintf("secciones_%s.parquet", max(anhos))
  )
}


# Devuelve un sf (WGS84) con: cusec, prov, mun, distrito, seccion,
# provmun y geometry. Los filtros territoriales se aplican en disco.
.leer_geo_secciones <- function(
    ruta_geo,
    anho,
    filtros
) {
  
  ruta <- .ruta_geo_secciones(ruta_geo, anho)
  
  message("Leyendo: ", normalizePath(ruta, mustWork = FALSE))
  
  prov_sel <- sprintf("%02d", as.integer(filtros$provincias))
  
  ds <- arrow::open_dataset(ruta) |>
    dplyr::filter(substr(cusec, 1, 2) %in% prov_sel)
  
  if (length(filtros$municipios) > 0) {
    
    # filtros$municipios son códigos provmun (p. ej. 28079)
    mun_sel <- sprintf("%05d", as.integer(filtros$municipios))
    
    ds <- ds |>
      dplyr::filter(substr(cusec, 1, 5) %in% mun_sel)
  }
  
  if (length(filtros$distritos) > 0) {
    
    dis_sel <- sprintf("%02d", as.integer(filtros$distritos))
    
    ds <- ds |>
      dplyr::filter(substr(cusec, 6, 7) %in% dis_sel)
  }
  
  if (length(filtros$secciones) > 0) {
    
    sec_sel <- sprintf("%03d", as.integer(filtros$secciones))
    
    ds <- ds |>
      dplyr::filter(substr(cusec, 8, 10) %in% sec_sel)
  }
  
  tiempo <- system.time({
    
    geo <- ds |>
      dplyr::select(cusec, geometry) |>
      dplyr::collect() |>
      dplyr::mutate(
        geometry = sf::st_as_sfc(geometry)
      ) |>
      sf::st_as_sf() |>
      sf::st_transform(4326)       # Leaflet exige WGS84
  })
  
  if (nrow(geo) == 0) {
    stop("Los filtros territoriales no dejan ninguna sección.", call. = FALSE)
  }
  
  geo <- geo |>
    dplyr::mutate(
      cusec    = .normalizar_codigo_geo(cusec, ancho = 10),
      prov     = as.integer(stringr::str_sub(cusec, 1, 2)),
      mun      = as.integer(stringr::str_sub(cusec, 3, 5)),
      distrito = as.integer(stringr::str_sub(cusec, 6, 7)),
      seccion  = as.integer(stringr::str_sub(cusec, 8, 10)),
      provmun  = prov * 1000L + mun,
      .before  = geometry
    )
  
  message(
    "Secciones leídas: ",
    format(nrow(geo), big.mark = ".", scientific = FALSE),
    ". Tiempo: ", round(tiempo[["elapsed"]], 2), " segundos."
  )
  
  geo
}


# Datos del mapa ----

# Devuelve un objeto sf (geometría + resultados + ganador).
# Ya no hace falta el atributo "geojson" ni .construir_geojson().
.obtener_datos_mapa <- function(
    ruta_resultados,
    ruta_geo,
    filtros,
    partidos = NULL,
    anho_geo = NULL,
    relmun
) {
  
  if (is.null(filtros$eleccion)) {
    stop("Es necesario indicar 'filtros$eleccion'.", call. = FALSE)
  }
  
  if (length(filtros$provincias) == 0) {
    stop(
      "Es necesario indicar al menos una provincia en 'filtros$provincias'.",
      call. = FALSE
    )
  }
  
  # Relación de municipios ----
  relmun <- relmun |>
    dplyr::mutate(COD = as.integer(COD))
  
  # Resultados electorales ----
  
  ds_res <- arrow::open_dataset(ruta_resultados) |>
    dplyr::filter(fecha_eleccion == filtros$eleccion) |>
    dplyr::filter(prov %in% filtros$provincias)
  
  if (length(filtros$municipios) > 0) {
    ds_res <- ds_res |> dplyr::filter(provmun %in% filtros$municipios)
  }
  
  if (length(filtros$distritos) > 0) {
    ds_res <- ds_res |> dplyr::filter(distrito %in% filtros$distritos)
  }
  
  if (length(filtros$secciones) > 0) {
    ds_res <- ds_res |> dplyr::filter(seccion %in% filtros$secciones)
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
  
  if (length(partidos) > 0) {
    variables_partidos <- c(
      paste0("votos_", partidos),
      paste0("votos_", partidos, "_pct_validos")
    )
  } else {
    variables_partidos <- character(0)
  }
  
  columnas_resultados <- intersect(
    unique(c(variables_generales, variables_partidos)),
    names(ds_res)
  )
  
  for (obligatoria in c("seccion_id", "provmun")) {
    if (!obligatoria %in% columnas_resultados) {
      stop(
        "No existe la columna '", obligatoria, "' en los resultados.",
        call. = FALSE
      )
    }
  }
  
  resultados <- ds_res |>
    dplyr::select(dplyr::all_of(columnas_resultados)) |>
    dplyr::collect() |>
    dplyr::mutate(
      cusec   = .normalizar_codigo_geo(gsub("-", "", seccion_id), ancho = 10),
      provmun = as.integer(provmun)
    ) |>
    dplyr::select(-seccion_id)
  
  message(
    "Filas de resultados: ",
    format(nrow(resultados), big.mark = ".", scientific = FALSE)
  )
  
  # Geometrías ----
  
  if (is.null(anho_geo)) {
    anho_geo <- as.integer(substr(as.character(filtros$eleccion), 1, 4))
  }
  
  geo <- .leer_geo_secciones(
    ruta_geo = ruta_geo,
    anho = anho_geo,
    filtros = filtros
  )
  
  # Join (el resultado sigue siendo sf) ----
  
  datos <- geo |>
    dplyr::left_join(resultados, by = c("cusec", "provmun")) |>
    dplyr::left_join(
      relmun |>
        dplyr::select(dplyr::any_of(c("COD", "NOMBRE", "DENOMINACION"))),
      by = c("provmun" = "COD")
    )
  
  # Partido ganador ----
  
  if (length(partidos) > 0) {
    
    variables_pct <- intersect(
      paste0("votos_", partidos, "_pct_validos"),
      names(datos)
    )
    
    if (length(variables_pct) > 0) {
      
      # st_drop_geometry: si no, as.matrix() arrastraría la columna geometry
      matriz <- datos |>
        sf::st_drop_geometry() |>
        dplyr::select(dplyr::all_of(variables_pct)) |>
        as.matrix()
      
      storage.mode(matriz) <- "numeric"
      
      sin_resultados <- rowSums(!is.na(matriz)) == 0
      
      matriz[is.na(matriz)] <- -Inf
      
      etiquetas <- sub("^votos_(.*)_pct_validos$", "\\1", variables_pct)
      
      ganador <- etiquetas[max.col(matriz, ties.method = "first")]
      ganador[sin_resultados] <- NA_character_
      
      datos$ganador <- ganador
      
    } else {
      datos$ganador <- NA_character_
    }
  }
  
  datos
}


# Mapa leaflet (sustituye a .mapa_plotly) ----

# - variable numérica  -> gradiente continuo
#   (si es "votos_<partido>_pct_validos" y hay color del partido en
#    paletacolpartidos, gradiente blanco -> color del partido)
# - variable "ganador" (texto) -> colores por categoría
# paletacolpartidos: vector con nombres, p. ej. c(PP = "#1D84CE", PSOE = "#E30613")
# Fondo: teselas de OpenStreetMap (leaflet::addTiles()).
.mapa_leaflet <- function(
    datos,
    variable,
    partidos = NULL,
    titulo = NULL,
    paletacolpartidos = NULL,
    opacidad = 0.75
) {
  
  if (!inherits(datos, "sf")) {
    stop("'datos' debe ser un objeto sf (salida de .obtener_datos_mapa).",
         call. = FALSE)
  }
  
  if (!variable %in% names(datos)) {
    stop("No existe la variable '", variable, "' en datos.", call. = FALSE)
  }
  
  valores <- datos[[variable]]
  
  if (is.null(titulo)) {
    titulo <- variable
  }
  
  sin_datos <- "#d9d9d9"
  
  # Paleta y etiqueta del valor ----
  
  if (is.character(valores) || is.factor(valores)) {
    
    valores <- as.character(valores)
    niveles <- sort(unique(valores[!is.na(valores)]))
    
    colores <- if (
      !is.null(paletacolpartidos) &&
      all(niveles %in% names(paletacolpartidos))
    ) {
      unname(paletacolpartidos[niveles])
    } else {
      grDevices::hcl.colors(max(length(niveles), 1), "Dark 3")
    }
    
    pal <- leaflet::colorFactor(
      palette = colores,
      domain = niveles,
      na.color = sin_datos
    )
    
    texto_valor <- ifelse(is.na(valores), "Sin datos", valores)
    leyenda_valores <- niveles
    
  } else {
    
    rango <- range(valores, na.rm = TRUE)
    
    if (!all(is.finite(rango))) {
      rango <- c(0, 1)
    }
    
    partido <- sub("^votos_(.*)_pct_validos$", "\\1", variable)
    
    color_partido <- if (
      !is.null(paletacolpartidos) &&
      partido %in% names(paletacolpartidos)
    ) {
      c("#f7f7f7", unname(paletacolpartidos[partido]))
    } else {
      "viridis"
    }
    
    pal <- leaflet::colorNumeric(
      palette = color_partido,
      domain = rango,
      na.color = sin_datos
    )
    
    texto_valor <- ifelse(
      is.na(valores),
      "Sin datos",
      paste0(formatC(valores, format = "f", digits = 2), " %")
    )
    
    leyenda_valores <- rango
  }
  
  nombre_mun <- if ("NOMBRE" %in% names(datos)) {
    datos$NOMBRE
  } else if ("DENOMINACION" %in% names(datos)) {
    datos$DENOMINACION
  } else {
    rep("", nrow(datos))
  }
  
  etiquetas <- lapply(
    sprintf(
      "<b>%s</b><br>Sección: %s<br>%s: %s",
      htmltools::htmlEscape(ifelse(is.na(nombre_mun), "", nombre_mun)),
      datos$cusec,
      htmltools::htmlEscape(titulo),
      htmltools::htmlEscape(texto_valor)
    ),
    htmltools::HTML
  )
  
  # Mapa ----
  
  leaflet::leaflet(
    datos,
    options = leaflet::leafletOptions(preferCanvas = TRUE)
  ) |>
    leaflet::addTiles() |>   # OpenStreetMap
    leaflet::addPolygons(
      fillColor = ~pal(valores),
      fillOpacity = opacidad,
      color = "#4b5563",
      weight = 0.5,
      opacity = 0.8,
      label = etiquetas,
      highlightOptions = leaflet::highlightOptions(
        weight = 2.5,
        color = "#d97706",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      pal = pal,
      values = leyenda_valores,
      title = titulo,
      na.label = "Sin datos",
      opacity = 1
    )
}