.guardar_mapas_html <- function(
    mapa_participacion,
    mapa_ganador,
    mapas_partidos,
    dir_output,
    eleccion,
    libdir = "libs"
) {
  
  mapas_interactivos <- c(
    list(
      participacion = mapa_participacion,
      ganador = mapa_ganador
    ),
    mapas_partidos
  )
  
  dir.create(
    dir_output,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  purrr::iwalk(
    mapas_interactivos,
    function(mapa, nombre) {
      
      ruta_salida <- file.path(
        dir_output,
        paste0(
          tolower(nombre),
          "_",
          eleccion,
          ".html"
        )
      )
      
      htmlwidgets::saveWidget(
        widget = mapa,
        file = ruta_salida,
        selfcontained = FALSE,
        libdir = libdir
      )
      
      message(
        "Guardado: ",
        ruta_salida
      )
    }
  )
  
  message(
    "Mapas generados: ",
    length(mapas_interactivos)
  )
  
  invisible(mapas_interactivos)
}