# Environment ----
library(dplyr)
library(purrr)
library(arrow)
library(plotly)

# Funciones ----
source("scr/mapa_secciones/fun/config.R")
source("scr/mapa_secciones/fun/funauxbis.R")
source("scr/mapa_secciones/fun/salvarenhtml.R")

# Config ----

ruta_geo <- file.path(
  "output",
  "geojsons"
)

ruta_relmunicipios <- file.path(
  "data",
  "geo",
  "municipios.xlsx"
)

ruta_resultados <- file.path(
  "output",
  "generales",
  "secciones_espana",
  "resultados_seccion.parquet"
)

dir_output <- file.path(
  "output",
  "mapas_fuencarral"
)

# Lectura ----

tiempo_lectura <- system.time({
  
  datos_mapa <- .obtener_datos_mapa(
    ruta_resultados = ruta_resultados,
    ruta_geo = ruta_geo,
    ruta_relmunicipios = ruta_relmunicipios,
    filtros = FILTROSXDEFECTO,
    partidos = PARTID
  )
})

message(
  "Tiempo total de lectura y preparación: ",
  round(
    tiempo_lectura[["elapsed"]],
    2
  ),
  " segundos."
)

message(
  "Secciones disponibles para el mapa: ",
  format(
    nrow(datos_mapa),
    big.mark = ".",
    scientific = FALSE
  )
)

# Participación ----

tiempo_participacion <- system.time({
  
  mapa_participacion <- .mapa_plotly(
    datos = datos_mapa,
    variable = "participacion_pct",
    partidos = PARTID,
    titulo = "Participación (%)"
  )
})

message(
  "Tiempo para construir el mapa de participación: ",
  round(
    tiempo_participacion[["elapsed"]],
    2
  ),
  " segundos."
)

# Ganador ----

tiempo_ganador <- system.time({
  
  mapa_ganador <- .mapa_plotly(
    datos = datos_mapa,
    variable = "ganador",
    partidos = PARTID,
    titulo = "Ganador por sección",
    paletacolpartidos = PALETA
  )
})

message(
  "Tiempo para construir el mapa de ganador: ",
  round(
    tiempo_ganador[["elapsed"]],
    2
  ),
  " segundos."
)

# Mapas por partido ----

tiempo_partidos <- system.time({
  
  mapas_partidos <- PARTID |>
    purrr::set_names() |>
    purrr::map(
      function(partido) {
        
        variable <- paste0(
          "votos_",
          partido,
          "_pct_validos"
        )
        
        if (!variable %in% names(datos_mapa)) {
          
          warning(
            "No existe la variable '",
            variable,
            "' en datos_mapa.",
            call. = FALSE
          )
          
          return(NULL)
        }
        
        .mapa_plotly(
          datos = datos_mapa,
          variable = variable,
          partidos = PARTID,
          titulo = paste0(
            partido,
            " (% votos válidos)"
          ),
          paletacolpartidos = PALETA
        )
      }
    )
})

message(
  "Tiempo para construir los mapas de partidos: ",
  round(
    tiempo_partidos[["elapsed"]],
    2
  ),
  " segundos."
)

# Eliminar partidos sin columna disponible

mapas_partidos <- mapas_partidos[
  !purrr::map_lgl(
    mapas_partidos,
    is.null
  )
]

# Guardado en HTML ----

tiempo_guardado <- system.time({
  
  .guardar_mapas_html(
    mapa_participacion = mapa_participacion,
    mapa_ganador = mapa_ganador,
    mapas_partidos = mapas_partidos,
    dir_output = dir_output,
    eleccion = FILTROSXDEFECTO$eleccion
  )
})

message(
  "Tiempo de guardado de los HTML: ",
  round(
    tiempo_guardado[["elapsed"]],
    2
  ),
  " segundos."
)