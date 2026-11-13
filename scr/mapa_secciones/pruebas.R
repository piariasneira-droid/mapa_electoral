# Environment ----
library(dplyr)
library(purrr)
library(arrow)
library(plotly)

# Funciones ----
source("scr/mapa_secciones/fun/config.R")
source("scr/mapa_secciones/fun/funaux.R")
source("scr/mapa_secciones/fun/salvarenhtml.R")

# Config ----
ruta_geo <- file.path(
  "output",
  "geoconsolidado"
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
datos_mapa <- .obtener_datos_mapa(
  ruta_resultados = ruta_resultados,
  ruta_geo = ruta_geo,
  ruta_relmunicipios = ruta_relmunicipios,
  filtros = FILTROSXDEFECTO,
  partidos = PARTID
)

# Participación ----
mapa_participacion <- .mapa_plotly(
  datos = datos_mapa,
  variable = "participacion_pct",
  partidos = PARTID,
  titulo = "Participación (%)"
)

# Ganador ----
mapa_ganador <- .mapa_plotly(
  datos = datos_mapa,
  variable = "ganador",
  partidos = PARTID,
  titulo = "Ganador por sección"
)

# Mapas por partido ----
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
          "' en datos_mapa."
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

# Eliminar partidos sin columna disponible
mapas_partidos <- mapas_partidos[
  !purrr::map_lgl(
    mapas_partidos,
    is.null
  )
]

# Guardado en HTML ----
.guardar_mapas_html(
  mapa_participacion = mapa_participacion,
  mapa_ganador = mapa_ganador,
  mapas_partidos = mapas_partidos,
  dir_output = dir_output,
  eleccion = FILTROSXDEFECTO$eleccion
)