# global.R
# PAQUETES ----
library(shiny)
library(shinyWidgets)
library(dplyr)
library(purrr)
library(arrow)
library(geoarrow)
library(sf)
library(leaflet)
library(stringr)

# CONFIGURACIÓN Y FUNCIONES ----
source("./scr/config.R", echo = TRUE)
source("./scr/funaux.R", echo = TRUE)

# Las geometrías vienen ahora de GeoParquet (secciones_<año>.parquet)
ruta_geo <- file.path("data", "geo_parquet")

# RELACION MUNICIPIOS ----
relmunicipios <- openxlsx::read.xlsx(
  ruta_relmunicipios
)

# ELECCIONES ----
choices_elecciones <- stats::setNames(
  as.integer(
    names(COD_ELECCIONES)
  ),
  unname(
    COD_ELECCIONES
  )
)

# PROVINCIAS ----
choices_provincias <- stats::setNames(
  as.integer(
    names(COD_PROV_INE)
  ),
  unname(
    COD_PROV_INE
  )
)

# PARTIDOS ----
choices_partidos <- stats::setNames(
  PARTIDOS,
  PARTIDOS
)

# TIPOS DE MAPA ----
choices_tipo_mapa <- c(
  "Ganador por sección" = "ganador",
  "Participación (%)" = "participacion_pct",
  stats::setNames(
    paste0(
      "votos_",
      PARTIDOS_INICIALES,
      "_pct_validos"
    ),
    paste0(
      PARTIDOS_INICIALES,
      " (% votos válidos)"
    )
  )
)

# MUNICIPIOS INICIALES ----
relmunicipios_iniciales <- relmunicipios |>
  dplyr::filter(
    CPRO %in% FILTROSXDEFECTO$provincias
  ) |>
  dplyr::distinct(
    CPRO,
    CMUN,
    COD,
    NOMBRE,
    DENOMINACION
  ) |>
  dplyr::arrange(
    CPRO,
    NOMBRE
  )

choices_municipios_iniciales <- stats::setNames(
  as.integer(relmunicipios_iniciales$COD),
  relmunicipios_iniciales$DENOMINACION
)