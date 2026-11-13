# Environment ----
library(readr)
library(dplyr)
library(purrr)
library(arrow)
library(stringr)

source("scr/lectura_mesas/fun/lectura_congreso_interim.R")

# Config ----
dir_input  <- "output/generales/mesas_espana"
dir_output <- "output/generales/secciones_espana"
nivel <- "seccion"

# Filtros territoriales opcionales (NULL = sin filtrar)
filccaa <- NULL
filprov <- NULL
filmun  <- NULL
fildis  <- NULL
filsec  <- NULL

# Debe coincidir con partidos_principales del ETL de mesas
partidos_principales <- c(
  "PSOE",
  "PP",
  "VOX",
  "IU",
  "PODEMOS",
  "SUMAR",
  "UPYD",
  "CIUDADANOS",
  "CIU_JUNTS",
  "ERC",
  "PNV",
  "HB_EH_BILDU",
  "BNG",
  "CC",
  "UPN",
  "CDS",
  "PA",
  "PAR",
  "CHA",
  "EA",
  "EE",
  "NA_BAI_GEROA_BAI",
  "COMPROMIS",
  "PRC",
  "FORO",
  "CUP",
  "TERUEL_EXISTE",
  "OTROS"
)

var_censo_problem <- 30

# Lectura resultados por mesa ----
mesas_resultados <- read_parquet(
  file.path(dir_input, "mesas_resultados.parquet")
) |>
  .filtrar_territorio(
    cod_ccaa = filccaa,
    cod_prov = filprov,
    cod_mun  = filmun,
    cod_dis  = fildis,
    cod_sec  = filsec
  )

if (nrow(mesas_resultados) == 0) {
  stop(
    "Los filtros territoriales no devuelven ninguna mesa. ",
    "Revisa filccaa/filprov/filmun/fildis/filsec o regenera el output de mesas.",
    call. = FALSE
  )
}

# Agregación ----
id_col <- paste0(nivel, "_id")

resultados <- .agregar_resultados(
  mesas = mesas_resultados,
  nivel = nivel,
  partidos = partidos_principales,
  id_col = id_col
)

# Cambios problemáticos ----
evolucion <- .evolucion_territorio(
  resultados,
  id_col = id_col,
  var_censo_problem = var_censo_problem
)

# Guardado de resultados ----
dir.create(
  dir_output,
  recursive = TRUE,
  showWarnings = FALSE
)

salidas <- list(resultados, evolucion)
names(salidas) <- c(
  paste0("resultados_", nivel),
  paste0("evolucion_", nivel)
)

iwalk(
  salidas,
  function(datos, nombre) {
    message("Guardando: ", nombre)
    
    write_csv(
      datos,
      file = file.path(dir_output, paste0(nombre, ".csv")),
      na = ""
    )
    
    write_parquet(
      datos,
      sink = file.path(dir_output, paste0(nombre, ".parquet"))
    )
  }
)

message(
  "Proceso finalizado. Archivos guardados en: ",
  normalizePath(dir_output)
)