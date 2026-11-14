# Conversión de shapefiles de secciones censales históricas a Parquet
# Environment ----
library(sf)
library(dplyr)
library(stringr)
library(arrow)
library(geojsonsf)

source("./scr/geoespacial/fun_sf_to_parquet.R", echo = TRUE)

# Configuración ----
anhos <- c(2001, 2003:2026)


## Dirs
input_dir <- file.path(
  "data",
  "geo",
  "secciones"
)

output_dir <- file.path(
  "output",
  "geo",
  "secciones"
)

output_log <- file.path(
  "logs",
  "geo"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_log, recursive = TRUE, showWarnings = FALSE)

# Ejecución ----
resultados <- lapply(
  anhos,
  function(anho) {
    
    tryCatch(
      .procesar_anho(anho),
      error = function(e) {
        
        mensaje_error <- conditionMessage(e)
        
        message("")
        message(
          "ERROR EN ",
          anho,
          ": ",
          mensaje_error
        )
        
        data.frame(
          anho = as.integer(anho),
          shapefiles = NA_integer_,
          filas = NA_integer_,
          segundos_serializacion = NA_real_,
          tamanho_mb = NA_real_,
          fichero = NA_character_,
          estado = paste0(
            "ERROR: ",
            mensaje_error
          ),
          stringsAsFactors = FALSE
        )
      }
    )
  }
)

resultados <- dplyr::bind_rows(resultados)

# Resumen ----
message("")
message(
  paste(
    rep(
      "=",
      70
    ),
    collapse = ""
  )
)
message("RESUMEN DEL PROCESO")
message(
  paste(
    rep(
      "=",
      70
    ),
    collapse = ""
  )
)

print(
  resultados,
  row.names = FALSE
)

fichero_resumen <- file.path(
  output_log,
  "resumen_conversion_secciones_2001_2010.csv"
)

write.csv(
  resultados,
  fichero_resumen,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

message("")
message(
  "Resumen guardado en: ",
  normalizePath(
    fichero_resumen,
    mustWork = FALSE
  )
)

anhos_correctos <- resultados |>
  dplyr::filter(
    estado == "OK"
  ) |>
  dplyr::pull(anho)

anhos_con_error <- resultados |>
  dplyr::filter(
    estado != "OK"
  ) |>
  dplyr::pull(anho)

message("")
message(
  "Años procesados correctamente: ",
  if (length(anhos_correctos) > 0) {
    paste(
      anhos_correctos,
      collapse = ", "
    )
  } else {
    "ninguno"
  }
)

message(
  "Años con error: ",
  if (length(anhos_con_error) > 0) {
    paste(
      anhos_con_error,
      collapse = ", "
    )
  } else {
    "ninguno"
  }
)