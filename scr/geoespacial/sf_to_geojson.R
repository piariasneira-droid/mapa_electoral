# Conversión de shapefiles históricos a GeoJSON provincial
# Environment ----

library(sf)
library(dplyr)
library(stringr)
library(geojsonsf)

source(
  "./scr/geoespacial/fun_sf_to_geojson.R",
  echo = TRUE
)

# Configuración ----

anhos <- c(
  2001,
  2003:2026
)

input_dir <- file.path(
  "data",
  "geo",
  "secciones"
)

output_dir <- file.path(
  "output",
  "geojson",
)

output_log <- file.path(
  "logs",
  "geo"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  output_log,
  recursive = TRUE,
  showWarnings = FALSE
)

# Ejecución ----

resultados <- lapply(
  anhos,
  function(anho) {
    
    tryCatch(
      
      .procesar_anho_geojson(anho),
      
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
          provincias = NA_integer_,
          filas = NA_integer_,
          tamanho_mb = NA_real_,
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

resultados <- bind_rows(resultados)

# Resumen ----

print(
  resultados,
  row.names = FALSE
)

write.csv(
  resultados,
  file.path(
    output_log,
    "resumen_geojson_provincial.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)