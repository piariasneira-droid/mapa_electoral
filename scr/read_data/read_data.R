# Environment ----
library(readr)
library(dplyr)
library(purrr)
library(arrow)

source("scr/read_data/fun/lectura_congreso.R")

# dirs_congreso <- c(
#   "data/generales/02199603_MESA",
#   "data/generales/02200003_MESA",
#   "data/generales/02200803_MESA",
#   "data/generales/02201111_MESA",
#   "data/generales/02201512_MESA",
#   "data/generales/02201606_MESA",
#   "data/generales/02201904_MESA",
#   "data/generales/02201911_MESA"
# )


dirs_congreso <- list.dirs(
  "data/generales",
  recursive = FALSE,
  full.names = TRUE
)

walk(
  dirs_congreso,
  function(path_eleccion) {
    
    periodo <- sub(
      "^02([0-9]{4})([0-9]{2})_MESA$",
      "\\1\\2",
      basename(path_eleccion)
    )
    
    message("----------------------------------------")
    message("Procesando ", periodo)
    message("----------------------------------------")
    
    tryCatch({
      
      congreso <- leer_proceso_congreso(
        path_eleccion
      )
      
      guardar_proceso_congreso(
        congreso,
        periodo
      )
      
      message("OK: ", periodo)
      
    }, error = function(e) {
      
      message(
        "ERROR en ",
        basename(path_eleccion),
        ": ",
        e$message
      )
      
    })
    
  }
)