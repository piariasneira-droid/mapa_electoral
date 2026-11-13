# Consolidación histórica de secciones censales
# Parquets individuales -> Dataset Arrow particionado por aaaamm

# Environment ----
library(arrow)
library(dplyr)
library(stringr)


# Configuración ----
input_dir <- file.path(
  "output",
  "geo",
  "secciones"
)

output_dir <- file.path(
  "output",
  "geoconsolidado"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# Localizar ficheros disponibles ----
ficheros <- list.files(
  input_dir,
  pattern = "^SECC_CE_[0-9]{8}\\.parquet$",
  full.names = TRUE,
  ignore.case = TRUE
)

ficheros <- sort(ficheros)

if (length(ficheros) == 0) {
  stop(
    "No se han encontrado Parquets de secciones en: ",
    input_dir,
    call. = FALSE
  )
}

# Obtener periodo AAAAMM de cada fichero ----
tabla_ficheros <- data.frame(
  fichero = ficheros,
  nombre = basename(ficheros),
  stringsAsFactors = FALSE
) |>
  dplyr::mutate(
    fecha = stringr::str_extract(
      nombre,
      "[0-9]{8}"
    ),
    aaaamm = as.integer(
      stringr::str_sub(
        fecha,
        1,
        6
      )
    )
  ) |>
  dplyr::arrange(
    aaaamm
  )


message("")
message("Parquets encontrados:")
print(
  tabla_ficheros |>
    dplyr::select(
      nombre,
      aaaamm
    ),
  row.names = FALSE
)


# Detectar periodos ya consolidados ----
particiones_existentes <- list.dirs(
  output_dir,
  recursive = FALSE,
  full.names = FALSE
)

aaaamm_existentes <- particiones_existentes |>
  stringr::str_subset(
    "^aaaamm=[0-9]{6}$"
  ) |>
  stringr::str_remove(
    "^aaaamm="
  ) |>
  as.integer()


if (length(aaaamm_existentes) > 0) {
  
  message("")
  message(
    "Periodos ya consolidados: ",
    paste(
      sort(aaaamm_existentes),
      collapse = ", "
    )
  )
  
} else {
  
  message("")
  message(
    "No existen periodos consolidados todavía."
  )
}


# Seleccionar únicamente periodos nuevos ----
tabla_nuevos <- tabla_ficheros |>
  dplyr::filter(
    !aaaamm %in% aaaamm_existentes
  )


if (nrow(tabla_nuevos) == 0) {
  
  message("")
  message(
    "El dataset consolidado ya está actualizado."
  )
  
} else {
  
  message("")
  message(
    "Periodos nuevos que se incorporarán:"
  )
  
  print(
    tabla_nuevos |>
      dplyr::select(
        nombre,
        aaaamm
      ),
    row.names = FALSE
  )
  
  
  # Incorporar periodos nuevos ----
  for (i in seq_len(nrow(tabla_nuevos))) {
    
    fichero <- tabla_nuevos$fichero[i]
    aaaamm_actual <- tabla_nuevos$aaaamm[i]
    
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
    
    message(
      "Procesando ",
      aaaamm_actual
    )
    
    message(
      "Fichero: ",
      basename(fichero)
    )
    
    
    # Lectura ----
    datos <- arrow::read_parquet(
      fichero
    )
    
    
    message(
      "Filas: ",
      format(
        nrow(datos),
        big.mark = ".",
        scientific = FALSE
      )
    )
    
    
    # Añadir periodo ----
    if ("aaaamm" %in% names(datos)) {
      datos <- datos |>
        dplyr::select(
          -aaaamm
        )
    }
    
    datos <- datos |>
      dplyr::mutate(
        aaaamm = as.integer(
          aaaamm_actual
        ),
        .before = 1
      )
    
    
    # Directorio de la partición ----
    particion <- file.path(
      output_dir,
      paste0(
        "aaaamm=",
        aaaamm_actual
      )
    )
    
    dir.create(
      particion,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    
    # Escritura ----
    fichero_salida <- file.path(
      particion,
      "part-0.parquet"
    )
    
    arrow::write_parquet(
      datos,
      fichero_salida,
      compression = "snappy",
      use_dictionary = TRUE
    )
    
    
    message(
      "Generado: ",
      fichero_salida
    )
    
    message(
      "Tamaño: ",
      round(
        file.info(fichero_salida)$size / 1024^2,
        1
      ),
      " MB"
    )
  }
}


# Comprobación final ----
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

message(
  "COMPROBACIÓN DEL DATASET CONSOLIDADO"
)

message(
  paste(
    rep(
      "=",
      70
    ),
    collapse = ""
  )
)


dataset <- arrow::open_dataset(
  output_dir,
  partitioning = "hive"
)


periodos_disponibles <- dataset |>
  dplyr::distinct(
    aaaamm
  ) |>
  dplyr::collect() |>
  dplyr::arrange(
    aaaamm
  )


message("")
message(
  "Periodos disponibles: ",
  paste(
    periodos_disponibles$aaaamm,
    collapse = ", "
  )
)


message("")
message(
  "Dataset consolidado disponible en: ",
  normalizePath(
    output_dir,
    mustWork = FALSE
  )
)

message("")
message(
  "Proceso terminado."
)