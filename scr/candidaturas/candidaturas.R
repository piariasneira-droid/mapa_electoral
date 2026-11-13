# Inventario histórico de candidaturas

# Environment ----
library(arrow)
library(dplyr)
library(stringr)
library(purrr)

source("./scr/candidaturas/candidaturasfun.R", echo = TRUE)


# Configuración ----
fecha_minima <- 200001

input_dir <- file.path(
  "interim",
  "generales"
)

output_dir <- file.path(
  "output",
  "candidaturas",
  "generales"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Localizar elecciones ----
elecciones <- list.dirs(
  input_dir,
  recursive = FALSE,
  full.names = FALSE
)

elecciones <- elecciones[
  stringr::str_detect(
    elecciones,
    "^[0-9]{6}$"
  )
]

elecciones <- elecciones[
  as.integer(elecciones) >= fecha_minima
]


if (length(elecciones) == 0) {
  stop(
    "No se han encontrado elecciones en ",
    input_dir,
    ".",
    call. = FALSE
  )
}

message(
  "Elecciones encontradas: ",
  length(elecciones)
)

message(
  paste(
    elecciones,
    collapse = ", "
  )
)

# Leer todas las elecciones ----
candidaturas <- elecciones |>
  purrr::map(
    .leer_candidaturas
  ) |>
  purrr::compact() |>
  dplyr::bind_rows()


# Comprobación ----
message("")
message(
  "Total de registros: ",
  format(
    nrow(candidaturas),
    big.mark = ".",
    scientific = FALSE
  )
)

message("")
message("Columnas disponibles:")

print(
  names(candidaturas)
)


# Mostrar estructura ----
message("")
message("Estructura:")

str(candidaturas)


# Buscar columnas de siglas y denominación ----
posibles_siglas <- c(
  "siglas",
  "sigla",
  "siglas_candidatura",
  "siglas_cand"
)

posibles_denominaciones <- c(
  "denominacion",
  "denominacion_candidatura",
  "nombre",
  "nombre_candidatura",
  "candidatura"
)


col_siglas <- intersect(
  posibles_siglas,
  names(candidaturas)
)

col_denominacion <- intersect(
  posibles_denominaciones,
  names(candidaturas)
)


if (length(col_siglas) == 0) {
  stop(
    paste0(
      "No encuentro la columna de siglas. ",
      "Columnas disponibles: ",
      paste(
        names(candidaturas),
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


if (length(col_denominacion) == 0) {
  stop(
    paste0(
      "No encuentro la columna de denominación. ",
      "Columnas disponibles: ",
      paste(
        names(candidaturas),
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


col_siglas <- col_siglas[1]
col_denominacion <- col_denominacion[1]


message("")
message(
  "Columna de siglas: ",
  col_siglas
)

message(
  "Columna de denominación: ",
  col_denominacion
)


# Catálogo histórico de candidaturas ----
catalogo <- candidaturas |>
  dplyr::transmute(
    aaaamm,
    siglas = as.character(
      .data[[col_siglas]]
    ),
    denominacion = as.character(
      .data[[col_denominacion]]
    )
  ) |>
  dplyr::mutate(
    siglas = stringr::str_squish(siglas),
    denominacion = stringr::str_squish(denominacion),
    siglas = dplyr::na_if(
      siglas,
      ""
    ),
    denominacion = dplyr::na_if(
      denominacion,
      ""
    )
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(
    aaaamm,
    siglas,
    denominacion
  )


# Resumen por candidatura ----
resumen_candidaturas <- catalogo |>
  dplyr::group_by(
    siglas,
    denominacion
  ) |>
  dplyr::summarise(
    primera_eleccion = min(
      aaaamm
    ),
    ultima_eleccion = max(
      aaaamm
    ),
    numero_elecciones = dplyr::n_distinct(
      aaaamm
    ),
    elecciones = paste(
      sort(
        unique(aaaamm)
      ),
      collapse = ", "
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    primera_eleccion,
    siglas,
    denominacion
  )


# Mostrar catálogo ----
message("")
message(
  paste(
    rep(
      "=",
      80
    ),
    collapse = ""
  )
)

message(
  "CATÁLOGO HISTÓRICO DE CANDIDATURAS"
)

message(
  paste(
    rep(
      "=",
      80
    ),
    collapse = ""
  )
)

print(
  resumen_candidaturas,
  n = Inf
)


# Guardar resultados ----
fichero_catalogo <- file.path(
  output_dir,
  "catalogo_candidaturas_generales.csv"
)

fichero_detalle <- file.path(
  output_dir,
  "candidaturas_por_eleccion.csv"
)


write.csv(
  resumen_candidaturas,
  fichero_catalogo,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  catalogo,
  fichero_detalle,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


message("")
message(
  "Catálogo guardado en: ",
  normalizePath(
    fichero_catalogo,
    mustWork = FALSE
  )
)

message(
  "Detalle por elección guardado en: ",
  normalizePath(
    fichero_detalle,
    mustWork = FALSE
  )
)