# Utilidades ----
# Los ficheros del INE están en Latin-1. readr convierte a UTF-8 al leer con
# este locale; NO hay que volver a pasar iconv() por encima (doble conversión).
.locale_ine <- readr::locale(encoding = "Latin1")

.read_fwf <- function(fichero, ancho, nombres){
  read_fwf(
    fichero,
    fwf_widths(ancho, nombres),
    col_types = cols(.default = "c"),
    locale = .locale_ine,
    trim_ws = FALSE,
    progress = FALSE
  )
}

# Lectura de líneas independiente del sistema (Windows/Linux/macOS):
# devuelve siempre UTF-8, sin depender del locale nativo de R.
.leer_lineas <- function(fichero){
  readr::read_lines(
    fichero,
    locale = .locale_ine,
    progress = FALSE
  )
}

# Localiza "<prefijo><codigo>.DAT" ignorando mayúsculas/minúsculas
# (en Linux el sistema de ficheros distingue .DAT de .dat).
.ruta_fichero <- function(path, prefijo, codigo){
  esperado <- tolower(paste0(prefijo, codigo, ".dat"))
  ficheros <- list.files(path, full.names = TRUE)
  ruta <- ficheros[tolower(basename(ficheros)) == esperado]
  
  if (length(ruta) != 1) {
    stop(
      "Se esperaba exactamente 1 fichero '", esperado, "' en ", path,
      " y se encontraron ", length(ruta)
    )
  }
  ruta
}

# Lectura congreso pipeline ----
leer_proceso_congreso <- function(path){
  
  fich <- list.files(
    path,
    pattern = "^01.*\\.dat$",
    ignore.case = TRUE,
    full.names = FALSE
  )
  
  if (length(fich) != 1) {
    stop(
      "Se esperaba exactamente 1 fichero 01*.DAT en ", path,
      " y se encontraron ", length(fich)
    )
  }
  
  codigo <- sub(
    "^01(.*)\\.dat$",
    "\\1",
    fich,
    ignore.case = TRUE
  )
  
  list(
    control = .leer_lineas(
      .ruta_fichero(path, "01", codigo)
    ),
    identificacion = .leer_lineas(
      .ruta_fichero(path, "02", codigo)
    ),
    candidaturas = .leer_candidaturas_03(
      .ruta_fichero(path, "03", codigo)
    ),
    candidatos = .leer_candidatos_04(
      .ruta_fichero(path, "04", codigo)
    ),
    datos_municipio = .leer_municipios_05(
      .ruta_fichero(path, "05", codigo)
    ),
    votos_municipio = .leer_candidaturas_municipio_06(
      .ruta_fichero(path, "06", codigo)
    ),
    datos_ambito = .leer_ambito_superior_07(
      .ruta_fichero(path, "07", codigo)
    ),
    votos_ambito = .leer_candidaturas_ambito_08(
      .ruta_fichero(path, "08", codigo)
    ),
    mesas = .leer_mesas_09(
      .ruta_fichero(path, "09", codigo)
    ),
    votos_por_mesa = .leer_votosxcandidatura_10(
      .ruta_fichero(path, "10", codigo)
    )
  )
  
}

# Lectura congreso archivos ----
.leer_candidaturas_03 <- function(fichero){
  ancho <- c(
    2,4,2,
    6,
    50,
    150,
    6,6,6
  )
  
  nombres <- c(
    "eleccion","anio","mes",
    "cod_cand",
    "siglas",
    "denominacion",
    "prov_acum",
    "aut_acum",
    "nac_acum"
  )
  
  .read_fwf(
    fichero = fichero,
    ancho = ancho,
    nombres = nombres
  )
}

.leer_candidatos_04 <- function(fichero){
  
  ancho <- c(
    2,4,2,1,   # elección, año, mes, vuelta
    2,1,3,     # provincia, distrito electoral, municipio/senador
    6,         # candidatura
    3,         # orden candidato
    1,         # tipo candidato
    25,25,25,  # nombre y apellidos
    1,         # sexo
    2,2,4,     # fecha nacimiento
    10,        # dni
    1          # elegido
  )
  
  nombres <- c(
    "eleccion","anio","mes","vuelta",
    "prov","distrito_electoral","cod_mun_senador",
    "cod_cand",
    "orden_candidato",
    "tipo_candidato",
    "nombre",
    "apellido1",
    "apellido2",
    "sexo",
    "dia_nac",
    "mes_nac",
    "anio_nac",
    "dni",
    "elegido"
  )
  
  .read_fwf(
    fichero = fichero,
    ancho = ancho,
    nombres = nombres
  )
}

.leer_municipios_05 <- function(fichero){
  
  ancho <- c(
    2,4,2,1,
    2,2,3,2,
    100,
    1,
    3,3,3,
    8,
    5,
    rep(8, 9),
    3,
    8,
    8,
    1
  )
  
  nombres <- c(
    "eleccion","anio","mes","vuelta",
    "ccaa","prov","mun","distrito_mun",
    "nombre",
    "distrito_electoral",
    "partido_judicial",
    "diputacion",
    "comarca",
    "poblacion",
    "n_mesas",
    "censo_ine",
    "censo_escrutinio",
    "censo_cere",
    "votantes_cere",
    "avance1",
    "avance2",
    "blanco",
    "nulo",
    "votos_candidaturas",
    "escanos",
    "votos_si",
    "votos_no",
    "oficial"
  )
  
  cat("anchos:", length(ancho), "\n")
  cat("nombres:", length(nombres), "\n")
  
  .read_fwf(
    fichero = fichero,
    ancho = ancho,
    nombres = nombres
  )
}

.leer_candidaturas_municipio_06 <- function(fichero){
  
  ancho <- c(
    2,4,2,1,
    2,3,2,
    6,
    8,
    3
  )
  
  nombres <- c(
    "eleccion","anio","mes","vuelta",
    "prov","mun","distrito_mun",
    "cod_cand",
    "votos",
    "electos"
  )
  
  .read_fwf(
    fichero = fichero,
    ancho = ancho,
    nombres = nombres
  )
}

.leer_ambito_superior_07 <- function(fichero){
  
  ancho <- c(
    2,4,2,1,
    2,2,1,
    50,
    8,
    5,
    rep(8, 9),
    6,
    8,
    8,
    1
  )
  
  nombres <- c(
    "eleccion","anio","mes","vuelta",
    "ccaa","prov","distrito_electoral",
    "nombre_ambito",
    "poblacion",
    "n_mesas",
    "censo_ine",
    "censo_escrutinio",
    "censo_cere",
    "votantes_cere",
    "avance1",
    "avance2",
    "blanco",
    "nulo",
    "votos_candidaturas",
    "escanos",
    "votos_si",
    "votos_no",
    "oficial"
  )
  
  .read_fwf(
    fichero = fichero,
    ancho = ancho,
    nombres = nombres
  )
}

.leer_candidaturas_ambito_08 <- function(fichero){
  
  ancho <- c(
    2,4,2,1,
    2,2,1,
    6,
    8,
    5
  )
  
  nombres <- c(
    "eleccion","anio","mes","vuelta",
    "ccaa","prov","distrito_electoral",
    "cod_cand",
    "votos",
    "electos"
  )
  
  .read_fwf(
    fichero = fichero,
    ancho = ancho,
    nombres = nombres
  )
}

.leer_mesas_09 <- function(fichero){
  
  ancho <- c(
    2,4,2,1,
    2,2,3,2,
    4,1,
    rep(7, 11),
    1
  )
  
  nombres <- c(
    "eleccion","anio","mes","vuelta",
    "ccaa","prov","mun","distrito",
    "seccion","mesa",
    "censo_ine",
    "censo_escrutinio",
    "censo_cere",
    "votantes_cere",
    "avance1",
    "avance2",
    "blanco",
    "nulo",
    "candidaturas",
    "si_ref",
    "no_ref",
    "oficial"
  )
  
  read_fwf(
    fichero,
    fwf_widths(ancho, nombres),
    col_types = cols(.default = "c")
  )
}

.leer_votosxcandidatura_10 <- function(fichero){
  
  ancho <- c(
    2,4,2,1,
    2,2,3,2,
    4,1,
    6,7
  )
  
  nombres <- c(
    "eleccion","anio","mes","vuelta",
    "ccaa","prov","mun","distrito",
    "seccion","mesa",
    "cod_cand",
    "votos"
  )
  
  .read_fwf(
    fichero = fichero,
    ancho = ancho,
    nombres = nombres
  )
}

# Guardado de ficheros congreso ----
guardar_proceso_congreso <- function(obj, periodo) {
  
  base_dir <- file.path(
    "interim",
    "generales",
    periodo
  )
  
  metadata_dir <- file.path(
    base_dir,
    "metadata"
  )
  
  dir.create(
    metadata_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # Metadata
  arrow::write_parquet(
    obj$candidaturas,
    file.path(metadata_dir, "candidaturas.parquet")
  )
  
  arrow::write_parquet(
    obj$candidatos,
    file.path(metadata_dir, "candidatos.parquet")
  )
  
  arrow::write_parquet(
    obj$datos_municipio,
    file.path(metadata_dir, "municipios.parquet")
  )
  
  arrow::write_parquet(
    obj$datos_ambito,
    file.path(metadata_dir, "ambitos.parquet")
  )
  
  # Resultados
  arrow::write_parquet(
    obj$mesas,
    file.path(base_dir, "mesas.parquet")
  )
  
  arrow::write_parquet(
    obj$votos_por_mesa,
    file.path(base_dir, "votos.parquet")
  )
  
}