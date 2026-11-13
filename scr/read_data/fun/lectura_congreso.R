# Utilidades ----
.read_fwf <- function(fichero, ancho, nombres){
  read_fwf(
    fichero,
    fwf_widths(ancho, nombres),
    col_types = cols(.default = "c"),
    locale = locale(encoding = "Latin1"),
    trim_ws = FALSE,
    progress = FALSE
  ) |>
    mutate(
      across(
        where(is.character),
        ~ iconv(.x, from = "Latin1", to = "UTF-8")
      )
    )
}

# Lectura congreso pipeline ----
leer_proceso_congreso <- function(path){
  
  fich <- list.files(
    path,
    pattern = "^01.*\\.DAT$",
    full.names = FALSE
  )
  
  codigo <- sub(
    "^01(.*)\\.DAT$",
    "\\1",
    fich
  )
  
  list(
    control = readLines(
      file.path(path, paste0("01", codigo, ".DAT"))
    ),
    identificacion = readLines(
      file.path(path, paste0("02", codigo, ".DAT"))
    ),
    candidaturas = .leer_candidaturas_03(
      file.path(path, paste0("03", codigo, ".DAT"))
    ),
    candidatos = .leer_candidatos_04(
      file.path(path, paste0("04", codigo, ".DAT"))
    ),
    datos_municipio = .leer_municipios_05(
      file.path(path, paste0("05", codigo, ".DAT"))
    ),
    votos_municipio = .leer_candidaturas_municipio_06(
      file.path(path, paste0("06", codigo, ".DAT"))
    ),
    datos_ambito = .leer_ambito_superior_07(
      file.path(path, paste0("07", codigo, ".DAT"))
    ),
    votos_ambito = .leer_candidaturas_ambito_08(
      file.path(path, paste0("08", codigo, ".DAT"))
    ),
    mesas = .leer_mesas_09(
      file.path(path, paste0("09", codigo, ".DAT"))
    ),
    votos_por_mesa = .leer_votosxcandidatura_10(
      file.path(path, paste0("10", codigo, ".DAT"))
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