# Función de lectura ----
.leer_candidaturas <- function(aaaamm) {
  
  fichero <- file.path(
    input_dir,
    aaaamm,
    "metadata",
    "candidaturas.parquet"
  )
  
  if (!file.exists(fichero)) {
    
    warning(
      "No existe: ",
      fichero,
      call. = FALSE
    )
    
    return(NULL)
  }
  
  
  message(
    "Leyendo ",
    aaaamm,
    ": ",
    fichero
  )
  
  
  datos <- arrow::read_parquet(
    fichero
  )
  
  
  message(
    "  Filas: ",
    format(
      nrow(datos),
      big.mark = ".",
      scientific = FALSE
    )
  )
  
  
  datos |>
    dplyr::mutate(
      aaaamm = as.integer(aaaamm),
      .before = 1
    )
}
