# Reutiliza las funciones ya validadas

source(
  "./scr/geoespacial/fun_sf_to_parquet.R",
  local = TRUE
)

.escribir_geojson_provincia <- function(
    datos,
    provincia,
    output_dir_anho
) {
  
  fichero <- file.path(
    output_dir_anho,
    paste0(
      "prov_",
      provincia,
      ".geojson"
    )
  )
  
  geojson <- geojsonsf::sf_geojson(
    datos,
    atomise = FALSE,
    simplify = FALSE,
    digits = 7
  )
  
  writeLines(
    geojson,
    fichero,
    useBytes = TRUE
  )
  
  file.info(fichero)$size / 1024^2
}

.procesar_anho_geojson <- function(anho) {
  
  message("")
  message(
    paste(
      rep("=", 70),
      collapse = ""
    )
  )
  
  message(
    "Procesando año ",
    anho
  )
  
  carpeta_anho <- file.path(
    input_dir,
    as.character(anho)
  )
  
  if (!dir.exists(carpeta_anho)) {
    
    stop(
      "No existe la carpeta: ",
      carpeta_anho
    )
  }
  
  ficheros_shp <- list.files(
    carpeta_anho,
    pattern = "\\.shp$",
    full.names = TRUE
  )
  
  ficheros_shp <- sort(
    ficheros_shp
  )
  
  bloques <- lapply(
    ficheros_shp,
    .leer_bloque_geografico,
    anho = anho
  )
  
  if (length(bloques) > 1) {
    
    bloques <- lapply(
      bloques,
      .convertir_atributos_a_texto
    )
  }
  
  secciones <- bind_rows(
    bloques
  )
  
  if ("prov" %in% names(secciones)) {
    
    if ("prov_original" %in% names(secciones)) {
      
      secciones <- secciones |>
        select(
          -prov_original
        )
    }
    
    secciones <- secciones |>
      rename(
        prov_original = prov
      )
  }
  
  secciones <- .construir_cusec(
    secciones
  )
  
  secciones <- .eliminar_registros_sin_cusec(
    secciones,
    anho
  )
  
  .guardar_diagnostico_cusec_invalidos(
    secciones,
    anho
  )
  
  secciones <- .consolidar_cusec_duplicados(
    secciones,
    anho
  )
  
  .validar_cusec(
    secciones,
    anho
  )
  
  secciones <- secciones |>
    arrange(
      cusec
    )
  
  output_dir_anho <- file.path(
    output_dir,
    as.character(anho)
  )
  
  dir.create(
    output_dir_anho,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  provincias <- sort(
    unique(
      secciones$prov
    )
  )
  
  tamanhos <- vapply(
    provincias,
    function(provincia) {
      
      message(
        "  Provincia ",
        provincia
      )
      
      datos_prov <- secciones |>
        filter(
          prov == provincia
        )
      
      .escribir_geojson_provincia(
        datos = datos_prov,
        provincia = provincia,
        output_dir_anho = output_dir_anho
      )
      
    },
    numeric(1)
  )
  
  data.frame(
    anho = as.integer(anho),
    provincias = length(
      provincias
    ),
    filas = nrow(
      secciones
    ),
    tamanho_mb = sum(
      tamanhos
    ),
    estado = "OK",
    stringsAsFactors = FALSE
  )
}