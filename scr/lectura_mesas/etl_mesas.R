# Environment ----
library(readr)
library(dplyr)
library(purrr)
library(arrow)
library(stringr)
library(tidyr)

source("scr/lectura_mesas/fun/lectura_congreso_interim.R")

# Config ----
ruta_generales <- "interim/generales"
dir_output <- "output/generales/mesas_espana"

fecha_minima <- "200001"

filccaa <- NULL
filprov <- NULL
filmun  <- NULL
fildis  <- NULL
filsec  <- NULL
filmesa <- NULL

var_censo_problem <- 30

# Configuración ----
fecha_minima <- 200001

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

# Códigos elecciones congresos desde 1996 ----
codigos_elecciones <- list.dirs(
  ruta_generales,
  recursive = FALSE,
  full.names = FALSE
) |>
  keep(
    ~ str_detect(
      .x,
      "^\\d{6}$"
    )
  ) |>
  keep(
    ~ .x >= fecha_minima
  ) |>
  sort()

# Lectura elecciones ----
elecciones_congreso <- codigos_elecciones |>
  set_names() |>
  map(
    .preparar_eleccion_congreso,
    cod_ccaa = filccaa,
    cod_prov = filprov,
    cod_mun = filmun,
    cod_dis = fildis,
    cod_sec = filsec,
    cod_mesa = filmesa
  )

# Construcción histórico de mesas ----
mesas_historico <- elecciones_congreso |>
  map("mesas") |>
  list_rbind()

# Construcción histórico de votos ----
votos_historico <- elecciones_congreso |>
  map("votos") |>
  list_rbind()

# Eliminar registros no geolocalizables ----
mesas_historico <- mesas_historico |>
  filter(
    !(
      prov == "99" &
        mun == "999" &
        distrito == "09" &
        seccion == "0000"
    )
  )

votos_historico <- votos_historico |>
  filter(
    !(
      prov == "99" &
        mun == "999" &
        distrito == "09" &
        seccion == "0000"
    )
  )

# Construcción identificador mesa ----
mesas_historico <- mesas_historico |>
  mutate(
    fecha_eleccion = paste0(
      anio,
      mes
    ),
    mesa_id = str_c(
      prov,
      mun,
      distrito,
      seccion,
      mesa,
      sep = "-"
    ),
    mesa_eleccion_id = str_c(
      fecha_eleccion,
      mesa_id,
      sep = "-"
    )
  )

votos_historico <- votos_historico |>
  mutate(
    fecha_eleccion = paste0(
      anio,
      mes
    ),
    mesa_id = str_c(
      prov,
      mun,
      distrito,
      seccion,
      mesa,
      sep = "-"
    ),
    mesa_eleccion_id = str_c(
      fecha_eleccion,
      mesa_id,
      sep = "-"
    )
  )

# Calcular participación, abstención, blancos y nulos ----
mesas_historico <- mesas_historico |>
  mutate(
    votos_candidaturas = candidaturas,
    votos_validos = votos_candidaturas + blanco,
    votos_emitidos = votos_validos + nulo,
    
    abstencion = pmax(
      censo_escrutinio - votos_emitidos,
      0L
    ),
    
    participacion_pct = if_else(
      censo_escrutinio > 0,
      100 * votos_emitidos / censo_escrutinio,
      NA_real_
    ),
    
    abstencion_pct = if_else(
      censo_escrutinio > 0,
      100 * abstencion / censo_escrutinio,
      NA_real_
    ),
    
    blanco_pct_validos = if_else(
      votos_validos > 0,
      100 * blanco / votos_validos,
      NA_real_
    ),
    
    nulo_pct_emitidos = if_else(
      votos_emitidos > 0,
      100 * nulo / votos_emitidos,
      NA_real_
    )
  )

# Agrupar principales fuerzas ----
## Homogenizar partidos ----
votos_historico <- votos_historico |>
  mutate(
    partido_original = siglas,
    
    familia_partido = .clasificar_partido_congreso(
      siglas,
      denominacion
    )
  )

## Agrupar candidaturas ----
votos_partido_mesa <- votos_historico |>
  group_by(
    fecha_eleccion,
    eleccion,
    anio,
    mes,
    prov,
    mun,
    distrito,
    seccion,
    mesa,
    mesa_id,
    familia_partido
  ) |>
  summarise(
    votos = sum(
      votos,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

## Cálculos porcentajes ----
votos_partido_mesa <- votos_partido_mesa |>
  left_join(
    mesas_historico |>
      select(
        fecha_eleccion,
        mesa_id,
        censo_escrutinio,
        votos_emitidos,
        votos_validos,
        votos_candidaturas,
        blanco,
        nulo,
        abstencion,
        participacion_pct,
        abstencion_pct
      ),
    by = c(
      "fecha_eleccion",
      "mesa_id"
    ),
    relationship = "many-to-one"
  ) |>
  mutate(
    pct_voto_valido = if_else(
      votos_validos > 0,
      100 * votos / votos_validos,
      NA_real_
    ),
    
    pct_candidaturas = if_else(
      votos_candidaturas > 0,
      100 * votos / votos_candidaturas,
      NA_real_
    ),
    
    pct_censo = if_else(
      censo_escrutinio > 0,
      100 * votos / censo_escrutinio,
      NA_real_
    )
  )

# Dataset ancho por mesa ----
votos_ancho <- votos_partido_mesa |>
  filter(
    familia_partido %in% partidos_principales
  ) |>
  select(
    fecha_eleccion,
    mesa_id,
    familia_partido,
    votos
  ) |>
  pivot_wider(
    names_from = familia_partido,
    values_from = votos,
    values_fill = 0L,
    names_prefix = "votos_"
  )

mesas_resultados <- mesas_historico |>
  select(
    fecha_eleccion,
    anio,
    mes,
    ccaa,
    prov,
    mun,
    distrito,
    seccion,
    mesa,
    mesa_id,
    mesa_eleccion_id,
    censo_ine,
    censo_escrutinio,
    votos_emitidos,
    votos_validos,
    votos_candidaturas,
    abstencion,
    blanco,
    nulo,
    participacion_pct,
    abstencion_pct,
    blanco_pct_validos,
    nulo_pct_emitidos,
    oficial
  ) |>
  left_join(
    votos_ancho,
    by = c(
      "fecha_eleccion",
      "mesa_id"
    ),
    relationship = "one-to-one"
  ) |>
  mutate(
    across(
      starts_with("votos_"),
      ~ replace_na(
        .x,
        0L
      )
    )
  ) |>
  arrange(
    fecha_eleccion,
    seccion,
    mesa
  )

# Cálculo porcentajes ----
mesas_resultados <- mesas_resultados |>
  mutate(
    across(
      all_of(
        str_c(
          "votos_",
          partidos_principales
        )
      ),
      list(
        pct_validos = ~ if_else(
          votos_validos > 0,
          100 * .x / votos_validos,
          NA_real_
        ),
        
        pct_censo = ~ if_else(
          censo_escrutinio > 0,
          100 * .x / censo_escrutinio,
          NA_real_
        )
      ),
      .names = "{.col}_{.fn}"
    )
  )

# Cambios problemáticos en mesas ----
evolucion_mesas <- mesas_resultados |>
  arrange(
    mesa_id,
    fecha_eleccion
  ) |>
  group_by(
    mesa_id
  ) |>
  mutate(
    censo_anterior = lag(
      censo_escrutinio
    ),
    
    variacion_censo =
      censo_escrutinio -
      censo_anterior,
    
    variacion_censo_pct = if_else(
      censo_anterior > 0,
      100 * variacion_censo / censo_anterior,
      NA_real_
    ),
    
    posible_cambio_mesa =
      abs(variacion_censo_pct) >=
      var_censo_problem
  ) |>
  ungroup()

# Guardado de resultados ----
## Crear directorio de salida ----
dir.create(
  dir_output,
  recursive = TRUE,
  showWarnings = FALSE
)

## Definir datasets de salida ----
salidas <- list(
  mesas_historico = .tipar_columnas(mesas_historico),
  votos_historico = .tipar_columnas(votos_historico),
  votos_partido_mesa = .tipar_columnas(votos_partido_mesa),
  mesas_resultados = .tipar_columnas(mesas_resultados),
  evolucion_mesas = .tipar_columnas(evolucion_mesas)
)

## Guardar cada dataset en CSV y Parquet ----
iwalk(
  salidas,
  function(datos, nombre) {
    
    ruta_csv <- file.path(
      dir_output,
      paste0(
        nombre,
        ".csv"
      )
    )
    
    ruta_parquet <- file.path(
      dir_output,
      paste0(
        nombre,
        ".parquet"
      )
    )
    
    message(
      "Guardando: ",
      nombre
    )
    
    # Salida CSV
    write_csv(
      datos,
      file = ruta_csv,
      na = ""
    )
    
    # Salida Parquet
    write_parquet(
      datos,
      sink = ruta_parquet
    )
  }
)

message(
  "Proceso finalizado. Archivos guardados en: ",
  normalizePath(
    dir_output
  )
)