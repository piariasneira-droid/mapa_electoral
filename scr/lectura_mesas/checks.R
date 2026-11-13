# Estructura dataframes ----
glimpse(mesas_historico)
glimpse(votos_historico)

count(mesas_historico, anio, mes)
count(votos_historico, anio, mes)

# Comrpbación votos ---
comprobacion_votos <- votos_historico |>
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
    mesa_id
  ) |>
  summarise(
    candidaturas_calculadas = sum(votos, na.rm = TRUE),
    .groups = "drop"
  )

control_historico <- mesas_historico |>
  left_join(
    comprobacion_votos,
    by = c(
      "fecha_eleccion",
      "eleccion",
      "anio",
      "mes",
      "prov",
      "mun",
      "distrito",
      "seccion",
      "mesa",
      "mesa_id"
    ),
    relationship = "one-to-one"
  ) |>
  mutate(
    diferencia_candidaturas =
      candidaturas_calculadas - votos_candidaturas,
    
    control_candidaturas =
      coalesce(diferencia_candidaturas, 0L) == 0L,
    
    control_censo =
      votos_emitidos <= censo_escrutinio
  )

control1 <- control_historico |>
  filter(
    !control_candidaturas |
      !control_censo |
      is.na(candidaturas_calculadas)
  ) |>
  select(
    fecha_eleccion,
    mesa_id,
    censo_escrutinio,
    votos_emitidos,
    votos_candidaturas,
    candidaturas_calculadas,
    diferencia_candidaturas,
    control_candidaturas,
    control_censo
  )

control2 <- votos_historico |>
  filter(is.na(siglas) | is.na(denominacion)) |>
  distinct(
    fecha_eleccion,
    cod_cand,
    siglas,
    denominacion
  )