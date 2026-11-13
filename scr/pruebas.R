library(arrow)
library(geoarrow)
library(sf)
library(dplyr)
library(leaflet)

mapa_secciones_leaflet <- function(
    prov = 1L:52L,
    anho = 2001
) {
  ruta <- sprintf("./output/geo_parquet/secciones_%s.parquet", anho)
  
  # Normalizar prov a carácter de 2 dígitos ("01", "02", ..., "52")
  prov_chr <- sprintf("%02d", as.integer(prov))
  
  # 1. Leer, filtrar en disco y asegurar coordenadas WGS84 (requerido por Leaflet)
  dat_sf <- arrow::open_dataset(ruta) |>
    filter(substr(cusec, 1, 2) %in% prov_chr) |>
    collect() |>
    mutate(geometry = sf::st_as_sfc(geometry)) |>
    sf::st_as_sf() |>
    sf::st_transform(4326)
  
  # 2. Renderizar mapa con OpenStreetMap y resaltado interactivo
  leaflet(dat_sf) |>
    addTiles() |>
    addPolygons(
      fillColor = "steelblue",
      fillOpacity = 0.35,
      color = "#1f2937",
      weight = 1,
      opacity = 0.8,
      label = ~paste("Sección:", cusec),
      highlightOptions = highlightOptions(
        weight = 2.5,
        color = "#d97706",
        fillOpacity = 0.6,
        bringToFront = TRUE
      )
    )
}

# --- Pruebas ---
mapa_secciones_leaflet(prov = 1L:52L, anho = 2001)