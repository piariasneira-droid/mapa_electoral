library(sf)
library(plotly)
library(arrow)

.mapa_plotly_prueba <- function(
    datos,
    estilo = "open-street-map"
) {
  
  df <- as.data.frame(datos)
  df$geometry <- sf::st_as_sfc(df$geometry, EWKB = TRUE)
  dat_sf <- sf::st_as_sf(df, crs = 4326)
  
  
  p <- plot_ly() |> 
    add_sf(
      data =dat_sf,
      color = ~I("blue"),
      stroke = ~I("black"),
      span = ~I(1)
    ) |> 
    layout(
      title = "Mapa de secciones",
      mapbox = list(
        style = estilo,
        zoom = 8
      )
    )
  
  return(p)
}

prov <- "27"

dat <- arrow::open_dataset("./output/geo_parquet/secciones_2001.parquet") |>
  filter(substr(cusec,1, 2) == prov) |>
  dplyr::collect()

plot <- .mapa_plotly_prueba(datos = dat)
plot
