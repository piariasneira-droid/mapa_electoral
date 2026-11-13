# Informe electoral por mesa

Archivos:

- `_quarto.yml`: configuración compartida del proyecto Quarto.
- `config/informe_mesa.yml`: mesa y rutas de datos.
- `informe_mesa.qmd`: informe PDF parametrizado.

## Renderizado

Desde la raíz del proyecto:

```bash
quarto render informe_mesa.qmd
```

El PDF se escribirá en `output/informes_mesa/`.

Antes de renderizar, cambie `mesa_id` en `config/informe_mesa.yml` y compruebe que `datos_mesas` apunta a `output/mesas_resultados.parquet`.

Dependencias R: `arrow`, `dplyr`, `ggplot2`, `knitr`, `scales`, `stringr`, `tidyr` y `yaml`.
