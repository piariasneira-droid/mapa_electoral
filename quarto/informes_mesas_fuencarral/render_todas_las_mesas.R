#!/usr/bin/env Rscript

# Genera para todas las mesas electorales:
#   1. HTML autocontenido con Plotly.
#   2. PDF maquetado con LaTeX (LuaLaTeX).
#
# Ejecutar desde la raiz del proyecto:
#   Rscript quarto/render_todas_las_mesas.R

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(purrr)
  library(readr)
  library(yaml)
})

# Configuracion -------------------------------------------------------------

ruta_qmd_html <- "quarto/informe_mesa.qmd"
ruta_qmd_pdf <- "quarto/informe_mesa_pdf.qmd"
ruta_qmd_html_render <- "informe_mesa_html_render.qmd"
ruta_qmd_pdf_render <- "informe_mesa_pdf_render.qmd"
ruta_config_base <- "quarto/config/informe_mesa.yml"
ruta_datos <- "output/mesas_resultados.parquet"

dir_render_quarto <- "output/informes_mesa"
dir_base <- "output/informe_mesas"
dir_html <- file.path(dir_base, "htmls")
dir_pdf <- file.path(dir_base, "pdf")
dir_config_temp <- file.path(dir_base, "config_temp")

# Inf procesa todas las mesas. Para pruebas, usar por ejemplo 1 o 3.
limite_mesas <- Inf

# FALSE permite reanudar sin regenerar informes ya existentes.
sobrescribir_html <- FALSE
sobrescribir_pdf <- FALSE

normalizar_nombre <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  gsub("_+", "_", x)
}

ejecutar_comando <- function(comando, argumentos, descripcion) {
  salida <- system2(
    command = comando,
    args = argumentos,
    stdout = TRUE,
    stderr = TRUE
  )
  estado <- attr(salida, "status")
  if (is.null(estado)) estado <- 0L

  if (as.integer(estado) != 0L) {
    stop(
      descripcion, " ha fallado con codigo ", estado, ".\n",
      paste(salida, collapse = "\n"),
      call. = FALSE
    )
  }
  invisible(salida)
}

preparar_qmd_render <- function(origen, destino) {
  if (!file.copy(origen, destino, overwrite = TRUE)) {
    stop("No se ha podido preparar el QMD temporal: ", destino,
         call. = FALSE)
  }
  invisible(destino)
}

renderizar_informe <- function(qmd, formato, config_mesa) {
  ejecutar_comando(
    quarto_bin,
    c(
      "render", shQuote(qmd),
      "--to", formato,
      "-P", shQuote(paste0("config:", config_mesa))
    ),
    paste("Renderizado", toupper(formato))
  )
}

localizar_salida <- function(nombre, extension) {
  candidatos <- c(
    file.path(dir_render_quarto, paste0(nombre, ".", extension)),
    paste0(nombre, ".", extension),
    file.path("quarto", paste0(nombre, ".", extension))
  )
  encontrados <- candidatos[file.exists(candidatos)]
  if (!length(encontrados)) {
    stop(
      "No se localiza el archivo generado ", nombre, ".", extension,
      ". Buscado en:\n- ", paste(candidatos, collapse = "\n- "),
      call. = FALSE
    )
  }
  encontrados[[1]]
}

mover_archivo <- function(origen, destino) {
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(destino)) unlink(destino, force = TRUE)

  movido <- file.rename(origen, destino)
  if (!isTRUE(movido)) {
    copiado <- file.copy(origen, destino, overwrite = TRUE)
    if (!isTRUE(copiado)) {
      stop("No se ha podido mover ni copiar ", origen, " a ", destino,
           call. = FALSE)
    }
    unlink(origen, force = TRUE)
  }
  invisible(destino)
}

limpiar_render_temporal <- function(eliminar_qmd = FALSE) {
  nombres <- c("informe_mesa_html_render", "informe_mesa_pdf_render")
  extensiones <- c("html", "pdf", "tex", "md", "knit.md", "log", "aux",
                   "out", "toc")
  archivos <- unlist(lapply(nombres, function(n) {
    unlist(lapply(extensiones, function(e) c(
      file.path(dir_render_quarto, paste0(n, ".", e)),
      paste0(n, ".", e),
      file.path("quarto", paste0(n, ".", e))
    )))
  }), use.names = FALSE)
  archivos <- unique(archivos[file.exists(archivos)])
  if (length(archivos)) unlink(archivos, recursive = TRUE, force = TRUE)

  recursos <- unlist(lapply(nombres, function(n) c(
    file.path(dir_render_quarto, paste0(n, "_files")),
    paste0(n, "_files"),
    file.path("quarto", paste0(n, "_files"))
  )), use.names = FALSE)
  recursos <- recursos[dir.exists(recursos)]
  if (length(recursos)) unlink(recursos, recursive = TRUE, force = TRUE)

  if (eliminar_qmd) {
    temporales <- c(ruta_qmd_html_render, ruta_qmd_pdf_render)
    temporales <- temporales[file.exists(temporales)]
    if (length(temporales)) unlink(temporales, force = TRUE)
  }
  invisible(NULL)
}

# Validaciones --------------------------------------------------------------

quarto_bin <- Sys.which("quarto")
if (!nzchar(quarto_bin)) {
  stop("No se encuentra el ejecutable Quarto en PATH.", call. = FALSE)
}

archivos_necesarios <- c(
  ruta_qmd_html, ruta_qmd_pdf, ruta_config_base, ruta_datos, "_quarto.yml"
)
faltantes <- archivos_necesarios[!file.exists(archivos_necesarios)]
if (length(faltantes)) {
  stop("Faltan los siguientes archivos:\n- ",
       paste(faltantes, collapse = "\n- "), call. = FALSE)
}

# Directorios ---------------------------------------------------------------

for (directorio in c(dir_render_quarto, dir_html, dir_pdf, dir_config_temp)) {
  dir.create(directorio, recursive = TRUE, showWarnings = FALSE)
}

# Lectura de mesas ----------------------------------------------------------

config_base <- yaml::read_yaml(ruta_config_base)
mesas <- arrow::read_parquet(ruta_datos, col_select = "mesa_id") |>
  as_tibble() |>
  distinct(mesa_id) |>
  filter(!is.na(mesa_id), nzchar(mesa_id)) |>
  arrange(mesa_id) |>
  pull(mesa_id)

if (is.finite(limite_mesas)) mesas <- head(mesas, as.integer(limite_mesas))
if (!length(mesas)) stop("No se han encontrado mesas para procesar.")

message("Mesas a procesar: ", length(mesas))
message("HTML: ", dir_html)
message("PDF LaTeX: ", dir_pdf)

# Las copias en la raiz mantienen juntas la plantilla y los recursos generados,
# lo que permite usar embed-resources en HTML sin mezclar ambos formatos.
limpiar_render_temporal(eliminar_qmd = TRUE)
preparar_qmd_render(ruta_qmd_html, ruta_qmd_html_render)
preparar_qmd_render(ruta_qmd_pdf, ruta_qmd_pdf_render)

# Generacion ----------------------------------------------------------------

resultados <- map_dfr(seq_along(mesas), function(i) {
  mesa_id <- mesas[[i]]
  mesa_nombre <- normalizar_nombre(mesa_id)
  nombre_base <- paste0("evolucion_historica_mesa_", mesa_nombre)

  ruta_html_final <- file.path(dir_html, paste0(nombre_base, ".html"))
  ruta_pdf_final <- file.path(dir_pdf, paste0(nombre_base, ".pdf"))
  ruta_config_mesa <- file.path(
    dir_config_temp, paste0("config_", mesa_nombre, ".yml")
  )

  config_mesa <- config_base
  config_mesa$mesa_id <- mesa_id
  config_mesa$datos_mesas <- ruta_datos
  yaml::write_yaml(config_mesa, ruta_config_mesa, fileEncoding = "UTF-8")

  message(sprintf("[%d/%d] Procesando mesa %s", i, length(mesas), mesa_id))
  estado_html <- "PENDIENTE"
  estado_pdf <- "PENDIENTE"
  error_html <- NA_character_
  error_pdf <- NA_character_

  if (!sobrescribir_html && file.exists(ruta_html_final)) {
    estado_html <- "EXISTENTE"
    message("  HTML existente: se omite.")
  } else {
    estado_html <- tryCatch({
      limpiar_render_temporal()
      renderizar_informe(
        ruta_qmd_html_render, "html", ruta_config_mesa
      )
      mover_archivo(
        localizar_salida("informe_mesa_html_render", "html"),
        ruta_html_final
      )
      "OK"
    }, error = function(e) {
      error_html <<- conditionMessage(e)
      message("  ERROR HTML: ", error_html)
      "ERROR"
    })
  }

  if (!sobrescribir_pdf && file.exists(ruta_pdf_final)) {
    estado_pdf <- "EXISTENTE"
    message("  PDF existente: se omite.")
  } else {
    estado_pdf <- tryCatch({
      limpiar_render_temporal()
      renderizar_informe(
        ruta_qmd_pdf_render, "pdf", ruta_config_mesa
      )
      mover_archivo(
        localizar_salida("informe_mesa_pdf_render", "pdf"),
        ruta_pdf_final
      )
      "OK"
    }, error = function(e) {
      error_pdf <<- conditionMessage(e)
      message("  ERROR PDF: ", error_pdf)
      "ERROR"
    })
  }

  limpiar_render_temporal()
  if (file.exists(ruta_config_mesa)) unlink(ruta_config_mesa, force = TRUE)

  tibble(
    mesa_id = mesa_id,
    estado_html = estado_html,
    estado_pdf = estado_pdf,
    html = if (file.exists(ruta_html_final)) ruta_html_final else NA_character_,
    pdf = if (file.exists(ruta_pdf_final)) ruta_pdf_final else NA_character_,
    error_html = error_html,
    error_pdf = error_pdf
  )
})

# Limpieza y registro -------------------------------------------------------

limpiar_render_temporal(eliminar_qmd = TRUE)
config_temporales <- list.files(dir_config_temp, full.names = TRUE)
if (length(config_temporales)) {
  unlink(config_temporales, recursive = TRUE, force = TRUE)
}

ruta_registro <- file.path(dir_base, "registro_generacion_informes.csv")
readr::write_csv(resultados, ruta_registro, na = "")

html_correctos <- sum(resultados$estado_html %in% c("OK", "EXISTENTE"))
pdf_correctos <- sum(resultados$estado_pdf %in% c("OK", "EXISTENTE"))
html_errores <- sum(resultados$estado_html == "ERROR")
pdf_errores <- sum(resultados$estado_pdf == "ERROR")

message("")
message("Proceso terminado.")
message("HTML correctos: ", html_correctos)
message("HTML con error: ", html_errores)
message("PDF correctos: ", pdf_correctos)
message("PDF con error: ", pdf_errores)
message("Registro: ", ruta_registro)

if (html_errores > 0 || pdf_errores > 0) quit(status = 1L)
