# server.R ----

function(
    input,
    output,
    session
) {
  
  # Actualizar municipios al cambiar las provincias ----
  
  shiny::observeEvent(
    input$provincias,
    {
      
      provincias_seleccionadas <- as.integer(
        input$provincias
      )
      
      # No queda ninguna provincia seleccionada
      if (
        is.null(provincias_seleccionadas) ||
        length(provincias_seleccionadas) == 0
      ) {
        
        shiny::freezeReactiveValue(
          input,
          "municipios"
        )
        
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "municipios",
          choices = stats::setNames(
            integer(0),
            character(0)
          ),
          selected = integer(0)
        )
        
        return()
      }
      
      # Municipios disponibles para las provincias seleccionadas
      municipios_disponibles <- relmunicipios |>
        dplyr::filter(
          CPRO %in% provincias_seleccionadas
        ) |>
        dplyr::distinct(
          CPRO,
          CMUN,
          COD,
          NOMBRE,
          DENOMINACION
        ) |>
        dplyr::arrange(
          CPRO,
          NOMBRE
        )
      
      # DENOMINACION es la etiqueta visible
      # COD es el valor interno
      choices_municipios <- stats::setNames(
        as.integer(
          municipios_disponibles$COD
        ),
        municipios_disponibles$DENOMINACION
      )
      
      # Selección municipal anterior
      seleccion_anterior <- as.integer(
        input$municipios
      )
      
      if (
        is.null(seleccion_anterior) ||
        length(seleccion_anterior) == 0
      ) {
        
        seleccion_actual <- integer(0)
        
      } else {
        
        # Conservar solamente municipios todavía válidos
        seleccion_actual <- intersect(
          seleccion_anterior,
          as.integer(
            municipios_disponibles$COD
          )
        )
      }
      
      # Evitar estados reactivos intermedios
      shiny::freezeReactiveValue(
        input,
        "municipios"
      )
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "municipios",
        choices = choices_municipios,
        selected = seleccion_actual
      )
    },
    ignoreNULL = FALSE,   # al quitar todas las provincias también hay que vaciar municipios
    ignoreInit = TRUE,
    priority = 100
  )
  
  # Actualizar tipos de mapa según los partidos seleccionados ----
  
  shiny::observeEvent(
    input$partidos,
    {
      
      # Partidos seleccionados actualmente
      partidos_seleccionados <- input$partidos
      
      if (
        is.null(partidos_seleccionados) ||
        length(partidos_seleccionados) == 0
      ) {
        
        opciones_partidos <- stats::setNames(
          character(0),
          character(0)
        )
        
      } else {
        
        opciones_partidos <- stats::setNames(
          paste0(
            "votos_",
            partidos_seleccionados,
            "_pct_validos"
          ),
          paste0(
            partidos_seleccionados,
            " (% votos válidos)"
          )
        )
      }
      
      # Solo ganador, participación y partidos seleccionados
      opciones_mapa <- c(
        "Ganador por sección" = "ganador",
        "Participación (%)" = "participacion_pct",
        opciones_partidos
      )
      
      # Mantener el mapa seleccionado si sigue siendo válido
      tipo_mapa_seleccionado <- isolate(
        input$tipo_mapa
      )
      
      if (
        is.null(tipo_mapa_seleccionado) ||
        length(tipo_mapa_seleccionado) == 0 ||
        !tipo_mapa_seleccionado %in% unname(opciones_mapa)
      ) {
        tipo_mapa_seleccionado <- "ganador"
      }
      
      shiny::freezeReactiveValue(
        input,
        "tipo_mapa"
      )
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "tipo_mapa",
        choices = opciones_mapa,
        selected = tipo_mapa_seleccionado
      )
    },
    ignoreInit = TRUE,
    priority = 90
  )
  
  # Disparador automático solo tras renderizar la UI inicial ----
  disparador_mapa <- shiny::reactiveVal(0)
  
  session$onFlushed(function() {
    disparador_mapa(1)
  }, once = TRUE)
  
  shiny::observeEvent(input$actualizar, {
    disparador_mapa(disparador_mapa() + 1)
  }, ignoreInit = TRUE)
  
  
  
  # Lectura de datos al cargar la app y al pulsar Actualizar mapa ----
  objeto_mapa <- shiny::eventReactive(
    disparador_mapa(),
    {
      # Evitar ejecución antes de que onFlushed dispare el valor 1
      shiny::req(disparador_mapa() > 0)
      
      # Valores obligatorios
      shiny::req(
        input$eleccion
      )
      
      # Inicio de la medición total
      inicio_total <- proc.time()[["elapsed"]]
      
      resultado <- shiny::withProgress(
        message = "Actualizando el mapa...",
        detail = "Preparando los filtros.",
        value = 0,
        {
          
          shiny::incProgress(
            amount = 0.10,
            detail = "Preparando los filtros..."
          )
          
          
          # Filtros territoriales ----
          
          filtros <- list(
            provincias = as.integer(
              input$provincias
            ),
            municipios = as.integer(
              input$municipios
            ),
            distritos = as.integer(
              input$distritos
            ),
            secciones = as.integer(
              input$secciones
            ),
            eleccion = as.integer(
              input$eleccion
            )
          )
          
          
          # Partidos ----
          
          partidos <- input$partidos
          
          if (
            is.null(partidos) ||
            length(partidos) == 0
          ) {
            partidos <- PARTIDOS_INICIALES
          }
          
          
          # Variable del mapa ----
          
          variable <- input$tipo_mapa
          
          if (
            is.null(variable) ||
            length(variable) == 0
          ) {
            variable <- "ganador"
          }
          
          
          # Información de diagnóstico ----
          
          message(
            ""
          )
          
          message(
            paste0(
              "===== ACTUALIZACIÓN DEL MAPA ",
              format(
                Sys.time(),
                "%Y-%m-%d %H:%M:%S"
              ),
              " ====="
            )
          )
          
          message(
            "Provincias: ",
            if (length(filtros$provincias) == 0) {
              "Todas"
            } else {
              paste(
                filtros$provincias,
                collapse = ", "
              )
            }
          )
          
          message(
            "Municipios: ",
            if (
              is.null(filtros$municipios) ||
              length(filtros$municipios) == 0
            ) {
              "Todos"
            } else {
              paste(
                filtros$municipios,
                collapse = ", "
              )
            }
          )
          
          message(
            "Distritos: ",
            if (
              is.null(filtros$distritos) ||
              length(filtros$distritos) == 0
            ) {
              "Todos"
            } else {
              paste(
                filtros$distritos,
                collapse = ", "
              )
            }
          )
          
          message(
            "Secciones: ",
            if (
              is.null(filtros$secciones) ||
              length(filtros$secciones) == 0
            ) {
              "Todas"
            } else {
              paste(
                filtros$secciones,
                collapse = ", "
              )
            }
          )
          
          message(
            "Elección: ",
            filtros$eleccion
          )
          
          
          shiny::incProgress(
            amount = 0.20,
            detail = "Leyendo resultados electorales..."
          )
          
          
          # Medición de lectura y preparación de datos ----
          
          tiempo_datos <- system.time({
            
            datos <- .obtener_datos_mapa(
              ruta_resultados = ruta_resultados,
              ruta_geo = ruta_geo,
              relmun = relmunicipios,
              filtros = filtros,
              partidos = partidos
            )
          })
          
          
          message(
            "TIEMPO DATOS: ",
            round(
              tiempo_datos[["elapsed"]],
              3
            ),
            " segundos"
          )
          
          message(
            "Filas obtenidas: ",
            nrow(datos)
          )
          
          if ("cusec" %in% names(datos)) {
            
            message(
              "Secciones distintas: ",
              dplyr::n_distinct(
                datos$cusec
              )
            )
            
            message(
              "CUSEC duplicados: ",
              sum(
                duplicated(
                  datos$cusec
                )
              )
            )
          }
          
          
          shiny::incProgress(
            amount = 0.50,
            detail = "Comprobando las secciones censales..."
          )
          
          
          # Validación antes de representar ----
          
          if (
            is.null(datos) ||
            nrow(datos) == 0
          ) {
            stop(
              paste0(
                "No se han encontrado secciones censales ",
                "para los filtros seleccionados."
              ),
              call. = FALSE
            )
          }
          
          if (!inherits(datos, "sf")) {
            stop(
              "Los datos no son un objeto sf con geometrías.",
              call. = FALSE
            )
          }
          
          geometrias_validas <- sum(
            !sf::st_is_empty(datos)
          )
          
          message(
            "Geometrías válidas: ",
            geometrias_validas,
            " de ",
            nrow(datos)
          )
          
          message(
            "Tamaño del objeto en memoria: ",
            format(
              utils::object.size(datos),
              units = "MB"
            )
          )
          
          
          shiny::incProgress(
            amount = 0.15,
            detail = "Datos preparados."
          )
          
          
          # Guardar datos y parámetros ----
          
          list(
            datos = datos,
            filtros = filtros,
            partidos = partidos,
            variable = variable,
            tiempo_datos = unname(
              tiempo_datos[["elapsed"]]
            )
          )
        }
      )
      
      tiempo_total <- (
        proc.time()[["elapsed"]] -
          inicio_total
      )
      
      message(
        "TIEMPO TOTAL DE PREPARACIÓN: ",
        round(
          tiempo_total,
          3
        ),
        " segundos"
      )
      
      message(
        "========================================"
      )
      
      resultado$tiempo_total <- tiempo_total
      
      resultado
    },
    ignoreNULL = TRUE
  )
  
  
  # Subtítulo ----
  
  output$subtitulo <- shiny::renderUI({
    
    objeto <- objeto_mapa()
    
    codigo_eleccion <- as.character(
      objeto$filtros$eleccion
    )
    
    etiqueta_eleccion <- unname(
      COD_ELECCIONES[
        codigo_eleccion
      ]
    )
    
    # Municipios aplicados
    codigos_municipios <- objeto$filtros$municipios
    
    if (
      is.null(codigos_municipios) ||
      length(codigos_municipios) == 0
    ) {
      
      etiqueta_territorio <- if (
        length(objeto$filtros$provincias) == 0
      ) {
        "Toda España"
      } else {
        paste0(
          length(
            objeto$filtros$provincias
          ),
          " provincia(s), todos sus municipios"
        )
      }
      
    } else {
      
      nombres_municipios <- relmunicipios |>
        dplyr::filter(
          COD %in% codigos_municipios
        ) |>
        dplyr::pull(
          NOMBRE
        ) |>
        unique()
      
      if (length(nombres_municipios) <= 3) {
        
        etiqueta_territorio <- paste(
          nombres_municipios,
          collapse = ", "
        )
        
      } else {
        
        etiqueta_territorio <- paste0(
          length(nombres_municipios),
          " municipios seleccionados"
        )
      }
    }
    
    shiny::tags$div(
      shiny::tags$h4(
        paste0(
          "Elecciones generales · ",
          etiqueta_eleccion
        )
      ),
      shiny::tags$p(
        etiqueta_territorio,
        style = "color: #666666;"
      )
    )
  })
  
  
  # Representación del mapa ----
  
  output$mapa <- leaflet::renderLeaflet({
    
    objeto <- objeto_mapa()
    
    datos <- objeto$datos
    partidos <- objeto$partidos
    variable <- objeto$variable
    
    
    # Título ----
    
    if (
      identical(
        variable,
        "ganador"
      )
    ) {
      
      titulo <- "Ganador por sección"
      
    } else if (
      identical(
        variable,
        "participacion_pct"
      )
    ) {
      
      titulo <- "Participación (%)"
      
    } else {
      
      partido <- sub(
        "^votos_",
        "",
        variable
      )
      
      partido <- sub(
        "_pct_validos$",
        "",
        partido
      )
      
      titulo <- paste0(
        partido,
        " (% votos válidos)"
      )
    }
    
    
    # Medición de la construcción del mapa ----
    
    tiempo_mapa <- system.time({
      
      mapa <- .mapa_leaflet(
        datos = datos,
        variable = variable,
        partidos = partidos,
        titulo = titulo,
        paletacolpartidos = PALETA
      )
    })
    
    
    message(
      "TIEMPO MAPA LEAFLET: ",
      round(
        tiempo_mapa[["elapsed"]],
        3
      ),
      " segundos"
    )
    
    message(
      "El tiempo posterior hasta visualizarlo corresponde ",
      "principalmente a serialización, transferencia y navegador."
    )
    
    
    mapa
  })
}