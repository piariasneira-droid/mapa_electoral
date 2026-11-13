# ui.R ----

shiny::fluidPage(
  
  # Cabecera ----
  
  shiny::titlePanel(
    "Resultados electorales por sección censal"
  ),
  
  
  # Contenido principal ----
  
  shiny::sidebarLayout(
    
    # Panel lateral ----
    
    shiny::sidebarPanel(
      
      width = 3,
      
      
      # Territorio ----
      
      shiny::h4(
        "Territorio"
      ),
      
      
      # Provincias ----
      
      shinyWidgets::pickerInput(
        inputId = "provincias",
        label = "Provincias",
        choices = choices_provincias,
        selected = FILTROSXDEFECTO$provincias,
        multiple = TRUE,
        width = "100%",
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          selectAllText = "Seleccionar todas",
          deselectAllText = "Desmarcar todas",
          noneSelectedText = "Ninguna provincia seleccionada",
          selectedTextFormat = "count > 3",
          countSelectedText = "{0} provincias seleccionadas",
          liveSearch = TRUE,
          liveSearchPlaceholder = "Buscar provincia...",
          size = 12
        )
      ),
      
      
      # Municipios ----
      
      shinyWidgets::pickerInput(
        inputId = "municipios",
        label = "Municipios",
        choices = choices_municipios_iniciales,
        selected = FILTROSXDEFECTO$municipios,
        multiple = TRUE,
        width = "100%",
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          selectAllText = "Seleccionar todos",
          deselectAllText = "Desmarcar todos",
          noneSelectedText = "Todos los municipios",
          selectedTextFormat = "count > 3",
          countSelectedText = "{0} municipios seleccionados",
          liveSearch = TRUE,
          liveSearchPlaceholder = "Buscar municipio...",
          size = 12
        )
      ),
      
      
      # Distritos ----
      
      shinyWidgets::pickerInput(
        inputId = "distritos",
        label = "Distritos",
        choices = 1L:99L,
        selected = FILTROSXDEFECTO$distritos,
        multiple = TRUE,
        width = "100%",
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          selectAllText = "Seleccionar todos",
          deselectAllText = "Desmarcar todos",
          noneSelectedText = "Todos los distritos",
          selectedTextFormat = "count > 3",
          countSelectedText = "{0} distritos seleccionados",
          liveSearch = TRUE,
          liveSearchPlaceholder = "Buscar distrito...",
          size = 12
        )
      ),
      
      
      # Secciones ----
      
      shinyWidgets::pickerInput(
        inputId = "secciones",
        label = "Secciones",
        choices = 1L:999L,
        selected = FILTROSXDEFECTO$secciones,
        multiple = TRUE,
        width = "100%",
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          selectAllText = "Seleccionar todas",
          deselectAllText = "Desmarcar todas",
          noneSelectedText = "Todas las secciones",
          selectedTextFormat = "count > 3",
          countSelectedText = "{0} secciones seleccionadas",
          liveSearch = TRUE,
          liveSearchPlaceholder = "Buscar sección...",
          size = 12
        )
      ),
      
      
      shiny::tags$hr(),
      
      
      # Elección ----
      
      shiny::h4(
        "Elección"
      ),
      
      shinyWidgets::pickerInput(
        inputId = "eleccion",
        label = "Elecciones generales",
        choices = choices_elecciones,
        selected = FILTROSXDEFECTO$eleccion,
        multiple = FALSE,
        width = "100%",
        options = shinyWidgets::pickerOptions(
          noneSelectedText = "Selecciona una elección",
          liveSearch = TRUE,
          liveSearchPlaceholder = "Buscar elección...",
          size = 12
        )
      ),
      
      
      shiny::tags$hr(),
      
      
      # Partidos ----
      
      shiny::h4(
        "Partidos"
      ),
      
      shinyWidgets::pickerInput(
        inputId = "partidos",
        label = "Partidos incluidos",
        choices = choices_partidos,
        selected = PARTIDOS_INICIALES,
        multiple = TRUE,
        width = "100%",
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          selectAllText = "Seleccionar todos",
          deselectAllText = "Desmarcar todos",
          noneSelectedText = "Ningún partido seleccionado",
          selectedTextFormat = "count > 5",
          countSelectedText = "{0} partidos seleccionados",
          liveSearch = TRUE,
          liveSearchPlaceholder = "Buscar partido...",
          size = 12
        )
      ),
      
      
      # Tipo de mapa ----
      
      shinyWidgets::pickerInput(
        inputId = "tipo_mapa",
        label = "Mapa a visualizar",
        choices = choices_tipo_mapa,
        selected = "ganador",
        multiple = FALSE,
        width = "100%",
        options = shinyWidgets::pickerOptions(
          noneSelectedText = "Selecciona un mapa",
          liveSearch = TRUE,
          liveSearchPlaceholder = "Buscar mapa...",
          size = 12
        )
      ),
      
      
      shiny::tags$br(),
      
      
      # Botón de actualización ----
      
      shiny::actionButton(
        inputId = "actualizar",
        label = "Actualizar mapa",
        icon = shiny::icon(
          "rotate"
        ),
        class = "btn-primary",
        width = "100%"
      )
    ),
    
    
    # Panel principal ----
    
    shiny::mainPanel(
      
      width = 9,
      
      shiny::uiOutput(
        outputId = "subtitulo"
      ),
      
      leaflet::leafletOutput(
        outputId = "mapa",
        height = "800px"
      )
    )
  )
)