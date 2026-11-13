# Rutas ----
ruta_geo <- file.path(
  "data",
  "geo_parquet"
)

ruta_resultados <- file.path(
  "data",
  "resultados_seccion.parquet"
)

ruta_relmunicipios <- file.path(
  "data",
  "municipios.xlsx"
)

# Valores territoriales ----
FILTROSXDEFECTO <- list(
  provincias = c(28L),
  municipios = NULL,
  distritos = NULL,
  secciones = NULL,
  eleccion = 202307L
)

# Partidos ----
PALETA <- c(
  PSOE = "#E53935",
  PP = "#1E88E5",
  VOX = "#7CB342",
  IU = "#B71C1C",
  PODEMOS = "#6A1B9A",
  SUMAR = "#E91E63",
  UPYD = "#D81B60",
  CIUDADANOS = "#FB8C00",
  CIU_JUNTS = "#00A6A6",
  ERC = "#F9A825",
  PNV = "#00897B",
  HB_EH_BILDU = "#66A61E",
  BNG = "#1976D2",
  CC = "#FDD835",
  UPN = "#0D47A1",
  CDS = "#2E7D32",
  PA = "#43A047",
  PAR = "#FF8F00",
  CHA = "#8E0000",
  EA = "#C0CA33",
  EE = "#D4E157",
  NA_BAI_GEROA_BAI = "#7CB342",
  COMPROMIS = "#F57C00",
  PRC = "#6D4C41",
  FORO = "#3949AB",
  CUP = "#FBC02D",
  TERUEL_EXISTE = "#8D6E63",
  OTROS = "#9E9E9E"
)

PARTIDOS <- names(PALETA)

PARTIDOS_INICIALES <- c(
  "PSOE",
  "PP",
  "PODEMOS",
  "SUMAR",
  "VOX"
)

# Provincias ----
COD_PROV_INE <- c(
  `1`  = "Araba/Álava",
  `2`  = "Albacete",
  `3`  = "Alacant/Alicante",
  `4`  = "Almería",
  `5`  = "Ávila",
  `6`  = "Badajoz",
  `7`  = "Illes Balears",
  `8`  = "Barcelona",
  `9`  = "Burgos",
  `10` = "Cáceres",
  `11` = "Cádiz",
  `12` = "Castelló/Castellón",
  `13` = "Ciudad Real",
  `14` = "Córdoba",
  `15` = "A Coruña",
  `16` = "Cuenca",
  `17` = "Girona",
  `18` = "Granada",
  `19` = "Guadalajara",
  `20` = "Gipuzkoa",
  `21` = "Huelva",
  `22` = "Huesca",
  `23` = "Jaén",
  `24` = "León",
  `25` = "Lleida",
  `26` = "La Rioja",
  `27` = "Lugo",
  `28` = "Madrid",
  `29` = "Málaga",
  `30` = "Murcia",
  `31` = "Navarra",
  `32` = "Ourense",
  `33` = "Asturias",
  `34` = "Palencia",
  `35` = "Las Palmas",
  `36` = "Pontevedra",
  `37` = "Salamanca",
  `38` = "Santa Cruz de Tenerife",
  `39` = "Cantabria",
  `40` = "Segovia",
  `41` = "Sevilla",
  `42` = "Soria",
  `43` = "Tarragona",
  `44` = "Teruel",
  `45` = "Toledo",
  `46` = "València/Valencia",
  `47` = "Valladolid",
  `48` = "Bizkaia",
  `49` = "Zamora",
  `50` = "Zaragoza",
  `51` = "Ceuta",
  `52` = "Melilla"
)

# Elecciones al congreso ----
COD_ELECCIONES <- c(
  `200003` = "12 de marzo de 2000",
  `200403` = "14 de marzo de 2004",
  `200803` = "9 de marzo de 2008",
  `201111` = "20 de noviembre de 2011",
  `201512` = "20 de diciembre de 2015",
  `201606` = "26 de junio de 2016",
  `201904` = "28 de abril de 2019",
  `201911` = "10 de noviembre de 2019",
  `202307` = "23 de julio de 2023"
)