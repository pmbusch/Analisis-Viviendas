## Load data. Carga los datos y los filtras por los parametros elegidos para el reporte
# PBH Jun 2020

# Tipo de las variables: c character, d double, D date
cols_type <- "cccdddDddddddccddDccc"
df <- read_delim("Data/DataReporte.csv", delim = ";", skip = 1, na = c("NA"),
                 col_types = cols_type)
rm(cols_type)

spec(df)

## Solo departamentos o casa (depende del barrio,a veces hay muy poco de uno)
# Tipo de viviendas definido en el reporte como params
df <- df %>% filter(tipo_vivienda %in% viv)

# Filtrar datos a venta o arriendo
df <- df %>% filter(venta_arriendo %in% ven)


if (barrio!="NA"){
  # Filtrar por barrio
  dicc_barrios <- tribble(
    ~barrio_nom, ~codigo_barrio,
    "PlazaÑuñoa", "plaza-nunoa-santiago-metropolitana", 
    "Valdivia","valdivia-de-los-rios",
    "Coquimbo","coquimbo-coquimbo",
    "Lastarria","barrio-lastarria-santiago-santiago-metropolitana",
    "Providencia","providencia-metropolitana")
  
  
  codigo_barrio <- dicc_barrios %>% filter(barrio_nom==barrio) %>% pull(codigo_barrio)
  df <- df %>% filter(barrio==codigo_barrio)
}

# EoF