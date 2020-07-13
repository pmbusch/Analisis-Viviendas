## Web scraping datos Viviendas
## Datos obtenidos del Portal Inmobiliario
## Nov 2018
## PBH


source("00-Funciones.R")
source("01-f_scrap_viv.R")
source("02-f_scrap_barrio.R")

# Cargar librerias necesarias
library(rvest)
library(stringr)
library(readr)


df <- data.frame(
  barrio = as.character(),
  dir_url = as.character(),
  nombre = as.character(),
  precio = as.numeric(),
  precio_uf = as.numeric(),
  codigo = as.numeric(),
  fecha_publicacion = as.character(),
  dorm = as.numeric(),
  banos = as.numeric(),
  sup = as.numeric(),
  sup_total = as.numeric(),
  descripcion = as.character(),
  equipamiento = as.character(),
  vende = as.character(),
  fecha_consulta = as.character(),
  venta_arriendo = as.character(),
  tipo_vivienda=as.character()
)

# Posibles barrios de interes, dp se podria iterar por estos
barrios <- c(
  "pocuro-providencia-santiago-metropolitana",
  "metro-tobalabamall-costanera-providencia-santiago-metropolitana",
  "plaza-nunoa-santiago-metropolitana",
  "metro-los-leones-providencia-santiago-metropolitana",
  "barrio-italia-providencia-santiago-metropolitana",
  "metro-manquehueapumanque-las-condes-santiago-metropolitana",
  "plaza-italia-providencia-santiago-metropolitana",
  "parque-ines-de-suarez-providencia-santiago-metropolitana",
  "parque-juan-xxiii-nunoa-santiago-metropolitana",
  "nunoa-metropolitana",
  "providencia-metropolitana"
)
barrios <- c("plaza-nunoa-santiago-metropolitana")
barrios <- c("valdivia-de-los-rios")

tipos <- c("departamento")
tipos <- c("departamento", "casa")

ventas <- c(T,F)

for (v in ventas){
  for (bar in barrios) {
    for (tip in tipos) {
      df <- rbind(df, f.scrap.barrio(bar, venta = v,tip))
    }
  }
}



# Exportar tabla de datos a Excel o un formato .csv
# Grabo dataframe en hoja excel para analisis
write.table(x = df, file = "Data.csv", row.names = FALSE, sep = ";")

# EoF