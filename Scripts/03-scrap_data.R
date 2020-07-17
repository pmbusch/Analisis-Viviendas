## Web scraping datos Viviendas
## Datos obtenidos del Portal Inmobiliario
## PBH Nov 2018
# Actualizado PBH Jun 2020


source("Scripts/00-Funciones.R")
source("Scripts/00-CargaLibrerias.R")
source("Scripts/01-f_scrap_viv.R")
source("Scripts/02-f_scrap_barrio.R")

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
  ambientes= as.numeric(),
  estacionamientos = as.numeric(),
  descripcion = as.character(),
  vende = as.character(),
  longitud=as.numeric(),
  latitud=as.numeric(),
  fecha_consulta = as.character(),
  venta_arriendo = as.character(),
  tipo_vivienda=as.character(),
  barrio_portal=as.character()
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
barrios <- c("valdivia-de-los-rios")
barrios <- c("plaza-nunoa-santiago-metropolitana")

tipos <- c("departamento")
tipos <- c("departamento", "casa")
ventas <- c(T,F)

# Datos Terreno Aysen
tipos <- c("sitio", "parcela", "terreno-en-construccion")
barrios <- c("aysen-aysen",
             "coyhaique-aysen",
             "chile-chico-aysen",
             "cochrane-aysen",
             "cisnes-aysen",
             "tortel-aysen",
             "rio-ibanez-aysen")
ventas <- c(T)


for (v in ventas){
  for (bar in barrios) {
    for (tip in tipos) {
      tryCatch(
        {
          df <- rbind(df, f.scrap.barrio(bar, venta = v,tip))
        }
        , error = function(cond) return(NULL) )
    }
  }
}


# Exportar tabla de datos a Excel o un formato .csv
# Grabo dataframe en hoja excel para analisis
cat('sep=; \n',file = "Data/DataReporte.csv")
write.table(x = df, file = "Data/DataReporte.csv",row.names = FALSE, sep = ";",append = T)

# EoF