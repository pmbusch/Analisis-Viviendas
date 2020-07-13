## Analisis datos Viviendas
##Datos obtenidos del Portal Inmobiliario
## Nov 2017
##PBH


##LIMPIAR WORKSPACE
rm(list=ls())

#tipo_precio <- 'venta'
 tipo_precio <- 'arriendo'

## cargamos los datos
fecha <- "20-01-2018"
bd<-read_excel(paste(fecha,"_",tipo_precio,"_InfoPortal.xlsx",sep=''),sheet="BD.Precios")

dim(bd)
names(bd)
bd

##Se necesita instalar una vez la libreria que utilizamos para leer excel
install.packages("readxl")
##Cargamos la libreria ya instalada
library(readxl)
##Utilizamos el comando "read_excel" para leer el excel ubicado en la misma carpeta que el script de R
datos_berberis <- read_excel("tabla Berberis Shrubs.xlsx",sheet = "Datos")
##La info del excel se cargo a la variable datos_berberis
datos_berberis$diameter <- as.numeric(datos_berberis$diameter)
datos_berberis
summary(datos_berberis)
names(datos_berberis)
diametro = datos_berberis$diameter
altura=datos_berberis$height
linea3=lm(altura~diametro)
summary(linea3)
plot(altura~diametro)
abline(linea3,col="Red")
#


# convertimos a factores
bd$direccion <- as.factor(bd$direccion)
bd$barrio <- as.factor(bd$barrio)


#Breve descripcion
summary(bd)

## HACER COMO UN VLOOKUP PARA CLASIFICAR LOS BARRIOS EN CODIGOS
barrio <- unique(bd$barrio)
bar <- c('Pocuro','Costanera','Plaza Nunoa','Barrio Italia','Manquehue','Nunoa','Providencia')
lookup_barrios <- data.frame(barrio,bar)
names(lookup_barrios)


bd <- merge(lookup_barrios,bd,by='barrio')
bd$bar <- as.factor(bd$bar)
summary(bd)


#Guardar datos
#
save.image(".RData")

#eof
