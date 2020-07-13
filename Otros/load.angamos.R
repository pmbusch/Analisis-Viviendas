## Analisis datos Angamos
##
## 23 Agosto 2017

## ========= se requieren estan bibliotecas =========

##LIMPIAR WORKSPACE
rm(list=ls())


## cargamos los datos

db_folder<-Sys.getenv('db_folder')
path<-file.path(db_folder,"\\04-GL-Proyectos\\2017-Angamos\\Trabajo\\BD.Emisiones vs generacion.xlsx")

bd_angamos<-read_excel(path,sheet="BD_horaria")
bd_angamos_mensual<-read_excel(path,sheet="BD_mensual")


#View(bd_angamos)
#View(bd_angamos_mensual)
dim(bd_angamos)
dim(bd_angamos_mensual)
colnames(bd_angamos)
colnames(bd_angamos_mensual)

##FUNCIONES
f.descr = function(data) {
  print(names(data))
  print(dim(data))
  print(summary(data))
}

# convertimos a factores
bd_angamos$est_uge = as.factor(bd_angamos$est_uge)
bd_angamos$est_nox = as.factor(bd_angamos$est_nox)
bd_angamos$est_so2 = as.factor(bd_angamos$est_so2)
bd_angamos$est_mp = as.factor(bd_angamos$est_mp)
bd_angamos$est_o2 = as.factor(bd_angamos$est_o2)
bd_angamos$est_q = as.factor(bd_angamos$est_q)
bd_angamos$mes = as.factor(bd_angamos$mes)
bd_angamos$ano = as.factor(bd_angamos$ano)
bd_angamos$dia = as.factor(bd_angamos$dia)
bd_angamos$hora = as.factor(bd_angamos$hora)
bd_angamos$fecha.hora = as.factor(bd_angamos$fecha.hora)
bd_angamos$fecha = as.factor(bd_angamos$fecha)

bd_angamos_mensual$mes = as.factor(bd_angamos_mensual$mes)
bd_angamos_mensual$ano = as.factor(bd_angamos_mensual$ano)
bd_angamos_mensual$`ano-mes` = as.factor(bd_angamos_mensual$`ano-mes`)


#Adjuntar
#attach(bd_angamos)
#attach(bd_angamos_mensual)
#Breve descripcion
f.descr(bd_angamos)
f.descr(bd_angamos_mensual)

bd_angamos$Temp_GS[bd_angamos$Temp_GS==0]<-NA
bd_angamos$Hum_GS[bd_angamos$Hum_GS==0]<-NA
bd_angamos$Pres_GS[bd_angamos$Pres_GS==0]<-NA

f.descr(bd_angamos)
f.descr(bd_angamos_mensual)
#Guardar datos
#save.image(paste(db_folder,"/30-Analisis/Stats/discor/ProyectosR/Angamos/Angamos.RData",sep=""),version=NULL,ascii=FALSE,compress=!ascii,safe=TRUE)
save.image(file.path(db_folder,"\\30-Analisis\\Stats\\discor\\ProyectosR\\Angamos\\Angamos.RData"))

#eof
