## descripcion datos angamos

datos=bd_angamos
datosmes=bd_angamos_mensual

##Funcion Quintiles
##Esta funcion genera un dataframe igual al que se le entrega, pero con una columna adicional 
##con la clasifcacion de una columna numerica en factores. Los intervalos de esta clasificacion
## pueden ser iguales o en base a los percentiles. PBH 09/2017
f.factor.quantile <- function(dataframe,
                              column_name,
                              intervals=5,
                              percentil=F) ##Igual a True los intervalos se generan en base a los percentiles
  {
  x <-dataframe[[column_name]]
  if (percentil==T) { 
    percentiles <- quantile(x, probs = seq(0, 1, by= 1/intervals)) }
  else { 
    minimo <- min(x)
    maximo <- max(x)
    step <- (maximo-minimo)/intervals
    percentiles <- seq(minimo,maximo,by=step)
  }
  ext <- if (percentil==T) 'percentiles' else 'tramos'
  nombre <- paste(column_name,ext,sep = '_')
  print(nombre)
  dataframe[[nombre]] <- cut(x,percentiles,include.lowest = T)
  return(dataframe)
}



f.desc.ggplot <- function(datos,
                          x_data,
                          y_data,
                          x_lab=as.character(substitute(x_data)),
                          y_lab=as.character(substitute(y_data)),
                          aes_fill,MultipleRegresion=T)
                          {
 
##FIGURAS GGPLOT
titulo = paste(y_lab," vs ",x_lab)
filename = paste("pdf_Descr\\Desc.",y_lab,"-",x_lab," por ",as.character(substitute(aes_fill)),".pdf")
#pdf(filename)

## Histograma de x
##p2 = ggplot(datos, aes_string(x=x_data))+ggtitle(x_lab)+xlab(x_lab)
##print(p2 + geom_density())
##print(p2 + geom_density( aes_string(x_data,fill=aes_fill), alpha = .4))
##print(p2 + geom_density( aes_string(fill='ano'),alpha=.4) + facet_wrap(aes_fill))

## CDF de x
##p4 = ggplot(datos, aes_string(x=x_data))+ggtitle(x_lab)+xlab(x_lab)+ylab("cdf")
##print(p4 + stat_ecdf(geom = "step", lwd=1))
##print(p4 + stat_ecdf(geom = "step", lwd=1,aes_string(col=aes_fill)))
##print(p4 + stat_ecdf(geom = "step", lwd=1,aes_string(col=aes_fill))+facet_wrap(~mes)) ##para cada mes

##BOXPLOT de x
##p5 = ggplot(datos, aes_string(y=x_data,x=aes_fill))+ggtitle(x_lab)+xlab(aes_fill)+ylab(x_lab)
##print(p5 + geom_boxplot(aes_string(col=aes_fill)))
##print(p5 + geom_boxplot(aes_string(col=aes_fill)) + geom_jitter(aes_string(col=aes_fill)))
##print(p5 + geom_boxplot(aes_string(col=aes_fill)) + coord_flip())

## histograma de y
#p2 = ggplot(datos, aes_string(x=y_data))+ggtitle(y_lab)+xlab(y_lab)
#(p2 + geom_density())
#print(p2 + geom_density( aes_string(y_data,fill=aes_fill), alpha = .4))
#print(p2 + geom_density( aes_string(fill='ano'),alpha=.4) + facet_wrap(aes_fill))

##CDF de y
#p4 = ggplot(datos, aes_string(x=y_data))+ggtitle(y_lab)+xlab(y_lab)+ylab("cdf")
#print(p4 + stat_ecdf(geom = "step", lwd=1))
#print(p4 + stat_ecdf(geom = "step", lwd=1,aes_string(col=aes_fill)))
#print(p4 + stat_ecdf(geom = "step", lwd=1,aes_string(col=aes_fill))+facet_wrap(~mes)) ##para cada mes

## BOXPLOT de y
#p5 = ggplot(datos, aes_string(y=y_data,x=aes_fill))+ggtitle(y_lab)+xlab(aes_fill)+ylab(y_lab)
#print(p5 + geom_boxplot(aes_string(col=aes_fill)))
#print(p5 + geom_boxplot(aes_string(col=aes_fill)) + geom_jitter(aes_string(col=aes_fill)))
#print(p5 + geom_boxplot(aes_string(col=aes_fill)) + coord_flip())

##SCATTER
p3 = ggplot(datos, aes_string(y=y_data,x=x_data)  )+ggtitle(titulo)+xlab(x_lab)+ylab(y_lab)
#print(p3 + geom_point( aes_string(color=aes_fill) ,alpha=.4))
#print(p3 + geom_point( aes_string(col='ano'),alpha=.4) + facet_wrap(aes_fill))
#print(p3 + geom_point( aes_string(color=aes_fill) ,alpha=.4)+facet_wrap(~mes)) ##para cada mes
##print(p3 + geom_point( aes_string(color=aes_fill) ,alpha=.4)+geom_smooth(aes_string(colour=aes_fill, group=aes_fill),se=TRUE,alpha=.4, method="gam", size=1))
p_reg <- if (MultipleRegresion) geom_smooth(method='lm',aes_string(color=aes_fill),se=T)
          else geom_smooth(method='lm',se=T)
p3 + geom_point( aes_string(color=aes_fill) ,alpha=.4)+p_reg
      #+geom_ribbon(stat='smooth',se=TRUE, method="gam",alpha=.2,aes_string(colour=aes_fill, group=aes_fill))
      #+geom_line(stat='smooth',method='gam',aes_string(colour=aes_fill, group=aes_fill))

#dev.off()
}



##FILTROS
##Categorias generacion
f.desc.ggplot(datos=datos,x_data='gen',y_data='gen',aes_fill='est_uge')
datos=datos[datos$est_uge!='DP',]
datos=datos[datos$est_uge!='DNP',]

## tipo dato caudal
f.desc.ggplot(datos=datos,x_data='gen',y_data='q',aes_fill='est_q')
datos=datos[datos$est_q!='MR',]

## por concentraciones
f.desc.ggplot(datos=datos,x_data='gen',y_data='c_mp',aes_fill='est_mp')
f.desc.ggplot(datos=datos,x_data='gen',y_data='c_so2',aes_fill='est_so2')
f.desc.ggplot(datos=datos,x_data='gen',y_data='c_nox',aes_fill='est_nox')
datos=datos[datos$c_mp<300,]
f.desc.ggplot(datos=datos,x_data='gen',y_data='c_o2',aes_fill='est_o2')

##Cantidad de datos filtrados
dim(bd_angamos)-dim(datos)

##Descripcion datos filtrados
f.descr(datos)


f.desc.ggplot(datos=datos,x_data='gen',y_data='c_nox',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='gen',y_data='c_so2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='gen',y_data='c_mp',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='gen',y_data='c_o2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='gen',y_data='c_co2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='gen',y_data='c_co',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='gen',y_data='q',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='gen',y_data='gen',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='gen',y_data='e_nox',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='gen',y_data='e_so2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='gen',y_data='e_mp',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='gen',y_data='e_co2',aes_fill='ano')
f.desc.ggplot(datos=datos,x_data='gen',y_data='e_mp',aes_fill='ano')
f.desc.ggplot(datos=datos,x_data='gen',y_data='e_nox',aes_fill='ano')
f.desc.ggplot(datos=datos,x_data='gen',y_data='e_so2',aes_fill='ano')
f.desc.ggplot(datos=datos,x_data='porc_s',y_data='e_so2',aes_fill='ano')
f.desc.ggplot(datos=datos,x_data='porc_s',y_data='e_so2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_s',y_data='c_so2',aes_fill='est_uge')

f.desc.ggplot(datos=datos,x_data='FR',y_data='c_so2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='FR',y_data='c_nox',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='FR',y_data='c_mp',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='FR',y_data='c_o2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='FR',y_data='c_co2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='FR',y_data='e_so2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='FR',y_data='e_nox',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='FR',y_data='e_mp',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='FR',y_data='e_co2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='FR',y_data='q',aes_fill='est_uge')

f.desc.ggplot(datos=datos,x_data='porc_s',y_data='c_so2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_s',y_data='c_nox',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_s',y_data='c_mp',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_s',y_data='c_o2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_s',y_data='c_co2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_s',y_data='e_so2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_s',y_data='e_nox',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_s',y_data='e_mp',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_s',y_data='e_co2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_s',y_data='q',aes_fill='est_uge')

f.desc.ggplot(datos=datos,x_data='pcs',y_data='c_so2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='pcs',y_data='c_nox',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='pcs',y_data='c_mp',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='pcs',y_data='c_o2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='pcs',y_data='c_co2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='pcs',y_data='e_so2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='pcs',y_data='e_nox',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='pcs',y_data='e_mp',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='pcs',y_data='e_co2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='pcs',y_data='q',aes_fill='est_uge')

f.desc.ggplot(datos=datos,x_data='porc_agua',y_data='c_so2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_agua',y_data='c_nox',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_agua',y_data='c_mp',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_agua',y_data='c_o2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_agua',y_data='c_co2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_agua',y_data='e_so2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_agua',y_data='e_nox',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_agua',y_data='e_mp',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_agua',y_data='e_co2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='porc_agua',y_data='q',aes_fill='est_uge')

f.desc.ggplot(datos=datos,x_data='c_mp',y_data='c_so2',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='c_so2',y_data='c_nox',aes_fill='est_uge')
f.desc.ggplot(datos=datos,x_data='c_nox',y_data='c_mp',aes_fill='est_uge')

##Time Series
datos$ano_mes=paste(datos$ano,"-",datos$mes,sep="")
datos$ano_mes=as.factor(datos$ano_mes)
meses <- unique(datos$ano_mes)
contaminates <- c("e_mp","e_so2","e_nox","e_co2","q","c_mp","c_so2","c_co2","c_o2","c_nox")

formato_tema <- theme(plot.title=element_text(size=28),axis.title.x=element_text(size = 24),axis.title.y=element_text(size=24),axis.text=element_text(size=24),legend.text=element_text(size=24),legend.title=element_text(size=24))
g_max<- max(datos[["gen"]])

for (c in contaminates){

filename = paste("pdf_TimeSerie\\TimeSerie.",c,".pdf",sep="")
pdf(filename,height=21.6,width = 27.9)

c_max <- max(datos[[c]])

  for (i in meses){
    p1 = ggplot(datos[datos$ano_mes==i,], aes_string(y=c,x="fecha.hora",group="est_uge")  )+ggtitle(i)+ylim(0,c_max)+formato_tema
    plot1 = p1+geom_point(aes_string(col='est_uge'))+geom_smooth(aes_string(group='mes'),method="loess", size=1,se=F,span=0.01)
    
    p2 = ggplot(datos[datos$ano_mes==i,], aes(y=gen,x=fecha.hora,group=est_uge)  ) + ylim(0,g_max) +formato_tema
    plot2 = p2+geom_point(aes_string(col='est_uge'))+geom_smooth(aes_string(group='mes'),method="loess", size=1,se=F,span=0.01)
    
    print(grid.arrange(plot1,plot2,nrow=2))
  }
dev.off()

}

##regresiones

lm_eqn2<- function(fit){
  eq<-substitute(italic(y)==a+b%.%italic(x),list(a=format(coef(fit)[1],digits=2),b=format(coef(fit)[2],digits=2)))
  as.character(as.expression(eq))
}

lm_eqn1<- function(fit){
  eq<-substitute(italic(y)==a%.%italic(x),list(a=format(coef(fit)[1],digits=2)))
  as.character(as.expression(eq))
}


##Q
## Si lo hago filtrando por estado UGE puedo tener los mismos resultados que hacer el geom_smooth
## ¿Que significa la significancia del intercepto? (sin c_o2 es más significativo)
## Temp_GS tambi?n es significativo pero solo aplica para 2017
fitq<-lm(q~gen+c_o2,datos[datos$est_uge=='RE',])
fitq<-lm(q~gen+c_o2,datos)
fitq<-lm(q~gen,datos)
summary(fitq)
summary(datosfiltradosFECO2$q)
pq = ggplot(datosfiltradosFECO2, aes(y=q,x=gen)  )+xlab("Potencia Operaci?n MW")+ylab("Q m3N/h")
pq + geom_point(aes(color=est_uge))+geom_smooth(method='lm')

f.desc.ggplot(datos,'q','gen',aes_fill = 'est_uge',MultipleRegresion = F)


##heat Rate parece depender principalmente de PCS hay variaciones por año pero no es significativa siempre
datosmesfiltrado=datosmes[datosmes$ano!='2013',]
#fitHR<-lm(heat_rate~c_o2,datosmes,weights = 1/sd_c_o2^2)
fitHR<-lm(heat_rate~pcs,datosmes)
summary(fitHR)

f.desc.ggplot(datosmes,'heat_rate','pcs',aes_fill = 'ano',MultipleRegresion = F)

pHR=ggplot(datosmes, aes(y=heat_rate,x=pcs))+xlab("PCS kcal/kg")+ylab("Heat Rate kcal/kWh")
pHR + geom_point(aes(color=ano))+geom_smooth(method='lm')


##PCS se explica por su composición, mientras más carbono fijo y materia volatil mayor PCS
fitpcs<-lm(pcs~porc_s,datosmes)
summary(fitpcs)
ppcs=ggplot(datosmes, aes(y=pcs,x=porc_s))+xlab("% Azufre")+ylab("PCS kcal/kg")
ppcs + geom_point(aes(color=ano))+geom_smooth(method='lm')

fitpcs<-lm(pcs~porc_cfijo+porc_mvolatil,datosmes)
summary(fitpcs)

datosmes <- f.factor.quantile(dataframe=datosmes,'porc_mvolatil',intervals = 5,percentil = F)

f.desc.ggplot(datosmes,'pcs','porc_cfijo',aes_fill='porc_mvolatil_tramos',MultipleRegresion = F)

ppcs=ggplot(datosmes, aes(y=pcs,x=porc_cfijo))+xlab("% Carbono fijo")+ylab("PCS kcal/kg")
ppcs + geom_point(aes(color=porc_mvolatil))+geom_smooth(method='lm')

##CO2
#Filtado por FE 
datos <- f.factor.quantile(dataframe=datos,'c_o2',intervals = 5,percentil = F)
datos <- f.factor.quantile(dataframe=datos,'porc_s',intervals = 5,percentil = F)
datos$pot=datos$gen
datos$feco2=datos$e_co2/datos$gen
datosfiltradosFECO2=datos[datos$est_o2=='DM',]
datosfiltradosFECO2=datosfiltradosFECO2[datosfiltradosFECO2$feco2>0.6,]
datosfiltradosFECO2=datosfiltradosFECO2[datosfiltradosFECO2$feco2<2,]

fitCO2<-lm(c_co2~gen+porc_cfijo+porc_mvolatil+c_o2,datosfiltradosFECO2[datosfiltradosFECO2$est_uge=="RE",])
summary(fitCO2)
pco2 = ggplot(datosfiltradosFECO2[datosfiltradosFECO2$est_uge=="RE",], aes(y=c_co2,x=gen)  )+xlab("Potencia Operacion[MW]")+ylab("Concentraci?n CO2 [%]")
pco2 = ggplot(datosfiltradosFECO2, aes(y=c_co2,x=gen)  )+xlab("Potencia Operacion[MW]")+ylab("Concentraci?n CO2 [%]")
pco2 + geom_point( aes(color=c_o2))+geom_smooth(method='lm')


f.desc.ggplot(datosfiltradosFECO2[datosfiltradosFECO2$est_uge=="RE",],'c_co2','gen',aes_fill ='c_o2_tramos' )

##NOx
datosfiltradosFECO2$fenox=datosfiltradosFECO2$e_nox/datosfiltradosFECO2$gen
pnox=ggplot(datosfiltradosFECO2, aes(y=fenox,x=gen)  )+xlab("gen MWh")+ylab("fe_NOx ton/Mwh")
pnox +geom_point( aes(color=est_uge))

fitnox<-lm(c_nox~pot,datos)
fitnox<-lm(c_nox~c_o2+FR+porc_s+porc_cfijo+porc_mvolatil+porc_agua,datosfiltradosFECO2[datosfiltradosFECO2$est_uge=="RE",])
summary(fitnox)

f.desc.ggplot(datosfiltradosFECO2[datosfiltradosFECO2$est_uge=='RE'],'pot','c_mp',aes_fill = 'ano')

pn = ggplot(datosfiltradosFECO2, aes(y=c_so2,x=porc_s)  )
pn + geom_point( aes(color=c_o2_tramos))+geom_smooth(method='lm')+facet_wrap(~est_uge)


pnox = ggplot(datosfiltradosFECO2, aes(y=c_nox,x=pot)  )+xlab("Potencia Operación [MW]")+ylab("conc. NOx [mg/m3N]")
pnox + geom_point( aes(color=est_uge))+geom_smooth(method='lm')+geom_hline(yintercept=500)


##SO2
pso2=ggplot(datosfiltradosFECO2,aes(y=c_so2,x=gen))
pso2+geom_point(aes(color=est_uge))
fitSO2<-lm(c_so2~porc_s+c_o2+porc_cfijo+FR,datos[datos$est_uge=='RE',])
summary(fitSO2)
psO2 = ggplot(datos, aes(y=c_so2,x=porc_s)  )+xlab("Contenido S %")+ylab("conc SO2 mg/m3N")
psO2 + geom_point( aes(color=est_uge))+geom_smooth(method='lm')+geom_hline(yintercept=400)


##MP
pmp=ggplot(datosfiltradosFECO2,aes(y=c_mp,x=gen))
pmp+geom_point(aes(color=est_uge))
fitMP<-lm(c_mp~gen+c_o2+porc_cfijo+pcs+porc_mvolatil,datosfiltradosFECO2[datosfiltradosFECO2$est_uge='RE'])
summary(fitMP)
pmp = ggplot(datosfiltradosFECO2[datosfiltradosFECO2$est_uge=='RE',], aes(y=c_mp,x=pot)  )+xlab("Potencia de Operación [MW]")+ylab("conc. MP [mg/m3N]")
pmp + geom_point( aes(color=ano))+geom_smooth(method='lm',aes(color=ano))+geom_hline(yintercept=50)

pmp = ggplot(datosfiltradosFECO2, aes(y=c_mp,x=pot)  )+xlab("Potencia de Operación [MW]")+ylab("conc. MP [mg/m3N]")
pmp + geom_point( aes(color=est_uge))+geom_smooth(method='lm')+geom_hline(yintercept=50)

##O2
po2=ggplot(datosfiltradosFECO2,aes(y=c_o2,x=gen))
po2+geom_point(aes(color=est_uge))


fito2<-lm(c_o2~gen+pcs,datosfiltradosFECO2[datosfiltradosFECO2$est_uge=='RE',])
summary(fito2)
po2 = ggplot(datosfiltradosFECO2, aes(y=c_o2,x=gen)  )+xlab("gen MWh")+ylab("O2 %")
po2 + geom_point( aes(color=est_uge))+geom_smooth(method='lm',aes(color=est_uge))
