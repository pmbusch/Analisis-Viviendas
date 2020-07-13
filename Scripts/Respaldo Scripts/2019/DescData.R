## descripcion datos 




##FILTRO BUSQUEDA DEPTO ENERO 2018
datos<- datos[datos$precio>100&datos$precio<1000,] ##OUTLIERS
##1D1B sin amoblar
datos<- datos[datos$dormitorios==1&datos$banos==1&datos$amoblado==0,]
summary(datos$precio)

titulo_especifico <- "1D1B"

{
pdf(paste('Figuras ',tipo_precio,titulo_especifico,".pdf",sep=''))

##DEFINICION VARIABLES
y_data_array=c('precio','precio_m2')
x_data='m2'
aes_col='bar'
aes_cat='dormitorios'
datos[[aes_cat]] <- as.factor(datos[[aes_cat]]) ##CONVERTIMOS EN FACTOR

for (y_data in y_data_array){

##X vs Y
p0 <- ggplot(data=datos,aes_string(y=y_data,x=x_data))
p1 <- p0 + geom_point(aes_string(col=aes_col))
print(p1+geom_smooth(aes_string(col=aes_col),se=F,alpha=.4,method = 'loess'))
p2 <- p0 + geom_point(aes_string(col=aes_cat))
p2 <- p2+geom_smooth(aes_string(col=aes_cat),se=F,alpha=.4,method = 'lm')
print(p2+facet_wrap(~bar))

for (b in unique(datos[[aes_col]])){
  datos_fil <- datos[datos[aes_col]==b,]
  p0 <- ggplot(data=datos_fil,aes_string(y=y_data,x=x_data))+ggtitle(b)
  p2 <- p0 + geom_point(aes_string(col=aes_cat))
  p2 <- p2+geom_smooth(aes_string(col=aes_cat),se=F,alpha=.4,method = 'lm')
  print(p2)
}


##BOXPLOT
box <- ggplot(data=datos,aes_string(x=aes_col,y=y_data))
print(box+geom_boxplot(aes_string(col=aes_col)))

box1 <- ggplot(data=datos[datos$precio_m2<5,],aes_string(x=aes_col,y=y_data))
print(box1+geom_boxplot(aes_string(col=aes_col)))

for (b in unique(datos$banos)){
  datos_fil <- datos[datos[aes_cat]==b,]
  box1 <- ggplot(data=datos_fil,aes_string(x=aes_col,y=y_data))+ggtitle(paste('N ', aes_cat,': ',b,sep=''))
  print(box1+geom_boxplot(aes_string(col=aes_col)))
}

}
dev.off()
}
