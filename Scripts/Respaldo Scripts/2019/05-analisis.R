## Analisis de los datos
#PBH Nov 2018

library(skimr)
library(ggplot2)

# Resumen general
df %>% skim()



# Resumenes de tablas!

# Precio total por Dormitorio/Bano
df %>% filter(tipo!='s/i') %>%  group_by(tipo) %>% 
  summarise(count=n(),
            precio.p10=quantile(precio_MM,0.1,na.rm=T),
            precio.mean=mean(precio_MM,na.rm=T),
            precio.p90=quantile(precio_MM,0.9,na.rm=T),
            precioUF.p10=quantile(precio_uf,0.1,na.rm=T),
            precioUF.mean=mean(precio_uf,na.rm=T),
            precioUF.p90=quantile(precio_uf,0.9,na.rm=T))
.Last.value %>% write.table("clipboard", sep="\t",row.names = F)


# Precio por m2 por Dormitorio/Bano
df %>% filter(tipo!='s/i') %>%  group_by(tipo) %>% 
  summarise(count=n(),
            precio.p10=quantile(precio_m2tot,0.1,na.rm=T),
            precio.mean=mean(precio_m2tot,na.rm=T),
            precio.p90=quantile(precio_m2tot,0.9,na.rm=T),
            precioUF.p10=quantile(uf_m2tot,0.1,na.rm=T),
            precioUF.mean=mean(uf_m2tot,na.rm=T),
            precioUF.p90=quantile(uf_m2tot,0.9,na.rm=T))
.Last.value %>% write.table("clipboard", sep="\t",row.names = F)


# Superficie por Dormitorio/Bano
df %>% filter(tipo!='s/i') %>%  group_by(tipo) %>% 
  summarise(count=n(),
            sup.p10=quantile(sup,0.1,na.rm=T),
            sup.mean=mean(sup,na.rm=T),
            sup.p90=quantile(sup,0.9,na.rm=T),
            supTotal.p10=quantile(sup_total,0.1,na.rm=T),
            supTotal.mean=mean(sup_total,na.rm=T),
            supTotal.p90=quantile(sup_total,0.9,na.rm=T))
.Last.value %>% write.table("clipboard", sep="\t",row.names = F)




# GGPLOT

# Boxplot
df %>% filter(tipo!='s/i') %>% 
  ggplot(aes(x=tipo,y=precio_uf))+geom_boxplot()


# Scatter
df %>% filter(tipo!='s/i') %>% 
  ggplot(aes(y=precio_uf,x=sup,col=tipo))+geom_point(alpha=.5)

last_plot()+geom_smooth(se=F)

df %>% filter(tipo!='s/i') %>% 
  ggplot(aes(y=precio_uf,x=sup_total,col=tipo))+geom_jitter(alpha=.5)

# Solo 2D2B
df %>% filter(tipo=='2D2B') %>% 
  ggplot(aes(y=precio_uf,x=sup,col=tipo))+geom_point(alpha=.5)

df %>% filter(tipo=='2D2B',sup_total>49) %>% 
  ggplot(aes(y=precio_uf,x=sup_total,col=tipo))+geom_point(alpha=.5)


## Distibucion
df %>% filter(tipo!='s/i') %>% 
  ggplot(aes(x=precio_uf))+geom_density(aes(fill=tipo),alpha=.3)

df %>% filter(tipo!='s/i') %>% 
  ggplot(aes(x=precio_uf,col=tipo))+stat_ecdf()




# Otros
df %>% ggplot(aes(x=precio_MM))+geom_density()
df %>% ggplot(aes(x=precio_MM))+geom_density(aes(fill=dorm %>% as.factor()),alpha=.3)

df %>% ggplot(aes(x=uf_m2))+geom_density()
df %>% ggplot(aes(x=uf_m2))+geom_density(aes(fill=dorm %>% as.factor()),alpha=.3)

df  %>% ggplot(aes(x=fecha_publicacion,y=precio))+geom_point(aes(col=dorm %>% as.factor()))



mod1 <- lm(precio~sup+sup_total+dorm+banos,data=df)
mod1 %>% summary()

df %>% ggplot(aes(x=dorm %>% as.factor(),y=precio_MM))+geom_boxplot()


