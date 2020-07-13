## Analisis de los datos de Valdivia
#PBH Jul 2019

library(skimr)
library(ggplot2)


## VENTA --------------------

df <- df_orig %>% filter(venta_arriendo=='venta')

# Resumen general
df %>% skim()

# Resumenes de tablas!

# Precio total por Dormitorio/Bano
df %>% filter(tipo!='s/i') %>%  group_by(tipo_vivienda,tipo) %>% 
  summarise(count=n(),
            precio.p10=quantile(precio_MM,0.1,na.rm=T),
            precio.mean=mean(precio_MM,na.rm=T),
            precio.p90=quantile(precio_MM,0.9,na.rm=T),
            precioUF.p10=quantile(precio_uf,0.1,na.rm=T),
            precioUF.mean=mean(precio_uf,na.rm=T),
            precioUF.p90=quantile(precio_uf,0.9,na.rm=T))
.Last.value %>% write.table("clipboard", sep="\t",row.names = F)


# Precio por m2 por Dormitorio/Banoen UF
df %>% filter(tipo!='s/i') %>%  group_by(tipo_vivienda,tipo) %>% 
  summarise(count=n(),
            uf_m2.p10=quantile(uf_m2,0.1,na.rm=T),
            uf_m2.mean=mean(uf_m2,na.rm=T),
            uf_m2.p90=quantile(uf_m2,0.9,na.rm=T),
            uf_m2tot.p10=quantile(uf_m2tot,0.1,na.rm=T),
            uf_m2tot.mean=mean(uf_m2tot,na.rm=T),
            uf_m2tot.p90=quantile(uf_m2tot,0.9,na.rm=T))
.Last.value %>% write.table("clipboard", sep="\t",row.names = F)


# Superficie por Dormitorio/Bano
df %>% filter(tipo!='s/i') %>%  group_by(tipo_vivienda,tipo) %>% 
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
  ggplot(aes(x=tipo,y=precio_uf,fill=tipo_vivienda))+geom_boxplot()+
  coord_cartesian(ylim=c(0,15e3))


# Scatter
df_sin <- df %>% filter(tipo!='s/i',sup<500) %>%  select(-tipo_vivienda)

df %>% filter(tipo!='s/i',sup<500) %>% 
  ggplot(aes(y=precio_uf,x=sup,col=tipo))+
  geom_point(data=df_sin,aes(y=precio_uf,x=sup),col='grey')+
  geom_point(alpha=.5)+
  facet_wrap(~tipo_vivienda)

# Sup total
df %>% filter(tipo!='s/i',sup_total<10e3) %>% 
  ggplot(aes(y=precio_uf,x=sup_total,col=tipo))+
  geom_point(data=df_sin %>% filter(sup_total<10e3),aes(y=precio_uf,x=sup_total),col='grey')+
  geom_point(alpha=.5)+
  facet_wrap(~tipo_vivienda)


# Solo 2D2B
df %>% filter(tipo!='s/i',sup<500,tipo=='2D2B') %>% 
  ggplot(aes(y=precio_uf,x=sup,col=tipo))+
  geom_point(data=df_sin %>% filter(tipo=='2D2B'),aes(y=precio_uf,x=sup),col='grey')+
  geom_point(alpha=.5)+
  facet_wrap(~tipo_vivienda)


df %>% filter(tipo!='s/i',sup<500,tipo=='2D2B') %>% 
  ggplot(aes(y=precio_uf,x=sup_total,col=tipo))+
  geom_point(data=df_sin %>% filter(tipo=='2D2B'),aes(y=precio_uf,x=sup_total),col='grey')+
  geom_point(alpha=.5)+
  facet_wrap(~tipo_vivienda)


## Distibucion
df %>% filter(tipo!='s/i') %>% 
  ggplot(aes(x=precio_uf))+geom_density(aes(fill=tipo),alpha=.3)+
  facet_grid(~tipo_vivienda,scales='free')

df %>% filter(tipo!='s/i') %>% 
  ggplot(aes(x=precio_uf,col=tipo))+stat_ecdf()+
  facet_grid(~tipo_vivienda,scales='free')

## Arriendo --------------------

df <- df_orig %>% filter(venta_arriendo=='arriendo')

# Resumen general
df %>% skim()

# Resumenes de tablas!

# Precio total por Dormitorio/Bano
df %>% filter(tipo!='s/i') %>%  group_by(tipo_vivienda,tipo) %>% 
  summarise(count=n(),
            precio.p10=quantile(precio_mil,0.1,na.rm=T),
            precio.mean=mean(precio_mil,na.rm=T),
            precio.p90=quantile(precio_mil,0.9,na.rm=T),
            precioUF.p10=quantile(precio_uf,0.1,na.rm=T),
            precioUF.mean=mean(precio_uf,na.rm=T),
            precioUF.p90=quantile(precio_uf,0.9,na.rm=T))
.Last.value %>% write.table("clipboard", sep="\t",row.names = F)


# Precio por m2 por Dormitorio/Banoen UF
df %>% filter(tipo!='s/i') %>%  group_by(tipo_vivienda,tipo) %>% 
  summarise(count=n(),
            precio_m2.p10=quantile(precio_m2,0.1,na.rm=T),
            precio_m2.mean=mean(precio_m2,na.rm=T),
            precio_m2.p90=quantile(precio_m2,0.9,na.rm=T),
            precio_m2tot.p10=quantile(precio_m2tot,0.1,na.rm=T),
            precio_m2tot.mean=mean(precio_m2tot,na.rm=T),
            precio_m2tot.p90=quantile(precio_m2tot,0.9,na.rm=T))
.Last.value %>% write.table("clipboard", sep="\t",row.names = F)


# Superficie por Dormitorio/Bano
df %>% filter(tipo!='s/i') %>%  group_by(tipo_vivienda,tipo) %>% 
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
  ggplot(aes(x=tipo,y=precio_mil,fill=tipo_vivienda))+geom_boxplot()


# Scatter
df_sin <- df %>% filter(tipo!='s/i') %>%  select(-tipo_vivienda)

df %>% filter(tipo!='s/i') %>% 
  ggplot(aes(y=precio_mil,x=sup,col=tipo))+
  geom_point(data=df_sin,aes(y=precio_mil,x=sup),col='grey')+
  geom_point(alpha=.5)+
  facet_wrap(~tipo_vivienda)

# Sup total
df %>% filter(tipo!='s/i') %>% 
  ggplot(aes(y=precio_mil,x=sup_total,col=tipo))+
  geom_point(data=df_sin,aes(y=precio_mil,x=sup_total),col='grey')+
  geom_point(alpha=.5)+
  facet_wrap(~tipo_vivienda)


# Solo 2D2B
df %>% filter(tipo!='s/i',tipo=='2D2B') %>% 
  ggplot(aes(y=precio_mil,x=sup,col=tipo))+
  geom_point(data=df_sin %>% filter(tipo=='2D2B'),aes(y=precio_mil,x=sup),col='grey')+
  geom_point(alpha=.5)+
  facet_wrap(~tipo_vivienda)


df %>% filter(tipo!='s/i',tipo=='2D2B') %>% 
  ggplot(aes(y=precio_mil,x=sup_total,col=tipo))+
  geom_point(data=df_sin %>% filter(tipo=='2D2B'),aes(y=precio_mil,x=sup_total),col='grey')+
  geom_point(alpha=.5)+
  facet_wrap(~tipo_vivienda)


## Distibucion
df %>% filter(tipo!='s/i') %>% 
  ggplot(aes(x=precio_mil))+geom_density(aes(fill=tipo),alpha=.3)+
  facet_grid(~tipo_vivienda,scales='free')

df %>% filter(tipo!='s/i') %>% 
  ggplot(aes(x=precio_mil,col=tipo))+stat_ecdf()+
  facet_grid(~tipo_vivienda,scales='free')

