## Feature data. Agregar variables para analisis
## Analisis
## Mapa
### Script especial para los terrenos
## Adaptado de los scripts originales de feat_data, analisis y mapa
# PBH Jul 2020

## FEAT DATA --------------

df <- df %>% tibble()
df %>% nrow()
df %>% skim()


# Autocompletar con precio UF
#No todas tienen el valor en UF, completamos con el valor promedio
df %>% filter(!is.na(precio_uf)) %>% 
  mutate(UF=precio/precio_uf) %>% pull(UF) %>% skim()
uf_prom <- df %>% filter(!is.na(precio_uf)) %>% 
  mutate(UF=precio/precio_uf) %>% pull(UF) %>% mean()
# mismo valor en aprox 28 mil
df <- df %>% mutate(precio_uf=ifelse(is.na(precio_uf),precio/uf_prom,precio_uf))
rm(uf_prom)

## Superficie HA (extraccion de estos datos)
df <- df %>% mutate(metros_cuad=descripcion %>% str_remove_all(" ") %>% 
                      str_extract("\\d[.,]*\\d*m2") %>% 
                      str_remove_all("m2|\\.") %>% 
                      str_replace_all(",",".") %>% 
                      as.numeric(),
                    ha=descripcion %>% str_remove_all(" ") %>% 
                      str_extract("\\d*[.,]*\\d*[Hh]ect.{0,5}reas") %>% 
                      str_remove_all("[Hh]ect.{0,5}reas|\\.") %>% 
                      str_replace_all(",",".") %>% 
                      as.numeric())
df$metros_cuad %>% unique()
df$ha %>% unique()

df <- df %>% mutate(sup_total=case_when(
  !is.na(sup_total) ~ sup_total,
  !is.na(metros_cuad) ~ metros_cuad,
  !is.na(ha) ~ ha*10000,
  T ~ 0
))

df <- df %>% mutate(ha=sup_total/10000)

## Remuevo filas con superficie no identificada
df <- df %>% filter(ha>1 & precio_uf>0)

# Nuevas variables
df <- df %>% mutate(uf_ha=precio_uf/ha)

# Barrio
df <- df %>% mutate(barrio=str_remove(barrio,"-aysen"))

# Factores
df <- df %>% mutate(barrio_portal=barrio_portal %>% as.factor(),
                    barrio=barrio %>% as.factor(),
                    tipo_vivienda=tipo_vivienda %>% as.factor())





## ANALISIS --------------
## Tablas --------

df %>% names()

# Precio total 
tabla_precio <- df %>%  group_by(barrio) %>% 
  summarise(n=n(),
            Minimo=min(precio_uf,na.rm=T),
            Promedio=mean(precio_uf,na.rm=T),
            Maximo=max(precio_uf,na.rm=T))


# Superficie en ha
tabla_sup <- df %>%  group_by(barrio) %>%
  summarise(n=n(),
            Minimo=min(ha,na.rm=T),
            Promedio=mean(ha,na.rm=T),
            Maximo=max(ha,na.rm=T))

# Precio en UF por ha
tabla_preciosup <- df %>%  group_by(barrio) %>%
  summarise(n=n(),
            Minimo=min(uf_ha,na.rm=T),
            Promedio=mean(uf_ha,na.rm=T),
            Maximo=max(uf_ha,na.rm=T))

## Graficos --------
theme_set(theme_bw())

# Boxplot
boxplot <- df %>%
  ggplot(aes(x=barrio,y=precio_uf, fill=tipo_vivienda))+geom_boxplot(alpha=.5)+
  labs(x = "", y = "Precio [UF]", fill = "Tipo Terreno")+
  scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()
# boxplot

## Distibucion
# densidad <- df %>%
#   ggplot(aes(x=precio_uf,y=..count.., fill=barrio))+geom_density_line(alpha=.3)+
#   labs(y = "", x = "Precio [UF]", fill = "Tipo")+
#   facet_grid(barrio~tipo_vivienda, scales = "free")+
#   scale_x_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
#   scale_y_continuous(labels=function(x) format(x,digits=2, decimal.mark = ".", scientific = F))+
#   scale_color_viridis_d()+theme(legend.position ="none")
# densidad


# ecdf <- df %>%
#   ggplot(aes(x=precio_uf,col=tipo, y=..count..))+stat_ecdf()+
#   facet_wrap(tipo_vivienda~.)+
#   labs(y = "Densidad Acumulada", x = "Precio [UF]", col = "Tipo")+
#   theme(legend.title = element_blank())+
#   scale_x_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
#   scale_y_continuous(labels=function(x) format(x,digits=2, decimal.mark = ".", scientific = F))+
#   scale_color_viridis_d()

len <- df %>% group_by(barrio, tipo_vivienda) %>% summarise(len=n())
df <- df %>% left_join(len, by=c("barrio","tipo_vivienda"))
rm(len)

ecdf <- df %>%
  ggplot(aes(x=precio_uf,col=barrio))+geom_step(aes(len=len, y=..y..*len), stat="ecdf")+
  facet_wrap(tipo_vivienda~.)+
  labs(y = "Total Terrenos", x = "Precio [UF]", col = "Sector")+
  theme(legend.title = element_blank())+
  scale_x_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_y_continuous(labels=function(x) format(x,digits=2, decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()
# ecdf

# jitter
jitter <- df %>%
  ggplot(aes(x=barrio,y=precio_uf, col=barrio))+geom_jitter(alpha=.5) + 
  theme(legend.position = "none")+
  labs(x = "Sector", y = "Precio [UF]")+
  scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()


## Scatter
scatter <- df %>%
  ggplot(aes(y=precio_uf,x=ha,col=barrio))+geom_point(alpha=.5)+
  labs(x = "Superficie Total [ha]", y = "Precio [UF]")+
  theme(legend.title = element_blank())+
  scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()


# Solo terrenos bajo 10K uf
scatter_interes <- df %>% filter(precio_uf<10000) %>%
  ggplot(aes(y=precio_uf,x=ha,col=barrio))+geom_point(alpha=.5)+
  labs(x = "Superficie Total [ha]", y = "Precio [UF]")+
  theme(legend.title = element_blank())+
  scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()


## Analisis por precio/ha

scatter_precio <- df %>%
  ggplot(aes(y=uf_ha,x=ha,col=barrio, size=-uf_ha))+
  geom_point(alpha=.5)+
  labs(x = "Superficie Total [ha]", y = "UF/ha", 
       caption = "Notar que ambos ejes estan en escala logaritmica en base 10")+
  theme(legend.title = element_blank())+
  scale_y_log10(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_x_log10(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()+
  scale_size(guide="none")
# scatter_precio

scatter_precio_interes <- df %>% filter(precio_uf<10000) %>% 
  ggplot(aes(y=uf_ha,x=ha,col=barrio, size=-uf_ha))+
  geom_point(alpha=.5)+
  labs(x = "Superficie Total [ha]", y = "UF/ha", 
       caption = "Notar que ambos ejes estan en escala logaritmica en base 10")+
  theme(legend.title = element_blank())+
  scale_y_log10(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_x_log10(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()+
  scale_size(guide="none")
# scatter_precio_interes


boxplot_precio <- df %>%
  ggplot(aes(x=barrio,y=uf_ha, fill=tipo_vivienda))+geom_boxplot(alpha=.5)+
  labs(x = "Sector", y = "UF/ha", fill = "Tipo Terreno",
       caption = "Notar que eje Y esta en escala logaritmica en base 10")+
  scale_y_log10(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()

## MAPA --------
df_mapa <- df
location <- make_bbox(df_mapa$longitud,df_mapa$latitud, f=0.15)
map <- get_map(location=location, source = "stame", maptype = "terrain", crop=T)

m <- ggmap(map)+geom_point(data=df, aes(x=longitud, y=latitud, col=barrio), alpha=.5)+
  scale_color_viridis_d()+
  theme(legend.title=element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+xlab('')+ylab('')

# 15 Mejores opciones segun precio por area
df_15 <- df %>% arrange(uf_ha) %>% head(15)

m_15 <- ggmap(map)+geom_point(data=df_15, aes(x=longitud, y=latitud, col=barrio))+
  scale_color_viridis_d()+
  theme(legend.title=element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+xlab('')+ylab('')

## EOF