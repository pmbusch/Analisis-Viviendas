## Analisis de los datos
#PBH Nov 2018


# Resumen general -------------------
df %>% names()
# skim_format(numeric = list(digits = 0))
summ_df <- df %>% select(barrio,tipo, fecha_consulta, 
                         fecha_publicacion, banos, banos_factor, dorm,
                         dorm_factor,
                         precio, precio_uf, precio_MM, precio_m2tot,
                         uf_m2tot) %>% skim()
summ_df %>% names()


# Summary numeric
summ_num <- summ_df %>% filter(skim_type=="numeric") %>% 
  select(skim_variable, numeric.mean, numeric.sd, numeric.p0,
         numeric.p25, numeric.p50, numeric.p75, numeric.p100)

# Summary factor
summ_fac <- summ_df %>% filter(skim_type=="factor") %>% 
  select(skim_variable, factor.ordered, factor.n_unique, factor.top_counts) 


# Resumenes de tablas ------------------

# Precio total por Dormitorio/Bano
tabla_precioUF <- df %>%  group_by(tipo) %>% 
  summarise(n=n(),
            P5=quantile(precio_uf,0.05,na.rm=T),
            Promedio=mean(precio_uf,na.rm=T),
            P95=quantile(precio_uf,0.95,na.rm=T))

tabla_precio <- df %>%  group_by(tipo) %>% 
  summarise(n=n(),
            P5=quantile(precio_MM,0.05,na.rm=T),
            Promedio=mean(precio_MM,na.rm=T),
            P95=quantile(precio_MM,0.95,na.rm=T))


# Superficie por Dormitorio/Bano
tabla_sup <- df %>%  group_by(tipo) %>%
  summarise(n=n(),
            P5=quantile(sup_total,0.05,na.rm=T),
            Promedio=mean(sup_total,na.rm=T),
            P95=quantile(sup_total,0.95,na.rm=T))

# Precio por m2 por Dormitorio/Bano
tabla_preciosup <- df %>%  group_by(tipo) %>%
  summarise(n=n(),
            P5=quantile(uf_m2tot,0.05,na.rm=T),
            Promedio=mean(uf_m2tot,na.rm=T),
            P95=quantile(uf_m2tot,0.95,na.rm=T))

## GGPLOT -------------------
theme_set(theme_bw())

# Boxplot
boxplot <- df %>%
  ggplot(aes(x=tipo,y=precio_uf, fill=tipo_vivienda))+geom_boxplot(alpha=.5)+
  labs(x = "", y = "Precio [UF]", fill = "Tipo Vivienda")+
  scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()

 
## Distibucion
densidad <- df %>%
  ggplot(aes(x=precio_uf, y=..count.., fill=tipo))+geom_density_line(alpha=.3)+
  labs(y = "", x = "Precio [UF]", fill = "Tipo")+
  facet_grid(tipo~tipo_vivienda)+
  scale_x_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_y_continuous(labels=function(x) format(x,digits=2, decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()+theme(legend.position ="none")

# ecdf <- df %>%
#   ggplot(aes(x=precio_uf,col=tipo, y=..count..))+stat_ecdf()+
#   facet_wrap(tipo_vivienda~.)+
#   labs(y = "Densidad Acumulada", x = "Precio [UF]", col = "Tipo")+
#   theme(legend.title = element_blank())+
#   scale_x_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
#   scale_y_continuous(labels=function(x) format(x,digits=2, decimal.mark = ".", scientific = F))+
#   scale_color_viridis_d()

len <- df %>% group_by(tipo) %>% summarise(len=n())
df <- df %>% left_join(len, by="tipo")
rm(len)

ecdf <- df %>%
  ggplot(aes(x=precio_uf,col=tipo))+geom_step(aes(len=len, y=..y..*len), stat="ecdf")+
  facet_wrap(tipo_vivienda~.)+
  labs(y = "Total Viviendas", x = "Precio [UF]", col = "Tipo")+
  theme(legend.title = element_blank())+
  scale_x_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_y_continuous(labels=function(x) format(x,digits=2, decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()


# jitter
jitter <- df %>%
  ggplot(aes(x=tipo,y=precio_uf, col=tipo))+geom_jitter(alpha=.5) + 
  theme(legend.position = "none")+
  labs(x = "Tipo", y = "Precio [UF]")+facet_wrap(tipo_vivienda~.)+
  scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()

## Scatter
scatter <- df %>%
  ggplot(aes(y=precio_uf,x=sup_total,col=tipo))+geom_point(alpha=.5)+
  facet_wrap(tipo_vivienda~.)+
  labs(x = expression(paste("Superficie Total [", m^2, "]")), y = "Precio [UF]")+
  theme(legend.title = element_blank())+
  scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()

last_plot()+geom_smooth(se=F)


# Solo Vivienda de interes
scatter_interes <- df %>% filter(tipo==tipo_interes) %>%
  ggplot(aes(y=precio_uf,x=sup_total,col=tipo))+geom_point(alpha=.5)+
  facet_wrap(tipo_vivienda~.)+
  labs(x = expression(paste("Superficie Total [", m^2, "]")), y = "Precio [UF]")+
  theme(legend.title = element_blank())+
  scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()


scatter_dorm_bano <- df %>%
  ggplot(aes(y=banos,x=dorm,col=precio_uf, size=sup_total))+geom_jitter(alpha=.5)+
  facet_wrap(tipo_vivienda~.)+
  labs(x = "N° Dormitorios", y = "N° Baños", col = "Precio UF", 
       size = expression(paste("Sup Total ", m^2)))+
  scale_x_continuous(breaks = 1:max(df$dorm))+
  scale_y_continuous(breaks = 1:max(df$banos))+
  scale_color_viridis_c(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_size_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))


## Analisis por precio por metro cuadrado

scatter_precio <- df %>%
  ggplot(aes(y=uf_m2tot,x=sup_total,col=tipo, size=-uf_m2tot))+
  geom_point(alpha=.5)+
  facet_wrap(tipo_vivienda~.)+
  labs(x = "Superficie Total", y = expression(paste("UF/", m^2)))+
  theme(legend.title = element_blank())+
  scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()+
  scale_size(guide="none")


scatter_precio_interes <- df %>% filter(tipo==tipo_interes) %>%
  ggplot(aes(y=uf_m2tot,x=sup_total,col=tipo, size=-uf_m2tot))+geom_point(alpha=.5)+
  facet_wrap(tipo_vivienda~.)+
  labs(x = "Superficie Total", y = expression(paste("UF/", m^2)))+
  theme(legend.title = element_blank())+
  scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()+
  scale_size(guide="none")


boxplot_precio <- df %>%
  ggplot(aes(x=tipo,y=uf_m2tot, fill=tipo_vivienda))+geom_boxplot(alpha=.5)+
  labs(x = "Tipo", y = expression(paste("UF/", m^2)), fill = "Tipo Vivienda")+
  scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()

# Otros
df$barrio_portal %>% unique()
