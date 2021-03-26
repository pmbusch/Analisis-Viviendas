## Script para imprimir reportes
## PBH Jul 2020

# Imprimir reporte individual
rmarkdown::render("Reporte_Viviendas.Rmd",
                  params = list(viv = "departamento",
                                barrio="PlazaÑuñoa",
                                ven="venta"),
                  output_file = paste("Reportes/Reporte", 
                                      "PlazaÑuñoa","departamento",
                                      "26Mar2021", sep="_"))



# Valores que se usarán como "parámetros" (variables) del reporte
barrios <- c("PlazaÑuñoa", "Valdivia")
viviendas <- c("casa", "departamento")

# Alonso
barrios <- c("PlazaÑuñoa","Lastarria","Providencia")
viviendas <- c("departamento")

# Definir función para generación de reportes
for (b in barrios){
  for (v in viviendas){
    rmarkdown::render("Reporte_Viviendas.Rmd",
                      params = list(viv = v,
                                    barrio=b,
                                    ven="venta"),
                      output_file = paste("Reportes/Reporte", b,v,
                                          "26Mar2021", sep="_"))
  }
}


# Imprimir reporte Terrenos
rmarkdown::render("Reporte_Terrenos.Rmd",
                  output_file = paste("Reportes/Reporte", "Aysen", sep="_"))


# Ggplot Aldini
cols_type <- "cccdddDddddddccddDccc"
file_name <- "Data/DataReporte - ValdiviaMar2021.csv"
file_name <- "Data/DataReporte - Alonso Marzo2021.csv"
df <- read_delim(file_name, delim = ";", skip = 1, na = c("NA"),
                 col_types = cols_type)
rm(cols_type)



p1 <- df %>% 
  filter(venta_arriendo=="venta",
         banos<4, dorm<4) %>% 
  mutate(tipo_vivienda=factor(tipo_vivienda),
         precio_millones=precio/1e6,
         Dorms_Banos=paste0(dorm,"D-",banos,"B"),
         codigo=as.character(codigo),
         barrio_nuevo=barrio %>% 
           str_remove_all("santiago|metropolitana") %>% 
           str_remove_all("--|---") %>% str_replace("-"," ")) %>% 
  ggplot(aes(sup,precio_millones,
             # col=tipo_vivienda,
             col=barrio_nuevo,
             label=Dorms_Banos,text=nombre))+
  facet_wrap(~barrio_nuevo)+
  geom_point(alpha=.5)+
  xlab("Superficie Interior [metros cuadrados]")+
  ylab("$ Precio en millones ")+
  scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  theme_bw(20)+
  ylim(0,300)+xlim(0,250)+
  theme(legend.title = element_blank(), legend.position = "none") 
p1

library(plotly)
p <- ggplotly(p1)

htmlwidgets::saveWidget(as_widget(p), "ViviendasAlonso.html")
# EoF