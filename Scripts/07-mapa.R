## Generacion de Mapas
#PBH Jun 2020

# # Opcion 1 Usar rangos maximos y minimos -----------
# m <- df %>%
#   leaflet() %>%
#   fitBounds(lng1 = min(df$longitud),
#           lat1 = min(df$latitud),
#           lng2 = max(df$longitud),
#           lat2 = max(df$latitud)) %>%
#   addTiles() %>%
#   addCircleMarkers(~longitud, ~latitud, color = "red", weight = 1, radius = 3)
# mapshot(m, file="Mapa/Mapa1.png")

# # Opcion 2 Usar promedio y ajustar zoom -----------
# long <- df$longitud %>% mean()
# ltd <- df$latitud %>% mean()
# 
# m <- leaflet(df) %>% 
#   setView(lng = long, lat = ltd, zoom = 15) %>% 
#   addTiles() %>%  
#   addCircleMarkers(~longitud, ~latitud, color = "red", weight = 1, radius = 3)
# mapshot(m, file="Mapa/Mapa2.png")

# Opcion 3 ggmap -----------

# Filtro outliers para generar el mapa
## Fuente: https://ropensci.github.io/CoordinateCleaner/articles/Tutorial_geographic_outliers.html
df_clean <- df %>% filter(!is.na(latitud))
df_mapa <- CoordinateCleaner::cc_outl(df_clean, lon="longitud", lat="latitud", species = "barrio", method = "quantile")
rm(df_clean)
# location <- c(min(df_mapa$longitud), min(df_mapa$latitud), 
#               max(df_mapa$longitud), max(df_mapa$latitud))

location <- make_bbox(df_mapa$longitud,df_mapa$latitud, f=0.15)


map <- get_map(location=location, source = "stame", maptype = "terrain", crop=T)

m <- ggmap(map)+geom_point(data=df, aes(x=longitud, y=latitud, col=tipo), alpha=.5)+
  scale_color_viridis_d()+
  theme(legend.title=element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+xlab('')+ylab('')
  
ggsave(m, file="Mapa/Mapa.png", dpi = 300)


# 15 Mejores opciones segun precio por area
df_15 <- df %>% arrange(uf_m2tot) %>% head(15)

m <- ggmap(map)+geom_point(data=df_15, aes(x=longitud, y=latitud, col=tipo))+
  scale_color_viridis_d()+
  theme(legend.title=element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+xlab('')+ylab('')

ggsave(m, file="Mapa/Mapa_15.png", dpi = 300)


# 15 Mejores opciones 2D2B (o interes) segun precio por area
df_15 <- df %>% filter(tipo==tipo_interes) %>% arrange(uf_m2tot) %>% head(15)

m <- ggmap(map)+geom_point(data=df_15, aes(x=longitud, y=latitud, col=tipo))+
  scale_color_viridis_d()+
  theme(legend.title=element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+xlab('')+ylab('')

ggsave(m, file="Mapa/Mapa_15_interes.png", dpi = 300)

# EoF