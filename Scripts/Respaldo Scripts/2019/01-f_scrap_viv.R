## Funcion para extrer toda la infor necesaria de una sola vivienda, accediendo a su pagina
# PBH Nov 2018

library(rvest)
library(stringr)
library(readr)

f.scrap.viv <- function(url,barrio,venta_arriendo,tipo_vivienda){
  # Descarga pagina
  web <- read_html(url)
  
  #Obtiene atributos
  
  nombre <- html_node(web,'#divImagenes > div.media-block-meta > h4') %>% 
    html_text() %>% str_replace_all('\r','') %>% str_trim(side='both')
  
  precio <- html_node(web,'#divImagenes > div.media-block-meta > div > p.price') %>% 
    html_text() %>% str_replace_all("\\.", "") %>% str_replace_all('\\$', "") %>%
    str_trim(side="both") %>% as.numeric()
  
  precio_uf <- html_node(web,'#divImagenes > div.media-block-meta > div > p.price-ref') %>%
    html_text() %>% str_replace_all("\\.", "") %>% str_replace_all('UF ', "") %>%
    str_replace_all(',','.') %>% str_trim(side="both") %>% as.numeric()
  
  codigo <- html_node(web,'#wrapper > section > div > div > div.col-sm-9.span-fix-content > article > div > div.propiedad-ficha.clearfix > div.content-left-col > div.propiedad-ficha-mini > div.content-panel.small-content-panel > p:nth-child(1) > strong') %>% 
    html_text() %>% str_replace_all('Código: ','') 
  
  fecha_publicacion <- html_node(web,'#wrapper > section > div > div > div.col-sm-9.span-fix-content > article > div > div.propiedad-ficha.clearfix > div.content-left-col > div.propiedad-ficha-mini > div.content-panel.small-content-panel > p:nth-child(2) > strong') %>% 
    html_text() %>% str_replace_all('Publicada: ','') %>% strptime('%d-%m-%Y') %>% as.POSIXct()
  
  dorm <- html_node(web,'#wrapper > section > div > div > div.col-sm-9.span-fix-content > article > div > div.propiedad-ficha.clearfix > div.content-right-col > div.property-data-sheet.clearfix > div.data-sheet-column.data-sheet-column-programm > p') %>% 
    html_text() %>% f.split.n('&nbsp',1) %>% as.numeric()
  
  banos <- html_node(web,'#wrapper > section > div > div > div.col-sm-9.span-fix-content > article > div > div.propiedad-ficha.clearfix > div.content-right-col > div.property-data-sheet.clearfix > div.data-sheet-column.data-sheet-column-programm > p') %>% 
    html_text() %>% f.split.n('&nbsp',2) %>% parse_number()
  
  #Texto separador superficies
  sep_sup <- ifelse(tipo_vivienda=='departamento','til','uida')
  
  sup <- html_node(web,'#wrapper > section > div > div > div.col-sm-9.span-fix-content > article > div > div.propiedad-ficha.clearfix > div.content-right-col > div.property-data-sheet.clearfix > div.data-sheet-column.data-sheet-column-area > p') %>% 
    html_text() %>% f.split.n(sep_sup,1) %>% parse_number()
  
  sup_total <- html_node(web,'#wrapper > section > div > div > div.col-sm-9.span-fix-content > article > div > div.propiedad-ficha.clearfix > div.content-right-col > div.property-data-sheet.clearfix > div.data-sheet-column.data-sheet-column-area > p') %>% 
    html_text() %>% f.split.n(sep_sup,2) %>% str_remove_all('[.]') %>% parse_number()
  
  descripcion <- html_node(web,'#wrapper > section > div > div > div.col-sm-9.span-fix-content > article > div > div.propiedad-ficha.clearfix > div.content-right-col > div.row > div > div') %>% 
    html_text()

  equipamiento <- html_node(web,'#wrapper > section > div > div > div.col-sm-9.span-fix-content > article > div > div.propiedad-ficha.clearfix > div.content-left-col > div.propiedad-ficha-mini > div.row > div > div > p') %>% 
    html_text()
  
  vende <- html_node(web,'#wrapper > section > div > div > div.col-sm-9.span-fix-content > article > div > div.propiedad-ficha.clearfix > div.content-left-col > div.content-panel > p') %>% 
    html_text()
  
  # Retorna un dataframe
  return(data.frame(barrio=barrio,
                    dir_url=url,
                    nombre=nombre,
                    precio=precio,
                    precio_uf=precio_uf,
                    codigo=codigo,
                    fecha_publicacion=fecha_publicacion,
                    dorm=dorm,
                    banos=banos,
                    sup=sup,
                    sup_total=sup_total,
                    descripcion=descripcion,
                    equipamiento=equipamiento,
                    vende=vende,
                    fecha_consulta=Sys.Date(),
                    venta_arriendo=venta_arriendo,
                    tipo_vivienda=tipo_vivienda))
}
