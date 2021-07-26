## Funcion para extrer toda la infor necesaria de una sola vivienda, accediendo a su pagina
# PBH Nov 2018
# Actualizado PBH Jun 2020

library(rvest)
library(stringr)
library(readr)

f.scrap.viv <- function(url,barrio,venta_arriendo,tipo_vivienda){
  
  # Descarga pagina y despues la lee (para evitar errores de proxy)
  
  # download.file(url, destfile = "prueba.html", quiet=TRUE)
  web <- read_html(url)
  
  #Obtiene atributos
  nombre <-  html_node(web, '#root-app > div > div.ui-pdp-container.ui-pdp-container--pdp > div > div.ui-pdp-container__col.col-1.ui-pdp-container--column-right.mt-20.mr-20 > div:nth-child(1) > div > div.ui-pdp-container__row.ui-pdp-container__row--header > div > div.ui-pdp-header__title-container > h1') %>% 
    html_text() %>% str_replace_all('\r','') %>% str_trim(side='both')

  # Break
  if (is.na(nombre)){
    break
  }
  
  str_detect(nombre,"Proyecto|proyecto")
  
  # Puede que el precio principal no este en UF
  tag_precio <- html_node(web, '#root-app > div > div.ui-pdp-container.ui-pdp-container--pdp > div > div.ui-pdp-container__col.col-1.ui-pdp-container--column-right.mt-20.mr-20 > div:nth-child(1) > div > div.ui-pdp-container__row.ui-pdp-container__row--price > div > div.ui-pdp-price__second-line > span > span.price-tag-text-sr-only') %>% 
    html_text()
  
  if (str_detect(tag_precio,"undefined|UF")){
    precio <- html_node(web,'#root-app > div > div.ui-pdp-container.ui-pdp-container--pdp > div > div.ui-pdp-container__col.col-1.ui-pdp-container--column-right.mt-20.mr-20 > div:nth-child(1) > div > div.ui-pdp-container__row.ui-pdp-container__row--price > div > div.ui-pdp-price__subtitles > p > span > span.price-tag-amount > span.price-tag-fraction') %>% 
      html_text() %>% str_replace_all("\\.", "") %>% str_replace_all('\\$', "") %>%
      str_trim(side="both") %>% as.numeric()
  
    precio_uf <- html_node(web,'#root-app > div > div.ui-pdp-container.ui-pdp-container--pdp > div > div.ui-pdp-container__col.col-1.ui-pdp-container--column-right.mt-20.mr-20 > div:nth-child(1) > div > div.ui-pdp-container__row.ui-pdp-container__row--price > div > div.ui-pdp-price__second-line > span > span.price-tag-amount > span.price-tag-fraction') %>%
      html_text() %>% str_replace_all("\\.", "") %>% str_replace_all('UF ', "") %>%
      str_replace_all(',','.') %>% str_trim(side="both") %>% as.numeric()
  } else {
    precio <- html_node(web,'#root-app > div > div.ui-pdp-container.ui-pdp-container--pdp > div > div.ui-pdp-container__col.col-1.ui-pdp-container--column-right.mt-20.mr-20 > div:nth-child(1) > div > div.ui-pdp-container__row.ui-pdp-container__row--price > div > div > span > span.price-tag-amount > span.price-tag-fraction') %>%
      html_text() %>% str_replace_all("\\.", "") %>% str_replace_all('\\$', "") %>%
      str_replace_all(',','.') %>% str_trim(side="both") %>% as.numeric()
    precio_uf <- NA
    }
  
  codigo <- html_node(web,'#root-app > div > div > div.layout-col.layout-col--right > section.ui-view-more.vip-section-seller-info > div.card-description.card-phone-description > div.official-store-info.info-property-code > p.info') %>% 
    html_text()
  
  fecha_publicacion <- html_node(web,'#root-app > div > div.ui-pdp-container.ui-pdp-container--pdp > div > div.ui-pdp-container__col.col-1.ui-pdp-container--column-right.mt-20.mr-20 > div:nth-child(1) > div > div.ui-pdp-container__row.ui-pdp-container__row--header > div > p') %>% 
    html_text()
  
  # dorm <- html_node(web,'#productInfo > div.item-attributes > dl:nth-child(2) > dd') %>% 
  #   html_text() %>% str_remove("dormitorios|dormitorio") %>% 
  #   str_trim() %>% as.numeric()
  # banos <- html_node(web,'#productInfo > div.item-attributes > dl:nth-child(3) > dd') %>% 
  #   html_text() %>% str_remove("baños|baño") %>% str_trim() %>% parse_number()
  
  # Nuevo metodo para extraer info. Cargo la pagina como puro texto y busco las palabras
  # claves precisas seguidos de caracters y numeros!
  
  dorm <-   web %>% html_text() %>% str_extract("[Dd]ormitorios* *\\d") %>% 
    str_remove_all("\\t|\\n|[Dd]ormitorios*") %>% as.numeric()
  # . significa cualquier caracter, por si la ñ se desconfigura, idem acentos
  # banos <-  web %>% html_text() %>% str_extract("[Bb]a.*os*\\n\\t* *\\d*") %>% 
    # str_remove_all("\\t|\\n|[Bb]a.*os*") %>% as.numeric()
  banos <- web %>% html_text() %>% str_extract("[Bb]a.os* *\\d") %>% 
    str_remove_all("\\t|\\n|[Bb]a.os*") %>% as.numeric()
  
  # Caso que la unidad sea hectareas
  # sup_total <- web %>% html_text() %>% str_extract("[Ss]uperficie total\\n\\t* *\\d*") %>% 
  #   str_remove_all("\\t|\\n|[Ss]uperficie total") %>% as.numeric()
  sup_total<- web %>% html_text() %>% 
    str_extract("[Ss]uperficie total *\\d*\\.*\\d*.{0,3}")
  ajuste_ha <- if_else(str_detect(sup_total, "ha"),10000,1)
  sup_total <- sup_total %>% str_remove_all("\\t|\\n|[Ss]uperficie total| |ha|m.") %>% 
    as.numeric()*ajuste_ha
  
  sup <- web %>% html_text() %>% str_extract("[Ss]uperficie .til *\\d*") %>% 
    str_remove_all("\\t|\\n|[Ss]uperficie .til") %>% as.numeric()
  
  amb <- web %>% html_text() %>% str_extract("[Aa]mbientes* *\\d*") %>% 
    str_remove_all("\\t|\\n|[Aa]mbientes*") %>% as.numeric()
  
  estac <- web %>% html_text() %>% str_extract("[Ee]stacionamientos* *\\d*") %>% 
    str_remove_all("\\t|\\n|[Ee]stacionamientos*") %>% as.numeric()
  
  
  #Texto separador superficies
  # sep_sup <- ifelse(tipo_vivienda=='departamento','til','uida')
  
  # sup_total <- html_node(web,'#productInfo > div.item-attributes > dl:nth-child(1) > dd') %>% 
  #   html_text() %>% str_remove("m²|totales|total|m") %>% str_trim() %>% parse_number()
  # sup <- html_node(web,'#root-app > div > div > div.layout-col.layout-col--left > section.ui-view-more.vip-section-specs.main-section > div > div > div > section > ul > li:nth-child(2) > span') %>% 
  #   html_text() %>% str_remove("m²|m") %>% str_trim() %>% parse_number()
  
  descripcion <- html_node(web,'#root-app > div > div.ui-pdp-container.ui-pdp-container--pdp > div > div.ui-pdp-container__col.col-2.ui-pdp-container--column-left.pb-40 > div.ui-pdp-container__col.col-1.ui-vip-core-container--content-left > div.ui-pdp-container__row.ui-pdp-container__row--description > div > p') %>% 
    html_text()
  
  vende <- html_node(web,'#seller_profile > div > div.ui-vip-profile-info__info-container > div > h3') %>% 
    html_text()
  
  # barrio segun portalInmobiliario
  barrio_por <- html_nodes(web, '#root-app > div > div.ui-pdp-container.ui-pdp-container--top > div > div > div.ui-pdp-container__row.ui-vip-grouped-header > div.ui-pdp-breadcrumb > ul > li') %>% 
    html_text() %>% str_remove_all('\\t') %>% str_replace_all('\\n+','\\xyz') %>% 
    str_split("xyz") %>% unlist()
  barrio_por <- barrio_por[length(barrio_por)]

  ## Latitud y Longitud
  ## Notas: No estan en la estructura de XML de la pagina, sino como funciones despues
  ## Para lograr extraerlo miro el archivo como texto entero y extraigo los string que lo contienen

  long <- web %>% html_text() %>% str_extract("longitude.:.-\\d*.\\d*") %>% 
    f.split.n(":.",2) %>% as.numeric()
  
  lat <- web %>% html_text() %>% str_extract("latitude.:.-\\d*.\\d*") %>% 
    f.split.n(":.",2) %>% as.numeric()
  
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
                    ambientes= amb,
                    estacionamientos = estac,
                    descripcion=descripcion,
                    vende=vende,
                    longitud=long,
                    latitud=lat,
                    fecha_consulta=Sys.Date(),
                    venta_arriendo=venta_arriendo,
                    tipo_vivienda=tipo_vivienda,
                    barrio_portal=barrio_por))
}
