## Funcion para extrer toda la info necesaria del mismo barrio
# Utiliza la funcion de vivienda
# PBH Nov 2018


# Cargar librerias necesarias
library(rvest)
library(stringr)
library(readr)

# Funcion para scrap un barrio completo. Tipo puede ser:departamento, casa
f.scrap.barrio <- function(barrio,venta=True,tipo_vivienda){
  
  # Seleccionar venta o arriendo
  venta_arriendo <- if (venta) 'venta' else 'arriendo'

  
  # Crear el string URL de la pagina a scrapear
  url <- 'https://www.portalinmobiliario.com/%s/%s/%s?ca=3&ts=1&mn=1&or=&sf=1&sp=0&at=0&pg=%s'
  
  dir_portal <- 'https://www.portalinmobiliario.com'
  
  # sprintf permite crear un string tipo, y despues reemplazar los %s a gusto! Muy util para crear un codigo que Scrapee varios barrios a la vez!
  cat(sprintf(url,venta_arriendo,tipo_vivienda,barrio,1),'\n')
  
  # Descargar la pagina completa (mediante un comando)
  web <- read_html(sprintf(url,venta_arriendo,tipo_vivienda,barrio,1))
  
  # Navegar por la estructura html de la pagina, y extraer la informacion necesaria!
  #Obtenemos la ultima pagina de resultados para iterar en todas
  last_pg <- html_nodes(web,'#PaginacionSuperior > div > ul > li > a') %>% html_attr('href') %>% 
    str_split('&pg=') %>% sapply(., function(x) x[2]) %>% as.numeric() %>% max() 
  cat('Barrio tiene ',last_pg,' paginas a scrapear','\n')
  
  df <- data.frame(barrio=as.character(),
                   dir_url=as.character(),
                   nombre=as.character(),
                   precio=as.numeric(),
                   precio_uf=as.numeric(),
                   codigo=as.numeric(),
                   fecha_publicacion=as.character(),
                   dorm=as.numeric(),
                   banos=as.numeric(),
                   sup=as.numeric(),
                   sup_total=as.numeric(),
                   descripcion=as.character(),
                   equipamiento=as.character(),
                   vende=as.character(),
                   fecha_consulta=as.character(),
                   venta_arriendo=as.character(),
                   tipo_vivienda=as.character())
  
  
  # For para recorrer y entrar en cada aviso
  #Mensaje
  cat("Descargando informacion para barrio: ",barrio)
  cat("Paginas: ")
  for (i in (1:last_pg)){
    cat(" ",i)
    web <- read_html(sprintf(url,venta_arriendo,tipo_vivienda,barrio,i))
    
    # Incluyo unicamente los registros con todos los datos, en este caso el label de 3D/2B no siempre esta
    incluir <- html_nodes(web,'#wrapper > section.content-section.content-sidedar-equals >
                          div > div > div.col-sm-9.span-fix-content > article > div.products-list >
                          div > div.col-sm-9.product-item-data > div > 
                          div.col-sm-6.product-item-summary')
    incluir <- incluir %>% str_locate('label label-default') %>% as.data.frame() %>% .$start
    incluir <- ifelse(incluir>0,T,F) 
    
    
    
    #Obtenemos la direccion (calle) de cada aviso y su direccion URL!
    direccion <- html_nodes(web,'#wrapper > section.content-section.content-sidedar-equals > div > div > div.col-sm-9.span-fix-content > article > div.products-list > div > div.col-sm-9.product-item-data > div > div.col-sm-6.product-item-summary > h4 > a')
    dir_url <- html_attr(direccion,'href') %>% f.filtrar.vec(incluir)
    rm(direccion)
    
    for (url_depto in dir_url){
      df <- rbind(df,f.scrap.viv(paste(dir_portal,url_depto,sep=''),barrio,venta_arriendo,tipo_vivienda))
    }
    
  }
  
  return(df)
  
}




