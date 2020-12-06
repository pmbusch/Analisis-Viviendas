## Funcion para extrer toda la info necesaria del mismo barrio
# Utiliza la funcion de vivienda
# PBH Nov 2018
# Actualizado PBH Jun 2020



# Funcion para scrap un barrio completo. Tipo puede ser:departamento, casa
f.scrap.barrio <- function(barrio,venta=TRUE,tipo_vivienda){
  
  # Seleccionar venta o arriendo
  venta_arriendo <- if (venta) 'venta' else 'arriendo'

  
  # Crear el string URL de la pagina a scrapear
  url <- 'https://www.portalinmobiliario.com/%s/%s/propiedades-usadas/%s/_Desde_%s'
  
  dir_portal <- 'https://www.portalinmobiliario.com'
  
  # sprintf permite crear un string tipo, y despues reemplazar los %s a gusto! Muy util para crear un codigo que Scrapee varios barrios a la vez!
  cat('\n',sprintf(url,venta_arriendo,tipo_vivienda,barrio,1),'\n')
  
  # Descargar la pagina completa (mediante un comando)
  web <- read_html(sprintf(url,venta_arriendo,tipo_vivienda,barrio,1))
  
  # Navegar por la estructura html de la pagina, y extraer la informacion necesaria!
  # Obtenemos el total de resultados
  total_viv <- html_node(web,'#root-app > div > div > aside > div.ui-search-sidebar__result-container > div > span') %>% 
    html_text() %>% str_trim() %>% str_remove(" resultados") %>% as.numeric()
  
  # 50 resultados por pagina
  total_paginas <- floor(total_viv/50)+1
  
  cat('Barrio tiene ',total_viv,' viviendas a scrapear','\n')
  cat('Barrio tiene ',total_paginas,' paginas a scrapear','\n')
  
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
                   ambientes= as.numeric(),
                   estacionamientos = as.numeric(),
                   descripcion=as.character(),
                   vende=as.character(),
                   longitud=as.numeric(),
                   latitud=as.numeric(),
                   fecha_consulta=as.character(),
                   venta_arriendo=as.character(),
                   tipo_vivienda=as.character(),
                   barrio_portal=as.character())
  
  # For para recorrer y entrar en cada aviso
  #Mensaje
  cat("Descargando informacion para barrio: ",barrio,'\n')
  cat("Paginas: ")
  for (i in (1:total_paginas)){
    cat(" ",i)
    web <- read_html(sprintf(url,venta_arriendo,tipo_vivienda,barrio,50*(i-1)+1))
    
    #Obtenemos la direccion URL de cada aviso
    dir_url <- html_nodes(web,xpath='//*[@id="root-app"]/div/div/section/ol/li/div/div/a[1]') %>% 
      html_attr('href')
    # filtro url con portalinmobiliario
    dir_url <- dir_url[str_detect(dir_url, "https://www.portalinmobiliario.com")]
    # remuevo proyectos, los cuales en su URL tienen [BB:1] al final
    dir_url <- dir_url[!(str_detect(dir_url,"BB:[1-9]"))]
    
    
    for (url_depto in dir_url){
      tryCatch(
        {
          df <- rbind(df,f.scrap.viv(url_depto,barrio,venta_arriendo,tipo_vivienda))
        }, error = function(cond) return(NULL) )
      }
    
  }
  
  return(df)
  
}

# EoF