## Funciones para Web Scrap
##PBH Nov 2018


# Funcion para filtrar un vector a partir de valores del otro
# Recibe un vector a ser filtrado, y un vector booleans indicando que valores se deben tener
# En un contexto de data frame seria inutil
f.filtrar.vec <- function(vec,vec_cond){
  vec1 <- vec %>% as.data.frame(stringsAsFactors=F)
  names(vec1) <- 'vec'
  vec2 <- vec_cond %>% as.data.frame(stringsAsFactors=F)
  names(vec2) <- 'incluir'
  
  # Los combino como Dataframe y hago el filtro
  df_comb <- cbind(vec1,vec2) %>% filter(incluir==T)
  
  # Retorna el vector filtrado
  return(df_comb$vec)
}

# Funcion para hacer split y obtener el valor de cierto arreglo. Compatible conv vectores
f.split.n <- function(texto,patron,n){
  texto %>% str_split(patron) %>% sapply(., function(x) x[n])
}

