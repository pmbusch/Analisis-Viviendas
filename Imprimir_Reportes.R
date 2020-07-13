## Script para imprimir reportes
## PBH Jul 2020

# Valores que se usarán como "parámetros" (variables) del reporte
barrios <- c("PlazaÑuñoa", "Valdivia")
viviendas <- c("casa", "departamento")

b <- "PlazaÑuñoa"
v <- "departamento"


# Imprimir reporte individual
rmarkdown::render("Reporte_Viviendas.Rmd",
                  params = list(viv = v,
                                barrio=b),
                  output_file = paste("Reportes/Reporte", b,v, sep="_"))

# Definir función para generación de reportes
for (b in barrios){
  for (v in viviendas){
    rmarkdown::render("Reporte_Viviendas.Rmd",
                      params = list(viv = v,
                                    barrio=b),
                      output_file = paste("Reportes/Reporte", b,v, sep="_"))
  }
}


# Imprimir reporte Terrenos
rmarkdown::render("Reporte_Terrenos.Rmd",
                  output_file = paste("Reportes/Reporte", "Aysen", sep="_"))

# EoF