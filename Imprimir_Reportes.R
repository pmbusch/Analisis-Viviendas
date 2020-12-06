## Script para imprimir reportes
## PBH Jul 2020

# Imprimir reporte individual
rmarkdown::render("Reporte_Viviendas.Rmd",
                  params = list(viv = "departamento",
                                barrio="Valdivia"),
                  output_file = paste("Reportes/Reporte", "Valdivia","depto","5Dic2020", sep="_"))


# Valores que se usarán como "parámetros" (variables) del reporte
barrios <- c("PlazaÑuñoa", "Valdivia")
viviendas <- c("casa", "departamento")

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