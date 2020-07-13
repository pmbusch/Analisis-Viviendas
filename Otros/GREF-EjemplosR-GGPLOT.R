
library(ggplot2)

### Ejemplos de BOXPLOTS


#### EJEMPLOS BOXPLOTS normales
p1 = ggplot(bd.fuentes, aes(contaminante, concentracion))
p1 + geom_boxplot()   # boxplot normal
p1 + geom_boxplot() + coord_flip()      # boxplot apaisado
p1 + geom_boxplot() + geom_jitter()     # boxplot con puntos indicando cada observacion
p1 + geom_boxplot(aes(fill = contaminante)) + coord_flip() 



## Densidades
p2 = ggplot(bd.fuentes, aes(x=concentracion))
p2 + geom_density() + scale_y_log10()                      ## pdf con escala logartimica
p2 + geom_density( aes(fill=contaminante)) + scale_y_log10() 


p3 = ggplot(bd.fuentes, aes(x=concentracion, color=contaminante))
p3 + stat_ecdf() + scale_y_log10()                        ## Cdf con escala logartimica

