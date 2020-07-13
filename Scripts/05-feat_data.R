## Feature data. Agregar variables para analisis
# PBH Nov 2018

## Limpieza --------------
# Remuevo cualquier dato que no tenga sup_total, dorm o baño
df <- df %>% tibble()
df %>% nrow()
df <- df %>% filter(!is.na(sup_total)&!is.na(dorm)&!is.na(banos))
df %>% nrow()

df %>% skim()

# sup mayor a 25 m2
df <- df %>% filter(sup_total>25)

# Autocompletar con precio UF ---------
#No todas tienen el valor en UF, completamos con el valor promedio
df %>% filter(!is.na(precio_uf)) %>% 
  mutate(UF=precio/precio_uf) %>% pull(UF) %>% skim()
uf_prom <- df %>% filter(!is.na(precio_uf)) %>% 
  mutate(UF=precio/precio_uf) %>% pull(UF) %>% mean()
# mismo valor en aprox 28 mil
df <- df %>% mutate(precio_uf=ifelse(is.na(precio_uf),precio/uf_prom,precio_uf))

df %>% skim()

# Nuevas variables -----------
df <- df %>% mutate(precio_m2=precio/sup,
                    precio_m2tot=precio/sup_total,
                    precio_MM=precio/1e6,
                    uf_m2=precio_uf/sup,
                    uf_m2tot=precio_uf/sup_total,
                    precio_mil=precio/1e3)


# Factores
df <- df %>% mutate(dorm_factor=dorm %>% as.factor(),
                    banos_factor=banos %>% as.factor(),
                    barrio_portal=barrio_portal %>% as.factor(),
                    barrio=barrio %>% as.factor(),
                    tipo_vivienda=tipo_vivienda %>% as.factor())

# Date
# df <- df %>% mutate(fecha_publicacion=fecha_publicacion %>% 
#                       strptime(format="%d-%m-%y") %>% as_date(),
#                     fecha_consulta=fecha_consulta %>% 
#                       strptime(format="%d-%m-%y") %>% as_date())


# Clasificacion tipo (ej: 2D2B) 
df <- df %>% mutate(tipo_all=paste(dorm, "D", banos, "B", sep=""))

# Eligo los 6 tipos mas frecuentes
top_6 <- df %>% group_by(tipo_all) %>% summarise(count=n()) %>% arrange(desc(count)) %>% 
  head(6) %>% pull(tipo_all)

# Mas frecuente
tipo_interes <- df %>% group_by(tipo_all) %>% summarise(count=n()) %>% 
  arrange(desc(count)) %>% head(1) %>% pull(tipo_all)
# tipo_interes <- ifelse(viv=="departamento","2D2B","3D2B")

# Asigno la clasificacion
df <- df %>% mutate(tipo=ifelse(tipo_all %in% top_6, tipo_all, "Otros") %>% factor())
df$tipo %>% unique()
df %>% group_by(tipo) %>% summarise(count=n())


# Filtro para ver solo tipos de viviendas de interes
# df <- df %>% filter(tipo!='Otros')
