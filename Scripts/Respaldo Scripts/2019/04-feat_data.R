## Feature data. Agregar variables para analisis
# PBH Nov 2018


# Nuevas variables
df <- df %>% mutate(precio_m2=precio/sup,
                    precio_m2tot=precio/sup_total,
                    precio_MM=precio/1e6,
                    uf_m2=precio_uf/sup,
                    uf_m2tot=precio_uf/sup_total,
                    precio_mil=precio/1e3)


# Factores
df <- df %>% mutate(dorm_factor=dorm %>% as.factor(),
                    banos_factor=banos %>% as.factor())

# Clasificacion 2D2B 
df <- df %>% mutate(tipo=case_when(
  dorm==1 & banos==1 ~ '1D1B',
  dorm==2 & banos==1 ~ '2D1B',
  dorm==2 & banos==2 ~ '2D2B',
  dorm==3 & banos==2 ~ '3D2B',
  dorm==3 & banos==3 ~ '3D3B',
  TRUE ~ 's/i') %>% as.factor())
df$tipo %>% unique()
df %>% group_by(tipo) %>% summarise(count=n())

df_orig <- df


df_eq <- df %>% filter(!is.na(equipamiento))

df_eq$equipamiento[1] %>% print()
df_eq$dir_url[1]


