#==============================================================================#
# Autor(es): Eduard Martinez
# Colaboradores: 
# Fecha creacion: 10/08/2019
# Fecha modificacion: 28/04/2021
# Version de R: 2.0.3.
#==============================================================================#

# intial configuration
rm(list = ls()) # limpia el entorno de R
pacman::p_load(tidyverse,sf,raster,getSpatialData) # cargar y/o instalar paquetes a usar

cat("Vamos a calcular la correlacion entre luminosidad y actividad comercial")

# Importar bases de datos
luces = raster("data/input/NOAA/colombia_202004.tif") # luces
crs(luces)
  
magdalena = readRDS(file = 'data/input/mgn/centros poblados.rds') %>% st_as_sf() %>%
            subset(codmpio %in% c(47001:48000 %>% as.character())) %>% st_transform(crs = st_crs(luces)) # centros poblados
ggplot() + geom_sf(data = magdalena)

nbi = readRDS("data/input/dane/nbi.rds") %>% mutate(codmpio = as.character(codmpio))# nbi
  
# clip luces
luces_magdalena = crop(luces,magdalena) %>% mask(magdalena)

# convertir en sf
point_magdalena = rasterToPoints(luces_magdalena,spatial = T) %>% st_as_sf()
point_magdalena %>% class()
View(point_magdalena)
  
# asignar el centro poblado
point_magdalena = st_join(point_magdalena,magdalena)

# luminosidad promedio
df = point_magdalena %>% data.frame() %>% group_by(codmpio) %>% summarise(mean_l = mean(colombia_202004))

# agregar nbi
df = left_join(df,nbi,by = "codmpio")

# plot
ggplot() + geom_point(data = df, aes(x=mean_l,y=nbi)) + theme_bw()

ggplot() + geom_point(data = df, aes(x=mean_l,y=IPM_urb)) + theme_bw()
  
  

