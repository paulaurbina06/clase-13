#==============================================================================#
# Autor(es): Eduard Martinez
# Colaboradores: 
# Fecha creacion: 10/08/2019
# Fecha modificacion: 28/04/2021
# Version de R: 2.0.3.
#==============================================================================#

# intial configuration
rm(list = ls()) # limpia el entorno de R
pacman::p_load(tidyverse,sf,sp,raster,viridis,rgdal,rstudioapi,getSpatialData) # cargar y/o instalar paquetes a usar

cat("Vamos a calcular la correlacion entre luminosidad y actividad comercial")

# Importar bases de datos
luces = 
  
c_poblado = readRDS(file = 'data/input/mgn/centros poblados.rds') %>% st_as_sf() %>%
            subset(codmpio %in% c(47001:48000 %>% as.character())) %>% st_transform(crs = st_crs(luces)) 


  
  
### 6.2. Funcion que calcula distancias

