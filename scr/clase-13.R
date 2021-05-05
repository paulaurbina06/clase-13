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

#----------------------#
#  1. Importar raster  #
#----------------------#

### 1.1. Verifiquemos los atributos del raster a cargar
GDALinfo("data/input/siac/magdalena_deforestacion_1990_2000.tif")

### 1.2. Importar raster
browseURL(url = "http://www.siac.gov.co", browser = getOption("browser")) # Fuente
deforestacion = raster("data/input/siac/magdalena_deforestacion_1990_2000.tif")

### 1.2.1 plot basico del raster
plot(deforestacion)
viewer(url = 'lecture/pics/simbolos.png')

### 1.2.1. Vamos a darle contexto a los datos
magdalena = st_read(dsn= 'data/input/mgn/MGN_Municipio.shp') %>% 
            subset(MPIO_CCDGO %in% c('189','980','053'))
plot(magdalena,border="blue",col=NA,add=T)

### 1.3. Atributos del raster
deforestacion

#### 1.3.1. Nombres de las bandas
names(deforestacion)
names(deforestacion) = "cobertura_vegetal"

#### 1.3.2. Extension
st_bbox(deforestacion)
deforestacion@extent

#### 1.3.3. Proyeccion
deforestacion@crs
crs(deforestacion)
st_crs(deforestacion)

#### 1.3.4. Valores de los pixeles (ver diccionario)
minValue(deforestacion$cobertura_vegetal)  
maxValue(deforestacion$cobertura_vegetal)  
values(deforestacion)

### 1.3. Trabajar con los values de los pixeles
values(deforestacion)[1] <- 0
values(deforestacion)[is.na(values(deforestacion))==T] <- 0

#### 1.3.1. Descriptivas
values(deforestacion) %>% table()
values(deforestacion) %>% summary()

### 1.4. Exportar raster
writeRaster(x = deforestacion,filename = "data/output/magdalena_deforestacion_1990_2000.tif",overwrite=TRUE)

### 1.5. Puedo reproyectar un raster?
proj4string(deforestacion)
deforestacion_pr = raster::projectRaster(deforestacion,crs = '+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs')
proj4string(deforestacion_pr)

#### 1.5.1. Veamos que pasa con los valores de los pixeles
minValue(deforestacion_pr) ; maxValue(deforestacion_pr) # Ups!

#------------------------------#
#  2. Operaciones geometricas  #
#------------------------------#

### 2.0. Limpiemos el entorno
rm(list = ls())

### 2.1. Importar raster de luces
browseURL(url = "https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html", browser = getOption("browser")) # Fuente
rstudioapi::viewer(url = 'data/input/NOAA/colombia_202004.tif')
luces = raster(x = 'data/input/NOAA/colombia_202004.tif')
luces
cataca = st_read(dsn= 'data/input/mgn/MGN_Municipio.shp') %>% subset(MPIO_CCDGO %in% c('053'))

### 2.2. Cliping un raster
l_cataca = crop(luces,cataca) # Que hice mal?
crs(luces) 
crs(cataca)

ggplot() + geom_tile(data = as.data.frame(l_cataca, xy=TRUE), aes(y=y,x=x,fill=colombia_202004)) + 
scale_fill_distiller(palette='Spectral',na.value = 'gray') + 
geom_sf(data =cataca,color = 'black',fill=NA) + theme_bw()
   
l_cataca = crop(luces,cataca) %>% mask(cataca)
ggplot() + geom_tile(data = as.data.frame(l_cataca, xy=TRUE), aes(y=y,x=x,fill=colombia_202004)) + 
scale_fill_distiller(palette='Spectral',na.value = 'gray') + 
geom_sf(data =cataca,color = 'black',fill=NA) + theme_bw()

### 2.3. Extraer los valores de un raster
data = values(l_cataca) %>% .[is.na(.)==F]
summary(data)

### 2.4. Raster a datos vectoriales

### 2.4.1. Raster a puntos
point_1 = rasterToPoints(l_cataca)
point_2 = rasterToPoints(l_cataca,spatial = T) %>% st_as_sf()
ggplot() + geom_sf(data = point_2 , aes(color=colombia_202004),size=0.1)

### 2.4.2. Raster a polygonos
polygon = rasterToPolygons(l_cataca) %>% st_as_sf()
ggplot() + geom_sf(data = polygon , aes(fill=colombia_202004)) +
scale_fill_distiller(palette='Spectral',na.value = 'gray')

### 2.5. Exportar sf
saveRDS(object = polygon , file = 'data/output/Sf luces Aracataca.rds')

#-------------------------------------------#
#  3. Trabajar con raster de varias bandas  #
#-------------------------------------------#

### 3.0. Como se ve un RGB?
dev.off()
plotRGB(stack("help/figures/rgb_raster.png"),r = 1, g = 2, b = 3) # Imagen tomada de https://www.neonscience.org

### 3.1. Importar raster
browseURL(url = 'https://data.neonscience.org/apps/browse', browser = getOption("browser")) # Fuente
GDALinfo("data/input/neon/HARV_RGB_Ortho.tif")
banda_r <- raster(x = "data/input/neon/HARV_RGB_Ortho.tif") # Como no le vamos a indicar que banda cargar, el va a cargar por 'default' la banda 1, es decir la roja

### 3.2. Veamos que tenemos
paleta_col <- gray.colors(n = 100, start = 0.0,end = 1.0,alpha = NULL) 
plot(banda_r, col=paleta_col, axes=FALSE, main="Imagen RGB - Banda 1 (roja)") 

### 3.2.1.  En un RGB podemos tener 255*255*255 posibles combinaciones, es decir 16.581.375 colores
minValue(banda_r) ; maxValue(banda_r)

### 3.3. Importando las otras bandas
banda_g <- raster("data/input/neon/HARV_RGB_Ortho.tif", band = 2)
plot(banda_g,col=paleta_col,axes=FALSE, main="Imagen RGB - Banda 2 (verde)") 

### 3.4. Las tres bandas de una sola vez

### 3.4.1. Cargando cada banda por aparte y apliando
RGB_apilado = stack(raster("data/input/neon/HARV_RGB_Ortho.tif",band=1),
                    raster("data/input/neon/HARV_RGB_Ortho.tif",band=2),
                    raster("data/input/neon/HARV_RGB_Ortho.tif",band=3))
RGB_apilado

### 3.4.2. Usando stack
RGB_stack = stack("data/input/neon/HARV_RGB_Ortho.tif")

### 3.4.3. Podriamos usar la opcion brick que es mas eficiente
RGB_brick <- brick(x = "data/input/neon/HARV_RGB_Ortho.tif")
object.size(RGB_brick)
object.size(RGB_stack)
object.size(RGB_apilado)

### 3.4.4. Veamos que tenemos
dev.off()
plotRGB(RGB_brick, r = 1, g = 2, b = 3)

### 3.5. Veamos los atribustos
RGB_stack

### 3.5.1. names
names(RGB_stack)
names(RGB_stack) = c('red','green','blue') 
names(RGB_stack)

### 3.5.1. Layers
RGB_stack@layers
RGB_apilado@layers
RGB_apilado[[1]]

### 3.6. Extraer atributos
point_RGB = rasterToPoints(RGB_stack,spatial = T) %>% st_as_sf()
class(point_RGB)
View(point_RGB)
