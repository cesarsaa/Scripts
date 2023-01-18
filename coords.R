# #=-----------------------------------------------------------------------------------------------
# por hacer a Kenya historico  1981-2019
# 1. Los .fst con indices por: season, mensuales -> convertir a .tifs
# un tif por indice y por season tif con: 
# mensual 469=layers, season  78=layers
# 2. completar chirps agregar hasta la fecha de 2022
# 3. completar AGERA5 con mi cod (hacer en africalab)
# 4. subir scripts: descarga era5 & chirps verificar años pasados y descargar ultimo mes 
# 5. suitability -> preguntar sobre si lo hacemos nosotros o ellos
# 5. terminar el codigo para extraer por coordenadas
#=-----------------------------------------------------------------------------------------------
# 1. Input
# coordenadas
# datos chirps
# datos ERA5
# extraer coordenadas
# 2. Output
# tabla con variables
# 3. Falta
# tener en cuenta las fechas, como ?
# guardar la tabla con coords
# 4. Dudas
# como se ingresan las coordenadas ?
# es necesario un cod iso ?

#=-----------------------------------------------------------------------------------------------
library(tidyverse)
library(raster)
library(terra)
#=-----------------------------------------------------------------------------------------------
root <- "D:/OneDrive - CGIAR/Documents"
iso = ""
coords = c()

#=-----------------------------------------------------------------------------------------------
# .nc files
vars <- c("solar_radiation_flux","2m_temperature-24_hour_mean","2m_relative_humidity") # "solar_radiation_flux","10m_wind_speed","2m_temperature-24_hour_maximum","2m_temperature-24_hour_mean","2m_temperature-24_hour_minimum","2m_relative_humidity"

# Global mask 1 km resolution
msk <- terra::rast("//CATALOGUE/Workspace14/WFP_ClimateRiskPr/1.Data/chirps-v2.0.1981.01.1.tif")

# Country shapefile
shp <- terra::vect("//CATALOGUE/Workspace14/WFP_ClimateRiskPr/1.Data/shps/Burundi/bdi_gadm/Burundi_GADM2.shp")

# Raster template
tmp <- msk %>%
  terra::crop(x = ., y = terra::ext(shp)) %>%
  terra::mask(mask = shp)

r <- vars %>%
  purrr::map(.f = function(var){
    
    r <- terra::rast(list.files(path  = paste0(root,'/ERA5_data/', vars),
                                full.names = T, recursive = T))
    # list.files(path = pth, pattern = '.nc$', full.names = T) %>% gtools::mixedsort()
    
    r <- raster::stack(gn_data) %>% raster::crop(., ext) %>% raster::mask(. , ext)
    
    # if(vars == 'solar_radiation_flux'){
    #   r <- r/1000000 
    # }else if(vars %in% c("2m_temperature-24_hour_mean")){
    #   r <- r -273.15
    # }
    
    r <- r %>%
      terra::crop(x = ., y = terra::ext(tmp)) %>%
      terra::resample(x = ., y = tmp) %>%
      terra::mask(mask = tmp)
    return(r)
    
  })
#=-----------------------------------------------------------------------------------------------
# longitude & latitude
long = c(29.92500313,
         29.87500313,
         29.32500312,
         29.32500312,
         29.27500312,
         29.27500312)

lat = c(-3.625000799,
        -3.125000792,
        -3.575000798,
        -3.625000799,
        -3.125000792,
        -3.175000792)

#columna 1 longitud, columna 2 latitud
coords <- cbind.data.frame(long,lat)
spts <- SpatialPoints(coords, proj4string=CRS("+proj=longlat +datum=WGS84"))
#Extract data by spatial point
df <- extract(r, coords)
df <- cbind(coords, df)
#r <- terra::rast(r)
df <- terra::as.data.frame(r, xy = T)

plot(r[[1400]])
plot(spts, add=T, col="black")
