# -------------------------------------------------- #
# Climate Risk Profiles -- Crop Calendar
# C. Saavedra
# Alliance Bioversity-CIAT, 2021
# -------------------------------------------------- #
library(tidyverse)
library(raster)
library(terra)
library(sp)
library(sf)
library(foreign)
library(rgdal)
library(ggplot2)

#-------------------------------#

# 1. Load growing season dates raster per crop
# Maize
r1 <- raster::raster('//dapadfs.cgiarad.org//workspace_cluster_13/WFP_ClimateRiskPr/1.Data/crop_calendar/GGCMI/Maize_rf_growing_season_dates_v1.25.nc4', varname = 'planting day')
r2 <- raster::raster('//dapadfs.cgiarad.org//workspace_cluster_13/WFP_ClimateRiskPr/1.Data/crop_calendar/GGCMI/Maize_rf_growing_season_dates_v1.25.nc4', varname = 'harvest day')
# Cassava
r3 <- raster::raster("//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr/1.Data/crop_calendar/GGCMI/Cassava_ir_growing_season_dates_v1.25.nc4", varname = 'planting day')
r4 <- raster::raster('//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr/1.Data/crop_calendar/GGCMI/Cassava_rf_growing_season_dates_v1.25.nc4', varname = 'harvest day' )
# Pulses
r5 <- raster::raster("//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr/1.Data/crop_calendar/GGCMI/Pulses_ir_growing_season_dates_v1.25.nc4", varname = 'planting day')
r6 <- raster::raster("//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr/1.Data/crop_calendar/GGCMI/Pulses_rf_growing_season_dates_v1.25.nc4", varname = 'harvest day' )
# Millet
r7 <- raster::raster("//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr/1.Data/crop_calendar/GGCMI/Millet_ir_growing_season_dates_v1.25.nc4", varname = 'planting day')
r8 <- raster::raster("//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr/1.Data/crop_calendar/GGCMI/Millet_rf_growing_season_dates_v1.25.nc4", varname = 'harvest day' )
# Sorghum
r9 <- raster::raster("//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr/1.Data/crop_calendar/GGCMI/Sorghum_ir_growing_season_dates_v1.25.nc4", varname = 'planting day')
r10 <- raster::raster("//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr/1.Data/crop_calendar/GGCMI/Sorghum_rf_growing_season_dates_v1.25.nc4", varname = 'harvest day' )
# Rice
r11 <- raster::raster("//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr/1.Data/crop_calendar/GGCMI/Rice_ir_growing_season_dates_v1.25.nc4", varname = 'planting day')
r12 <- raster::raster("//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr/1.Data/crop_calendar/GGCMI/Rice_rf_growing_season_dates_v1.25.nc4", varname = 'harvest day')
# Wheat
r13 <- raster::raster("//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr/1.Data/crop_calendar/GGCMI/Wheat_ir_growing_season_dates_v1.25.nc4", varname = 'planting day')
r14 <- raster::raster("//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr/1.Data/crop_calendar/GGCMI/Wheat_rf_growing_season_dates_v1.25.nc4", varname = 'harvest day')
# Groundhunt
r15 <- raster::raster("//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr/1.Data/crop_calendar/GGCMI/Groundnuts_ir_growing_season_dates_v1.25.nc4", varname = 'planting day')
r16 <- raster::raster("//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr/1.Data/crop_calendar/GGCMI/Groundnuts_rf_growing_season_dates_v1.25.nc4", varname = 'harvest day')

r <- raster::stack(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16) ; rm(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16)
for(i in 1:10){r[[i]][r[[i]][] == -99  ] <- NA}

names(r)
names(r) = c("Maize_P", "Maize_H","Cassava_P", "Cassava_H","Pulses_P", "Pulses_H","Millet_P", "Millet_H","Sorghum_P", "Sorghum_H", "Rice_P", "Rice_H", "Wheat_P", "Wheat_H", "Groundhunt_H", "Groundhunt_P")


# 2. load shp GADM level 0 extent
# shp <- getData(name="GADM", country="BDI", level=0)
Countrys <- c('BDI','GNB','GIN','HTI','NPL','NER','PAK','SOM','TZA')
Country <- list()
for (i in 1:length(Countrys)){
  Country[[i]] <- getData('GADM', country=Countrys[i], level=0)
}
shp <- do.call("bind", Country)


rr = raster::raster('//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr/1.Data/chirps-v2.0.2020.01.01.tif')
rr[rr[]==-9999] <- NA

# 3. Cut chirps 
rr = rr %>% raster::crop(.,raster::extent(shp))

# Calendar for each country 
crops <- function(r, shp){
  limits = extent(shp)
  r = crop(r, limits)
  rr = crop(rr, limits)
  r = raster::resample(x = r, y = rr, method = "ngb")
  r = mask(r,rr)
  table = raster::rasterToPoints(r) %>% as.data.frame()

  P <- table %>%
    tidyr::pivot_longer(cols = contains('_P'),names_to = 'crop', values_to = 'Planting') %>%
    mutate(crop = str_remove(crop, '_P'), id_f = 1:nrow(.)) %>% dplyr::select(-contains('_H'))
  H <- table %>%
    tidyr::pivot_longer(cols = contains('_H'),names_to = 'crop', values_to = 'Harvest') %>%
    mutate(crop = str_remove(crop, '_H'), id_f = 1:nrow(.)) %>% dplyr::select(-contains('_P'))
  
  table <- full_join(P, H) %>% dplyr::select(-id_f)
  
  names(table) = c("Lon", "Lat", 'Crop',"Planting", "Harvest")
  table$Planting = as.Date(table$Planting, origin = "2001-01-01") %>% lubridate::month()
  table$Harvest = as.Date(table$Harvest, origin = "2001-01-01") %>% lubridate::month()
  table2 = unique(table[,3:5])
  return(table2)
}

Table <- purrr::map(Country, .f=crops, r=r) 

Calendar <- purrr::map2(.x = Country, .y = Table, .f = function(x, y){
  name_0 <- unique(x$NAME_0)
  y <- mutate(y, ID = name_0) %>% dplyr::select(ID, everything())
  return(y)}) %>% bind_rows()

Calendar <- unique(Calendar$ID) %>% 
  purrr::map(.f=function(pais){
    df = Calendar[Calendar$ID == pais,]
  })

Calendar

