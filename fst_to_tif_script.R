#-----------------------------------------------------------
library(raster)
library(dplyr)
library(purrr)
#-----------------------------------------------------------
df <- fst::read.fst("//CATALOGUE/Workspace14/WFP_ClimateRiskPr/7.Results/Kenya/past/KEN_indices.fst")
df <- fst::read.fst("//CATALOGUE/Workspace14/WFP_ClimateRiskPr/1.Data/soil/KEN/soilcp_data.fst")
df
names(df)

# indices
vars <- c("TAI","ATR","AMT","NDD","P5D","P95",
          "NT_X","NDWS","NWLD","NWLD50","NWLD90","IRR","SHI","HSI_0","HSI_1","HSI_2","HSI_3",
          "THI_0","THI_1","THI_2","THI_3","CSDI","gSeason","SLGP","LGP")
vars <- c("ssat") # scp ssat

r = rasterFromXYZ(df[,c("x","y",vars)])
r
plot(r)
terra::writeRaster(x = r, filename = paste0(vars, '.tif'), bylayer=T, overwrite = T)
#-----------------------------------------------------------
# Transform from xyzt into raster
#-----------------------------------------------------------
# s_df_raster <- sdf %>% 
#   group_split(season, year) %>% 
#   map(~rasterFromXYZ(.x[,c("x", "y", "TAI")])) %>% stack()
# names(s_df_raster)
# writeRaster(s_df_raster,'test1.tif',options=c('TFW=YES'))
# s_raster[[grep("AMT", names(s_raster))]]

s_df_raster <- df %>% 
  group_split(id) %>% 
  map(~rasterFromXYZ(.x[,c("x", "y", vars)]))

fecha <- c(paste0("_s1_", 1981:2019),paste0("_s2_", 1981:2019))
 
  for (i in 1:length(s_df_raster)) {
   names(s_df_raster[[i]]) <- paste0(names(s_df_raster[[i]]), fecha[i])
  }

s_raster <- s_df_raster %>% raster::stack() 
names(s_raster)

  for (i in vars) {
  #f <- paste0(i, '.tif')
  # terra::writeRaster(s_raster[[grep(i,names(s_raster))]], filename= paste0(i, '.tif'), overwrite = T)
  terra::writeRaster(s_raster[[grep(i,names(s_raster))]], filename= paste0(i, '.tif'), format="GTiff", overwrite = T)
  }

vars %>%
  purrr::map(.f = function(j){
    terra::writeRaster(x = r, filename = paste0(j,'.tif'), bylayer=T, overwrite = T)
  })

#-----------------------------------------------------------
R.test <- terra::rast("../Documents/AMT.tif")
names(R.test)
plot(R.test[[1]])
plot(R.test[[3:14]])

#=-----------------------------------------------------------------------------------------------
# longitude & latitude
long = c()
lat = c()

#columna 1 longitud, columna 2 latitud
coords <- cbind.data.frame(long,lat)
# spts <- SpatialPoints(coords, proj4string=CRS("+proj=longlat +datum=WGS84"))
#Extract data by spatial point
df.test <- extract(R.test, coords)

#=--------------------------------------------------------------------------------------------
r_test <- raster::brick("../Documents/AMT.tif")
names(s_raster[[1]])
