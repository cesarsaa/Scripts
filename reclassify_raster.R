#-------------------------------------------------------------------------------
# Categorizar un mapa continuo 
#-------------------------------------------------------------------------------
g <- gc(reset = T); rm(list = ls())

OSys <<- Sys.info()[1]
root <<- switch(OSys,
                'Linux'   = '/catalogue/WFP_ClimateRiskPr1',
                'Windows' = '//CATALOGUE/WFP_ClimateRiskPr1')
outfile <- paste0(root, '/7.Results/urg/'); if(!dir.exists(outfile)){dir.create(outfile, F, T)}
#-------------------------------------------------------------------------------
# 1.
CDD <- raster::raster("//CATALOGUE/WFP_ClimateRiskPr1/7.Results/urg/continuous variables/Number of dry days (days)/CDD_mean.tif")
plot(CDD)

# reclassify
values <- summary(CDD[])
mtx <- matrix(c(0, 2, 1, 2, 5, 2, 5, 10, 3, 10, 20, 4, 20, 30, 5, 30, 40, 6), 
              byrow = T, ncol = 3)
mtx

CDD.RCL <- raster::reclassify(x=CDD, rcl=mtx)
plot(CDD.RCL)
raster::writeRaster(CDD.RCL, filename = paste0(outfile,'CDD_C.tif'))
#-------------------------------------------------------------------------------
# 2.
NTX35 <- raster::raster("//CATALOGUE/WFP_ClimateRiskPr1/7.Results/urg/continuous variables/Heat stress generic crop (days)/NTx45_mean.tif")
plot(NTX35)

# reclassify
values <- summary(NTX35[]); values
mtx <- matrix(c(0, 2, 1, 2, 4, 2, 4, 6, 3, 6, 8, 4, 8, 10, 5), 
              byrow = T, ncol = 3)
mtx

NTX35.RCL <- raster::reclassify(x=NTX35, rcl=mtx)
plot(NTX35.RCL)
raster::writeRaster(NTX35.RCL, filename = paste0(outfile,'NTX35_C.tif'))
#-------------------------------------------------------------------------------
# 3.
HSH <- raster::raster("//CATALOGUE/WFP_ClimateRiskPr1/7.Results/urg/continuous variables/Heat stress humans (ºC)/HSH_mean.tif")
plot(HSH)

# reclassify
values <- summary(HSH[]); values
mtx <- matrix(c(12, 20, 1, 20, 28, 2, 28, 36, 3, 36, 44), 
              byrow = T, ncol = 3)
mtx

HSH.RCL <- raster::reclassify(x=HSH, rcl=mtx)
plot(HSH.RCL)
raster::writeRaster(HSH.RCL, filename = paste0(outfile,'HSH_C.tif'))
#-------------------------------------------------------------------------------
# 4.
THI <- raster::raster("//CATALOGUE/WFP_ClimateRiskPr1/7.Results/urg/continuous variables/Heat stress livestock (cattle) (THI units)/THI_mean.tif")
plot(THI)

# reclassify
values <- summary(THI[]); values
mtx <- matrix(c(0, 0.2, 1, 0.2, 0.4, 2, 0.4, 0.6, 3, 0.6, 0.8, 4, 0.8, 1, 5), 
              byrow = T, ncol = 3)
mtx

THI.RCL <- raster::reclassify(x=THI, rcl=mtx)
plot(THI.RCL)
raster::writeRaster(THI.RCL, filename = paste0(outfile,'THI_C.tif'))
#-------------------------------------------------------------------------------
# 5.
NTX28 <- raster::raster("//CATALOGUE/WFP_ClimateRiskPr1/7.Results/urg/continuous variables/Heat stress maize/NTx28_mean.tif")
plot(NTX28)

# reclassify
values <- summary(NTX28[]); values
mtx <- matrix(c(0, 5, 1, 5, 10, 2, 10, 15, 3, 15, 20, 4, 20, 25, 5, 25, 30, 6, 30, 35, 7), 
              byrow = T, ncol = 3)
mtx

NTX28.RCL <- raster::reclassify(x=NTX28, rcl=mtx)
plot(NTX28.RCL)
raster::writeRaster(NTX28.RCL, filename = paste0(outfile,'NTX28_C.tif'))
#-------------------------------------------------------------------------------
# 6.
NDWS <- raster::raster("//CATALOGUE/WFP_ClimateRiskPr1/7.Results/urg/continuous variables/Number of soil water stress days (days)/NDWS_mean.tif")
plot(NDWS)

# reclassify
values <- summary(NDWS[]); values
mtx <- matrix(c(0, 5, 1, 5, 10, 2, 10, 15, 3, 15, 20, 4, 20, 25, 5, 25, 30, 6, 30, 35, 7), 
              byrow = T, ncol = 3)
mtx

NDWS.RCL <- raster::reclassify(x=NDWS, rcl=mtx)
plot(NDWS.RCL)
raster::writeRaster(NDWS.RCL, filename = paste0(outfile,'NDWS_C.tif'))
#-------------------------------------------------------------------------------
# 7.
NWLD <- raster::raster("//CATALOGUE/WFP_ClimateRiskPr1/7.Results/urg/continuous variables/Number of waterlogging days (days)/NWLD50_mean.tif")
plot(NWLD)

# reclassify
values <- summary(NWLD[]); values
mtx <- matrix(c(0, 0.5, 1, 0.5, 1, 2, 1, 1.5, 3), 
              byrow = T, ncol = 3)
mtx

NWLD.RCL <- raster::reclassify(x=NWLD, rcl=mtx)
plot(NWLD.RCL)
raster::writeRaster(NWLD.RCL, filename = paste0(outfile,'NWLD_C.tif'))

df <- raster::raster("//CATALOGUE/WFP_ClimateRiskPr1/7.Results/urg/categorical_variables/Heat stress humans (ºC)/heat_stress_humans.tif")
plot(df)
hsh <- raster::raster("//CATALOGUE/WFP_ClimateRiskPr1/7.Results/urg/HSH_C.tif")
plot(hsh)
