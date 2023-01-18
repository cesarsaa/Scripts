

#------------------------------------------------------------------------------#
# NTX40
g=gc();rm(list=ls())
root <- "//catalogue/WFP_ClimateRiskPr1/"
IND = 'NTX40'# NDWLD50, NDWS, NTX40, THI, TAI
PRD = 'future' # future
SSP = 'ssp245' # ssp245
YRS = '2030s' # 2050s

ras <- raster(paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/numeric/",IND,"_",YRS,"_",SSP,".tif"))
ras; summary(ras)
plot(ras)

# NTX40
# No significant stress: 0
# Moderate: 1 - 5
# Severe: 5 - 10
# Extreme: > 10

cat <- c(-Inf, 1,   1,
         1  , 5 ,  2,
         5  , 10 , 3,
         10 , Inf, 4)

cat_m <- matrix(cat,ncol=3,byrow = T)
r1 <- raster::reclassify(ras, cat_m)
plot(r1)

r2 <- ratify(r1)
a <- levels(r2)[[1]]
a$etiqueta <-c("No significant stress","Moderate","Severe","Extreme")
levels(r2) <- a

outfile <- paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/categoric/"); if(!dir.exists(outfile)){dir.create(outfile, F, T)}
writeRaster(r2, paste0(outfile,IND,"_",YRS,"_",SSP,".tif"), overwrite=TRUE)


#------------------------------------------------------------------------------#
# NDWS

g=gc();rm(list=ls())
root <- "//catalogue/WFP_ClimateRiskPr1/"
IND = 'NDWS'# NDWLD50, NDWS, NTX40, THI, TAI
PRD = 'future' # future
SSP = 'ssp245' # ssp245
YRS = '2030s' # 2050s

ras <- raster(paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/numeric/",IND,"_",YRS,"_",SSP,".tif"))
ras; summary(ras)
plot(ras)

# NDWS
# No significant stress: < 15
# Moderate: 15 to 20
# Severe: 20 to 25
# Extreme: > 25

cat <- c(-Inf, 15,  1,
         15  , 20 , 2,
         20  , 25 , 3,
         25  , Inf, 4)

cat_m <- matrix(cat,ncol=3,byrow = T)
r1 <- raster::reclassify(ras, cat_m)
plot(r1)

r2 <- ratify(r1)
a <- levels(r2)[[1]]
a$etiqueta <-c("No significant stress","Moderate","Severe","Extreme")
levels(r2) <- a

outfile <- paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/categoric/"); if(!dir.exists(outfile)){dir.create(outfile, F, T)}
writeRaster(r2, paste0(outfile,IND,"_",YRS,"_",SSP,".tif"), overwrite=TRUE)

#------------------------------------------------------------------------------#
# NDWL50

g=gc();rm(list=ls())
root <- "//catalogue/WFP_ClimateRiskPr1/"
IND = 'NDWL50'# NDWLD50, NDWS, NTX40, THI, TAI
PRD = 'future' # future
SSP = 'ssp585' # ssp245
YRS = '2030s' # 2050s

ras <- raster(paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/numeric/",IND,"_",YRS,"_",SSP,".tif"))
ras; summary(ras)
plot(ras)

# No significant stress: < 2
# Moderate: 2 to 5
# Severe: 5 to 8
# Extreme: > 8

cat <- c(-Inf, 2,  1,
         2  , 5 , 2,
         5  , 8 , 3,
         8  , Inf, 4)

cat_m <- matrix(cat,ncol=3,byrow = T)
r1 <- raster::reclassify(ras, cat_m)
plot(r1)

r2 <- ratify(r1)
a <- levels(r2)[[1]]
a$etiqueta <-c("No significant stress","Moderate","Severe","Extreme")
levels(r2) <- a

outfile <- paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/categoric/"); if(!dir.exists(outfile)){dir.create(outfile, F, T)}
writeRaster(r2, paste0(outfile,IND,"_",YRS,"_",SSP,".tif"), overwrite=TRUE)

#------------------------------------------------------------------------------#
# THI MAX

g=gc();rm(list=ls())
root <- "//catalogue/WFP_ClimateRiskPr1/"
IND = 'THI'# NDWLD50, NDWS, NTX40, THI, TAI
PRD = 'future' # future
SSP = 'ssp245' # ssp245
YRS = '2050s' # 2050s

ras <- raster(paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/numeric/","max_annual_",IND,"_",YRS,"_",SSP,".tif"))
ras; summary(ras)
plot(ras)

# No stress: ??? 72
# Mild stress: 73-78
# Moderate stress: 79-89
# Severe stress: ???90	

cat <- c(-Inf, 72,  1,
         72  ,  78, 2,
         78  , 89, 3,
         89  , Inf, 4)

cat_m <- matrix(cat,ncol=3,byrow = T)
r1 <- raster::reclassify(ras, cat_m)
plot(r1)

r2 <- ratify(r1)
a <- levels(r2)[[1]]
a$etiqueta <-c("No stress","Mild stress","Moderate stress","Severe stress")
levels(r2) <- a

outfile <- paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/categoric/"); if(!dir.exists(outfile)){dir.create(outfile, F, T)}
writeRaster(r2, paste0(outfile,"max_annual_",IND,"_",YRS,"_",SSP,".tif"), overwrite=TRUE)

#------------------------------------------------------------------------------#
# THI MEAN

g=gc();rm(list=ls())
root <- "//catalogue/WFP_ClimateRiskPr1/"
IND = 'THI'# NDWLD50, NDWS, NTX40, THI, TAI
PRD = 'future' # future
SSP = 'ssp245' # ssp245
YRS = '2030s' # 2050s

ras <- raster(paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/numeric/","mean_annual_",IND,"_",YRS,"_",SSP,".tif"))
ras; summary(ras)
plot(ras)

# No stress: ??? 72
# Mild stress: 73-78
# Moderate stress: 79-89
# Severe stress: ???90	

cat <- c(-Inf, 72,  1,
         72  ,  78, 2,
         78  , 89, 3,
         89  , Inf, 4)

cat_m <- matrix(cat,ncol=3,byrow = T)
r1 <- raster::reclassify(ras, cat_m)
plot(r1)

r2 <- ratify(r1)
a <- levels(r2)[[1]]
a$etiqueta <-c("No stress","Mild stress","Moderate stress","Severe stress")
levels(r2) <- a

outfile <- paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/categoric/"); if(!dir.exists(outfile)){dir.create(outfile, F, T)}
writeRaster(r2, paste0(outfile,"mean_annual_",IND,"_",YRS,"_",SSP,".tif"), overwrite=TRUE)

#------------------------------------------------------------------------------#
# HSH MAX

g=gc();rm(list=ls())
root <- "//catalogue/WFP_ClimateRiskPr1/"
IND = 'HSH'# NDWLD50, NDWS, NTX40, THI, TAI
PRD = 'future' # future
SSP = 'ssp585' # ssp245
YRS = '2050s' # 2050s

ras <- raster(paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/numeric/","max_annual_",IND,"_",YRS,"_",SSP,".tif"))
ras; summary(ras)
plot(ras)

# Mild or no stress: < 25
# Caution: 25-30
# Danger: 30-35
# Extreme danger: > 35

cat <- c(-Inf, 10,  1,
         10  ,  15, 2,
         15  , 25, 3,
         25  , Inf, 4)

cat_m <- matrix(cat,ncol=3,byrow = T)
r1 <- raster::reclassify(ras, cat_m)
plot(r1)

r2 <- ratify(r1)
a <- levels(r2)[[1]]
a$etiqueta <-c("Mild or no stress","Caution","Danger","Extreme danger")
levels(r2) <- a

outfile <- paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/categoric/"); if(!dir.exists(outfile)){dir.create(outfile, F, T)}
writeRaster(r2, paste0(outfile,"max_annual_",IND,"_",YRS,"_",SSP,".tif"), overwrite=TRUE)

#------------------------------------------------------------------------------#
# HSH MEAN

g=gc();rm(list=ls())
root <- "//catalogue/WFP_ClimateRiskPr1/"
IND = 'HSH'# NDWLD50, NDWS, NTX40, THI, TAI
PRD = 'future' # future
SSP = 'ssp585' # ssp245
YRS = '2030s' # 2050s

ras <- raster(paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/numeric/","mean_annual_",IND,"_",YRS,"_",SSP,".tif"))
ras; summary(ras)
plot(ras)

# Mild or no stress: < 25
# Caution: 25-30
# Danger: 30-35
# Extreme danger: > 35

cat <- c(-Inf, 10,  1,
         10  ,  15, 2,
         15  , 25, 3,
         25  , Inf, 4)

cat_m <- matrix(cat,ncol=3,byrow = T)
r1 <- raster::reclassify(ras, cat_m)
plot(r1)

r2 <- ratify(r1)
a <- levels(r2)[[1]]
a$etiqueta <-c("Mild or no stress","Caution","Danger","Extreme danger")
levels(r2) <- a

outfile <- paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/categoric/"); if(!dir.exists(outfile)){dir.create(outfile, F, T)}
writeRaster(r2, paste0(outfile,"mean_annual_",IND,"_",YRS,"_",SSP,".tif"), overwrite=TRUE)

#------------------------------------------------------------------------------#
# HSHB MAX

g=gc();rm(list=ls())
root <- "//catalogue/WFP_ClimateRiskPr1/"
IND = 'HSHB'# NDWLD50, NDWS, NTX40, THI, TAI
PRD = 'future' # future
SSP = 'ssp245' # ssp245
YRS = '2050s' # 2050s

ras <- raster(paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/numeric/","max_annual_",IND,"_",YRS,"_",SSP,".tif"))
ras; summary(ras)
plot(ras)

# Mild or no stress: < 27
# Caution: 27-32
# Extreme caution: 32-41
# Danger & Extreme danger: > 41 (with extreme danger being > 54)

cat <- c(-Inf, 27,  1,
         27  ,  32, 2,
         32  , 41, 3,
         41  , Inf, 4)

cat_m <- matrix(cat,ncol=3,byrow = T)
r1 <- raster::reclassify(ras, cat_m)
plot(r1)

r2 <- ratify(r1)
a <- levels(r2)[[1]]
a$etiqueta <-c("Mild or no stress","Caution","Danger","Extreme danger")
levels(r2) <- a

outfile <- paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/categoric/"); if(!dir.exists(outfile)){dir.create(outfile, F, T)}
writeRaster(r2, paste0(outfile,"max_annual_",IND,"_",YRS,"_",SSP,".tif"), overwrite=TRUE)

#------------------------------------------------------------------------------#
# HSHB MEAN

g=gc();rm(list=ls())
root <- "//catalogue/WFP_ClimateRiskPr1/"
IND = 'HSHB'# NDWLD50, NDWS, NTX40, THI, TAI
PRD = 'future' # future
SSP = 'ssp245' # ssp245
YRS = '2030s' # 2050s

ras <- raster(paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/numeric/","mean_annual_",IND,"_",YRS,"_",SSP,".tif"))
ras; summary(ras)
plot(ras)

# Mild or no stress: < 27
# Caution: 27-32
# Extreme caution: 32-41
# Danger & Extreme danger: > 41 (with extreme danger being > 54)

cat <- c(-Inf, 27,  1,
         27  ,  32, 2,
         32  , 41, 3,
         41  , Inf, 4)

cat_m <- matrix(cat,ncol=3,byrow = T)
r1 <- raster::reclassify(ras, cat_m)
plot(r1)

r2 <- ratify(r1)
a <- levels(r2)[[1]]
a$etiqueta <-c("Mild or no stress","Caution","Danger","Extreme danger")
levels(r2) <- a

outfile <- paste0(root,"9.Results/Indicators/",IND,"/",PRD,"/categoric/"); if(!dir.exists(outfile)){dir.create(outfile, F, T)}
writeRaster(r2, paste0(outfile,"mean_annual_",IND,"_",YRS,"_",SSP,".tif"), overwrite=TRUE)
