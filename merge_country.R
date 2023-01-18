# unir Rasters
### --------------------------------------------------------------------------------- ##
## --------------------------------------------------------------------------------- ##
# R options
g <- gc(reset = T); rm(list = ls()) # Empty garbage collector
# .rs.restartR()                      # Restart R session
options(warn = -1, scipen = 999)    # Remove warning alerts and scientific notation
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, terra, lubridate,foreach,fields,gtools,raster))

prd <- '2050s'
ssp <- 'ssp585'
gcm <- 'ACCESS-ESM1-5' # EC-Earth3-Veg, MRI-ESM2-0, MPI-ESM1-2-HR, INM-CM5-0 ACCESS-ESM1-5

union  <- function(iso,ind, fol){ 
  root <- "//catalogue/WFP_ClimateRiskPr1/7.Results/KEN_Githu/"
  path <- paste0(root, "Future","/",prd,"/",ssp,"/",gcm,"/",iso,"_", ind,".tif")
  
  r <- lapply(1:length(path) , function(i){
    ras <- raster::stack(path[i])
    return(ras)
  })
  
  ras <- terra::merge(r[[1]],r[[2]],r[[3]])
  
  outfile <- paste0(root, "indices/",prd,"/"); if(!dir.exists(outfile)){dir.create(outfile, F, T)}
  writeRaster(ras, paste0(outfile,ind,"_",prd,"_",ssp,"_",gcm,".tif"), overwrite = T)  
  
}

folder <- list.files("//catalogue/WFP_ClimateRiskPr1/7.Results/KEN_Githu/indices/")
iso <- c("KEN","ETH","SSD")
indicadores<- c("TR","AT","CDD","P5D","P95")


lapply (1:length(indicadores), function(i){
  cat(paste0("Procesando pais :::: ",indicadores[i], "---- > posicion : ",i , "\n" ))
  union(ind = indicadores[i], iso = iso, fol = folder[i])
})
