# R options
g <- gc(reset = T); rm(list = ls()) # Empty garbage collector
.rs.restartR()                      # Restart R session
options(warn = -1, scipen = 999)    # Remove warning alerts and scientific notation
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, terra, lubridate, foreach))

# 
OSys <<- Sys.info()[1]
root <<- switch(OSys,
                'Linux'   = '/catalogue/WFP_ClimateRiskPr',
                'Windows' = '//CATALOGUE/WFP_ClimateRiskPr1')
# 
# outfile <- paste0(root, '/1.Data/shps/',iso); if(!dir.exists(outfile)){dir.create(outfile, F, T)}
AFR <- raster::shapefile("//CATALOGUE/Workspace14/WFP_ClimateRiskPr/1.Data/shps/world_shapefile/Africa/africa.shp")
iso <- unique(c(AFR@data$ISO3))
# iso <- c('PHL', 'GTM')
iso

# 
lowest_gadm <- function(iso, out){
  out <- paste0(root, '/1.Data/shps/',iso); if(!dir.exists(out)){dir.create(out, F, T)}
  suppressMessages(if(!require(pacman)){install.packages('pacman');library(pacman)})
  suppressMessages(pacman::p_load(geodata,terra,sf,raster))
  levels <- 1
  for(i in 1:length(iso)){
    tryCatch(expr = {
      shp <- getData(name="GADM", country=iso[i], level=levels, path = tempdir())
      break
    },
    error = function(e){
      cat(paste0("Getting GADM level ",levels[i]," failed... Trying a higher level\n"))
      return("\n")
    })
  }
  if(!is.null(out)){
    shp <- sf::st_as_sf(shp)
    sf::st_write(shp, paste0(out,'/',iso,'_GADM1.shp'))
  }
  shp <- as(shp, 'Spatial')
  return(shp)
}

# 
parallel::detectCores()
cl <- parallel::makeCluster(1)
doSNOW::registerDoSNOW(cl)

library(foreach)

rsl <- foreach(i =  1:length(iso), .verbose = TRUE) %dopar% {
  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(tidyverse, terra, lubridate, raster, rgdal, rgeos, 
                                  stringr, sf, foreach, doSNOW, geodata))
  iso1 = iso[i]
  cat(paste0('Processing: ', iso1))
  lowest_gadm(iso=iso1, out)
}

parallel::stopCluster(cl)
# lowest_gadm(iso, outfile)




