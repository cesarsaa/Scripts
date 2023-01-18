# R options
g <- gc(reset = T); rm(list = ls()) # Empty garbage collector
# .rs.restartR()                      # Restart R session
options(warn = -1, scipen = 999)    # Remove warning alerts and scientific notation
suppressMessages(if(!require(pacman)){install.packages('pacman');library(pacman)} else {library(pacman)})
suppressMessages(pacman::p_load(tidyverse, terra, lubridate))

root <- '//catalogue/WFP_ClimateRiskPr1'
calendar <- terra::rast(paste0(root,'/7.Results/crop_calendars_5km.tif'))

country_path <- paste0(root,'/7.Results/Africa')
iso <- list.dirs(path = country_path, full.names = F, recursive = F)
country_path <- paste0(country_path,'/',iso)

# Heat stress maize index
1:length(country_path) %>%
  purrr::map(.f = function(i){
    # List, sort, and load monthly indices
    hsm <- terra::rast(gtools::mixedsort(list.files(path = country_path[i], pattern = 'NTx28', full.names = T)))
    terra::time(hsm) <- seq(from = as.Date('1995-01-01'), to = as.Date('2014-12-01'), by = 'month')
    
    # Obtain country calendar
    country_calendar <- calendar %>% terra::crop(terra::ext(hsm)) %>% terra::mask(hsm[[1]])
    ini <- median(terra::values(country_calendar[[1]]), na.rm = T)
    end <- median(terra::values(country_calendar[[2]]), na.rm = T)
    if(ini > end){season <- c(ini:12,1:end)} else {season <- ini:end}
    
    # Flowering month: middle month of the season
    if((length(season) %% 2) == 0){season <- season[-length(season)]}
    flw_mth <- season[ceiling(length(season)/2)]
    
    # Identify the flowering month within annual results and compute the multi-annual average
    cnd <- which(lubridate::month(terra::time(hsm)) %in% flw_mth)
    heat_stress_maize <- terra::app(x = hsm[[cnd]], mean, na.rm = T)
    terra::writeRaster(x = heat_stress_maize, filename = paste0(root,'/8.Final_results_current/country/00_mean_general/',iso[i],'/NTx28.tif'), overwrite = T)
    return(cat(paste0(iso[i],' done.\n')))
  })
