# R options
g <- gc(reset = T); rm(list = ls()) # Empty garbage collector
.rs.restartR()                      # Restart R session
options(warn = -1, scipen = 999)    # Remove warning alerts and scientific notation
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, terra, gtools, sf, furrr, future))

OSys <<- Sys.info()[1]
root <<- switch(OSys,
                'Linux'   = '/catalogue/WFP_ClimateRiskPr1',
                'Windows' = '//CATALOGUE/WFP_ClimateRiskPr1')

source('https://raw.githubusercontent.com/CIAT-DAPA/agro-clim-indices/main/_main_functions.R')
calc_SHIMP <- function(TMAX, RH){SHI <- ifelse(TMAX >= 29 & RH > 50, 1, 0); return(SHI)}; calc_SHIMP <- compiler::cmpfun(calc_SHIMP)
calc_THIMP <- function(TMAX, RH){THI <- (1.8 * TMAX + 32) - ((0.55 - 0.0055 * RH) * (1.8 * TMAX - 26.8)); THI_n <- ifelse(test = THI >= 79 & THI < 89 | THI >= 89, yes = 1, no = 0); return(THI_n)}; calc_THIMP <- compiler::cmpfun(calc_THIMP)
calc_HSIMP <- function(TMAX, RH){
  # 2 ~ danger, 3 ~ emergency
  # HSI_n <- ifelse((TMAX <= 26 & RH > 70 |
  #                 TMAX <= 27 & RH >= 40 & RH < 85 |
  #                 TMAX <= 28 & RH < 85 |
  #                 TMAX <= 29 & RH < 60 |
  #                 TMAX <= 30 & RH < 40), 1, 0)
  HSI_n <- ifelse((TMAX <= 27 & RH >= 85 |
                     TMAX <= 28 & RH >= 85 |
                     TMAX <= 29 & RH >= 60 |
                     TMAX <= 30 & RH >= 40 |
                     TMAX > 30), 1, 0)
  return(HSI_n)
}; calc_HSIMP <- compiler::cmpfun(calc_HSIMP)

seasons <- list(Kharif = 7:10, Rabi = c(10:12,1:2), Zaid = 3:6)
seasons <- list(s1=1, s2=2, s3=3, s4=4, s5=5, s6=6, s7=7, s8=8, s9=9, s10=10, s11=11, s12=12) 
PRD <- '2030s' # 2050s
yrs <- c('2021-2040') # 2041, 2060
ssp <- 'ssp585' # ssp585
GCM <- 'EC-Earth3-Veg' # EC-Earth3-Veg MRI-ESM2-0 MPI-ESM1-2-HR

shp_fl <- "//CATALOGUE/WFP_ClimateRiskPr1/1.Data/shps/KEN_GIT/KEN_d.shp"

# Function to compute basic Agro-climatic indices
calc_AgrClm <- function(season = season, shp_fl = shp_fl){
  
  ## ROI: regions of interest
  shp <- terra::vect(shp_fl)
  
  ## Daily files
  # Precipitation
  pr_pth <- paste0(root,'/10.New_Results/bias_corrected/',PRD,'/',iso,'/',ssp,'/',GCM)
  pr_fls <- list.files(pr_pth, pattern = '_pr_', full.names = T)
  pr_fls <- pr_fls[grep(pattern = '*.tif$', x = pr_fls)]
  pr_dts <- strsplit(x = pr_fls, split = '__', fixed = T) %>% purrr::map(2) %>% unlist()
  pr_dts <- strsplit(x = pr_dts, split = '.tif', fixed = T) %>% purrr::map(1) %>% unlist()
  pr_fls <- pr_fls[grep(pattern = '-', x = pr_dts, fixed = T)]
  pr_dts <- pr_dts[grep(pattern = '-', x = pr_dts, fixed = T)]
  wrg_fls <- grep(pattern = 'chirps', x = pr_dts, fixed = T)
  if(length(wrg_fls) > 0){
    pr_fls <- pr_fls[grep(pattern = 'chirps', x = pr_dts, fixed = T, invert = T)]
    pr_dts <- pr_dts[grep(pattern = 'chirps', x = pr_dts, fixed = T, invert = T)]
  }
  pr_dts <- as.Date(pr_dts)
  
  # Maximum temperature
  tmx_pth <- paste0(root,'/10.New_Results/bias_corrected/',PRD,'/',iso,'/',ssp,'/',GCM)
  tmx_fls <- list.files(tmx_pth, pattern = '_tasmax_', full.names = T)
  tmx_fls <- tmx_fls[grep(pattern = '*.tif$', x = tmx_fls)]
  tmx_dts <- strsplit(x = tmx_fls, split = '__', fixed = T) %>% purrr::map(2) %>% unlist()
  tmx_dts <- strsplit(x = tmx_dts, split = '.tif', fixed = T) %>% purrr::map(1) %>% unlist()
  tmx_fls <- tmx_fls[grep(pattern = '-', x = tmx_dts, fixed = T)]
  tmx_dts <- tmx_dts[grep(pattern = '-', x = tmx_dts, fixed = T)]
  wrg_fls <- grep(pattern = 'chirts', x = tmx_dts, fixed = T)
  if(length(wrg_fls) > 0){
    tmx_fls <- tmx_fls[grep(pattern = 'chirts', x = tmx_dts, fixed = T, invert = T)]
    tmx_dts <- tmx_dts[grep(pattern = 'chirts', x = tmx_dts, fixed = T, invert = T)]
  }
  tmx_dts <- as.Date(tmx_dts)
  
  # Minimum temperature
  tmn_pth <- paste0(root,'/10.New_Results/bias_corrected/',PRD,'/',iso,'/',ssp,'/',GCM)
  tmn_fls <- list.files(tmn_pth, pattern = '_tasmin_', full.names = T)
  tmn_fls <- tmn_fls[grep(pattern = '*.tif$', x = tmn_fls)]
  tmn_dts <- strsplit(x = tmn_fls, split = '__', fixed = T) %>% purrr::map(2) %>% unlist()
  tmn_dts <- strsplit(x = tmn_dts, split = '.tif', fixed = T) %>% purrr::map(1) %>% unlist()
  tmn_fls <- tmn_fls[grep(pattern = '-', x = tmn_dts, fixed = T)]
  tmn_dts <- tmn_dts[grep(pattern = '-', x = tmn_dts, fixed = T)]
  wrg_fls <- grep(pattern = 'chirts', x = tmn_dts, fixed = T)
  if(length(wrg_fls) > 0){
    tmn_fls <- tmn_fls[grep(pattern = 'chirts', x = tmn_dts, fixed = T, invert = T)]
    tmn_dts <- tmn_dts[grep(pattern = 'chirts', x = tmn_dts, fixed = T, invert = T)]
  }
  tmn_dts <- as.Date(tmn_dts)
  
  era5Dir <- '//catalogue/Workspace14/WFP_ClimateRiskPr/1.Data/ERA5'
  
  # Solar radiation
  srd_pth <- paste0(era5Dir,'/solar_radiation_flux')
  srd_fls <- gtools::mixedsort(list.files(srd_pth, pattern = '*.nc$', full.names = T))
  srd_dts <- strsplit(x = srd_fls, split = 'glob-agric_AgERA5_', fixed = T) %>% purrr::map(2) %>% unlist()
  srd_dts <- strsplit(x = srd_dts, split = '_final-v1.0.nc', fixed = T) %>% purrr::map(1) %>% unlist()
  srd_dts <- as.Date(srd_dts, "%Y%m%d")
  
  # Filtering days within the season
  yrs <- lubridate::year(tmx_dts)
  yrs <- names(table(yrs)[table(yrs) %in% 365:366])
  
  avl_dts <- base::intersect(as.character(pr_dts), as.character(tmx_dts))
  avl_dts <- base::intersect(avl_dts, as.character(tmn_dts))
  avl_dts <- base::intersect(avl_dts, as.character(srd_dts))
  avl_dts <- as.Date(avl_dts)
  
  pr_fls <- pr_fls[pr_dts %in% avl_dts]
  tmx_fls <- tmx_fls[tmx_dts %in% avl_dts]
  tmn_fls <- tmn_fls[tmn_dts %in% avl_dts]
  srd_fls <- srd_fls[srd_dts %in% avl_dts]
  
  pr_dts <- pr_dts[pr_dts %in% avl_dts]
  tmx_dts <- tmx_dts[tmx_dts %in% avl_dts]
  tmn_dts <- tmn_dts[tmn_dts %in% avl_dts]
  srd_dts <- srd_dts[srd_dts %in% avl_dts]
  
  if(length(season) < 12){
    cnd <- lubridate::month(tmx_dts) %in% season # Days within the season
    yrs_dts <<- split(tmx_dts[cnd],cumsum(c(1,diff(tmx_dts[cnd])!=1)))
  } else {
    yrs <- lubridate::year(tmx_dts)
    grp <- with(rle(yrs), rep(seq_along(values), lengths))
    yrs_dts <<- split(tmx_dts, grp)
  }
  
  
  
  cat('..... Computing: AT. Average temperature.\n')
  AT <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      # tav <- terra::rast(tav_fls[tav_dts %in% yrs_dts[[i]]])
      tmx <- terra::rast(tmx_fls[tmx_dts %in% yrs_dts[[i]]])
      tmn <- terra::rast(tmn_fls[tmn_dts %in% yrs_dts[[i]]])
      # Tav
      tav <- (tmn + tmx)/2
      tav <- tav %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      tav <- tav
      AT <- terra::app(x = tav, fun = function(x){ y = mean(x, na.rm = T); return(y) })
      names(AT) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
      return(AT)
    }) %>% terra::rast()
  AT <- AT %>% terra::mask(shp)
  
  cat('..... Computing: TR. Total rainfall.\n')
  TR <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      prc <- terra::rast(pr_fls[pr_dts %in% yrs_dts[[i]]])
      prc <- prc %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      prc[prc == -9999] <- NA
      TR <- terra::app(x = prc, fun = function(x){ y = sum(x, na.rm = T); return(y) })
      names(TR) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
      return(TR)
    }) %>% terra::rast()
  TR <- TR %>% terra::mask(shp)
  
  # cat('..... Computing: NTx35. Number of days with Tmax above 35C.\n')
  # NTx35 <- 1:length(yrs_dts) %>%
  #   purrr::map(.f = function(i){
  #     tmx <- terra::rast(tmx_fls[tmx_dts %in% yrs_dts[[i]]])
  #     tmx <- tmx %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
  #     tmx <- tmx - 273.15
  #     NTx35 <- terra::app(x = tmx, fun = function(x){ y = calc_htsCMP(tmax = x, t_thresh = 35); return(y) })
  #     names(NTx35) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
  #     return(NTx35)
  #   }) %>% terra::rast()
  # NTx35 <- NTx35 %>% terra::mask(shp)
  
  cat('..... Computing: CDD. Maximum number of consecutive dry days.\n')
  CDD <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      prc <- terra::rast(pr_fls[pr_dts %in% yrs_dts[[i]]])
      prc <- prc %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      prc[prc == -9999] <- NA
      CDD <- terra::app(x = prc, fun = function(x){ y = calc_cddCMP(PREC = x, p_thresh = 1); return(y) })
      names(CDD) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
      return(CDD)
    }) %>% terra::rast()
  CDD <- CDD %>% terra::mask(shp)
  
  cat('..... Computing: P5D. Rolling precipitation average of 5 days.\n')
  P5D <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      prc <- terra::rast(pr_fls[pr_dts %in% yrs_dts[[i]]])
      prc <- prc %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      prc[prc == -9999] <- NA
      P5D <- terra::app(x = prc, fun = function(x){ y = calc_p5dCMP(PREC = x); return(y) })
      names(P5D) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
      return(P5D)
    }) %>% terra::rast()
  P5D <- P5D %>% terra::mask(shp)
  
  cat('..... Computing: P95. Percentile 95% of daily precipitation.\n')
  P95 <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      prc <- terra::rast(pr_fls[pr_dts %in% yrs_dts[[i]]])
      prc <- prc %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      prc[prc == -9999] <- NA
      P95 <- terra::app(x = prc, fun = function(x){ y = calc_p95CMP(PREC = x); return(y) })
      names(P95) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
      return(P95)
    }) %>% terra::rast()
  P95 <- P95 %>% terra::mask(shp)
  
  # cat('..... Computing: CSDI. Cold spell duration Index.\n')
  # CSDI <- 1:length(yrs_dts) %>%
  #   purrr::map(.f = function(i){
  #     tmn <- terra::rast(tmn_fls[tmn_dts %in% yrs_dts[[i]]])
  #     tmn <- tmn %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
  #     tmn <- tmn - 273.15
  #     CSDI <- terra::app(x = tmn, fun = function(x){ y = calc_csdiMP(TMIN = x); return(y) })
  #     names(CSDI) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
  #     return(CSDI)
  #   }) %>% terra::rast()
  # CSDI <- CSDI %>% terra::mask(shp)
  
  # cat('..... Computing: SHI. Number of days with maximum temperature > 29C and relative humidity > 50% (sheep)\n')
  # SHI <- 1:length(yrs_dts) %>%
  #   purrr::map(.f = function(i){
  #     tmx <- terra::rast(tmx_fls[tmx_dts %in% yrs_dts[[i]]])
  #     tmx <- tmx %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
  #     tmx <- tmx - 273.15
  #     rhy <- terra::rast(rhy_fls[rhy_dts %in% yrs_dts[[i]]])
  #     rhy <- rhy %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
  #     SHI <- terra::lapp(x = terra::sds(tmx, rhy), fun = calc_SHIMP)
  #     SHI <- sum(SHI)
  #     names(SHI) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
  #     return(SHI)
  #   }) %>% terra::rast()
  # SHI <- SHI %>% terra::mask(shp)
  
  # cat('..... Computing: THI. Thermal humidity index (cattle).\n')
  # THI <- 1:length(yrs_dts) %>%
  #   purrr::map(.f = function(i){
  #     tmx <- terra::rast(tmx_fls[tmx_dts %in% yrs_dts[[i]]])
  #     tmx <- tmx %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
  #     tmx <- tmx - 273.15
  #     rhy <- terra::rast(rhy_fls[rhy_dts %in% yrs_dts[[i]]])
  #     rhy <- rhy %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
  #     THI <- terra::lapp(x = terra::sds(tmx, rhy), fun = calc_THIMP)
  #     THI <- sum(THI)/terra::nlyr(tmx)
  #     names(THI) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
  #     return(THI)
  #   }) %>% terra::rast()
  # THI <- THI %>% terra::mask(shp)
  
  # cat('..... Computing: HSI. Heat stress index (pig).\n')
  # HSI <- 1:length(yrs_dts) %>%
  #   purrr::map(.f = function(i){
  #     tmx <- terra::rast(tmx_fls[tmx_dts %in% yrs_dts[[i]]])
  #     tmx <- tmx %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
  #     tmx <- tmx - 273.15
  #     rhy <- terra::rast(rhy_fls[rhy_dts %in% yrs_dts[[i]]])
  #     rhy <- rhy %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
  #     HSI <- terra::lapp(x = terra::sds(tmx, rhy), fun = calc_HSIMP)
  #     HSI <- sum(HSI)/terra::nlyr(tmx)
  #     names(HSI) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
  #     return(HSI)
  #   }) %>% terra::rast()
  # HSI <- HSI %>% terra::mask(shp)
  
  cat('..... End.\n')
  return(list(AT    = AT,
              TR    = TR,
              # NTx35 = NTx35,
              CDD   = CDD,
              P5D   = P5D,
              P95   = P95
              # CSDI  = CSDI,
              # SHI   = SHI,
              # THI   = THI,
              # HSI   = HSI
              ))
  
}

# Loop through seasons
1:length(seasons) %>%
  purrr::map(.f = function(s){
    cat(paste0('Processing season ',names(seasons)[s],':\n'))
    # Indices calculation
    indices <- calc_AgrClm(seasons[[s]], shp_fl)
    # Load 5 km raster template
    tmp <- terra::rast('//catalogue/Workspace14/WFP_ClimateRiskPr/1.Data/chirps-v2.0.2020.01.01.tif')
    shp <- terra::vect(shp_fl)
    tmp <- tmp %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
    tmp[!is.na(tmp)] <- 1
    # Indices resampling
    indices <- indices %>% purrr::map(.f = function(r){r <- r %>% terra::resample(x = ., y = tmp) %>% terra::mask(shp); return(r)})
    # Saving results
    out <- paste0('D:/india/results/',names(seasons)[s]); if(!dir.exists(out)){dir.create(out,F,T)}
    1:length(names(indices)) %>%
      purrr::map(.f = function(j){
        terra::writeRaster(x = indices[[j]], filename = paste0(out,'/',names(indices)[j],'.tif'), overwrite = T)
      })
    return(cat('Process finished successfully!\n'))
  })