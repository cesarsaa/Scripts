# CHIRTS
# By: H. Achicanoy, 2022

# R options
g <- gc(reset = T); rm(list = ls()) # Empty garbage collector
.rs.restartR()                      # Restart R session
options(warn = -1, scipen = 999)    # Remove warning alerts and scientific notation
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(raster, geodata, terra, imager, lubridate))

# Time frame
ini <- as.Date('1995-01-01')
end <- as.Date(Sys.time())
dts <- seq(from = ini, to = end, by = 'day'); rm(ini, end)

# Output directory
Out  <- '//CATALOGUE/Workspace14/WFP_ClimateRiskPr/1.Data/Chirts/Rh'; if(!dir.exists(Out)){dir.create(Out,F,T)}

# Main function
chirps2gray <- function(date = dts[1]){
  # CHIRTS base URL
  chrts <- 'http://data.chc.ucsb.edu/products/CHIRTSdaily/v1.0/global_tifs_p05/RHum/'
  # Get day and year
  Day  <- date
  Year <- lubridate::year(Day)
  # Target file
  tfile <- paste0(chrts,Year,'/RH.',gsub('-','.',Day,fixed=T),'.tif')
  # Destination file
  dfile <- paste0(Out,'/',basename(tfile))
  # Raster file
  rfile <- gsub('.tif','',dfile,fixed = T)
  
  if(!file.exists(rfile)){
    tryCatch(expr = {
      # utils::download.file(url = tfile, destfile = dfile)
      utils::download.file(url = tfile,destfile = dfile,method ='curl')
    },
    error = function(e){
      cat(paste0(basename(tfile),' failed.\n'))
    })
  }
}

# Loop through the dates
dts %>% purrr::map(.f = chirps2gray)
