# R options
g <- gc(reset = T); rm(list = ls()) # Empty garbage collector
options(warn = -1, scipen = 999)    # Remove warning alerts and scientific notation
suppressMessages(if(!require(pacman)){install.packages('pacman');library(pacman)} else {library(pacman)})
suppressMessages(pacman::p_load(tidyverse, terra))

root <- '//CATALOGUE/WFP_ClimateRiskPr1/10.New_Results/bias_corrected/2030s/AGO/ssp245/MPI-ESM1-2-HR'

# Source
pr_fls <- list.files(root, pattern = '*chirps*.*.tif$', full.names = T)
basename(pr_fls)

# Target
pr_trg <- gsub(pattern = 'chirps-v2.0.', replacement = '', x = basename(pr_fls))
pr_trg <- gsub(pattern = '.tif', replacement = '', x = pr_trg, fixed = T)
pr_trg <- gsub(pattern = '.', replacement = '-', x = pr_trg, fixed = T)
pr_trg <- paste0(pr_trg, '.tif')

file.rename(from = pr_fls, to = paste0(root,'/',pr_trg))
