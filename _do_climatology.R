### Do climathology graph - improved version
### A. Esquivel, H. Achicanoy
### CIAT, 2020

# R options
options(warn = -1, scipen = 999)

# Load libraries
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, lubridate, fst))
root <- '//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr'



do_climatology <- function(...)
{
  
  site <- fst::read_fst("//dapadfs.cgiarad.org/workspace_cluster_13/WFP_ClimateRiskPr/1.Data/observed_data/HTI/HTI.fst")
  site$id1 <- site$id
  site$year <- NULL
  site <- site %>%
    tidyr::nest(Climate = c('id','date','prec','tmean','tmax','tmin','srad','wind','rh')) %>%
    dplyr::rename(id = 'id1') %>%
    dplyr::select(id, dplyr::everything(.))
  
  if(nrow(site) > 100){
    set.seed(1235)
    smpl <- sample(x = 1:nrow(site), size = 100, replace = F) %>% sort
  } else {
    smpl <- 1:nrow(site)
  }
  
  all_clmtlgy <- 1:length(smpl) %>%
    purrr::map(.f = function(i){
      clmtlgy <- site[smpl[i],] %>%
        dplyr::pull('Climate') %>%
        .[[1]] %>% 
        dplyr::mutate(Year  = lubridate::year(lubridate::as_date(date)),
                      Month = lubridate::month(lubridate::as_date(date))) %>%
        dplyr::group_by(Year, Month) %>%
        dplyr::summarise(Prec = sum(prec, na.rm = T),
                         Tmin = mean(tmin, na.rm = T),
                         Tmax = mean(tmax, na.rm = T)) %>%
        dplyr::group_by(Month) %>%
        dplyr::summarise(Prec = mean(Prec, na.rm = T),
                         Tmin = mean(Tmin, na.rm = T),
                         Tmax = mean(Tmax, na.rm = T))
      return(clmtlgy)
    }) %>%
    dplyr::bind_rows()
  
  avrgs <- all_clmtlgy %>%
    dplyr::group_by(Month) %>%
    dplyr::summarise(Prec = mean(Prec, na.rm = T),
                     Tmin = mean(Tmin, na.rm = T),
                     Tmax = mean(Tmax, na.rm = T))
  
gg1 =  avrgs %>%
    ggplot2::ggplot(aes(x = Month, y = Prec)) +
    ggplot2::geom_bar(stat="identity", fill = 'lightblue') +
    ggplot2::xlab('Month') +
    ggplot2::ylab('Precipitation (mm)') +
    ggplot2::xlim(0,13)+
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(breaks = 1:12) +
    ggplot2::labs(title = "Haiti", subtitle = "Climatology")
  
}


# annotate("rect", xmin = 4, xmax = 6, ymin = 0, ymax = 200, 
# alpha = 0.5, fill = "pink") + 
  # annotate("rect", xmin = 6, xmax = 8, ymin = 0, ymax = 200, 
# alpha = 0.5, fill = "turquoise") + 
  # annotate("text", x = 4, y = 210, 
# label = "Planting") +
  # annotate("text", x = 6, y = 210, 
# label = "Harvest")
