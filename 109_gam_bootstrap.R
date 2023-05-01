

#
# Packages ----
#

library(mgcv)        # gam 
library(dplyr)       # tidyverse 
library(ggplot2)     # tidyverse
library(viridis)     # for scale_color_viridis  
library(forcats)     # tidyverse#
library(purrr)       # tidyverse
library(glue)        # tidyverse, easier alternative to paste0
library(tictoc)      # tic() and toc() for timing   
library(terra)       # spatial functions  

source("109_gam_bootstrap_functions.R")

#
# Read data ----
#

# Data for analysis  

# Grid cell resolution (in km)
dx <- dy <- 50

dat <- readRDS("Input_data/WISE_GWNO3_sel3.rda") %>%
  mutate(log_no3 = log10(no3)) %>%
  # Add gridcell to data  
  mutate(
    grid_x = dx*round(x/dx),
    grid_y = dy*round(y/dy))

wbs <- unique(dat$waterbody) 
length(wbs)

set.seed(42)

for (i in 1:1){
  
  # i <- 1
  tic()
  
  wbs_sel <- sample(wbs, size = length(wbs), replace = TRUE)
  length(wbs_sel)
  table(table(wbs_sel))
  
  # First try - but this only filters away non-selected wbs,
  #   it doesn't give any waterbodies twice or several times  
  # row <- which(dat$waterbody %in% wbs_sel)
  # dat_sel <- dat[row,]
  
  row <- map(wbs_sel, ~which(dat$waterbody %in% .)) %>% flatten_dbl
  # row
  # length(row)

  dat_sel <- dat[row,]
  nrow(dat_sel)

  # Data for model predictions  
  # pdata_for_gam <- readRDS("Data/108_pdata_for_gam.rds")
  
  # debugonce(get_year_effect)
  pdata <- get_pdata(dat_sel)  
  
  fn <- paste0("Data/109_bootstraps/109_sample_", sprintf("%03.f", i), ".rds")
  #saveRDS(pdata, fn)
  
  toc()
  
}


#
# Using function 'get_pdata_ran'
# - based on the data above  
#
test <- purrr::map(1:2, ~get_pdata_ran(dat, wbs, .))

library(furrr)
# Check number cores
future::availableCores()

# Set a "plan" for how many cores to use:
plan(multisession, workers = 3)

# Then replace map() by future_map(), map_dfr() by future_map_dfr() etc. 
pdata_list <- future_map(1:100, ~get_pdata_ran(dat, wbs, .))


