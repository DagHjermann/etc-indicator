
get_pdata <- function(data, 
                            referenceyear = NULL){
  
  run_datasel <- list(
    with(data, year >= 1992 & sitegroup %in% 1),
    with(data, year >= 2000 & sitegroup %in% c(1,2,3)),
    with(data, year >= 2000 & sitegroup %in% c(1,2)),
    with(data, year >= 2000 & sitegroup %in% c(1))
  )
  names(run_datasel) <- c("Since 1992, site group 1",
                          "Since 2000, site group 1-3",
                          "Since 2000, site group 1-2",
                          "Since 2000, site group 1")
  
  
  #
  # Run gam ----
  #
  
  mod1 <- gam(log_no3 ~ s(x, y, year, k=600), data = data[run_datasel[[1]],])
  mod2 <- gam(log_no3 ~ s(x, y, year, k=600), data = data[run_datasel[[2]],])
  mod3 <- gam(log_no3 ~ s(x, y, year, k=600), data = data[run_datasel[[3]],])
  mod4 <- gam(log_no3 ~ s(x, y, year, k=600), data = data[run_datasel[[4]],])

  mod <- list(mod1, mod2, mod3, mod4)
  names(mod) <- names(run_datasel)
  rm(mod1, mod2, mod3, mod4)
  
  #
  # Grids for prediction ----
  #
  
  # dat[.,] contains only grid cells with data
  # so pdata_space_byruns (list of 4) will also only contain grid cells with data
  pdata_space_byruns <- purrr::map(
    run_datasel,
    ~distinct(data[.,], grid_x, grid_y)
  )
  
  expand_grid_with_years <-function(griddata, years){
    map_dfr(years, 
            ~mutate(griddata, year = .) %>% 
              rename(x = grid_x, y = grid_y))
  } 
  
  # Data for all grid cells with data, and all years 
  #  (note: all years for all grid cells, even when that cell x year combination is lacking)
  pdata_byruns <- purrr::map2(
    pdata_space_byruns,
    list(1992:2020, 2000:2020, 2000:2020, 2000:2020),
    expand_grid_with_years
  )
  
  
  #
  # Calculate predicted values ----
  #

  pred <- purrr::map2(
    mod,
    pdata_byruns,
    ~ predict(.x, newdata = .y, se.fit = TRUE)
  )
  
  pdata <- purrr::map2(
    pred,
    pdata_byruns,
    ~ mutate(
      .y, 
      log_no3 = as.numeric(.x$fit))
  )
  # To save a little memory (from 1.8 MB to 1.4MB for the 4 runs)
  pdata$x <- as.integer(pdata$x)
  pdata$y <- as.integer(pdata$y)
  
  pdata

}


if (FALSE){
  
  # TEST ----
  
  library(mgcv)        # gam 
  library(dplyr)       # tidyverse 
  library(ggplot2)     # ggplot, only needed for this test 
  
  dat <- readRDS("Input_data/WISE_GWNO3_sel3.rda") %>%
    mutate(log_no3 = log10(no3))
  
  dx <- dy <- 50
  dat <- dat %>%
    mutate(
      grid_x = dx*round(x/dx),
      grid_y = dy*round(y/dy))
  
  
  sites <- unique(dat$site) 
  length(sites)
  sites_sel <- sample(sites, size = 1000, replace = TRUE)
  
  dat_sel <- dat %>%
    filter(site %in% sites_sel)
  
  # Data for model predictions  
  # pdata_for_gam <- readRDS("Data/108_pdata_for_gam.rds")
  
  # debugonce(get_pdata)
  pdata <- get_pdata(dat_sel)
  
  str(pdata[[1]])
  str(pdata[[2]])
  
}


get_pdata_ran <- function(data, waterbodies, i){
  
  set.seed(i)
  
  waterbodies_sel <- sample(waterbodies, size = length(waterbodies), replace = TRUE)
  length(waterbodies_sel)
  table(table(waterbodies_sel))

  row <- map(waterbodies_sel, ~which(data$waterbody %in% .)) %>% flatten_dbl
  # row
  # length(row)
  
  dat_sel <- data[row,]
  
  result <- get_pdata(dat_sel)  
  
  fn <- paste0("Data/109_bootstraps/109_sample_", sprintf("%03.f", i), ".rds")
  saveRDS(result, fn)
  
  result

}

