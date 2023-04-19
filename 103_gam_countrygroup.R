
#
# Script for running in the "background jobs" pane (of RStudio), or "sourced"  
#
# - Runs GAM for selected countries, time and the k parameter of GAM
# - Automatically saved as 'rds' file in in "Data" 
# - Refuses to run GAM if the file already exists

library(mgcv)        # gam
library(dplyr)       # tidyverse 
library(forcats)     # tidyverse
library(tictoc)

#
# Data
#

dat <- readRDS("Input_data/WISE_GWNO3_sel2.rda") %>%
  mutate(log_no3 = log10(no3))

#
# Select country/ies
#
# country_text <- "FR"
country_text <- "DE, DK, BE, AT, LI, SI, NL, LU, CH"
countries <- strsplit(country_text, ", ")[[1]]
# countries

dat2 <- dat %>% filter(countryCode %in% countries)

#
# Select years
#

min_year <- 2014
min_year <- 1990
k <- 100
k <- 400

dat3 <- dat2 %>% filter(year >= min_year)
# range(dat3$year)

# 
# Filename
#

filename <- paste0("103_", 
                   paste(countries, collapse = "-"), 
                   "_yr", min_year, "_k", k, ".rds")

existing_filenames <- dir("Data", "103")


#
# GAM
# - only if file doesn't already exist
#

if (filename %in% existing_filenames){
  stop("The file ", sQuote(filename), " already exists. Rename or delete it if you want to make another run.\n\n")
} else {
  t1 <- 
  # Run model
  tic()
  mod <- gam(log_no3 ~ s(x, y, year, k = k), data = dat3, method = "REML")
  toc()
  # Save file
  saveRDS(mod, paste0("Data/", filename))
}


