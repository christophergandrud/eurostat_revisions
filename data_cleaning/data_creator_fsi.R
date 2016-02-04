# --------------------------------------------------------------------------- #
# Clean Klaus et al. (2015) FSI financial market stress indicator
# Downloaded from https://sites.google.com/site/thibautduprey/research/crisesdating
# MIT LICENSE
# --------------------------------------------------------------------------- #


library(rio)
library(repmis)
library(tidyr)
library(dplyr)
library(countrycode)
library(lubridate)
library(ggplot2)
library(DataCombine)

possibles <- '/git_repositories/eurostat_revisions//'
set_valid_wd(possibles)

# FSI from Klaus et al. (2015) -----
fsi <- rio::import('data_cleaning/raw/Duprey et al - Crises Dating - dataset.xls',
                   sheet = 3)

countries_count <- ncol(fsi)
fsi <- fsi %>% gather(iso2c, fsi, 2:countries_count)

fsi$iso2c <- gsub('_fsi', '', fsi$iso2c)
fsi$iso2c <- countrycode(fsi$iso2c, origin = 'iso2c', destination = 'iso2c', 
                         warn = T)
names(fsi) <- c('date', 'iso2c', 'fsi')
fsi$date <- ymd(fsi$date)
fsi$fsi[fsi$fsi == -1] <- NA

# Create annual FSI scores
fsi$year <- year(fsi$date)

fsi <- subset(fsi, !is.na(fsi))

fsi <- fsi %>% group_by(iso2c, year) %>%
        summarise(fsi_annual_mean = mean(fsi, na.rm = T))

fsi$country <- countrycode(fsi$iso2c, origin = 'iso2c', 
                           destination = 'country.name')

fsi <- fsi[, -1]

export(fsi, file = 'data_cleaning/raw/fsi_mean.csv')
