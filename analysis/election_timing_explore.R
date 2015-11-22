# ---------------------------------------------------------------------------- #
# Eurostat revisions and election timing explore
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(lubridate)
library(countrycode)
library(ggplot2)

# Set working directory. Change as needed
setwd('/git_repositories/eurostat_revisions/')

# Load revisions data
revisions <- import('data_cleaning/comb_cumulative.csv')

# Load election timing data
timing <- import('https://raw.githubusercontent.com/christophergandrud/yrcurnt_corrected/master/data/yrcurnt_corrected.csv') %>% select(-V5)

# Load FinStress and create annual averages
FinStress <- rio::import("http://bit.ly/1LFEnhM")

# Annual data --------
FinStress$year <- year(FinStress$date)

finstress <- FinStress %>% select(iso2c, date, year, C1_ma) %>%
    rename(finstress = C1_ma)

# Annual mean
finstress_yr_mean <- finstress %>% group_by(iso2c, year) %>%
    summarise(finstress_mean = mean(finstress, na.rm = T))
finstress_yr_mean$country <- countrycode(finstress_yr_mean$iso2c, 
                                         origin = 'iso2c', 
                                         destination = 'country.name')

# Combine
comb <- merge(timing, revisions, by = c('country', 'year'))
comb <- merge(comb, finstress_yr_mean, by = c('country', 'year'))
    
# Plot debt revisions
debt <- comb %>% filter(component == 'debt')
ggplot(debt, aes(yrcurnt_corrected, cum_revision)) +
    geom_point() +
    stat_smooth(method = 'lm', se = F) +
    facet_wrap(~ country) +
    theme_bw()

m1 <- lm(cum_revision ~ yrcurnt_corrected + 
             as.factor(country) + as.factor(years_since_original), data = debt)

m1.2 <- lm(cum_revision ~ yrcurnt_corrected + finstress_mean +
               as.factor(country) + as.factor(years_since_original), data = debt)


deficit <- comb %>% filter(component == 'deficit')
m2 <- lm(cum_revision ~ yrcurnt_corrected + 
         as.factor(country) + as.factor(years_since_original), data = deficit)
