# ---------------------------------------------------------------------------- #
# Eurostat revisions and election timing explore
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(lubridate)
library(countrycode)
library(ggplot2)
library(devtools)

# Set working directory. Change as needed
setwd('/git_repositories/eurostat_revisions/')

# Load plot function
devtools::source_gist('d270ff55c2ca26286e90')

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

m1_1 <- lm(cum_revision ~ yrcurnt_corrected + years_since_original +
             as.factor(country), data = debt)

m1_2 <- lm(cum_revision ~ yrcurnt_corrected + finstress_mean + 
               years_since_original +
               as.factor(country), data = debt)

m1_3 <- lm(cum_revision ~ yrcurnt_corrected * finstress_mean + 
               years_since_original +
               as.factor(country), data = debt)


deficit <- comb %>% filter(component == 'deficit')
m2_1 <- lm(cum_revision ~ yrcurnt_corrected + years_since_original +
         as.factor(country), data = deficit)

m2_2 <- lm(cum_revision ~ yrcurnt_corrected + finstress_mean + 
               years_since_original +
               as.factor(country), data = deficit)


## Plot marginal effect
plot_me(m1_3, term1 = 'yrcurnt_corrected', term2 = 'finstress_mean',
        fitted2 = seq(0.2, 0.75, by = 0.05)) +
    xlab('\nAnnual FinStress Mean') + 
    ylab('Marginal Effect of Election Timing\n') + ggtitle('Debt Revisions\n')
