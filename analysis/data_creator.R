# ---------------------------------------------------------------------------- #
# Eurostat revisions and election timing data creator
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(lubridate)
library(countrycode)
library(ggplot2)
library(devtools)
library(stargazer)
library(DataCombine)
library(stringr)
library(tidyr)
library(WDI)

# Set working directory. Change as needed
setwd('/git_repositories/eurostat_revisions/')

# Load revisions data
revisions <- import('data_cleaning/comb_cumulative.csv')

revisions$country <- countrycode(revisions$country, 
                                 origin = 'country.name',
                                 destination = 'country.name')

# Load election timing data
timing <- import('https://raw.githubusercontent.com/christophergandrud/yrcurnt_corrected/master/data/yrcurnt_original_corrected.csv') %>% 
                select(-yrcurnt)

# Creat election year dummy
timing$elect_dummy <- 0
timing$elect_dummy[timing$yrcurnt_corrected == 0] <- 1

timing$country <- countrycode(timing$iso2c, 
                              origin = 'iso2c', 
                              destination = 'country.name')

timing <- timing %>% select(-iso2c)

# Load annual average FSI ---------
fsi <- import('data_cleaning/raw/fsi_mean.csv')

# Load FinStress and create annual averages --------
FinStress <- rio::import("https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/results_kpca_rescaled.csv")

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

## Import Endogenous Election Indicator from Hallerberg and Wehner ------
endog_election <- import('data_cleaning/raw/endogenous_elections.csv') %>%
    select(country, year, `Elect-endogHW`, `Elect-predHW`) %>%
    rename(endog_electionHW = `Elect-endogHW`) %>%
    rename(endog_predHW = `Elect-predHW`)

endog_election <- endog_election[!duplicated(endog_election[, 1:2]), ]

endog_election$country <- countrycode(endog_election$country, 
                                      origin = 'country.name',
                                      destination = 'country.name')

endog_election$endog_3[endog_election$endog_electionHW == 1] <- 1 
endog_election$endog_3[endog_election$endog_predHW == 1] <- 2
endog_election$endog_3[endog_election$endog_predHW == 0 & 
                           endog_election$endog_electionHW == 0] <- 3

FindDups(endog_election, c('country', 'year'))

## Debt figures from the World Bank Development Indicators ----
debt_raw <- WDI(indicator = c('GC.DOD.TOTL.GD.ZS', 'PA.NUS.FCRF', 
                              'NY.GDP.MKTP.KD.ZG'), 
                start = 2000, end = 2015)

debt_raw <- debt_raw %>% select(country, year, GC.DOD.TOTL.GD.ZS, PA.NUS.FCRF,
                                NY.GDP.MKTP.KD.ZG) %>%
    rename(central_gov_debt = GC.DOD.TOTL.GD.ZS) %>%
    rename(exchange_usd = PA.NUS.FCRF) %>%
    rename(gdp_growth = NY.GDP.MKTP.KD.ZG)

# Extract euro exchange rate and place it in for euro countries
euro_exchange <- debt_raw %>% filter(country == 'Euro area') %>% 
    select(year, exchange_usd)

debt_raw$country <- countrycode(debt_raw$country, origin = 'country.name',
                                destination = 'country.name')

## Deficit ------
# Downloaded from: http://ec.europa.eu/eurostat/tgm/table.do?tab=table&init=1&language=en&pcode=teina200&plugin=1
# 3 December 2015

deficit_raw <- import('data_cleaning/raw/teina200.tsv', header = T, 
                      na.strings = ':')

split <- str_split_fixed(deficit_raw[, 1], pattern = ',', n = 2) %>% 
    as.data.frame

deficit_raw <- cbind(split, deficit_raw[, 2:(ncol(deficit_raw))])
deficit_raw <- deficit_raw %>% filter(V1 == 'PC_GDP') %>% select(-V1)

deficit_raw <- deficit_raw %>% gather(year, general_gov_deficit, 
                                      2:ncol(deficit_raw))

for (i in 1:2) deficit_raw[, i] <- as.character(deficit_raw[, i])
for (i in 2:ncol(deficit_raw)) deficit_raw[, i] <- as.numeric(deficit_raw[, i])

deficit_raw$V2[deficit_raw$V2 == 'UK'] <- 'GB'
deficit_raw$country <- countrycode(deficit_raw$V2, origin = 'iso2c',
                                   destination = 'country.name')
deficit_raw <- deficit_raw %>% DropNA('country')

deficit_raw <- deficit_raw %>% select(country, year, general_gov_deficit) %>%
    arrange(country, year)

deficit_debt <- merge(deficit_raw, debt_raw, by = c('country', 'year'), 
                      all.x = T)

## Eurozone member ---------------
euro <- import('https://raw.githubusercontent.com/christophergandrud/euro_membership/master/data/euro_membership_data.csv')

euro$euro_member <- 1
euro <- euro %>% select(-iso2c)

## Combine ------
comb <- merge(timing, revisions, by = c('country', 'year'))
comb <- merge(comb, fsi, by = c('country', 'year'), all.x = T)
comb <- merge(comb, finstress_yr_mean, by = c('country', 'year'), all.x = T)
comb <- merge(comb, endog_election, by = c('country', 'year'), all.x = T)
comb <- merge(comb, deficit_debt, by = c('country', 'year'), all.x = T)
comb <- merge(comb, euro, by = c('country', 'year'), all.x = T)

comb$euro_member[is.na(comb$euro_member)] <- 0
comb <- comb %>% arrange(country, year, version)

# Reinsert Euroarea exchange rate
comb <- FillIn(comb, euro_exchange, Var1 = 'exchange_usd', KeyVar = 'year')

# Final clean up
comb <- MoveFront(comb, c('year', 'country', 'iso2c'))
comb$iso2c <- countrycode(comb$country, origin = 'country.name', 
                          destination = 'iso2c')

comb <- comb %>% arrange(country, year, version, component)

## Saved merged data ------
export(comb, 'data_cleaning/main_merged.csv')
