# ---------------------------------------------------------------------------- #
# Eurostat revisions and election timing data creator
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(rio)
library(foreign)
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

endog_election <- endog_election %>% select(country, year, endog_3)

# Add Gandrud updates based on http://www.nsd.uib.no/european_election_database
# and Wikipedia

endog_gandrud <- import('data_cleaning/raw/endog_gandrud.csv')

endog_election <- rbind_all(list(endog_election, endog_gandrud))
endog_election <- endog_election %>% arrange(country, year)

FindDups(endog_election, c('country', 'year'))

## Debt figures from the World Bank Development Indicators ----
cent_debt_raw <- WDI(indicator = c('GC.DOD.TOTL.GD.ZS', 'PA.NUS.FCRF', 
                              'NY.GDP.MKTP.KD.ZG', 'AG.LND.ARBL.ZS'), 
                start = 2000, end = 2015)

cent_debt_raw <- cent_debt_raw %>% select(country, year, PA.NUS.FCRF,
                                NY.GDP.MKTP.KD.ZG,
                                GC.DOD.TOTL.GD.ZS) %>%
    rename(central_gov_debt = GC.DOD.TOTL.GD.ZS) %>%
    rename(exchange_usd = PA.NUS.FCRF) %>%
    rename(gdp_growth = NY.GDP.MKTP.KD.ZG)

# Extract euro exchange rate and place it in for euro countries
euro_exchange <- cent_debt_raw %>% filter(country == 'Euro area') %>% 
    select(year, exchange_usd)

cent_debt_raw$country <- countrycode(cent_debt_raw$country, origin = 'country.name',
                                destination = 'country.name')

cent_debt_raw <- slide(cent_debt_raw, Var = 'central_gov_debt', 
                       TimeVar = 'year', GroupVar = 'country', 
                       NewVar = 'lag_cent_debt', slideBy = -1)

## Eurostat General Government Debt ------
# Downloaded from: http://ec.europa.eu/eurostat/en/web/products-datasets/-/TSDDE410
# 25 February 2016

gen_debt_raw <- import('data_cleaning/raw/tsdde410.tsv', header = T, 
                      na.strings = ':')

split <- str_split_fixed(gen_debt_raw[, 1], pattern = ',', n = 2) %>% 
    as.data.frame

gen_debt_raw <- cbind(split, gen_debt_raw[, 2:(ncol(gen_debt_raw))])
gen_debt_raw <- gen_debt_raw %>% filter(V1 == 'PC_GDP') %>% select(-V1)

gen_debt_raw <- gen_debt_raw %>% gather(year, gen_gov_debt, 
                                      2:ncol(gen_debt_raw))

for (i in 1:2) gen_debt_raw[, i] <- as.character(gen_debt_raw[, i])
for (i in 2:ncol(gen_debt_raw)) gen_debt_raw[, i] <- as.numeric(gen_debt_raw[, i])

gen_debt_raw$V2[gen_debt_raw$V2 == 'UK'] <- 'GB'
gen_debt_raw$V2[gen_debt_raw$V2 == 'EL'] <- 'GR'
gen_debt_raw$country <- countrycode(gen_debt_raw$V2, origin = 'iso2c',
                                   destination = 'country.name')
gen_debt_raw <- gen_debt_raw %>% DropNA('country')

gen_debt_raw <- gen_debt_raw %>% select(country, year, gen_gov_debt) %>%
    arrange(country, year)

gen_debt_raw <- slide(gen_debt_raw, Var = 'gen_gov_debt', 
                       TimeVar = 'year', GroupVar = 'country', 
                       NewVar = 'lag_gen_debt', slideBy = -1)

debt_debt <- merge(cent_debt_raw, gen_debt_raw, by = c('country', 'year'), 
                      all = T)

## Eurostat General Government Deficit ------
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
deficit_raw$V2[deficit_raw$V2 == 'EL'] <- 'GR'
deficit_raw$country <- countrycode(deficit_raw$V2, origin = 'iso2c',
                                   destination = 'country.name')
deficit_raw <- deficit_raw %>% DropNA('country')

deficit_raw <- deficit_raw %>% select(country, year, general_gov_deficit) %>%
    arrange(country, year)

deficit_raw <- slide(deficit_raw, Var = 'general_gov_deficit', 
                      TimeVar = 'year', GroupVar = 'country', 
                      NewVar = 'lag_gen_deficit', slideBy = -1)

deficit_debt <- merge(debt_debt, deficit_raw, by = c('country', 'year'), 
                      all = T)

## Eurozone member ---------------
euro <- import('https://raw.githubusercontent.com/christophergandrud/euro_membership/master/data/euro_membership_data.csv')

euro$euro_member <- 1
euro <- euro %>% select(-iso2c)

# Wang et al. GFS Fiscal transparency -----------
# Downloaded from https://www.imf.org/External/pubs/cat/longres.aspx?sk=43177.0
fiscal_trans <- import('data_cleaning/raw/wp15188.xlsx', sheet = "GFS Index Score", 
                       skip = 2)
fiscal_trans$country <- countrycode(fiscal_trans$Country, origin = 'country.name',
                                    destination = 'country.name')

fiscal_trans <- fiscal_trans[, c(17, 4:14)]

fiscal_trans <- fiscal_trans %>% gather(year, fiscal_trans_gfs, 
                                        2:ncol(fiscal_trans)) %>%
                    arrange(country, year)

## Excessive deficit procudure (updated from Baerg and Hallerberg 2016) --------
edp <- import('data_cleaning/raw/edp_updated.csv')
edp <- edp %>% rename(year = actualyear)

## Fiscal contracts (from Hallerberg) --------------
contracts <- import('data_cleaning/raw/fiscal_contracts/combined_fiscal.csv')

## Independent National Debt Rule Monitor  (Bova et al. 2015) -----------
independent <- read.dta('data_cleaning/raw/fiscal_rule_database11.dta') %>%
    filter(region == 2) %>% 
    # select observations where there is a independent national institution monitoring the debt rule
    filter(monitor_n_DR == 1 & monitor_s_DR == 1) %>%
    select(Country, year, monitor_n_DR) %>% rename(country = Country)

independent$country <- countrycode(independent$country, origin = "country.name",
                                   destination = 'country.name')


## Combine ------
comb <- merge(timing, revisions, by = c('country', 'year'))
comb <- merge(comb, fsi, by = c('country', 'year'), all.x = T)
comb <- merge(comb, finstress_yr_mean, by = c('country', 'year'), 
              all.x = T)
comb <- merge(comb, endog_election, by = c('country', 'year'), all.x = T)
comb <- merge(comb, deficit_debt, by = c('country', 'year'), all.x = T)
comb <- merge(comb, euro, by = c('country', 'year'), all.x = T)
comb <- merge(comb, fiscal_trans, by = c('country', 'year'), all.x = T)
comb <- merge(comb, edp, by = c('country', 'year'), all.x = T)
comb <- merge(comb, contracts, by = c('country', 'year'), all.x = T)
comb <- merge(comb, independent, by = c('country', 'year'), all.x = T)

# Fill in 0 for NAs in euro membership
comb$euro_member[is.na(comb$euro_member)] <- 0

# Fill in 0 for NAs in countries without national debt monitor
comb$monitor_n_DR[is.na(comb$monitor_n_DR)] <- 0

comb <- comb %>% arrange(country, year, version)

# Re-insert Euroarea exchange rate
comb <- FillIn(comb, euro_exchange, Var1 = 'exchange_usd', KeyVar = 'year')

# Fill in missing contracts and delegation years, assuming no change
comb <- comb %>% group_by(country) %>% 
    mutate(contracts = FillDown(Var = contracts))

comb <- comb %>% group_by(country) %>% 
    mutate(delegation = FillDown(Var = delegation))

comb <- comb %>% group_by(country) %>% 
    mutate(expcontracts = FillDown(Var = expcontracts))

# Final clean up
comb <- MoveFront(comb, c('year', 'country', 'iso2c'))
comb$iso2c <- countrycode(comb$country, origin = 'country.name', 
                          destination = 'iso2c')

comb <- comb %>% arrange(country, year, version, component)

comb <- comb %>% MoveFront('country')

## Saved merged data ------
export(comb, 'data_cleaning/main_merged.csv')
