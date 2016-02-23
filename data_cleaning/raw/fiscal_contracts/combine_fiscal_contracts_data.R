# ---------------------------------------------------------------------------- #
# Merge various versions of Hallerberg et al.'s fiscal contracts/delegation variables
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #


library(dplyr)
library(rio)
library(DataCombine)

setwd('/git_repositories/eurostat_revisions/')

# Data 1 ----------
contracts1 <- import('data_cleaning/raw/fiscal_contracts/DelegationtempStata11.dta') %>% 
                rename(wb = country)

contracts1$wb[contracts1$wb == 'BUL'] <- 'BGR'
contracts1$wb[contracts1$wb == 'GER'] <- 'DEU'
contracts1$wb[contracts1$wb == 'LAT'] <- 'LVA'
contracts1$wb[contracts1$wb == 'LIT'] <- 'LTU'
contracts1$wb[contracts1$wb == 'SLK'] <- 'SVK'

contracts1$country <- countrycode::countrycode(contracts1$wb, origin = 'wb',
                                               destination = 'country.name')
contracts1 <- contracts1[, c('country', 'year', 'delegation', 'contracts', 
                             'expcontracts')]
contracts1 <- contracts1 %>% DropNA(c('delegation', 'contracts'))


# Data 2 ------------
contracts2 <- import('data_cleaning/raw/fiscal_contracts/EUDeficitsDebt.dta') %>%
                select(Country, Year, Delegation, Contracts)  %>% 
                rename(wb = Country)
names(contracts2) <- names(contracts2) %>% tolower

contracts2$wb[contracts2$wb == 'BUL'] <- 'BGR'
contracts2$wb[contracts2$wb == 'GER'] <- 'DEU'
contracts2$wb[contracts2$wb == 'LAT'] <- 'LVA'
contracts2$wb[contracts2$wb == 'LITH'] <- 'LTU'
contracts2$wb[contracts2$wb == 'MAL'] <- 'MLT'
contracts2$wb[contracts2$wb == 'SLK'] <- 'SVK'

contracts2$country <- countrycode::countrycode(contracts2$wb, origin = 'wb',
                                               destination = 'country.name')
contracts2 <- contracts2[, c('country', 'year', 'delegation', 'contracts')]
contracts2$expcontracts <- NA
contracts2 <- contracts2 %>% DropNA(c('delegation', 'contracts'))

# Data 3 -------------
contracts3 <- import('data_cleaning/raw/fiscal_contracts/CEEC short.dta') %>%
                    select(-delegation) %>%
                    rename(contracts = contract3) %>% 
                    rename(delegation = delegate)
contracts3$country <- countrycode::countrycode(contracts3$country, 
                                               origin = 'country.name',
                                               destination = 'country.name')
contracts3 <- contracts3 %>% DropNA(c('delegation', 'contracts'))
contracts3 <- contracts3[, c('country', 'year', 'delegation', 'contracts')]
contracts3$expcontracts <- NA


# Combine ------------
comb <- rbind(contracts1, contracts2, contracts3)
comb <- FindDups(comb, c('country', 'year'), NotDups = T) %>% 
            arrange(country, year)

export(comb, file = 'data_cleaning/raw/fiscal_contracts/combined_fiscal.csv')
