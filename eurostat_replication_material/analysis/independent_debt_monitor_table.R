# ---------------------------------------------------------------------------- #
# Create table of indpendent national debt monitors as classified by Bova et al. (2015)
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(DataCombine)
library(xtable)

# Set working directory. Change as needed
setwd('/eurostat_replication_material/')

# Import
comb <- import('data_cleaning/main_merged.csv') %>% 
            filter(monitor_n_DR == 1) %>%
            select(country, year) %>% 
            FindDups(c('country', 'year'), NotDups = T)

comb <- StartEnd(comb, SpellVar = 'year', GroupVar = 'country')

start <- comb %>% filter(Spell_Start == 0) %>% select(country, year) %>%
            rename(Start = year)

end <- comb %>% filter(Spell_End == 0) %>% select(country, year) %>%
            rename(End = year)
end$End[end$End == 2013] <- NA

out_table <- merge(start, end, by = 'country') %>% rename(Country = country)


