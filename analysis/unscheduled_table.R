# ---------------------------------------------------------------------------- #
# Create table of country-years with unscheduled elections
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(DataCombine)
library(xtable)

# Set working directory. Change as needed
setwd('/git_repositories/eurostat_revisions/')

# Import data and do basic tidying
comb <- import('data_cleaning/main_merged.csv')

# Keep unique country-years with unsheduled elections
sub <- FindDups(comb, c('country', 'year'), NotDups = T)

sub <- sub %>% filter(endog_3 == 1)

sub <- sub[, 1:2]
names(sub) <- c('Country', 'Unscheduled Election Year')

# Create table
print(
    xtable(sub, caption = "Country-years With Unscheduled Elections in Our Sample", 
           label = 'unschedule_list'),
        caption.placement = 'top', include.rownames = F,
        file = 'working_paper/tables/unscheduled_list.tex')
