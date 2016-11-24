# ---------------------------------------------------------------------------- #
# Eurostat debt revisions descriptive statistics discussed in the article
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(DataCombine)
library(ggplot2)

# Set working directory. Change as needed
setwd('/eurostat_replication_material/')

# Import and final clean --------------
comb <- import('data_cleaning/main_merged.csv')  

debt <- comb %>% filter(component == 'debt')
FindDups(debt, c('country', 'year', 'version'))

# Keep only the final cumulative revision, if in the 3rd year from the original
# figure was published
debt <- debt[!duplicated(debt[, c('country', 'year')], fromLast = TRUE), ]
debt <- subset(debt, years_since_original == 3)


# Summary statistics -----------------------------------------------------------
summary(debt$cum_revision)

# Proportion of Non-zero changes
non_zero <- subset(debt, cum_revision != 0)

nrow(non_zero) / nrow(debt)

# High revisions (>= 2% of GDP)
high_revisions <- subset(debt, cum_revision >= 2)

length(unique(high_revisions$country))

very_high_revisions <- subset(debt, cum_revision >= 9)
unique(very_high_revisions$country)
