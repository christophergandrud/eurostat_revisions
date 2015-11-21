# ---------------------------------------------------------------------------- #
# Eurostat revisions and election timing explore
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(ggplot2)

# Set working directory. Change as needed
setwd('/git_repositories/eurostat_revisions/')

# Load revisions data
revisions <- import('data_cleaning/comb_cumulative.csv')

# Load election timing data
timing <- import('https://raw.githubusercontent.com/christophergandrud/yrcurnt_corrected/master/data/yrcurnt_corrected.csv') %>% select(-V5)

# Combine
comb <- merge(timing, revisions, by = c('country', 'year'))

# Plot debt revisions
debt <- comb %>% filter(component == 'debt')
ggplot(debt, aes(yrcurnt_corrected, cum_revision)) +
    geom_point() +
    stat_smooth(method = 'lm', se = F) +
    facet_wrap(~ country) +
    theme_bw()

m1 <- lm(cum_revision ~ yrcurnt_corrected + 
             as.factor(country) + as.factor(years_since_original), data = debt)


deficit <- comb %>% filter(component == 'deficit')
m2 <- lm(cum_revision ~ yrcurnt_corrected + 
         as.factor(country) + as.factor(years_since_original), data = deficit)
