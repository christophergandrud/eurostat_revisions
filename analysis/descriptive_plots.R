# ---------------------------------------------------------------------------- #
# Descriptive Revision Plots
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(DataCombine)

# Set working directory. Change as needed
setwd('/git_repositories/eurostat_revisions/')

# Import data and do basic tidying
comb <- import('data_cleaning/main_merged.csv')   

comb$endog_3 <- factor(comb$endog_3, 
                       levels = c(1:3),
                       labels = c('Unscheduled', 'Scheduled', 'No election'))

comb$endog_3 <- relevel(comb$endog_3, ref = 'No election')

# Debt Revision Plots ----------------------------------------
# Select only debt
debt <- comb %>% filter(component == 'debt')
FindDups(debt, c('country', 'year', 'version'))

# Create country year identifier
debt$country_year <- sprintf('%s_%s', debt$country, debt$year)
debt <- debt %>% arrange(country, year, version)

# Create a time count
debt$fake <- 1
debt <- debt %>% group_by(country_year) %>%
    mutate(counter = cumsum(fake)) %>% select(-fake)

## Create median revision for each election timing year
median_rev_timing <- debt %>% group_by(yrcurnt_corrected, counter) %>%
    summarise(median_revision = median(cum_revision, na.rm = T))

median_rev_timing$yrcurnt_corrected <- as.factor(
                                            median_rev_timing$yrcurnt_corrected)

timing_debt <- ggplot(median_rev_timing, aes(counter, median_revision, 
                              group = yrcurnt_corrected,
                              colour = yrcurnt_corrected,
                              linetype = yrcurnt_corrected)) +
    geom_line(size = 1) +
    scale_linetype(name = 'Yrs. to\nElection') +
    scale_color_brewer(palette = 'Set1', name = 'Yrs. to\nElection') +
    scale_x_continuous(breaks = c(1, 3, 5, 7)) +
    ylab('Median Cumulative Debt Revision\n') +
    xlab('') +
    theme_bw()

## Create median revision for each election type
median_rev_endog <- debt %>% group_by(endog_3, counter) %>%
    summarise(median_revision = median(cum_revision, na.rm = T))

endog_debt <- ggplot(median_rev_endog, aes(counter, median_revision, 
                                           group = endog_3, colour = endog_3,
                             linetype = endog_3)) +
    geom_line(size = 1) +
    scale_linetype(name = 'Election\nType') +
    scale_color_brewer(palette = 'Set2', name = 'Election\nType') +
    scale_x_continuous(breaks = c(1, 3, 5, 7)) +
    ylab('') +
    xlab('') +
    theme_bw()

pdf(file = 'working_paper/figures/median_debt_revisions.pdf', 
    width = 11, height = 7)
grid.arrange(timing_debt, endog_debt, nrow = 1, 
        bottom = '\nNumber of Eurostat Revisions Since Original Budget Data was Released')
dev.off()


# Deficit Revision Plots ----------------------------------------
# Select only deficit
deficit <- comb %>% filter(component == 'deficit')
FindDups(deficit, c('country', 'year', 'version'))

# Create country year identifier
deficit$country_year <- sprintf('%s_%s', deficit$country, deficit$year)
deficit <- deficit %>% arrange(country, year, version)

# Create a time count
deficit$fake <- 1
deficit <- deficit %>% group_by(country_year) %>%
    mutate(counter = cumsum(fake)) %>% select(-fake)

## Create median revision for each election timing year
median_rev_timing <- deficit %>% group_by(yrcurnt_corrected, counter) %>%
    summarise(median_revision = median(cum_revision, na.rm = T))

median_rev_timing$yrcurnt_corrected <- as.factor(
    median_rev_timing$yrcurnt_corrected)

timing_deficit <- ggplot(median_rev_timing, aes(counter, median_revision, 
                                             group = yrcurnt_corrected,
                                             colour = yrcurnt_corrected,
                                             linetype = yrcurnt_corrected)) +
    geom_line(size = 1) +
    scale_linetype(name = 'Yrs. to\nElection') +
    scale_color_brewer(palette = 'Set1', name = 'Yrs. to\nElection') +
    scale_x_continuous(breaks = c(1, 3, 5, 7)) +
    ylab('Median Cumulative Deficit Revision\n') +
    xlab('') +
    theme_bw()

## Create median revision for each election type
median_rev_endog <- deficit %>% group_by(endog_3, counter) %>%
    summarise(median_revision = median(cum_revision, na.rm = T))

endog_deficit <- ggplot(median_rev_endog, aes(counter, median_revision, 
                                           group = endog_3, colour = endog_3,
                                           linetype = endog_3)) +
    geom_line(size = 1) +
    scale_linetype(name = 'Election\nType') +
    scale_color_brewer(palette = 'Set2', name = 'Election\nType') +
    scale_x_continuous(breaks = c(1, 3, 5, 7)) +
    ylab('') +
    xlab('') +
    theme_bw()

pdf(file = 'working_paper/figures/median_deficit_revisions.pdf', 
    width = 11, height = 7)
grid.arrange(timing_deficit, endog_deficit, nrow = 1, 
        bottom = '\nNumber of Eurostat Revisions Since Original Budget Data was Released')
dev.off()
