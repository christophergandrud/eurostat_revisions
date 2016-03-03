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
                       labels = c('Unsched.', 'Sched.', 'None'))

comb$endog_3 <- relevel(comb$endog_3, ref = 'No election')

comb$excessdef <- factor(comb$excessdef, levels = 0:1,
                         labels = c('No', 'Yes'))

comb$euro_member <- factor(comb$euro_member, labels = c('No',
                                                        'Yes'))

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
    ylab('Median Cumulative Debt Revision (% GDP)\n') +
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

## Create median revision for being in an EDP
median_rev_edp <- debt %>% group_by(excessdef, counter) %>%
    summarise(median_revision = median(cum_revision, na.rm = T))

edp_debt <- ggplot(median_rev_edp, aes(counter, median_revision,
                                          group = excessdef, colour = excessdef,
                                          linetype = excessdef)) +
    geom_line(size = 1) +
    scale_linetype(name = 'EDP?') +
    scale_color_brewer(palette = 'Set2', name = 'EDP?') +
    scale_x_continuous(breaks = c(1, 3, 5, 7)) +
    ylab('') +
    xlab('') +
    theme_bw()

## Create median revision for Eurozone membership
median_rev_euro <- debt %>% group_by(euro_member, counter) %>%
    summarise(median_revision = median(cum_revision, na.rm = T))

euro_debt <- ggplot(median_rev_euro, aes(counter, median_revision,
                                            group = euro_member, colour = euro_member,
                                            linetype = euro_member)) +
    geom_line(size = 1) +
    scale_linetype(name = 'Euro?') +
    scale_color_brewer(palette = 'Set2', name = 'Euro?') +
    scale_x_continuous(breaks = c(1, 3, 5, 7)) +
    ylab('') +
    xlab('') +
    theme_bw()

pdf(file = 'working_paper/figures/median_debt_revisions.pdf',
    width = 11, height = 11)
grid.arrange(timing_debt, endog_debt, edp_debt, euro_debt, nrow = 2, ncol = 2,
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
    ylab('Median Cumulative Deficit Revision (% GDP)\n') +
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


## Create median revision for being in an EDP
median_rev_edp <- deficit %>% group_by(excessdef, counter) %>%
    summarise(median_revision = median(cum_revision, na.rm = T))

edp_deficit <- ggplot(median_rev_edp, aes(counter, median_revision,
                                           group = excessdef, colour = excessdef,
                                           linetype = excessdef)) +
    geom_line(size = 1) +
    scale_linetype(name = 'EDP?') +
    scale_color_brewer(palette = 'Set2', name = 'EDP?') +
    scale_x_continuous(breaks = c(1, 3, 5, 7)) +
    scale_y_continuous(limits = c(-0.2, 0)) +
    ylab('') +
    xlab('') +
    theme_bw()

## Create median revision for Eurozone membership
median_rev_euro <- deficit %>% group_by(euro_member, counter) %>%
    summarise(median_revision = median(cum_revision, na.rm = T))

euro_deficit <- ggplot(median_rev_euro, aes(counter, median_revision,
                                       group = euro_member, colour = euro_member,
                                       linetype = euro_member)) +
    geom_line(size = 1) +
    scale_linetype(name = 'Euro?') +
    scale_color_brewer(palette = 'Set2', name = 'Euro?') +
    scale_x_continuous(breaks = c(1, 3, 5, 7)) +
    scale_y_continuous(limits = c(-0.2, 0)) +
    ylab('') +
    xlab('') +
    theme_bw()


pdf(file = 'working_paper/figures/median_deficit_revisions_edp_euro.pdf',
    width = 11, height = 7)
grid.arrange(edp_deficit, euro_deficit, nrow = 1,
             bottom = '\nNumber of Eurostat Revisions Since Original Budget Data was Released')
dev.off()
