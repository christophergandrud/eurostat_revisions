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
library(stargazer)
library(DataCombine)
library(stringr)
library(tidyr)
library(WDI)

# Set working directory. Change as needed
setwd('/git_repositories/eurostat_revisions/')

# Load plot function
devtools::source_gist('d270ff55c2ca26286e90')

# Load revisions data
revisions <- import('data_cleaning/comb_cumulative.csv')

revisions$country <- countrycode(revisions$country, 
                                      origin = 'country.name',
                                      destination = 'country.name')

# Load election timing data
timing <- import('https://raw.githubusercontent.com/christophergandrud/yrcurnt_corrected/master/data/yrcurnt_original_corrected.csv') %>% select(-yrcurnt)

# Creat election year dummy
timing$elect_dummy <- 0
timing$elect_dummy[timing$yrcurnt_corrected == 0] <- 1

timing$country <- countrycode(timing$iso2c, 
                              origin = 'iso2c', 
                              destination = 'country.name')

timing <- timing %>% select(-iso2c)

# Load FinStress and create annual averages
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
debt_raw <- WDI(indicator = 'GC.DOD.TOTL.GD.ZS', start = 2000)
debt_raw$country <- countrycode(debt_raw$country, origin = 'country.name',
                                destination = 'country.name')

debt_raw <- debt_raw %>% select(country, year, GC.DOD.TOTL.GD.ZS) %>%
                rename(central_gov_debt = GC.DOD.TOTL.GD.ZS)

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

## Combine ------
comb <- merge(timing, revisions, by = c('country', 'year'))
comb <- merge(comb, finstress_yr_mean, by = c('country', 'year'))
comb <- merge(comb, endog_election, by = c('country', 'year'), all.x = T)
comb <- merge(comb, deficit_debt, by = c('country', 'year'), all.x = T)

comb <- comb %>% arrange(country, year, version)

## Saved merged data ------
export(comb, 'data_cleaning/main_merged.csv')


comb <- import('data_cleaning/main_merged.csv') # For working offline   

comb$endog_3 <- factor(comb$endog_3, 
                       levels = c(1:3),
                       labels = c('Endogenous', 'Non-endogenous', 'No election'))

comb$endog_3 <- relevel(comb$endog_3, ref = 'No election')

## Estimate models -------
# debt revisions
debt <- comb %>% filter(component == 'debt')
FindDups(debt, c('country', 'year', 'version'))

m1_1 <- lm(cum_revision ~ years_since_original + yrcurnt_corrected +
             as.factor(country), data = debt)

m1_2 <- lm(cum_revision ~ years_since_original + 
               endog_3 +
               as.factor(country), data = debt)

m1_3 <- lm(cum_revision ~ years_since_original + yrcurnt_corrected + 
               finstress_mean +
               as.factor(country), data = debt)

m1_4 <- lm(cum_revision ~ years_since_original + endog_3 + 
               finstress_mean +
               as.factor(country), data = debt)

m1_5 <- lm(cum_revision ~ years_since_original + 
               yrcurnt_corrected * finstress_mean +
               as.factor(country), data = debt)

m1_6 <- lm(cum_revision ~ years_since_original + 
               endog_3*finstress_mean +
               as.factor(country), data = debt)

# deficit revisions
deficit <- comb %>% filter(component == 'deficit')
m2_1 <- lm(cum_revision ~ years_since_original + yrcurnt_corrected +
               as.factor(country), data = deficit)

m2_2 <- lm(cum_revision ~ years_since_original + 
               endog_3 +
               as.factor(country), data = deficit)

m2_3 <- lm(cum_revision ~ years_since_original + yrcurnt_corrected + 
               finstress_mean +
               as.factor(country), data = deficit)

m2_4 <- lm(cum_revision ~ years_since_original + endog_3 + 
               finstress_mean +
               as.factor(country), data = deficit)

m2_5 <- lm(cum_revision ~ years_since_original + 
               yrcurnt_corrected * finstress_mean +
               as.factor(country), data = deficit)

m2_6 <- lm(cum_revision ~ years_since_original + 
               endog_3*finstress_mean +
               as.factor(country), data = deficit)

## Create results tables -------
vars <- c('Yrs. Since Original', 'Yrs. to Election', 'Endog. Election',
          'Non-Endog. Election',
          'FinStress', 'Yrs. to Elect.*FinStress', 
          'Endog. Elect.*FinStress', 'Non-Endog. Elect.*FinStress')


stargazer(m1_1, m1_2, m1_3, m1_4, m1_5, m1_6, omit = 'as.factor*', 
          omit.stat = c('f', 'ser'), # so that it fits on the page
          out.header = F,
          title = 'Linear Regression Estimation of Debt Revisions',
          dep.var.labels = 'Cumulative Debt Revisions',
          covariate.labels = vars,
          label = 'debt_results',
          add.lines = list(c('Country FE?', 'Yes', 'Yes', 'Yes')),
          font.size = 'tiny',
          out = 'working_paper/tables/debt_regressions.tex')


stargazer(m2_1, m2_2, m2_3, m2_4, m2_5, m2_6, omit = 'as.factor*', 
          omit.stat = c('f', 'ser'), # so that it fits on the page
          out.header = F,
          title = 'Linear Regression Estimation of Deficit Revisions',
          dep.var.labels = 'Cumulative Deficit Revisions',
          covariate.labels = vars,
          label = 'deficit_results',
          add.lines = list(c('Country FE?', 'Yes', 'Yes', 'Yes')),
          font.size = 'tiny',
          out = 'working_paper/tables/deficit_regressions.tex')


## Plot marginal effect -------
# Election timing and finstress
finstress_elect_me <- plot_me(m1_5, term1 = 'yrcurnt_corrected', 
                              term2 = 'finstress_mean',
        fitted2 = seq(0.2, 0.75, by = 0.05)) +
    xlab('\nAnnual FinStress Mean') + 
    ylab('Marginal Effect of Election Timing\n')

ggsave(finstress_elect_me, 
       filename = 'working_paper/figures/finstress_elect_me.pdf')

# Election timing and endogenous elections
finstress_endog_elect_me <- plot_me(m1_6, term1 = 'endog_3Endogenous', 
                              term2 = 'finstress_mean',
                              fitted2 = seq(0.2, 0.75, by = 0.05)) +
    xlab('\nAnnual FinStress Mean') + 
    ylab('Margingal Effect of an Endogenous Election\n')

ggsave(finstress_endog_elect_me, 
       filename = 'working_paper/figures/finstress_endog_elect_me.pdf')

## Simulate and plot predicted effects ------------------

# Scenarios for election timing ----

countries <- unique(debt$country)

# Drop Croatia, the newest EU member for which there are few revisions
countries <- countries[!(countries %in% 'Croatia')]

fitted <- NULL

for (i in (countries)) {
    temp <- debt %>% filter(country == i)
    min_finstress <- min(temp$finstress_mean, na.rm = T)
    max_finstress <- max(temp$finstress_mean, na.rm = T)
    
    temp_fitted <- data.frame(years_since_original = rep(3, 8),
                              yrcurnt_corrected = rep(0:3, 2),
                              finstress_mean = c(rep(min_finstress, 4), 
                                                 rep(max_finstress, 4)),
                              country = rep(i, 8)
                              )
    fitted <- rbind(fitted, temp_fitted)
    rm(temp, temp_fitted)
}

temp_levels <- c(rep('low', 4), rep('high', 4))
fitted$finstress_level <- rep(temp_levels, length(countries))

predictions <- predict(m1_5, newdata = fitted, interval = 'confidence')
predictions <- cbind(predictions, fitted[, c('country', 'finstress_level',
                                             "yrcurnt_corrected")])

country_predictions_timing <- ggplot(predictions, aes(yrcurnt_corrected, fit, 
                                        group = finstress_level,
                                        colour = finstress_level, 
                                        fill = finstress_level)) +
    geom_hline(yintercept = 0, linetype = 'dotted') +
    facet_wrap(~country) +
    geom_line() +
    #geom_ribbon(data = predictions, aes(ymin = lwr, ymax = upr, 
    #            fill = finstress_level), alpha = 0.1) +
    scale_x_reverse() +
    scale_color_manual(values = c('#e34a33', '#7fcdbb'), 
                       name = 'Credit Provision\nStress') +
    xlab('\nYears Until Election') +
    ylab('Predicted Cumulative Debt Revision\nAfter 3 Years (% GDP)\n') +
    theme_bw()

ggsave(country_predictions_timing, 
       filename = 'working_paper/figures/country_predict_timing.pdf')

# Scenarios for election endogenous elections ----

fitted <- NULL

for (i in (countries)) {
    temp <- debt %>% filter(country == i)
    min_finstress <- min(temp$finstress_mean, na.rm = T)
    max_finstress <- max(temp$finstress_mean, na.rm = T)
    
    temp_fitted <- data.frame(years_since_original = rep(3, 6),
                              endog_3 = as.factor(rep(c('No election',
                                                        'Endogenous',
                                                        'Non-endogenous'), 2)),
                              finstress_mean = c(rep(min_finstress, 3), 
                                                 rep(max_finstress, 3)),
                              country = rep(i, 6)
    )
    fitted <- rbind(fitted, temp_fitted)
    rm(temp, temp_fitted)
}

temp_levels <- c(rep('low', 3), rep('high', 3))
fitted$finstress_level <- rep(temp_levels, length(countries))

predictions <- predict(m1_6, newdata = fitted, interval = 'confidence')
predictions <- cbind(predictions, fitted[, c('country', 'finstress_level',
                                             'endog_3')])

predictions$endog_3 <- factor(predictions$endog_3, 
                              levels = c('No election', 'Non-endogenous', 
                                         'Endogenous'))

country_predictions_timing <- ggplot(predictions, aes(endog_3, fit, 
                                                     group = finstress_level,
                                                     colour = finstress_level, 
                                                     fill = finstress_level)) +
    geom_hline(yintercept = 0, linetype = 'dotted') +
    facet_wrap(~country, ncol =  7) +
    geom_line() +
    geom_point() +
    geom_ribbon(data = predictions, aes(ymin = lwr, ymax = upr, 
                fill = finstress_level), alpha = 0.4, colour = NA) +
    scale_color_manual(values = c('#e34a33', '#7fcdbb'), 
                       name = 'Credit\nProvision\nStress') +
    scale_fill_manual(values = c('#e34a33', '#7fcdbb'), 
                       name = 'Credit\nProvision\nStress') +
    scale_y_continuous(breaks = c(-2.5, 0, 3.5, 7.5)) +
    xlab('\n') +
    ylab('Predicted Cumulative Debt Revision\nAfter 4 Years (% GDP)\n') +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(country_predictions_timing, 
       filename = 'working_paper/figures/country_predict_required.pdf')
