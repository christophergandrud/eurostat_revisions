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

# Set working directory. Change as needed
setwd('/git_repositories/eurostat_revisions/')

# Load plot function
devtools::source_gist('d270ff55c2ca26286e90')

# Load revisions data
revisions <- import('data_cleaning/comb_cumulative.csv')

# Load election timing data
timing <- import('https://raw.githubusercontent.com/christophergandrud/yrcurnt_corrected/master/data/yrcurnt_original_corrected.csv') %>% select(-yrcurnt, -X)

# Load FinStress and create annual averages
FinStress <- rio::import("http://bit.ly/1LFEnhM")

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

## Combine ------
comb <- merge(timing, revisions, by = c('country', 'year'))
comb <- merge(comb, finstress_yr_mean, by = c('country', 'year'))
    
## Estimate models -------
# debt revisions
debt <- comb %>% filter(component == 'debt')

m1_1 <- lm(cum_revision ~ years_since_original + yrcurnt_corrected +
             as.factor(country), data = debt)

m1_2 <- lm(cum_revision ~ years_since_original + yrcurnt_corrected + 
               finstress_mean +
               as.factor(country), data = debt)

m1_3 <- lm(cum_revision ~ years_since_original + 
               yrcurnt_corrected * finstress_mean +
               as.factor(country), data = debt)

# deficit revisions
deficit <- comb %>% filter(component == 'deficit')
m2_1 <- lm(cum_revision ~ years_since_original + yrcurnt_corrected +
         as.factor(country), data = deficit)

m2_2 <- lm(cum_revision ~ years_since_original +
               yrcurnt_corrected + finstress_mean +
               as.factor(country), data = deficit)

m2_3 <- lm(cum_revision ~ years_since_original +
               yrcurnt_corrected*finstress_mean +
               as.factor(country), data = deficit)

## Create results tables -------
vars <- c('Yrs. Since Original', 'Yrs. to Election', 'FinStress',
          'Yrs. to Elect. * FinStress')


stargazer(m1_1, m1_2, m1_3, omit = 'as.factor*', 
          out.header = F,
          title = 'Linear Regression Estimation of Debt Revisions',
          dep.var.labels = 'Cumulative Debt Revisions',
          covariate.labels = vars,
          label = 'debt_results',
          add.lines = list(c('Country FE?', 'Yes', 'Yes', 'Yes')),
          font.size = 'footnotesize',
          out = 'working_paper/tables/debt_regressions.tex')


stargazer(m2_1, m2_2, m2_3, omit = 'as.factor*', 
          out.header = F,
          title = 'Linear Regression Estimation of Deficit Revisions',
          dep.var.labels = 'Cumulative Deficit Revisions',
          covariate.labels = vars,
          label = 'deficit_results',
          add.lines = list(c('Country FE?', 'Yes', 'Yes', 'Yes')),
          font.size = 'footnotesize',
          out = 'working_paper/tables/deficit_regressions.tex')


## Plot marginal effect -------
finstress_elect_me <- plot_me(m1_3, term1 = 'yrcurnt_corrected', 
                              term2 = 'finstress_mean',
        fitted2 = seq(0.2, 0.75, by = 0.05)) +
    xlab('\nAnnual FinStress Mean') + 
    ylab('Marginal Effect of Election Timing\n')

ggsave(finstress_elect_me, filename = 'working_paper/figures/finstress_elect_me.pdf')

## Simulate and plot predicted effects ------

# Scenarios

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

predictions <- predict(m1_3, newdata = fitted, interval = 'confidence')
predictions <- cbind(predictions, fitted[, c('country', 'finstress_level',
                                             "yrcurnt_corrected")])

country_predictions <-ggplot(predictions, aes(yrcurnt_corrected, fit, 
                                        group = finstress_level,
                            colour = finstress_level, fill = finstress_level)) +
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

ggsave(finstress_elect_me, filename = 'working_paper/figures/finstress_elect_me.pdf')
