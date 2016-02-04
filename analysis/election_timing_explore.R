# ---------------------------------------------------------------------------- #
# Eurostat revisions and election models
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(lubridate)
library(ggplot2)
library(devtools)
library(stargazer)
library(DataCombine)
library(stringr)
library(tidyr)

# Set working directory. Change as needed
setwd('/git_repositories/eurostat_revisions/')

# Load plot function
devtools::source_gist('d270ff55c2ca26286e90')

# Import
comb <- import('data_cleaning/main_merged.csv')   

comb$endog_3 <- factor(comb$endog_3, 
                       levels = c(1:3),
                       labels = c('Endogenous', 'Non-endogenous', 'No election'))

comb$endog_3 <- relevel(comb$endog_3, ref = 'No election')

comb$from_2010 <- 0
comb$from_2010[comb$year >= 2010] <- 1

## Estimate models -------
# debt revisions
debt <- comb %>% filter(component == 'debt')
FindDups(debt, c('country', 'year', 'version'))


debt <- slide(debt, Var = 'cum_revision', TimeVar = 'version', 
              GroupVar = 'country', NewVar = 'lag_cum_revision',
              slideBy = -1)

debt <- slide(debt, Var = 'cum_revision', TimeVar = 'version', 
              GroupVar = 'country', NewVar = 'lead_cum_revision',
              slideBy = 1)

debt <- slide(debt, Var = 'finstress_mean', TimeVar = 'version', 
              GroupVar = 'country', NewVar = 'lag_finstress_mean',
              slideBy = -1)

m1_1 <- lm(cum_revision ~ years_since_original + euro_member + 
             as.factor(country), data = debt)

m1_2 <- lm(cum_revision ~ years_since_original + central_gov_debt + 
               as.factor(country), data = debt)

m1_3 <- lm(cum_revision ~ years_since_original + general_gov_deficit +
               as.factor(country), data = debt)

m1_4 <- lm(cum_revision ~ years_since_original + yrcurnt_corrected +
               as.factor(country), data = debt)

m1_5 <- lm(cum_revision ~ years_since_original + endog_3 +
               as.factor(country), data = debt)

m1_6 <- lm(cum_revision ~ years_since_original + yrcurnt_corrected + 
               finstress_mean +
               as.factor(country), data = debt)

m1_7 <- lm(cum_revision ~ years_since_original + endog_3 + 
               finstress_mean +
               as.factor(country), data = debt)

m1_8 <- lm(cum_revision ~ years_since_original + 
               yrcurnt_corrected * finstress_mean +
               as.factor(country), data = debt)

m1_9 <- lm(cum_revision ~ years_since_original +
               endog_3*finstress_mean +
               as.factor(country), data = debt)

m1_10 <- lm(cum_revision ~ years_since_original + general_gov_deficit +
               endog_3*finstress_mean +
               as.factor(country), data = debt)

## Drop Greek outlier
debt_no_greece <- debt %>% filter(country != 'Greece')

m1_no_greece1 <- lm(cum_revision ~ years_since_original +
                    endog_3*finstress_mean +
                    as.factor(country), data = debt_no_greece)

m1_no_greece2 <- lm(cum_revision ~ years_since_original +
                       endog_3 + finstress_mean +
                       as.factor(country), data = debt_no_greece)

m1_no_greece3 <- lm(cum_revision ~ years_since_original +
                        yrcurnt_corrected +
                        as.factor(country), data = debt_no_greece)

m1_no_greece4 <- lm(cum_revision ~ years_since_original +
                        yrcurnt_corrected*finstress_mean +
                        as.factor(country), data = debt_no_greece)

m1_no_greece5 <- lm(cum_revision ~ years_since_original +
                        yrcurnt_corrected + finstress_mean +
                        as.factor(country), data = debt_no_greece)

# deficit revisions
deficit <- comb %>% filter(component == 'deficit')

m2_1 <- lm(cum_revision ~ years_since_original + euro_member +
               as.factor(country), data = deficit)

m2_2 <- lm(cum_revision ~ years_since_original + central_gov_debt +
               as.factor(country), data = deficit)

m2_3 <- lm(cum_revision ~ years_since_original + general_gov_deficit +
               as.factor(country), data = deficit)

m2_4 <- lm(cum_revision ~ years_since_original + yrcurnt_corrected +
               as.factor(country), data = deficit)

m2_5 <- lm(cum_revision ~ years_since_original + 
               endog_3 +
               as.factor(country), data = deficit)

m2_6 <- lm(cum_revision ~ years_since_original + yrcurnt_corrected + 
               finstress_mean +
               as.factor(country), data = deficit)

m2_7 <- lm(cum_revision ~ years_since_original + endog_3 + 
               finstress_mean +
               as.factor(country), data = deficit)

m2_8 <- lm(cum_revision ~ years_since_original + 
               yrcurnt_corrected * finstress_mean +
               as.factor(country), data = deficit)

m2_9 <- lm(cum_revision ~ years_since_original + 
               endog_3*finstress_mean +
               as.factor(country), data = deficit)

m2_10 <- lm(cum_revision ~ years_since_original + euro_member +
                general_gov_deficit + endog_3*finstress_mean +
               as.factor(country), data = deficit)

## Create results tables -------
vars <- c('Yrs. Since Original', 'Eurozone', 'Cent. Gov. Debt', 
          'Gen. Gov. Deficit', 'Yrs. to Election', 'Endog. Election',
          'Non-Endog. Election',
          'FinStress', 'Yrs. to Elect.*FinStress', 
          'Endog. Elect.*FinStress', 'Non-Endog. Elect.*FinStress')


stargazer(m1_1, m1_2, m1_3, m1_4, m1_5, m1_6, m1_7, m1_8, m1_9, m1_10,
          omit = 'as.factor*', 
          omit.stat = c('f', 'ser'), # so that it fits on the page
          out.header = F,
          title = 'Linear Regression Estimation of \\textbf{Debt} Revisions',
          dep.var.labels = 'Cumulative Debt Revisions',
          covariate.labels = vars,
          label = 'debt_results',
          add.lines = list(c('Country FE?', rep('Yes', 9))),
          font.size = 'tiny',
          out = 'working_paper/tables/debt_regressions.tex')


stargazer(m2_1, m2_2, m2_3, m2_4, m2_5, m2_6, m2_7, m2_8, m2_9, m2_10,
          omit = 'as.factor*', 
          omit.stat = c('f', 'ser'), # so that it fits on the page
          out.header = F,
          title = 'Linear Regression Estimation of \\textbf{Deficit} Revisions',
          dep.var.labels = 'Cumulative Deficit Revisions',
          covariate.labels = vars,
          label = 'deficit_results',
          add.lines = list(c('Country FE?', rep('Yes', 9))),
          font.size = 'tiny',
          out = 'working_paper/tables/deficit_regressions.tex')


## Plot marginal effect -------
# Election timing and finstress
finstress_elect_me <- plot_me(m1_8, term1 = 'yrcurnt_corrected', 
                              term2 = 'finstress_mean',
        fitted2 = seq(0.2, 0.75, by = 0.05)) +
    xlab('\nAnnual FinStress Mean') + 
    ylab('Marginal Effect of Election Timing\n')

ggsave(finstress_elect_me, 
       filename = 'working_paper/figures/finstress_elect_me.pdf')

# Election timing and endogenous elections
finstress_endog_elect_me <- plot_me(m1_9, term1 = 'endog_3Endogenous', 
                              term2 = 'finstress_mean',
                              fitted2 = seq(0.2, 0.75, by = 0.05)) +
    xlab('\nAnnual FinStress Mean') + 
    ylab('Margingal Effect of an Endogenous Election\n')

ggsave(finstress_endog_elect_me, 
       filename = 'working_paper/figures/finstress_endog_elect_me.pdf')

## ME for Deficit revisions
finstress_non_endog_deficit_me <- plot_me(m2_9, term1 = 'endog_3Non-endogenous', 
        term2 = 'finstress_mean',
        fitted2 = seq(0.2, 0.75, by = 0.05)) +
    xlab('\nAnnual FinStress Mean') + 
    ylab('Marginal Effect of a Non-endogenous Election\n')

ggsave(finstress_non_endog_deficit_me, 
       filename = 'working_paper/figures/finstress_non_endog_deficit_me.pdf')

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

predictions <- predict(m1_8, newdata = fitted, interval = 'confidence')
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

predictions <- predict(m1_9, newdata = fitted, interval = 'confidence')
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
    ylab('Predicted Cumulative Debt Revision\nAfter 3 Years (% GDP)\n') +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(country_predictions_timing, 
       filename = 'working_paper/figures/country_predict_required.pdf')

## Extra ----------------------------------------------------------------------

## Test if selection into election type by FinStress
library(nnet)

no_dups <- comb %>% FindDups(comb, Vars = c('country', 'year'), NotDups = T)

multi_endog3 <- nnet::multinom(endog_3 ~ finstress_mean, data = no_dups) 

# Find p-value
z <- summary(multi_endog3)$coefficients/summary(multi_endog3)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

logit_endog <- glm(endog_electionHW ~ finstress_mean + as.factor(country), 
                   data = no_dups)

stargazer(logit_endog, omit = 'as.factor*',
          covariate.labels = 'FinStress',
          out.header = F,
          title = 'Logistic Regression Estimation of Having an Endogenous Election',
          dep.var.labels = 'Endogenous Election',
          label = 'finstress_endog',
          add.lines = list(c('Country FE?', 'Yes')),
          out = 'working_paper/tables/finstress_predict_endog.tex'
          )
