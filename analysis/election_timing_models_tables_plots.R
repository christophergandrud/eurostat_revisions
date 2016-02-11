# ---------------------------------------------------------------------------- #
# Eurostat revisions and election models
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(devtools)
library(stargazer)
library(DataCombine)

# Set working directory. Change as needed
setwd('/git_repositories/eurostat_revisions/')

# Load plot function
devtools::source_gist('d270ff55c2ca26286e90')
source('~/Desktop/plot_me.R')

# Function to reverse the direction of the election timing variable
reverser <- function(x) max(x, na.rm = T) - x

# Import
comb <- import('data_cleaning/main_merged.csv')   

comb$endog_3 <- factor(comb$endog_3, 
                       levels = c(1:3),
                       labels = c('Unscheduled', 'Scheduled', 'No election'))

comb$endog_3 <- relevel(comb$endog_3, ref = 'No election')

comb$from_2010 <- 0
comb$from_2010[comb$year >= 2010] <- 1

comb$yrcurnt_corrected <- reverser(comb$yrcurnt_corrected)

## Estimate models -------
# debt revisions
debt <- comb %>% filter(component == 'debt')
FindDups(debt, c('country', 'year', 'version'))

# Create debt lag
debt <- slide(debt, Var = 'cum_revision', TimeVar = 'version', 
              GroupVar = 'country', NewVar = 'lag_cum_revision',
              slideBy = -1)

# Debt Models
m1_1 <- lm(cum_revision ~ lag_cum_revision + yrcurnt_corrected +
               as.factor(country), data = debt)

m1_2 <- lm(cum_revision ~ lag_cum_revision + endog_3 +
               as.factor(country), data = debt)

m1_3 <- lm(cum_revision ~ lag_cum_revision + yrcurnt_corrected + 
               fsi_annual_mean +
               as.factor(country), data = debt)

m1_4 <- lm(cum_revision ~ lag_cum_revision + endog_3 + 
               fsi_annual_mean +
               as.factor(country), data = debt)

m1_5 <- lm(cum_revision ~ lag_cum_revision + 
               yrcurnt_corrected * fsi_annual_mean +
               as.factor(country), data = debt)

m1_6 <- lm(cum_revision ~ lag_cum_revision +
               endog_3 * fsi_annual_mean +
               as.factor(country), data = debt)

m1_7 <- lm(cum_revision ~ lag_cum_revision + euro_member + 
               as.factor(country), data = debt)

m1_8 <- lm(cum_revision ~ lag_cum_revision + central_gov_debt + 
               as.factor(country), data = debt)

m1_9 <- lm(cum_revision ~ lag_cum_revision + general_gov_deficit +
               as.factor(country), data = debt)

m1_10 <- lm(cum_revision ~ lag_cum_revision + fiscal_trans_gfs +
               as.factor(country), data = debt)

## Drop Greek outlier
debt_no_greece <- debt %>% filter(country != 'Greece')

m1_no_greece1 <- lm(cum_revision ~ lag_cum_revision +
                        fsi_annual_mean * yrcurnt_corrected +
                        as.factor(country), data = debt_no_greece)

m1_no_greece2 <- lm(cum_revision ~ lag_cum_revision +
                        fsi_annual_mean* endog_3 +
                    as.factor(country), data = debt_no_greece)

# deficit revisions
deficit <- comb %>% filter(component == 'deficit')

# Create deficit lag
deficit <- slide(deficit, Var = 'cum_revision', TimeVar = 'version', 
              GroupVar = 'country', NewVar = 'lag_cum_revision',
              slideBy = -1)

# Deficit Models
m2_1 <- lm(cum_revision ~ lag_cum_revision + yrcurnt_corrected +
               as.factor(country), data = deficit)

m2_2 <- lm(cum_revision ~ lag_cum_revision + endog_3 +
               as.factor(country), data = deficit)

m2_3 <- lm(cum_revision ~ lag_cum_revision + yrcurnt_corrected + 
               fsi_annual_mean +
               as.factor(country), data = deficit)

m2_4 <- lm(cum_revision ~ lag_cum_revision + endog_3 + 
               fsi_annual_mean +
               as.factor(country), data = deficit)

m2_5 <- lm(cum_revision ~ lag_cum_revision + 
               yrcurnt_corrected * fsi_annual_mean +
               as.factor(country), data = deficit)

m2_6 <- lm(cum_revision ~ lag_cum_revision +
               endog_3 * fsi_annual_mean +
               as.factor(country), data = deficit)

m2_7 <- lm(cum_revision ~ lag_cum_revision + euro_member + 
               as.factor(country), data = deficit)

m2_8 <- lm(cum_revision ~ lag_cum_revision + central_gov_debt + 
               as.factor(country), data = deficit)

m2_9 <- lm(cum_revision ~ lag_cum_revision + general_gov_deficit +
               as.factor(country), data = deficit)

m2_10 <- lm(cum_revision ~ lag_cum_revision + fiscal_trans_gfs +
                as.factor(country), data = deficit)

## Create results tables -------
vars <- c('Cum. Revisions (lag)', 'Election Timing', 'Unscheduled Elect.',
          'Scheduled Elect.',
          'Financial Stress',
          'Eurozone', 'Cent. Gov. Debt', 'Gen. Gov. Deficit', 'Fiscal Trans.',
          'Elect. Timing*Fin. Stress', 
          'Unscheduled.Elect*Fin. Stress', 'Scheduled.Elect*Fin. Stress')


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

vars_no_greece <- c('Cum. Revisions (lag)', 'Financial Stress', 
                    'Election Timing', 'Unscheduled Elect.', 'Scheduled Elect.',
                    'Elect. Timing*Fin. Stress', 
                    'Unscheduled Elect*Fin. Stress', 
                    'Scheduled Elect*Fin. Stress')

stargazer(m1_no_greece1, m1_no_greece2,
          omit = 'as.factor*', 
          omit.stat = c('f', 'ser'), # so that it fits on the page
          out.header = F,
          title = 'Linear Regression Estimation of \\textbf{Debt} Revisions (excluding Greece)',
          dep.var.labels = 'Cumulative Debt Revisions',
          covariate.labels = vars_no_greece,
          label = 'debt_no_greece_results',
          add.lines = list(c('Country FE?', rep('Yes', 2))),
          font.size = 'small',
          out = 'working_paper/tables/debt_no_greece_regressions.tex')          
          

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
# Election timing and Financial Stress
fsi_elect_me <- plot_me(m1_8, term1 = 'yrcurnt_corrected', 
                              term2 = 'fsi_annual_mean',
        fitted2 = seq(0, 0.58, by = 0.05)) +
    xlab('') + 
    ylab('Marginal Effect of Being a Year Closer to an Election\n')

# Election timing and Unscheduled elections
fsi_scheduled_me <- plot_me(m1_9, term1 = 'endog_3Unscheduled', 
                              term2 = 'fsi_annual_mean',
                              fitted2 = seq(0, 0.58, by = 0.05)) +
    xlab('') + 
    ylab('Margingal Effect of an Unscheduled Election\n')

pdf(file = 'working_paper/figures/fsi_elect_me.pdf', 
    width = 11, height = 7)
grid.arrange(fsi_elect_me, fsi_scheduled_me, nrow = 1,
             bottom = 'Mean Annual Financial Market Stress')
dev.off()

## ME for Deficit revisions and scheduled elections
fsi_election_timing_deficit_me <- plot_me(m2_8, term1 = 'yrcurnt_corrected', 
        term2 = 'fsi_annual_mean',
        fitted2 = seq(0, 0.58, by = 0.05)) +
    xlab('\nAnnual Financial Stress Mean') + 
    ylab('Marginal Effect of Being a Year Closer to a Scheduled Election\n')

ggsave(fsi_election_timing_deficit_me, 
       filename = 'working_paper/figures/fsi_elect_timing_deficit_me.pdf')

# Election timing and Financial Stress (no Greece)
# Re run to flip term order
m1_no_greece1 <- lm(cum_revision ~ lag_cum_revision +
                        yrcurnt_corrected * fsi_annual_mean +
                        as.factor(country), data = debt_no_greece)

m1_no_greece2 <- lm(cum_revision ~ lag_cum_revision +
                        endog_3 * fsi_annual_mean +
                        as.factor(country), data = debt_no_greece)

fsi_elect_me_nogr <- plot_me(m1_no_greece1, term1 = 'yrcurnt_corrected', 
                             term2 = 'fsi_annual_mean',
                             fitted2 = seq(0, 0.58, by = 0.05)) +
    xlab('') + 
    ylab('Marginal Effect of Being a Year Closer to an Election\n')

# Election timing and Unscheduled elections
fsi_scheduled_me_nogr <- plot_me(m1_no_greece2, term1 = 'endog_3Unscheduled', 
                                 term2 = 'fsi_annual_mean',
                                 fitted2 = seq(0, 0.58, by = 0.05)) +
    xlab('') + 
    ylab('Margingal Effect of an Unscheduled Election\n')

# Combine and save

pdf(file = 'working_paper/figures/debt_me_nogreece.pdf', width = 15)
    grid.arrange(fsi_elect_me_nogr, fsi_scheduled_me_nogr, ncol = 2,
                 bottom = 'Mean Annual Financial Market Stress')
dev.off()

## Simulate and plot predicted effects ------------------

# Scenarios for election timing ----

countries <- unique(debt$country)

# Drop Croatia, the newest EU member for which there are few revisions
# Drop Estonia because it does not have FSI data
countries <- countries[!(countries %in% c('Croatia', 'Estonia'))]

fitted <- NULL

for (i in (countries)) {
    temp <- debt %>% filter(country == i)
    min_fsi <- min(temp$fsi_annual_mean, na.rm = T)
    max_fsi <- max(temp$fsi_annual_mean, na.rm = T)
    
    temp_fitted <- data.frame(years_since_original = rep(3, 8),
                              yrcurnt_corrected = rep(0:3, 2),
                              fsi_annual_mean = c(rep(min_fsi, 4), 
                                                 rep(max_fsi, 4)),
                              country = rep(i, 8)
                              )
    fitted <- rbind(fitted, temp_fitted)
    rm(temp, temp_fitted)
}

temp_levels <- c(rep('low', 4), rep('high', 4))
fitted$fsi_level <- rep(temp_levels, length(countries))

predictions <- predict(m1_8, newdata = fitted, interval = 'confidence')
predictions <- cbind(predictions, fitted[, c('country', 'fsi_level',
                                             "yrcurnt_corrected")])

country_predictions_timing <- ggplot(predictions, aes(yrcurnt_corrected, fit, 
                                        group = fsi_level,
                                        colour = fsi_level, 
                                        fill = fsi_level)) +
    geom_hline(yintercept = 0, linetype = 'dotted') +
    facet_wrap(~country) +
    geom_line() +
    #geom_ribbon(data = predictions, aes(ymin = lwr, ymax = upr, 
    #            fill = fsi_level), alpha = 0.1) +
    scale_x_reverse() +
    scale_color_manual(values = c('#e34a33', '#7fcdbb'), 
                       name = 'Credit Provision\nStress') +
    xlab('\nYears Until Election') +
    ylab('Predicted Cumulative Debt Revision\nAfter 3 Years (% GDP)\n') +
    theme_bw()

ggsave(country_predictions_timing, 
       filename = 'working_paper/figures/country_predict_timing.pdf')

# Scenarios for Unscheduled elections ----

fitted <- NULL

for (i in (countries)) {
    temp <- debt %>% filter(country == i)
    min_fsi <- min(temp$fsi_annual_mean, na.rm = T)
    max_fsi <- max(temp$fsi_annual_mean, na.rm = T)
    
    temp_fitted <- data.frame(years_since_original = rep(3, 6),
                              endog_3 = as.factor(rep(c('No election',
                                                        'Unscheduled',
                                                        'Scheduled'), 2)),
                              fsi_annual_mean = c(rep(min_fsi, 3), 
                                                 rep(max_fsi, 3)),
                              country = rep(i, 6)
    )
    fitted <- rbind(fitted, temp_fitted)
    rm(temp, temp_fitted)
}

temp_levels <- c(rep('low', 3), rep('high', 3))
fitted$fsi_level <- rep(temp_levels, length(countries))

predictions <- predict(m1_9, newdata = fitted, interval = 'confidence')
predictions <- cbind(predictions, fitted[, c('country', 'fsi_level',
                                             'endog_3')])

predictions$endog_3 <- factor(predictions$endog_3, 
                              levels = c('No election', 'Scheduled', 
                                         'Unscheduled'))

country_predictions_timing <- ggplot(predictions, aes(endog_3, fit, 
                                                     group = fsi_level,
                                                     colour = fsi_level, 
                                                     fill = fsi_level)) +
    geom_hline(yintercept = 0, linetype = 'dotted') +
    facet_wrap(~country, ncol =  7) +
    geom_line() +
    geom_point() +
    geom_ribbon(data = predictions, aes(ymin = lwr, ymax = upr, 
                fill = fsi_level), alpha = 0.4, colour = NA) +
    scale_color_manual(values = c('#e34a33', '#7fcdbb'), 
                       name = 'Financial\nStress') +
    scale_fill_manual(values = c('#e34a33', '#7fcdbb'), 
                       name = 'Financial\nStress') +
    scale_y_continuous(breaks = c(-2.5, 0, 3.5, 7.5)) +
    xlab('\n') +
    ylab('Predicted Cumulative Debt Revision\nAfter 3 Years (% GDP)\n') +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(country_predictions_timing, 
       filename = 'working_paper/figures/country_predict_required.pdf')

## Extra ----------------------------------------------------------------------

## Test if selection into election type by Financial Stress
library(nnet)

no_dups <- comb %>% FindDups(comb, Vars = c('country', 'year'), NotDups = T)

multi_endog3 <- nnet::multinom(endog_3 ~ fsi_annual_mean, data = no_dups) 

# Find p-value
z <- summary(multi_endog3)$coefficients/summary(multi_endog3)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

logit_endog <- glm(endog_electionHW ~ fsi_annual_mean + as.factor(country), 
                   data = no_dups)

stargazer(logit_endog, omit = 'as.factor*',
          covariate.labels = 'Financial Stress',
          out.header = F,
          title = 'Logistic Regression Estimation of Having an Unscheduled Election',
          dep.var.labels = 'Unscheduled Election',
          label = 'finstress_endog',
          add.lines = list(c('Country FE?', 'Yes')),
          out = 'working_paper/tables/fsi_predict_endog.tex'
          )
