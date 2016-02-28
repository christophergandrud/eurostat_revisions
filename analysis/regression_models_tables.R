

#pFtest(fixed, normal)


# ---------------------------------------------------------------------------- #
# Eurostat revisions and EDP, debt, elections, and contracts models
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
source('analysis/functions/plot_me.R')

# Function to reverse the direction of the election timing variable
reverser <- function(x) max(x, na.rm = T) - x

# Import and final clean --------------
comb <- import('data_cleaning/main_merged.csv')   

comb$endog_3 <- factor(comb$endog_3, 
                       levels = c(1:3),
                       labels = c('Unscheduled', 'Scheduled', 'No election'))

comb$endog_3 <- relevel(comb$endog_3, ref = 'No election')

comb$from_2010 <- 0
comb$from_2010[comb$year >= 2010] <- 1

comb$yrcurnt_corrected <- reverser(comb$yrcurnt_corrected)

## Estimate models 
# debt revisions ---------
debt <- comb %>% filter(component == 'debt')
FindDups(debt, c('country', 'year', 'version'))

# Create debt lag
debt <- slide(debt, Var = 'cum_revision', TimeVar = 'version', 
              GroupVar = 'country', NewVar = 'lag_cum_revision',
              slideBy = -1)

# Debt Models
m1_1 <- lm(cum_revision ~ lag_cum_revision + 
               excessdef + euro_member +
                as.factor(country),
             data = debt)

m1_2 <- lm(cum_revision ~ lag_cum_revision + 
               gen_gov_debt + euro_member +
               as.factor(country),
           data = debt)

m1_3 <- lm(cum_revision ~ lag_cum_revision + 
               excessdef + 
               yrcurnt_corrected + fsi_annual_mean +
               as.factor(country),
           data = debt)

m1_4 <- lm(cum_revision ~ lag_cum_revision + 
               excessdef + 
               yrcurnt_corrected * fsi_annual_mean +
               as.factor(country),
           data = debt)

m1_5 <- lm(cum_revision ~ lag_cum_revision + 
               excessdef + 
               endog_3 + fsi_annual_mean +
               as.factor(country),
           data = debt)

m1_6 <- lm(cum_revision ~ lag_cum_revision + 
               excessdef + 
               endog_3 * fsi_annual_mean +
               as.factor(country),
           data = debt)

m1_7 <- lm(cum_revision ~ lag_cum_revision + 
               excessdef + 
               contracts +
               as.factor(country),
           data = debt)

m1_8 <- lm(cum_revision ~ lag_cum_revision + 
               central_gov_debt * contracts + 
               as.factor(country),
           data = debt)


m1_9 <- lm(cum_revision ~ lag_cum_revision + 
                central_gov_debt * contracts +
                yrcurnt_corrected * fsi_annual_mean + 
                as.factor(country),
            data = debt)


# Create output table
var_labels_1 <- c('Cum Revisions (lag)', 'EDP', 'Revised Gen. Debt', 'Euro Member',
                'Election Timing', 'Unscheduled Elect.', 'Scheduled Elect.',
                'Financial Stress', 'Contracts', 
                'Elect. Timing * Fin. Stress',
                'Unscheduled Elect. * Fin. Stress', 
                'Scheduled Elect. * Fin. Stress', 'EDP * Contracts')


stargazer(m1_1, m1_2, m1_3, m1_4, m1_5, m1_6, m1_7, m1_8, m1_9,
          omit = 'as.factor*', 
          omit.stat = c('f', 'ser'), # so that it fits on the page
          out.header = F,
          #add.lines = list(c('Country FE?', rep('Yes', 9))),
          dep.var.labels = 'Cumulative Debt Revisions',
          covariate.labels = var_labels_1,
          star.cutoffs = c(0.05, 0.01, 0.001),
          label = 'debt_results',
          title = 'Linear Regression Estimation of Debt Revisions (Full Sample)',
          add.lines = list(c('Country FE?', rep('Yes', 11))),
          font.size = 'tiny',
          out = 'working_paper/tables/debt_regressions.tex')



## Drop Greek outlier -------
debt_no_greece <- debt %>% filter(country != 'Greece')

m_no_g1 <- lm(cum_revision ~ lag_cum_revision + 
               excessdef + central_gov_debt + euro_member +
               as.factor(country),
           data = debt_no_greece)

m_no_g2 <- lm(cum_revision ~ lag_cum_revision + 
               gen_gov_debt * euro_member + 
               as.factor(country),
           data = debt_no_greece)

m_no_g3 <- lm(cum_revision ~ lag_cum_revision + 
               gen_gov_debt + 
               yrcurnt_corrected + fsi_annual_mean +
               as.factor(country),
           data = debt_no_greece)

m_no_g4 <- lm(cum_revision ~ lag_cum_revision + 
               excessdef  + 
               yrcurnt_corrected * fsi_annual_mean +
               as.factor(country),
           data = debt_no_greece)

m_no_g6 <- lm(cum_revision ~ lag_cum_revision + 
               excessdef + 
               endog_3 + fsi_annual_mean +
               as.factor(country),
           data = debt_no_greece)

m_no_g7 <- lm(cum_revision ~ lag_cum_revision + 
               excessdef + 
               endog_3 * fsi_annual_mean +
               as.factor(country),
           data = debt_no_greece)

m_no_g8 <- lm(cum_revision ~ lag_cum_revision + 
               excessdef + gen_gov_debt + 
               contracts +
               as.factor(country),
           data = debt_no_greece)

m_no_g9 <- lm(cum_revision ~ lag_cum_revision + 
               excessdef * contracts + gen_gov_debt +
               as.factor(country),
           data = debt_no_greece)

m_no_g10 <- lm(cum_revision ~ lag_cum_revision + 
                excessdef + central_gov_debt * contracts +
                as.factor(country),
            data = debt_no_greece)

var_labels_1 <- c('Cum Revisions (lag)', 'EDP', 'Revised Gen. Debt', 'Euro Member',
                  'Election Timing', 'Unscheduled Elect.', 'Scheduled Elect.',
                  'Financial Stress', 'Contracts', 
                  'Elect. Timing * Fin. Stress',
                  'Unscheduled Elect. * Fin. Stress', 
                  'Scheduled Elect. * Fin. Stress', 'EDP * Contracts')


# Create output table
stargazer(m_no_g1, m_no_g2, m_no_g3, m_no_g4, m_no_g5, m_no_g6, m_no_g7, 
          m_no_g8, m_no_g9, m_no_g10, m_no_g11,
          omit = 'as.factor*', 
          omit.stat = c('f', 'ser'), # so that it fits on the page
          out.header = F,
          #add.lines = list(c('Country FE?', rep('Yes', 9))),
          dep.var.labels = 'Cumulative Debt Revisions',
          covariate.labels = var_labels,
          star.cutoffs = c(0.05, 0.01, 0.001),
          #start.character = c('✝', '*', '**'),
          add.lines = list(c('Country FE?', rep('Yes', 11))),
          type = 'text')
