

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
               central_gov_debt + euro_member +
               as.factor(country),
           data = debt)

m1_3 <- lm(cum_revision ~ lag_cum_revision + 
               central_gov_debt * euro_member +
               as.factor(country),
           data = debt)

m1_4 <- lm(cum_revision ~ lag_cum_revision + 
               central_gov_debt + 
               yrcurnt_corrected + fsi_annual_mean +
               as.factor(country),
           data = debt)

m1_5 <- lm(cum_revision ~ lag_cum_revision + 
               excessdef * 
               yrcurnt_corrected + fsi_annual_mean +
               as.factor(country),
           data = debt)

m1_6 <- lm(cum_revision ~ lag_cum_revision + 
               central_gov_debt + 
               yrcurnt_corrected * fsi_annual_mean +
               as.factor(country),
           data = debt)

m1_7 <- lm(cum_revision ~ lag_cum_revision + 
               central_gov_debt + 
               endog_3 + fsi_annual_mean +
               as.factor(country),
           data = debt)

m1_8 <- lm(cum_revision ~ lag_cum_revision + 
               central_gov_debt + 
               endog_3 * fsi_annual_mean +
               as.factor(country),
           data = debt)

m1_9 <- lm(cum_revision ~ lag_cum_revision + 
                central_gov_debt +
                fiscal_trans_gfs + gdp_growth + 
                as.factor(country),
            data = debt)

m1_10 <- lm(cum_revision ~ lag_cum_revision + 
                central_gov_debt +
                yrcurnt_corrected * fsi_annual_mean + 
                fiscal_trans_gfs + gdp_growth + 
                as.factor(country),
            data = debt)

m1_11 <- lm(cum_revision ~ lag_cum_revision + 
               central_gov_debt + 
               contracts +
               as.factor(country),
           data = debt)

m1_12 <- lm(cum_revision ~ lag_cum_revision + 
                yrcurnt_corrected * fsi_annual_mean + 
                central_gov_debt + contracts + 
                as.factor(country),
            data = debt)

m1_garbage_can <- lm(cum_revision ~ lag_cum_revision + 
                    yrcurnt_corrected * fsi_annual_mean + 
                    central_gov_debt * contracts +
                    as.factor(country),
            data = debt)


m1_gdp_trans <- lm(cum_revision ~ lag_cum_revision + 
                central_gov_debt +
                fiscal_trans_gfs + gdp_growth + 
                as.factor(country),
            data = debt)


# Create output table
var_labels_1 <- c('Cum Revisions (lag)', 'EDP', 'Revised Cent. Gov. Debt', 
                  'Euro Member',
                  'Election Timing', 'Unscheduled Elect.', 'Scheduled Elect.',
                  'Financial Stress', 'Fiscal Transparency',
                  'GDP Growth', 'Contracts', 
                  'Debt * Euro', 'EDP * Election Timing',
                  'Elect. Timing * Fin. Stress',
                  'Unscheduled Elect. * Fin. Stress', 
                  'Scheduled Elect. * Fin. Stress')


stargazer(m1_1, m1_2, m1_3, m1_4, m1_5, m1_6, m1_7, m1_8, m1_9, m1_10, m1_11,
          m1_12, 
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

# Debt Models
m_no_g1 <- lm(cum_revision ~ lag_cum_revision + 
                  excessdef + euro_member +
                  as.factor(country),
              data = debt_no_greece)

m_no_g2 <- lm(cum_revision ~ lag_cum_revision + 
                  excessdef * euro_member +
                  as.factor(country),
              data = debt_no_greece)

m_no_g3 <- lm(cum_revision ~ lag_cum_revision + 
                  central_gov_debt + euro_member +
                  as.factor(country),
              data = debt_no_greece)

m_no_g4 <- lm(cum_revision ~ lag_cum_revision + 
                  central_gov_debt * euro_member +
                  as.factor(country),
              data = debt_no_greece)

m_no_g5 <- lm(cum_revision ~ lag_cum_revision + 
                  central_gov_debt + 
                  yrcurnt_corrected + fsi_annual_mean +
                  as.factor(country),
              data =debt_no_greece)

m_no_g6 <- lm(cum_revision ~ lag_cum_revision + 
                  central_gov_debt + 
                  yrcurnt_corrected * fsi_annual_mean +
                  as.factor(country),
              data = debt_no_greece)

m_no_g7 <- lm(cum_revision ~ lag_cum_revision + 
                  excessdef * 
                  yrcurnt_corrected + fsi_annual_mean +
                  as.factor(country),
              data = debt_no_greece)

m_no_g8 <- lm(cum_revision ~ lag_cum_revision + 
                  central_gov_debt + 
                  endog_3 + fsi_annual_mean +
                  as.factor(country),
              data = debt_no_greece)

m_no_g9 <- lm(cum_revision ~ lag_cum_revision + 
                  central_gov_debt + 
                  endog_3 * fsi_annual_mean +
                  as.factor(country),
              data = debt_no_greece)

m_no_g10 <- lm(cum_revision ~ lag_cum_revision + 
               central_gov_debt +
               fiscal_trans_gfs + gdp_growth + 
               as.factor(country),
           data = debt_no_greece)

m_no_g11 <- lm(cum_revision ~ lag_cum_revision + 
                central_gov_debt +
                yrcurnt_corrected * fsi_annual_mean + 
                fiscal_trans_gfs + gdp_growth + 
                as.factor(country),
            data = debt_no_greece)

m_no_g12 <- lm(cum_revision ~ lag_cum_revision + 
                central_gov_debt + 
                contracts +
                as.factor(country),
            data = debt_no_greece)

m_no_g13 <- lm(cum_revision ~ lag_cum_revision + 
                yrcurnt_corrected * fsi_annual_mean + 
                central_gov_debt + contracts + 
                as.factor(country),
            data = debt_no_greece)

m_no_g_garbage_can <- lm(cum_revision ~ lag_cum_revision + 
                         central_gov_debt * contracts +
                         yrcurnt_corrected * fsi_annual_mean + 
                         as.factor(country),
                     data = debt_no_greece)

var_labels_2 <- c('Cum Revisions (lag)', 'EDP', 'Revised Cent.Gov. Debt', 
                  'Euro Member',
                  'Election Timing', 'Unscheduled Elect.', 'Scheduled Elect.',
                  'Financial Stress', 'Fiscal Transparency',
                  'GDP Growth', 'Contracts', 
                  'EDP * Euro', 'Debt * Euro',
                  'Elect. Timing * Fin. Stress', 'EDP * Election Timing',
                  'Unscheduled Elect. * Fin. Stress', 
                  'Scheduled Elect. * Fin. Stress')


# Create output table
stargazer(m_no_g1, m_no_g2, m_no_g3, m_no_g4, m_no_g5, m_no_g6, m_no_g7, 
          m_no_g8, m_no_g9, m_no_g10, m_no_g11, m_no_g12, m_no_g13,
          omit = 'as.factor*', 
          omit.stat = c('f', 'ser'), # so that it fits on the page
          out.header = F,
          #add.lines = list(c('Country FE?', rep('Yes', 9))),
          dep.var.labels = 'Cumulative Debt Revisions',
          label = 'results_no_greece',
          covariate.labels = var_labels_2,
          star.cutoffs = c(0.05, 0.01, 0.001),
          title = 'Linear Regression Estimation of Debt Revisions (Excluding Greece)',
          add.lines = list(c('Country FE?', rep('Yes', 11))),
          font.size = 'tiny',
          out = 'working_paper/tables/debt_regressions_no_greece.tex')


## Stress and selection into elections --------
no_dups <- comb %>% FindDups(comb, Vars = c('country', 'year'), NotDups = T)

no_dups$unsched <- 0
no_dups$unsched[no_dups$endog_3 == 'Unscheduled'] <- 1

no_dups <- slide(no_dups, Var = 'fsi_annual_mean', TimeVar = 'version', 
                 GroupVar = 'country', NewVar = 'lag_stress',
                 slideBy = -1)

logit_endog <- glm(unsched ~ fsi_annual_mean + as.factor(country), 
                   data = no_dups)

logit_endog_lag <- glm(unsched ~ lag_stress + as.factor(country), 
                   data = no_dups)

stargazer(logit_endog, omit = 'as.factor*',
          covariate.labels = 'Financial Stress',
          out.header = F,
          title = 'Logistic Regression Estimation of Having an Unscheduled Election',
          dep.var.labels = 'Unscheduled Election',
          label = 'finstress_endog',
          add.lines = list(c('Country FE?', 'Yes')),
          out = 'working_paper/tables/fsi_predict_endog.tex')


# deficit revisions ---------
deficit <- comb %>% filter(component == 'deficit')
FindDups(deficit, c('country', 'year', 'version'))

# Create debt lag
deficit <- slide(deficit, Var = 'cum_revision', TimeVar = 'version', 
                 GroupVar = 'country', NewVar = 'lag_cum_revision',
                 slideBy = -1)

# Deficit Models
m2_1 <- lm(cum_revision ~ lag_cum_revision + 
               excessdef + euro_member +
               as.factor(country),
           data = deficit)

m2_2 <- lm(cum_revision ~ lag_cum_revision + 
               general_gov_deficit + euro_member +
               as.factor(country),
           data = deficit)

m2_3 <- lm(cum_revision ~ lag_cum_revision + 
               general_gov_deficit * euro_member +
               as.factor(country),
           data = deficit)

m2_4 <- lm(cum_revision ~ lag_cum_revision + 
               general_gov_deficit + 
               yrcurnt_corrected + fsi_annual_mean +
               as.factor(country),
           data = deficit)

m2_5 <- lm(cum_revision ~ lag_cum_revision + 
               excessdef * 
               yrcurnt_corrected + fsi_annual_mean +
               as.factor(country),
           data = deficit)

m2_6 <- lm(cum_revision ~ lag_cum_revision + 
               general_gov_deficit + 
               yrcurnt_corrected * fsi_annual_mean +
               as.factor(country),
           data = deficit)

m2_7 <- lm(cum_revision ~ lag_cum_revision + 
               general_gov_deficit + 
               endog_3 + fsi_annual_mean +
               as.factor(country),
           data = deficit)

m2_8 <- lm(cum_revision ~ lag_cum_revision + 
               general_gov_deficit + 
               endog_3 * fsi_annual_mean +
               as.factor(country),
           data = deficit)


m2_9 <- lm(cum_revision ~ lag_cum_revision + 
                general_gov_deficit +
                   fiscal_trans_gfs + gdp_growth + 
                   as.factor(country),
               data = deficit)

m2_10 <- lm(cum_revision ~ lag_cum_revision + 
                general_gov_deficit +
                   yrcurnt_corrected * fsi_annual_mean + 
                   fiscal_trans_gfs + gdp_growth + 
                   as.factor(country),
               data = deficit)

m2_11 <- lm(cum_revision ~ lag_cum_revision + 
               general_gov_deficit + 
               contracts +
               as.factor(country),
           data = deficit)

m2_12 <- lm(cum_revision ~ lag_cum_revision + 
                general_gov_deficit + 
                yrcurnt_corrected * fsi_annual_mean + 
                contracts +
                as.factor(country),
            data = deficit)

# Create output table
var_labels_1 <- c('Cum Revisions (lag)', 'EDP', 'Revised Gen. Gov. Deficit', 
                  'Euro Member',
                  'Election Timing', 'Unscheduled Elect.', 'Scheduled Elect.',
                  'Financial Stress', 'Fiscal Transparency',
                  'GDP Growth', 'Contracts', 
                  'Deficit * Euro', 'EDP * Election Timing',
                  'Elect. Timing * Fin. Stress',
                  'Unscheduled Elect. * Fin. Stress', 
                  'Scheduled Elect. * Fin. Stress')


stargazer(m2_1, m2_2, m2_3, m2_4, m2_5, m2_6, m2_7, m2_8, m2_9, m2_10, m2_11,
          m2_12,
          omit = 'as.factor*', 
          omit.stat = c('f', 'ser'), # so that it fits on the page
          out.header = F,
          dep.var.labels = 'Cumulative Deficit Revisions',
          covariate.labels = var_labels_1,
          star.cutoffs = c(0.05, 0.01, 0.001),
          label = 'deficit_results',
          title = 'Linear Regression Estimation of Deficit Revisions (Full Sample)',
          add.lines = list(c('Country FE?', rep('Yes', 11))),
          font.size = 'tiny',
          out = 'working_paper/tables/deficit_regressions.tex')
