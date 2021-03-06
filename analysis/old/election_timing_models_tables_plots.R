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
source('analysis/functions/plot_me.R')

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
               fsi_annual_mean +
               as.factor(country), data = debt)

m1_2 <- lm(cum_revision ~ lag_cum_revision + endog_3 + 
               fsi_annual_mean +
               as.factor(country), data = debt)

m1_3 <- lm(cum_revision ~ lag_cum_revision + 
               yrcurnt_corrected * fsi_annual_mean +
               as.factor(country), data = debt)

m1_4 <- lm(cum_revision ~ lag_cum_revision +
               endog_3 * fsi_annual_mean +
               as.factor(country), data = debt)

m1_5 <- lm(cum_revision ~ lag_cum_revision + general_gov_deficit +
               central_gov_debt + euro_member +
               as.factor(country), data = debt)

m1_6 <- lm(cum_revision ~ lag_cum_revision + general_gov_deficit +
               central_gov_debt * euro_member +
               as.factor(country), data = debt)

m1_7 <- lm(cum_revision ~ lag_cum_revision + euro_member + excessdef +
               as.factor(country), data = debt)

m1_8 <- lm(cum_revision ~ lag_cum_revision + euro_member * excessdef +
               as.factor(country), data = debt)

m1_9 <- lm(cum_revision ~ lag_cum_revision + yrcurnt_corrected*fsi_annual_mean + 
                       excessdef + 
                       as.factor(country), data = debt)

m1_10 <- lm(cum_revision ~ lag_cum_revision + endog_3*fsi_annual_mean + 
               excessdef + 
               as.factor(country), data = debt)

m1_11 <- lm(cum_revision ~ lag_cum_revision + yrcurnt_corrected*excessdef + 
                fsi_annual_mean +
                as.factor(country), data = debt)

m1_12 <- lm(cum_revision ~ lag_cum_revision + endog_3*excessdef + 
                fsi_annual_mean +
                as.factor(country), data = debt)

## Drop Greek outlier
debt_no_greece <- debt %>% filter(country != 'Greece')

m1_no_greece1 <- lm(cum_revision ~ lag_cum_revision +
                        fsi_annual_mean * yrcurnt_corrected + excessdef +
                        as.factor(country), data = debt_no_greece)

m1_no_greece2 <- lm(cum_revision ~ lag_cum_revision +
                        fsi_annual_mean* endog_3 + excessdef +
                    as.factor(country), data = debt_no_greece)

# deficit revisions
deficit <- comb %>% filter(component == 'deficit')

# Create deficit lag
deficit <- slide(deficit, Var = 'cum_revision', TimeVar = 'version', 
              GroupVar = 'country', NewVar = 'lag_cum_revision',
              slideBy = -1)

# Deficit Models
m2_1 <- lm(cum_revision ~ lag_cum_revision + yrcurnt_corrected + 
               fsi_annual_mean +
               as.factor(country), data = deficit)

m2_2 <- lm(cum_revision ~ lag_cum_revision + endog_3 + 
               fsi_annual_mean +
               as.factor(country), data = deficit)

m2_3 <- lm(cum_revision ~ lag_cum_revision + 
               yrcurnt_corrected * fsi_annual_mean +
               as.factor(country), data = deficit)

m2_4 <- lm(cum_revision ~ lag_cum_revision +
               endog_3 * fsi_annual_mean +
               as.factor(country), data = deficit)

m2_5 <- lm(cum_revision ~ lag_cum_revision + general_gov_deficit +
               central_gov_debt + euro_member +
               as.factor(country), data = deficit)

m2_6 <- lm(cum_revision ~ lag_cum_revision + general_gov_deficit * euro_member + 
               central_gov_debt +
               as.factor(country), data = deficit)

m2_7 <- lm(cum_revision ~ lag_cum_revision + euro_member + excessdef +
               as.factor(country), data = deficit)

m2_8 <- lm(cum_revision ~ lag_cum_revision + euro_member * excessdef +
               as.factor(country), data = deficit)

m2_9 <- lm(cum_revision ~ lag_cum_revision + yrcurnt_corrected*fsi_annual_mean + 
               excessdef + 
               as.factor(country), data = deficit)

m2_10 <- lm(cum_revision ~ lag_cum_revision + endog_3*fsi_annual_mean + 
                excessdef + 
                as.factor(country), data = deficit)

m2_11 <- lm(cum_revision ~ lag_cum_revision + yrcurnt_corrected*excessdef + 
                fsi_annual_mean +
                as.factor(country), data = deficit)

m2_12 <- lm(cum_revision ~ lag_cum_revision + endog_3*excessdef + 
                fsi_annual_mean +
                as.factor(country), data = deficit)

## Create results tables -------
vars_debt <- c('Cum. Revisions (lag)', 'Election Timing', 'Unscheduled Elect.',
            'Scheduled Elect.',
            'Financial Stress',
            'Gen. Gov. Deficit', 'Cent. Gov. Debt', 'Euro Member', 'EDP',
            'Elect. Timing*Fin. Stress', 
            'Unscheduled.Elect*Fin. Stress', 'Scheduled.Elect*Fin. Stress', 
            'Cent. Gov. Debt*Euro', 'EDP*Euro', 'Elect. Timing*EDP')


stargazer(m1_1, m1_2, m1_3, m1_4, m1_5, m1_6, m1_7, m1_8, m1_9, m1_10, m1_11,
          omit = 'as.factor*', 
          omit.stat = c('f', 'ser'), # so that it fits on the page
          out.header = F,
          title = 'Linear Regression Estimation of \\textbf{Debt} Revisions',
          dep.var.labels = 'Cumulative Debt Revisions',
          covariate.labels = vars_debt,
          label = 'debt_results',
          add.lines = list(c('Country FE?', rep('Yes', 9))),
          font.size = 'tiny',
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = 'working_paper/tables/debt_regressions.tex')

vars_no_greece <- c('Cum. Revisions (lag)', 'Financial Stress', 
                    'Election Timing', 'Unscheduled Elect.', 'Scheduled Elect.',
                    'EDP',
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
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = 'working_paper/tables/debt_no_greece_regressions.tex')          
          
vars_deficit <- c('Cum. Revisions (lag)', 'Election Timing', 'Unscheduled Elect.',
               'Scheduled Elect.',
               'Financial Stress',
               'Gen. Gov. Deficit', 'Cent. Gov. Debt', 'Euro Member', 'EDP',
               'Elect. Timing*Fin. Stress', 
               'Unscheduled.Elect*Fin. Stress', 'Scheduled.Elect*Fin. Stress', 
               'Gov. Deficit*Euro', 'EDP*Euro', 'Elect. Timing*EDP')


stargazer(m2_1, m2_2, m2_3, m2_4, m2_5, m2_6, m2_7, m2_8, m2_9, m2_10, m2_11,
          omit = 'as.factor*', 
          omit.stat = c('f', 'ser'), # so that it fits on the page
          out.header = F,
          title = 'Linear Regression Estimation of \\textbf{Deficit} Revisions',
          dep.var.labels = 'Cumulative Deficit Revisions',
          covariate.labels = vars_deficit,
          label = 'deficit_results',
          add.lines = list(c('Country FE?', rep('Yes', 9))),
          font.size = 'tiny',
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = 'working_paper/tables/deficit_regressions.tex')


## Plot marginal effect -------
# Election timing and Financial Stress
fsi_elect_me <- plot_me(m1_4, term1 = 'yrcurnt_corrected', 
                              term2 = 'fsi_annual_mean',
        fitted2 = seq(0, 0.58, by = 0.05)) +
    xlab('') + 
    ylab('Marginal Effect of Being a Year Closer to an Election\n')

# Election timing and Unscheduled elections
fsi_scheduled_me <- plot_me(m1_6, term1 = 'endog_3Unscheduled', 
                              term2 = 'fsi_annual_mean',
                              fitted2 = seq(0, 0.58, by = 0.05)) +
    xlab('') + 
    ylab('Margingal Effect of an Unscheduled Election\n')

pdf(file = 'working_paper/figures/fsi_elect_me.pdf', 
    width = 11, height = 7)
grid.arrange(fsi_elect_me, fsi_scheduled_me, nrow = 1,
             bottom = 'Mean Annual Financial Market Stress')
dev.off()

# Central government debt and euro membership

## Reverse order
m1_6_rev <- lm(cum_revision ~ lag_cum_revision + general_gov_deficit +
                   euro_member * central_gov_debt +
               as.factor(country), data = debt)

debt_euro_me <- plot_me(m1_6_rev, term1 = 'euro_member', 
        term2 = 'central_gov_debt',
        fitted2 = seq(5, 200, by = 5)) +
    geom_vline(xintercept = 60, linetype = 'dashed') +
    scale_x_continuous(breaks = c(0, 60, 100, 150, 200)) +
    xlab('\nCentral Government Debt/GDP (%)') + 
    ylab('Marginal Effect of Being a Eurozone Member\n')

# Election timing and EDP

## Reverse order
m1_11_rev <- lm(cum_revision ~ lag_cum_revision + excessdef * yrcurnt_corrected + 
                fsi_annual_mean +
                as.factor(country), data = debt)

timing_edp_me <- plot_me(m1_11_rev, term1 = 'excessdef', 
                         term2 = 'yrcurnt_corrected',
                         fitted2 = seq(0, 4, by = 1)) +
    scale_x_continuous(labels = 4:0) +
    xlab('\nYears to Election') + 
    ylab('Marginal Effect of Being in an Excessive Deficit Procedure\n')

pdf(file = 'working_paper/figures/edp_debt_elect_me.pdf', 
    width = 11, height = 7)
grid.arrange(debt_euro_me, timing_edp_me, nrow = 1)
dev.off()

# Election timing and Financial Stress (no Greece)
# Re run to flip term order
m1_no_greece1 <- lm(cum_revision ~ lag_cum_revision +
                        yrcurnt_corrected * fsi_annual_mean + excessdef +
                        as.factor(country), data = debt_no_greece)

m1_no_greece2 <- lm(cum_revision ~ lag_cum_revision +
                        endog_3 * fsi_annual_mean + excessdef +
                        as.factor(country), data = debt_no_greece)

m1_no_greece3 <- lm(cum_revision ~ lag_cum_revision + general_gov_deficit +
                   euro_member * central_gov_debt +
                   as.factor(country), data = debt_no_greece)

m1_no_greece4 <- lm(cum_revision ~ lag_cum_revision + 
                        excessdef * yrcurnt_corrected + fsi_annual_mean +
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

# Central government debt and euro membership

## Reverse order
debt_euro_me_nogr <- plot_me(m1_no_greece3, term1 = 'euro_member', 
                        term2 = 'central_gov_debt',
                        fitted2 = seq(5, 200, by = 5)) +
    geom_vline(xintercept = 60, linetype = 'dashed') +
    scale_x_continuous(breaks = c(0, 60, 100, 150, 200)) +
    xlab('\nCentral Government Debt/GDP (%)') + 
    ylab('Marginal Effect of Being a Eurozone Member\n')

# Election timing and EDP
timing_edp_me_nogr <- plot_me(m1_no_greece4, term1 = 'excessdef', 
                         term2 = 'yrcurnt_corrected',
                         fitted2 = seq(0, 4, by = 1)) +
    scale_x_continuous(labels = 4:0) +
    xlab('\nYears to Election') + 
    ylab('Marginal Effect of Being in an Excessive Deficit Procedure\n')

pdf(file = 'working_paper/figures/edp_debt_elect_me_nogr.pdf', 
    width = 11, height = 7)
grid.arrange(debt_euro_me_nogr, timing_edp_me_nogr, nrow = 1)
dev.off()





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
