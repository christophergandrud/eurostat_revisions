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
library(Zelig)

# Set working directory. Change as needed
setwd('/git_repositories/eurostat_revisions/')

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

# Put FinStress on a more easily interpretable scale
#comb$finstress_mean <- comb$finstress_mean * 100

## Estimate models
# Debt revisions ---------
debt <- comb %>% filter(component == 'debt')
FindDups(debt, c('country', 'year', 'version'))

# Keep only the final cumulative revision, if in the 3rd year from the original
# figure was published
debt <- debt[!duplicated(debt[, c('country', 'year')], fromLast = TRUE), ]
debt <- subset(debt, years_since_original == 3)

# Export for reanalysis in Stata with clustered standard errors rather than
# fixed effects
foreign::write.dta(debt, 'data_cleaning/debt_sample.dta')

# Debt Models
m1_1 <- lm(cum_revision ~
               central_gov_debt + euro_member + excessdef +
               as.factor(country),
           data = debt)

m1_2 <- lm(cum_revision ~
               central_gov_debt * euro_member +
               as.factor(country),
           data = debt)

m1_3 <- lm(cum_revision ~
               central_gov_debt + euro_member +
               yrcurnt_corrected + finstress_mean +
               as.factor(country),
           data = debt)

m1_4 <- lm(cum_revision ~
               central_gov_debt + euro_member +
               yrcurnt_corrected * finstress_mean +
               as.factor(country),
           data = debt)

m1_5 <- lm(cum_revision ~
               central_gov_debt + euro_member +
               endog_3 + finstress_mean +
               as.factor(country),
           data = debt)

m1_6 <- lm(cum_revision ~
               central_gov_debt + euro_member +
               endog_3 * finstress_mean +
               as.factor(country),
           data = debt)

m1_7 <- lm(cum_revision ~
               central_gov_debt + euro_member +
               fiscal_trans_gfs + gdp_growth +
                as.factor(country),
            data = debt)

m1_8 <- lm(cum_revision ~
               central_gov_debt + euro_member +
               yrcurnt_corrected * finstress_mean +
                fiscal_trans_gfs + gdp_growth +
                as.factor(country),
            data = debt)

m1_9 <- lm(cum_revision ~
                central_gov_debt + euro_member +
                contracts +
               as.factor(country),
           data = debt)

m1_10 <- lm(cum_revision ~
                    central_gov_debt * euro_member + excessdef +
                    endog_3 * finstress_mean +
                    gdp_growth +
                    as.factor(country),
            data = debt)


m1_gdp_trans <- lm(cum_revision ~
                central_gov_debt +
                fiscal_trans_gfs + gdp_growth +
                as.factor(country),
            data = debt)

m1_indep <- lm(cum_revision ~
               central_gov_debt + euro_member + monitor_n_DR +
               as.factor(country),
           data = debt)

m1_indep_debt_monitor <- lm(cum_revision ~ lag_cum_revision +
               central_gov_debt + monitor_n_DR +
               as.factor(country),
           data = debt)

# Create output table
var_labels_1 <- c('Revised Cent. Gov. Debt', 'Euro Member', 'EDP',
                  'Election Timing', 'Unscheduled Elect.', 'Scheduled Elect.',
                  'Financial Stress', 'Fiscal Transparency',
                  'GDP Growth', 'Contracts',
                  'Debt * Euro',
                  'Elect. Timing * Fin. Stress',
                  'Unscheduled Elect. * Fin. Stress',
                  'Scheduled Elect. * Fin. Stress')


stargazer(m1_1, m1_2, m1_3, m1_4, m1_5, m1_6, m1_7, m1_8, m1_9, m1_10,
          omit = 'as.factor*',
          omit.stat = c('f', 'ser'), # so that it fits on the page
          out.header = F,
          #add.lines = list(c('Country FE?', rep('Yes', 9))),
          dep.var.labels = 'Cumulative Debt Revisions',
          covariate.labels = var_labels_1,
          star.cutoffs = c(0.05, 0.01, 0.001),
          label = 'debt_results',
          title = 'Linear Regression Estimation of Debt Revisions (Full Sample)',
          add.lines = list(c('Country FE?', rep('Yes', 12))),
          font.size = 'tiny',
          out = 'working_paper/tables/debt_regressions.tex')



## Drop Greek outlier -------
debt_no_greece <- debt %>% filter(country != 'Greece')

# Debt Models
m_no_g1 <- lm(cum_revision ~
               central_gov_debt + euro_member + excessdef +
               as.factor(country),
              data = debt_no_greece)

m_no_g2 <- lm(cum_revision ~
               central_gov_debt * euro_member +
               as.factor(country),
              data = debt_no_greece)

m_no_g3 <- lm(cum_revision ~
               central_gov_debt + euro_member +
               yrcurnt_corrected + finstress_mean +
               as.factor(country),
              data = debt_no_greece)

m_no_g4 <- lm(cum_revision ~
               central_gov_debt + euro_member +
               yrcurnt_corrected * finstress_mean +
               as.factor(country),
              data = debt_no_greece)

m_no_g5 <- lm(cum_revision ~
               central_gov_debt + euro_member +
               endog_3 + finstress_mean +
               as.factor(country),
              data = debt_no_greece)

m_no_g6 <- lm(cum_revision ~
               central_gov_debt + euro_member +
               endog_3 * finstress_mean +
               as.factor(country),
              data = debt_no_greece)

m_no_g7 <- lm(cum_revision ~
               central_gov_debt + euro_member +
               fiscal_trans_gfs + gdp_growth +
                as.factor(country),
              data = debt_no_greece)

m_no_g8 <- lm(cum_revision ~
               central_gov_debt + euro_member +
               yrcurnt_corrected * finstress_mean +
                fiscal_trans_gfs + gdp_growth +
                as.factor(country),
              data = debt_no_greece)

m_no_g9 <- lm(cum_revision ~
                central_gov_debt + euro_member +
                contracts +
               as.factor(country),
              data = debt_no_greece)

m_no_g10 <- lm(cum_revision ~
                    central_gov_debt * euro_member + excessdef +
                    endog_3 * finstress_mean +
                    gdp_growth +
                    as.factor(country),
           data = debt_no_greece)


m_no_g_garbage_can <- lm(cum_revision ~
                         central_gov_debt * contracts +
                         yrcurnt_corrected * finstress_mean +
                         as.factor(country),
                     data = debt_no_greece)

var_labels_2 <- c('Revised Cent. Gov. Debt', 'Euro Member', 'EDP',
                  'Election Timing', 'Unscheduled Elect.', 'Scheduled Elect.',
                  'Financial Stress', 'Fiscal Transparency',
                  'GDP Growth', 'Contracts',
                  'Debt * Euro',
                  'Elect. Timing * Fin. Stress',
                  'Unscheduled Elect. * Fin. Stress',
                  'Scheduled Elect. * Fin. Stress')


# Create output table
stargazer(m_no_g1, m_no_g2, m_no_g3, m_no_g4, m_no_g5, m_no_g6, m_no_g7,
          m_no_g8, m_no_g9, m_no_g10,
          omit = 'as.factor*',
          omit.stat = c('f', 'ser'), # so that it fits on the page
          out.header = F,
          #add.lines = list(c('Country FE?', rep('Yes', 9))),
          dep.var.labels = 'Cumulative Debt Revisions',
          label = 'results_no_greece',
          covariate.labels = var_labels_2,
          star.cutoffs = c(0.05, 0.01, 0.001),
          title = 'Linear Regression Estimation of Debt Revisions (Excluding Greece)',
          add.lines = list(c('Country FE?', rep('Yes', 13))),
          font.size = 'tiny',
          out = 'working_paper/tables/debt_regressions_no_greece.tex')


## Stress and selection into elections --------
no_dups <- comb %>% FindDups(comb, Vars = c('country', 'year'), NotDups = T)

no_dups$unsched <- 0
no_dups$unsched[no_dups$endog_3 == 'Unscheduled'] <- 1

no_dups <- slide(no_dups, Var = 'finstress_mean', TimeVar = 'version',
                 GroupVar = 'country', NewVar = 'lag_stress',
                 slideBy = -1)

logit_endog <- glm(unsched ~ finstress_mean + as.factor(country),
                   data = no_dups, family = 'binomial')

rare_logit <- zelig(unsched ~ finstress_mean + as.factor(country),
              data = no_dups, model = 'relogit')

logit_endog_lag <- glm(unsched ~ lag_stress + as.factor(country),
                   data = no_dups, family = 'binomial')

rare_logit_lag <- zelig(unsched ~ lag_stress + as.factor(country),
                    data = no_dups, model = 'relogit')

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

# Keep only the final cumulative revision, if in the 3rd year from the original
# figure was published
deficit <- deficit[!duplicated(debt[, c('country', 'year')], fromLast = TRUE), ]
deficit <- subset(deficit, years_since_original == 3)

# Deficit Models
m2_1 <- lm(cum_revision ~ general_gov_deficit +
                + euro_member + excessdef +
                as.factor(country),
           data = deficit)

m2_2 <- lm(cum_revision ~
               general_gov_deficit * euro_member +
               as.factor(country),
           data = deficit)

m2_3 <- lm(cum_revision ~
               general_gov_deficit * euro_member +
               as.factor(country),
           data = deficit)

m2_4 <- lm(cum_revision ~
               general_gov_deficit + euro_member +
               yrcurnt_corrected + finstress_mean +
               as.factor(country),
           data = deficit)

m2_5 <- lm(cum_revision ~
               general_gov_deficit +
               euro_member + endog_3 + finstress_mean +
               as.factor(country),
           data = deficit)

m2_6 <- lm(cum_revision ~
               general_gov_deficit + euro_member +
               endog_3 * finstress_mean +
               as.factor(country),
           data = deficit)

m2_7 <- lm(cum_revision ~
               general_gov_deficit + euro_member +
               fiscal_trans_gfs + gdp_growth +
                as.factor(country),
           data = deficit)

m2_8 <- lm(cum_revision ~
               general_gov_deficit + euro_member +
               yrcurnt_corrected * finstress_mean +
                fiscal_trans_gfs + gdp_growth +
                as.factor(country),
           data = deficit)


m2_9 <- lm(cum_revision ~
                general_gov_deficit + euro_member +
                contracts +
               as.factor(country),
               data = deficit)

m2_10 <- lm(cum_revision ~
                general_gov_deficit * euro_member + excessdef +
                endog_3 * finstress_mean +
                gdp_growth +
                as.factor(country),
               data = deficit)

# Create output table
var_labels_1 <- c('Revised Gen. Gov. Deficit',
                  'Euro Member', 'EDP',
                  'Election Timing', 'Unscheduled Elect.', 'Scheduled Elect.',
                  'Financial Stress', 'Fiscal Transparency',
                  'GDP Growth', 'Contracts',
                  'Debt * Euro',
                  'Elect. Timing * Fin. Stress',
                  'Unscheduled Elect. * Fin. Stress',
                  'Scheduled Elect. * Fin. Stress')


stargazer(m2_1, m2_2, m2_3, m2_4, m2_5, m2_6, m2_7, m2_8,
            m2_9, m2_10,
          omit = 'as.factor*',
          omit.stat = c('f', 'ser'), # so that it fits on the page
          out.header = F,
          dep.var.labels = 'Cumulative Deficit Revisions',
          covariate.labels = var_labels_1,
          star.cutoffs = c(0.05, 0.01, 0.001),
          label = 'deficit_results',
          title = 'Linear Regression Estimation of Deficit Revisions (Full Sample)',
          add.lines = list(c('Country FE?', rep('Yes', 12))),
          font.size = 'tiny',
          out = 'working_paper/tables/deficit_regressions.tex')
