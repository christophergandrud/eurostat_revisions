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
setwd('/eurostat_replication_material/')

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
comb$finstress_mean <- comb$finstress_mean * 100

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
# foreign::write.dta(debt, 'data_cleaning/debt_sample.dta')

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
               finstress_mean + yrcurnt_corrected +
               as.factor(country),
           data = debt)

m1_4 <- lm(cum_revision ~
               central_gov_debt + euro_member *
               finstress_mean +
               as.factor(country),
           data = debt)

m1_5 <- lm(cum_revision ~
               central_gov_debt + euro_member +
               finstress_mean * yrcurnt_corrected +
               as.factor(country),
           data = debt)

m1_6 <- lm(cum_revision ~
               central_gov_debt + euro_member +
               finstress_mean + endog_3 +
               as.factor(country),
           data = debt)

m1_7 <- lm(cum_revision ~
               central_gov_debt + euro_member +
               finstress_mean * endog_3 +
               as.factor(country),
           data = debt)

m1_8 <- lm(cum_revision ~
               central_gov_debt + euro_member +
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
                    finstress_mean * endog_3 +
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

m1_indep_debt_monitor <- lm(cum_revision ~
               central_gov_debt + monitor_n_DR +
               as.factor(country),
           data = debt)


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
                  finstress_mean + yrcurnt_corrected +
               as.factor(country),
              data = debt_no_greece)

m_no_g4 <- lm(cum_revision ~
                  central_gov_debt + euro_member *
                  finstress_mean +
               as.factor(country),
              data = debt_no_greece)

m_no_g5 <- lm(cum_revision ~
                  central_gov_debt + euro_member +
                  finstress_mean * yrcurnt_corrected +
               as.factor(country),
              data = debt_no_greece)

m_no_g6 <- lm(cum_revision ~
                  central_gov_debt + euro_member +
                  finstress_mean + endog_3 +
               as.factor(country),
              data = debt_no_greece)

m_no_g7 <- lm(cum_revision ~
                  central_gov_debt + euro_member +
                  finstress_mean * endog_3 +
                as.factor(country),
              data = debt_no_greece)

m_no_g8 <- lm(cum_revision ~
                  central_gov_debt + euro_member +
                  fiscal_trans_gfs + gdp_growth +
                as.factor(country),
              data = debt_no_greece)

m_no_g9 <- lm(cum_revision ~
                  central_gov_debt + euro_member +
                  contracts +
               as.factor(country),
              data = debt_no_greece)

m_no_g10 <- lm(um_revision ~
                   central_gov_debt * euro_member + excessdef +
                   finstress_mean * endog_3 +
                   gdp_growth +
                    as.factor(country),
           data = debt_no_greece)


m_no_g_garbage_can <- lm(cum_revision ~
                         central_gov_debt * contracts +
                         yrcurnt_corrected * finstress_mean +
                         as.factor(country),
                     data = debt_no_greece)


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
               general_gov_deficit + euro_member +
               finstress_mean + yrcurnt_corrected +
               as.factor(country),
           data = deficit)

m2_4 <- lm(cum_revision ~
               general_gov_deficit + euro_member *
               finstress_mean +
               as.factor(country),
           data = deficit)

m2_5 <- lm(cum_revision ~
               general_gov_deficit +
               euro_member +
               finstress_mean * yrcurnt_corrected +
               as.factor(country),
           data = deficit)

m2_6 <- lm(cum_revision ~
               general_gov_deficit + euro_member +
               finstress_mean + endog_3 +
               as.factor(country),
           data = deficit)

m2_7 <- lm(cum_revision ~
               general_gov_deficit + euro_member +
               finstress_mean * endog_3 +
                as.factor(country),
           data = deficit)

m2_8 <- lm(cum_revision ~
               general_gov_deficit + euro_member +
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
                finstress_mean * endog_3 +
                gdp_growth +
                as.factor(country),
               data = deficit)
