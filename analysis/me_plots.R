# ---------------------------------------------------------------------------- #
# Eurostat revisions and Marginal Effect Plots
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(ggplot2)
library(gridExtra)

# Set working directory. Change as needed
setwd('/git_repositories/eurostat_revisions/')

# Load plot function
source('analysis/functions/plot_me.R')

# Estimate models
source('analysis/regression_models_tables.R')


# Election timing and Financial Stress ------
fsi_elect_me <- plot_me(m1_6, term1 = 'yrcurnt_corrected', 
                        term2 = 'fsi_annual_mean',
                        fitted2 = seq(0, 0.58, by = 0.05)) +
    xlab('') + 
    ylab('Marginal Effect of Being a Year Closer to an Election\n')

# Election timing and Unscheduled elections ----------
fsi_scheduled_me <- plot_me(m1_8, term1 = 'endog_3Unscheduled', 
                            term2 = 'fsi_annual_mean',
                            fitted2 = seq(0, 0.58, by = 0.05)) +
    xlab('') + 
    ylab('Margingal Effect of an Unscheduled Election\n')

pdf(file = 'working_paper/figures/fsi_elect_me.pdf', 
    width = 11, height = 7)
grid.arrange(fsi_elect_me, fsi_scheduled_me, nrow = 1,
             bottom = 'Mean Annual Financial Market Stress')
dev.off()


## Debt Euro ------
# Reverse order for plotting
m1_debt_euro <- lm(cum_revision ~ lag_cum_revision +
                       euro_member * central_gov_debt +
                       as.factor(country), data = debt)

debt_euro_me <- plot_me(m1_debt_euro, term1 = 'euro_member', 
                        term2 = 'central_gov_debt',
                        fitted2 = seq(5, 200, by = 5)) +
    geom_vline(xintercept = 60, linetype = 'dashed') +
    scale_x_continuous(breaks = c(0, 60, 100, 150, 200)) +
    xlab('\nCentral Government Debt/GDP (%)') + 
    ylab('Marginal Effect of Being a Eurozone Member\n')

## Election timing and EDP -----------
## Reverse order
m1_edp_timing <- lm(cum_revision ~ lag_cum_revision + excessdef * yrcurnt_corrected + 
                        fsi_annual_mean +
                        as.factor(country), data = debt)

timing_edp_me <- plot_me(m1_edp_timing, term1 = 'excessdef', 
                         term2 = 'yrcurnt_corrected',
                         fitted2 = seq(0, 4, by = 1)) +
    scale_x_continuous(labels = 4:0) +
    xlab('\nYears to Election') + 
    ylab('Marginal Effect of Being in an Excessive Deficit Procedure\n')

pdf(file = 'working_paper/figures/edp_debt_elect_me.pdf', 
    width = 11, height = 7)
grid.arrange(debt_euro_me, timing_edp_me, nrow = 1)
dev.off()



m1_contracts_debt <- lm(cum_revision ~ lag_cum_revision + 
                            contracts * central_gov_debt + 
                            as.factor(country),
                        data = debt)

contracts_debt_me <- plot_me(m1_contracts_debt, term1 = 'contracts', 
                             term2 = 'central_gov_debt',
                             fitted2 = seq(5, 200, by = 5)) +
    scale_x_continuous(labels = 4:0) +
    xlab('\nCentral Government Debt/GDP (%)') + 
    ylab('Marginal Effect of Being a 1 Unit Increase in Contracts\n')
