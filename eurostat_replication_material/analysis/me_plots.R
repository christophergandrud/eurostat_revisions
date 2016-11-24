# ---------------------------------------------------------------------------- #
# Eurostat revisions and Marginal Effect Plots
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(plotMElm)
library(ggplot2)
library(gridExtra)

# Set working directory. Change as needed
setwd('/eurostat_replication_material/')

# Estimate models
source('analysis/main_regression_models_tables.R')

## Debt Euro ------
debt_euro_me <- plot_me(m1_2, term1 = 'euro_member',
                        term2 = 'central_gov_debt') +
    geom_vline(xintercept = 60, linetype = 'dashed') +
    scale_x_continuous(breaks = c(0, 60, 100, 150, 200)) +
    xlab('\nCentral Government Debt/GDP (%)') +
    ylab('Marginal Effect of Being a Eurozone Member\n')

# Financial market stress and Unscheduled elections ----------
fs_scheduled_me <- plot_me(m1_7, term1 = 'finstress_mean',
                            term2 = 'endog_3') +
    xlab('\nElection Type') +
    ylab('Margingal Effect of Financial Market Stress\n')


grid.arrange(debt_euro_me, fs_scheduled_me, nrow = 1, ncol = 2)


