# ---------------------------------------------------------------------------- #
# Eurostat revisions and Marginal Effect Plots
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(plotMElm)
library(ggplot2)
library(gridExtra)

# Set working directory. Change as needed
setwd('/git_repositories/eurostat_revisions/')

# Estimate models
source('analysis/regression_models_tables.R')

## Debt Euro ------
debt_euro_me <- plot_me(m1_2, term1 = 'euro_member', 
                        term2 = 'central_gov_debt') +
    geom_vline(xintercept = 60, linetype = 'dashed') +
    scale_x_continuous(breaks = c(0, 60, 100, 150, 200)) +
    xlab('\nCentral Government Debt/GDP (%)') + 
    ylab('Marginal Effect of Being a Eurozone Member\n')

# Election timing and Unscheduled elections ----------
fs_scheduled_me <- plot_me(m1_10, term1 = 'finstress_mean', 
                            term2 = 'endog_3') +
    xlab('\nElection Type') + 
    ylab('Margingal Effect of Financial Market Stress\n')


pdf(file = 'working_paper/figures/debt_me_comb.pdf', 
    width = 11, height = 6)
grid.arrange(debt_euro_me, fs_scheduled_me, nrow = 1, ncol = 2)
dev.off()



m1_contracts_debt <- lm(cum_revision ~  
                            contracts * central_gov_debt + 
                            as.factor(country),
                        data = debt)

contracts_debt_me <- plot_me(m1_contracts_debt, term1 = 'contracts', 
                             term2 = 'central_gov_debt', ci_type = 'fdr') +
    xlab('\nCentral Government Debt/GDP (%)') + 
    ylab('Marginal Effect of Being a 1 Unit Increase in Contracts\n')
