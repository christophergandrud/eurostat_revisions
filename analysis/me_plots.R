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
fsi_elect_me <- plot_me(m1_4, term1 = 'yrcurnt_corrected', 
                        term2 = 'fsi_annual_mean',
                        fitted2 = seq(0, 0.58, by = 0.05)) +
    xlab('') + 
    ylab('Marginal Effect of Being a Year Closer to an Election\n')

# Election timing and Unscheduled elections ----------
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



# Election timing and Unscheduled elections
fsi_scheduled_me <- plot_me(m1_7, term1 = 'endog_3Unscheduled', 
                            term2 = 'fsi_annual_mean',
                            fitted2 = seq(0, 0.58, by = 0.05)) +
    xlab('') + 
    ylab('Margingal Effect of an Unscheduled Election\n')
