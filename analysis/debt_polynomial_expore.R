# Examine polynomial relationship between debt and debt revisions
# Christopher Gandrud
# MIT License

library(rio)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(devtools)
library(stargazer)
library(DataCombine)
library(coreSim)

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

# Model with polynomial estimation
m1_poly <- lm(cum_revision ~
               central_gov_debt + I(central_gov_debt^2) + euro_member +
                  finstress_mean +
               as.factor(country),
           data = debt)


# Simulate and plot
debt_range <- round(min(debt$central_gov_debt, na.rm = TRUE)):
                round(max(debt$central_gov_debt, na.rm = TRUE))
newdata <- data.frame(central_gov_debt  = debt_range,
                      `I(central_gov_debt^2)` = as.numeric(I(debt_range^2)))

sims <- qi_builder(m1_poly, newdata = newdata, slim = TRUE)


p_poly <- ggplot(sims, aes(central_gov_debt, qi_median)) +
    geom_ribbon(aes(ymin = qi_min, ymax = qi_max), alpha = 0.2) +
    geom_line() +
    xlab('\nCentral Government Debt (% of GDP)') +
    ylab('Expected Revisions (% of GDP)\n')

ggsave(p_poly, filename = 'working_paper/figures/debt_poly_sims_plot.pdf')
