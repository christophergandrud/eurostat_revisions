# Previous misc plots of predicted revisions
# First run election_timing_models_tables_plots.R to estimate the underlying models





## Simulate and plot predicted effects ------------------

# Scenarios for election timing ----

countries <- unique(debt$country)

# Drop Croatia, the newest EU member for which there are few revisions
# Drop Estonia because it does not have FSI data
countries <- countries[!(countries %in% c('Croatia', 'Estonia'))]

fitted <- NULL

for (i in (countries)) {
    temp <- debt %>% filter(country == i)
    min_fsi <- min(temp$fsi_annual_mean, na.rm = T)
    max_fsi <- max(temp$fsi_annual_mean, na.rm = T)
    
    temp_fitted <- data.frame(years_since_original = rep(3, 8),
                              yrcurnt_corrected = rep(0:3, 2),
                              fsi_annual_mean = c(rep(min_fsi, 4), 
                                                  rep(max_fsi, 4)),
                              country = rep(i, 8)
    )
    fitted <- rbind(fitted, temp_fitted)
    rm(temp, temp_fitted)
}

temp_levels <- c(rep('low', 4), rep('high', 4))
fitted$fsi_level <- rep(temp_levels, length(countries))

predictions <- predict(m1_8, newdata = fitted, interval = 'confidence')
predictions <- cbind(predictions, fitted[, c('country', 'fsi_level',
                                             "yrcurnt_corrected")])

country_predictions_timing <- ggplot(predictions, aes(yrcurnt_corrected, fit, 
                                                      group = fsi_level,
                                                      colour = fsi_level, 
                                                      fill = fsi_level)) +
    geom_hline(yintercept = 0, linetype = 'dotted') +
    facet_wrap(~country) +
    geom_line() +
    #geom_ribbon(data = predictions, aes(ymin = lwr, ymax = upr, 
    #            fill = fsi_level), alpha = 0.1) +
    scale_x_reverse() +
    scale_color_manual(values = c('#e34a33', '#7fcdbb'), 
                       name = 'Credit Provision\nStress') +
    xlab('\nYears Until Election') +
    ylab('Predicted Cumulative Debt Revision\nAfter 3 Years (% GDP)\n') +
    theme_bw()

ggsave(country_predictions_timing, 
       filename = 'working_paper/figures/country_predict_timing.pdf')

# Scenarios for Unscheduled elections ----

fitted <- NULL

for (i in (countries)) {
    temp <- debt %>% filter(country == i)
    min_fsi <- min(temp$fsi_annual_mean, na.rm = T)
    max_fsi <- max(temp$fsi_annual_mean, na.rm = T)
    
    temp_fitted <- data.frame(years_since_original = rep(3, 6),
                              endog_3 = as.factor(rep(c('No election',
                                                        'Unscheduled',
                                                        'Scheduled'), 2)),
                              fsi_annual_mean = c(rep(min_fsi, 3), 
                                                  rep(max_fsi, 3)),
                              country = rep(i, 6)
    )
    fitted <- rbind(fitted, temp_fitted)
    rm(temp, temp_fitted)
}

temp_levels <- c(rep('low', 3), rep('high', 3))
fitted$fsi_level <- rep(temp_levels, length(countries))

predictions <- predict(m1_9, newdata = fitted, interval = 'confidence')
predictions <- cbind(predictions, fitted[, c('country', 'fsi_level',
                                             'endog_3')])

predictions$endog_3 <- factor(predictions$endog_3, 
                              levels = c('No election', 'Scheduled', 
                                         'Unscheduled'))

country_predictions_timing <- ggplot(predictions, aes(endog_3, fit, 
                                                      group = fsi_level,
                                                      colour = fsi_level, 
                                                      fill = fsi_level)) +
    geom_hline(yintercept = 0, linetype = 'dotted') +
    facet_wrap(~country, ncol =  7) +
    geom_line() +
    geom_point() +
    geom_ribbon(data = predictions, aes(ymin = lwr, ymax = upr, 
                                        fill = fsi_level), alpha = 0.4, colour = NA) +
    scale_color_manual(values = c('#e34a33', '#7fcdbb'), 
                       name = 'Financial\nStress') +
    scale_fill_manual(values = c('#e34a33', '#7fcdbb'), 
                      name = 'Financial\nStress') +
    scale_y_continuous(breaks = c(-2.5, 0, 3.5, 7.5)) +
    xlab('\n') +
    ylab('Predicted Cumulative Debt Revision\nAfter 3 Years (% GDP)\n') +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(country_predictions_timing, 
       filename = 'working_paper/figures/country_predict_required.pdf')
