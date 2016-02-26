# ---------------------------------------------------------------------------- #
# Load and update Execesive Deficit Procedure Dummy Variable
# Data from Baerg and Hallerberg (2016) and 
# http://ec.europa.eu/economy_finance/economic_governance/sgp/corrective_arm/index_en.htm
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(countrycode)

# Set working directory. Change as needed
setwd('/git_repositories/eurostat_revisions/data_cleaning/')

# Load Baerg and Hallerberg data
edp <- import('raw/excessdefforchristopher.dta')
edp$country <- countrycode(edp$country, origin = 'country.name',
                           destination = 'country.name')


edp <- edp %>% arrange(country, actualyear)

# New members early EDP update
new_m_update <- data.frame(
                    country = c(
                        rep('Cyprus', 2),
                        rep('Czech Republic', 2),
                        rep('Estonia', 2), 
                        rep('Hungary', 2),
                        rep('Latvia', 2),
                        rep('Lithuania', 2),
                        rep('Malta', 2),
                        rep('Poland', 2),
                        rep('Slovakia', 2),
                        rep('Slovenia', 2)
                    ),
                    actualyear = c(
                        rep(2004:2005, 10)
                    ),
                    excessdef = c(
                        0, 0,
                        1, 0,
                        0, 0,
                        1, 1,
                        0, 0,
                        0, 0,
                        1, 0,
                        1, 0,
                        1, 0,
                        0, 0
                    )
)

# Update from 2013 through 2015
update <- data.frame(
            country = c(
                rep('Austria', 3),
                rep('Belgium', 3),
                rep('Bulgaria', 3),
                rep('Czech Republic', 3),
                rep('Denmark', 3),
                rep('Germany', 3),
                rep('Hungary', 3),
                rep('Italy', 3),
                rep('Finland', 3),
                rep('Latvia', 3),
                rep('Lithuania', 3),
                rep('Luxembourg', 3),
                rep('Malta', 3),
                rep('Netherlands', 3),
                rep('Poland', 3),
                rep('Romania', 3),
                rep('Slovakia', 3),
                rep('Estonia', 3),
                rep('Sweden', 3),
                rep('Croatia', 3),
                rep('Cyprus', 3),
                rep('Portugal', 3),
                rep('Slovenia', 3),
                rep('France', 3),
                rep('Ireland', 3),
                rep('Greece', 3),
                rep('Spain', 3),
                rep('United Kingdom', 3)
            ),
            actualyear = rep(c(2013, 2014, 2015), 28),
            excessdef = c(
                1, 1, 0,
                1, 1, 0,
                0, 0, 0,
                1, 1, 0,
                1, 1, 0,
                0, 0, 0,
                1, 0, 0,
                1, 0, 0, 
                1, 0, 0,
                1, 0, 0,
                1, 0, 0,
                0, 0, 0,
                1, 1, 1, 
                1, 1, 0, 
                1, 1, 1,
                1, 0, 0,
                1, 1, 0,
                0, 0, 0,
                0, 0, 0,
                1, 1, 1,
                1, 1, 1,
                1, 1, 1,
                1, 1, 1, 
                1, 1, 1,
                1, 1, 1,
                1, 1, 1, 
                1, 1, 1,
                1, 1, 1
            )
)

update$country <- countrycode(update$country, origin = 'country.name',
                           destination = 'country.name')

# Combine update and original
edp <- rbind(new_m_update, edp, update)
edp <- edp %>% arrange(as.character(country), actualyear)

# Save
export(edp, file = 'raw/edp_updated.csv')
