#######################
# Combine and clean Eurostat budget revision tables 
# Christopher Gandrud 
# MIT License
#######################

# Set working directory
setwd('/eurostat_replication_material/')

# Load required packages
library(rio)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(DataCombine)

raw_path <- 'data_cleaning/csv_versions/'
files <- list.files(raw_path)
files <- sprintf('%s%s', raw_path, files)

comb <- data.frame(row.names = c("country", "revision_cause",
                                 "version", "component", "year", "revision"))
for (i in files) {
    # Load
    message(i)
    temp <- import(i)
    
    # Clean individual
    temp$version <- gsub(pattern = '.csv', replacement = '', i)
    temp$revision_cause <- gsub('-', '', temp$revision_cause)
    temp$country[temp$country == 'Czech'] <- 'Czech Republic'
    temp$revision_cause <- str_trim(temp$revision_cause)

    temp$version <- gsub('\\_1', '-04-01', temp$version)
    temp$version <- gsub('\\_2', '-10-01', temp$version)
    
    temp$version <- ymd(temp$version)
    
    # Reshape
    value_fields <- names(temp)[!(names(temp) %in% 
                                      c('country', 'revision_cause', 'version'))]
    
    temp_gathered <- temp %>% gather_('type_year', 'revision', value_fields)
    
    temp_split <- str_split_fixed(temp_gathered$type_year, 
                                  pattern = '_', n = 2) %>% as.data.frame
    names(temp_split) <- c('component', 'year')
    
    temp_gathered <- cbind(temp_gathered, temp_split) %>% 
                        dplyr::select(-type_year)
    
    comb <- rbind(comb, temp_gathered)
}

comb <- comb %>% arrange(country, component, year, version)

####### Revisions due to Revisions of Deficits/Debts ####
comb_dd <- comb %>% filter(revision_cause == 
                               'due to revision of deficit/surplus or debt')

# Find cumulative sum
comb_dd <- comb_dd %>% group_by(country, component, year) %>%
            mutate(cum_revision = round(cumsum(revision), digits = 2))

# Find years since original
comb_dd$revision_year <- year(comb_dd$version)
comb_dd$year <- as.numeric(levels(comb_dd$year))[comb_dd$year]
comb_dd$years_since_original <- comb_dd$revision_year - comb_dd$year

comb_dd <- comb_dd %>% select(-revision_year)

comb_dd <- MoveFront(comb_dd, c('country', 'year', 'version', 
                                'years_since_original', 'component'))

# Save
export(comb_dd, 'data_cleaning/comb_cumulative.csv')


#### Explore ####
library(ggplot2)
library(gridExtra)

## Debt revisions
comb_dd_debt <- comb_dd %>% filter(component == 'debt')

debt_list <- list()

for (i in unique(comb_dd_debt$country)) {
    message(i)
    temp <- comb_dd_debt %>% filter(country == i)
    debt_list[[i]] <- ggplot(temp, aes(version, cum_revision, colour = year)) +
                       # facet_grid( .~ component) +
                        geom_line() +
                        geom_hline(yintercept = 0, linetype = 'dashed') +
                        ylab('') + # Cumulative Revisions\n 
                        xlab('') + # \nData Version Released'
                        ggtitle(i) +
                        theme_bw()   
}

do.call(grid.arrange, debt_list)

## Deficit revisions
comb_dd_deficit <- comb_dd %>% filter(component == 'deficit')

deficit_list <- list()

for (i in unique(comb_dd_deficit$country)) {
    message(i)
    temp <- comb_dd_deficit %>% filter(country == i)
    deficit_list[[i]] <- ggplot(temp, aes(version, cum_revision, colour = year)) +
        # facet_grid( .~ component) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        ylab('') + # Cumulative Revisions\n 
        xlab('') + # \nData Version Released'
        ggtitle(i) +
        theme_bw()   
}

do.call(grid.arrange, deficit_list)

####### Revisions due to Revisions of GDP ####
comb_gdp <- comb %>% filter(revision_cause == 
                               'due to revision of GDP')

# Find cumulative sum
comb_gdp <- comb_gdp %>% group_by(country, component, year) %>%
    mutate(cum_revision = round(cumsum(revision), digits = 2))

## Debt revisions
comb_gdp_debt <- comb_gdp %>% filter(component == 'debt')

debt_list <- list()

for (i in unique(comb_gdp_debt$country)) {
    message(i)
    temp <- comb_gdp_debt %>% filter(country == i)
    debt_list[[i]] <- ggplot(temp, aes(version, cum_revision, colour = year)) +
        # facet_grid( .~ component) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        ylab('') + # Cumulative Revisions\n 
        xlab('') + # \nData Version Released'
        ggtitle(i) +
        theme_bw()   
}

do.call(grid.arrange, debt_list)

## Deficit revisions
comb_gdp_deficit <- comb_gdp %>% filter(component == 'deficit')

deficit_list <- list()

for (i in unique(comb_gdp_deficit$country)) {
    message(i)
    temp <- comb_gdp_deficit %>% filter(country == i)
    deficit_list[[i]] <- ggplot(temp, aes(version, cum_revision, colour = year)) +
        # facet_grid( .~ component) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        ylab('') + # Cumulative Revisions\n 
        xlab('') + # \nData Version Released'
        ggtitle(i) +
        theme_bw()   
}

do.call(grid.arrange, deficit_list)
