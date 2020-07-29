library(tidyverse)
library(data.table)
library(reticulate)
library(readxl)

# convert wide to long format & do preprocessing
wide_to_long <- function(wide_df){
  if('Country_Region' %in% colnames(wide_df)){
    wide_df <- wide_df %>% rename(Country.Region = Country_Region)
  }
  
  long_df <- wide_df %>%
    gather(Date, Cases, starts_with('x')) %>%
    mutate(Country.Region = case_when(Country.Region=='Korea, South'~'South Korea',
                                      Country.Region=='Taiwan*'~'Taiwan',
                                      Country.Region=='US'~'United States',
                                      Country.Region=='Saint Kitts and Nevis'~'Saint Kitts & Nevis',
                                      Country.Region=='Sao Tome and Principe'~'Sao Tome & Principe',
                                      Country.Region=='Congo (Brazzaville)'~'Congo',
                                      Country.Region=='Congo (Kinshasa)'~'DR Congo',
                                      Country.Region=='Czechia'~'Czech Republic (Czechia)',
                                      TRUE~as.character(Country.Region)),
           Date = Date %>%
             str_replace('X', '0') %>%
             str_replace_all('\\.', '-') %>%
             as.Date(format='%m-%d-%y'))
  return(long_df)
}

# get total cases by country
sum_by_region <- function(df, case_type){
  sum_df <- df %>% 
    group_by(Country.Region, Date) %>% 
    summarize(Cases = sum(Cases, na.rm=T))
  
  colnames(sum_df) <- c('Country.Region', 'Date', case_type)
  
  return(sum_df)
}

# filter by region and add state centroids for US data
filter_region <- function(df, region){
  case_type <- deparse(substitute(df)) %>% 
    str_remove('_df') %>% 
    tools::toTitleCase()
  
  region_df <- df %>% 
    filter(Country.Region==region) %>% 
    group_by(Province.State, Date) %>% 
    mutate(Cases = sum(Cases, na.rm=T), Map.View=region)
  
  # replace coordinates with state centroids for US data
  if(region=='United States'){
    region_df <- region_df %>% 
      select(-Lat, -Long) %>% 
      left_join(usa_centroids) %>% 
      select(Map.View, Country.Region, Province.State, Lat, Long, Date, Cases) %>% 
      distinct()
  } else {
    region_df <- region_df %>% 
      select(Map.View, Country.Region, Province.State, Lat, Long, Date, Cases) %>% 
      distinct()
  }
  
  colnames(region_df)[length(colnames(region_df))] <- case_type
  
  return(region_df)
}

# make df of cumulative cases by Date ----
cum_cases <- function(df, case_name, region='Worldwide'){
  if(region=='Worldwide'){
    new_df <- df %>% 
      group_by(Date) %>% 
      summarize(Total = sum(Cases, na.rm=T)) %>% 
      mutate(Daily = Total - lag(Total),
             case_type = case_name,
             region = region)
  } else {
    new_df <- df %>% 
      filter(Country.Region==region) %>% 
      group_by(Date) %>% 
      summarize(Total = sum(Cases, na.rm=T)) %>% 
      mutate(Daily = Total - lag(Total),
             case_type = case_name,
             region = region)
  }
  
  return(new_df)
}