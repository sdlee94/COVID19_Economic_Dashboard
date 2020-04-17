library(tidyverse)
library(data.table)
library(httr)
library(rvest)

# COVID DATA ----

# parent url to github repo with Covid Data (collected by John Hopkins University)
parent_url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/'

# urls for csv files
confirmed_url = str_c(parent_url,'time_series_covid19_confirmed_global.csv')
confirmed_us_url = str_c(parent_url,'time_series_covid19_confirmed_US.csv')

deaths_url = str_c(parent_url,'time_series_covid19_deaths_global.csv')
deaths_us_url = str_c(parent_url,'time_series_covid19_deaths_US.csv')

recovered_url = str_c(parent_url,'time_series_covid19_recovered_global.csv')
# recovered data from US not available

# fx to convert wide to long format
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

confirmed_us_df <- read.csv(confirmed_us_url) %>%
  wide_to_long() %>%
  filter(Cases>0) %>% 
  select(Province.State = Province_State, Country.Region,
         Lat, Long = Long_, Date, Cases)

deaths_us_df <- read.csv(deaths_us_url) %>%
  wide_to_long() %>%
  filter(Cases>0) %>% 
  select(Province.State = Province_State, Country.Region,
         Lat, Long = Long_, Date, Cases)

cases_by_pop <- function(df){
  by_pop_df <- df %>%
    left_join(population_df) %>% 
    mutate(Cases_per_100k = round(Cases / (Population / 1e5), 2))
  
  return(by_pop_df)
}

confirmed_df <- read.csv(confirmed_url) %>% 
  wide_to_long() %>% 
  filter(Country.Region != 'United States', Cases>0) %>% 
  rbind(confirmed_us_df) %>% 
  mutate(Cases.Sqrt = round(sqrt(Cases), 2))
  
deaths_df <- read.csv(deaths_url) %>% 
  wide_to_long() %>% 
  filter(Country.Region != 'United States', Cases>0) %>% 
  rbind(deaths_us_df) %>% 
  mutate(Cases.Sqrt = round(sqrt(Cases), 2))

# recovered data is reported by country
recovered_df <- read.csv(recovered_url) %>% 
  wide_to_long() %>%
  filter(Cases>0) %>% 
  mutate(Cases.Sqrt = round(sqrt(Cases), 2))

# save covid data as rds files
saveRDS(confirmed_df, 'data/confirmed_df.rds')
saveRDS(deaths_df, 'data/deaths_df.rds')
saveRDS(recovered_df, 'data/recovered_df.rds')


# population by country in 2020
url <- 'https://www.worldometers.info/world-population/population-by-country/'

population <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="example2"]') %>%
  html_table()

population_df <- as.data.frame(population[[1]]) %>% 
  select(Country.Region = `Country (or dependency)`, Population = `Population (2020)`) %>% 
  mutate(Population = Population %>% 
           str_replace_all(',', '') %>% 
           as.integer())

# make df of cumulative cases by Date ----
cum_cases <- function(df, case_name){
  new_df <- df %>% 
    group_by(Date) %>% 
    summarize(Total = sum(Cases)) %>% 
    mutate(case_type = case_name)
  
  return(new_df)
}

cumulative_df <- rbind(cum_cases(confirmed_df, 'Confirmed'),
                       cum_cases(deaths_df, 'Deaths'),
                       cum_cases(recovered_df, 'Recovered'))
  
saveRDS(cumulative_df, 'data/cumulative_df.rds')
# ----

# top 10 countries ----
cases_by_country_df <- confirmed_df %>% 
  filter(Date==max(confirmed_df$Date)) %>% 
  group_by(Country.Region) %>%
  summarize(Cases = sum(Cases)) %>% 
  left_join(population_df) %>%
  mutate(Cases.Pop = Cases/(Population/100000))

deaths_by_country_df <- deaths_df %>% 
  filter(Date==max(confirmed_df$Date)) %>% 
  group_by(Country.Region) %>%
  summarize(Deaths = sum(Cases))

recovered_by_country_df <- recovered_df %>% 
  filter(Date==max(confirmed_df$Date)) %>% 
  group_by(Country.Region) %>%
  summarize(Recovered = sum(Cases))

covid_summary_df <- cases_by_country_df %>% 
  left_join(deaths_by_country_df) %>% 
  left_join(recovered_by_country_df) %>% 
  mutate(Country.Region = as.factor(Country.Region),
         Fatality.Rate = round(Deaths/Cases*100, 2),
         Recovery.Rate = round(Recovered/Cases*100, 2))

saveRDS(covid_summary_df, 'data/covid_summary_df.rds')
# ----


get_pc_diff <- function(region = 'Worldwide', df) {
  current_day <- max(confirmed_df$Date)
  previous_day <- current_day - 1
  
  if(region=='Worldwide'){
    total_current <- sum(df[df$Date==current_day,]$Cases)
    total_previous <- sum(df[df$Date==previous_day,]$Cases)
  } else {
    total_current <- sum(df[df$Date==current_day & df$Country.Region==region,]$Cases)
    total_previous <- sum(df[df$Date==previous_day & df$Country.Region==region,]$Cases)
  }

  percent_diff <- round((total_current - total_previous)/total_previous*100, 2)
  return(percent_diff)
}

regions <- c('Worldwide', 'Canada', 'United States')

pc_diff_df <- data.frame(region = regions,
                         confirmed = (regions %>% map(get_pc_diff, confirmed_df) %>% unlist()),
                         deaths = (regions %>% map(get_pc_diff, deaths_df) %>% unlist()),
                         recovered = (regions %>% map(get_pc_diff, recovered_df) %>% unlist()))
saveRDS(pc_diff_df, 'data/pc_diff_df.rds')


confirmed_by_country_df <- confirmed_df %>% 
  group_by(Country.Region, Date) %>% 
  summarize(Cases = sum(Cases)) %>% 
  mutate(diff = round((Cases - lag(Cases)) / lag(Cases) * 100, 2),
         diff = if_else(is.na(diff), 0, diff))
            
  

# MAPS ----
# spatial dataframe of the world
if(!file.exists('data/world_map.rds')){
  world <- getMap(resolution = 'low')
  saveRDS(world, 'data/world_map.rds')
}

# https://eric.clst.org/tech/usgeojson/
if(!file.exists('data/usa_map.rds')){
  usa <- rgdal::readOGR('data/USA_20m.json')
  saveRDS(usa, 'data/usa_map.rds')
}

# https://thomson.carto.com/tables/canada_provinces/public/map
if(!file.exists('data/canada_map.rds')){
  canada <- rgdal::readOGR('data/canada_provinces.geojson')
  saveRDS(canada, 'data/canada_map.rds')
}
# ----

# STOCKS DATA ----
eco_url = 'http://finmindapi.servebeer.com/api/data'

# Fx to obtain stock time series data
get_stock_data <- function(stock_id){
  payload <- list('dataset' = 'USStockPrice',
                  'stock_id' = stock_id,
                  'date'='2020-01-22')
  response <- POST(eco_url, body = payload, encode = "form")
  print(stock_id)
  data <- response %>% content
  
  df <- do.call('cbind', data$data) %>% 
    data.table %>% 
    unnest(cols = colnames(.))
  
  return(df)
}

stock_data <- c('^GSPC', '^DJI', '^IXIC') %>%
  map(get_stock_data) %>%
  bind_rows() %>% 
  mutate(date = as.Date(date))
saveRDS(stock_data, 'data/stock_data.rds')
# ----
