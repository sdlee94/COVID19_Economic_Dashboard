library(tidyverse)
library(data.table)
library(httr)
library(rvest)
library(reticulate)

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

# make df of cumulative cases by Date ----
cum_cases <- function(df, case_name, region='Worldwide'){
  if(region=='Worldwide'){
    new_df <- df %>% 
      group_by(Date) %>% 
      summarize(Total = sum(Cases)) %>% 
      mutate(Daily = Total - lag(Total),
             case_type = case_name,
             region = region)
  } else {
    new_df <- df %>% 
      filter(Country.Region==region) %>% 
      group_by(Date) %>% 
      summarize(Total = sum(Cases)) %>% 
      mutate(Daily = Total - lag(Total),
             case_type = case_name,
             region = region)
  }

  return(new_df)
}

cumulative_df <- rbind(cum_cases(confirmed_df, 'Confirmed'),
                       cum_cases(deaths_df, 'Deaths'),
                       cum_cases(recovered_df, 'Recovered'),
                       cum_cases(confirmed_df, 'Confirmed', region='United States'),
                       cum_cases(deaths_df, 'Deaths', region='United States'),
                       cum_cases(recovered_df, 'Recovered', region='United States'),
                       cum_cases(confirmed_df, 'Confirmed', region='Canada'),
                       cum_cases(deaths_df, 'Deaths', region='Canada'),
                       cum_cases(recovered_df, 'Recovered', region='Canada')) %>% 
  mutate(pc_change = round(Daily / lag(Total)*100, 2),
         case_type = factor(case_type, levels=c('Confirmed', 'Recovered', 'Deaths')))

cumulative_df[is.na(cumulative_df)] <- 0 

saveRDS(cumulative_df, 'data/cumulative_df.rds')
# ----

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
         Recovery.Rate = round(Recovered/Cases*100, 2)) %>% 
  na.omit()

saveRDS(covid_summary_df, 'data/covid_summary_df.rds')
# ----

# population by state in 2019
url <- 'https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population'

population <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill=T)

population_df <- data.frame(population[[1]])[-1,] %>% 
  select(Province.State = State, Population = Census.population) %>% 
  mutate(Population = Population %>% 
           str_replace_all(',', '') %>% 
           as.integer())

cases_by_state_df <- confirmed_df %>% 
  filter(Country.Region=='United States',
         Date==max(confirmed_df$Date)) %>% 
  group_by(Province.State) %>%
  summarize(Cases = sum(Cases)) %>% 
  left_join(population_df) %>%
  mutate(Cases.Pop = Cases/(Population/100000))
            
deaths_by_state_df <- deaths_df %>% 
  filter(Country.Region=='United States',
         Date==max(confirmed_df$Date)) %>% 
  group_by(Province.State) %>%
  summarize(Deaths = sum(Cases))

summary_by_state_df <- cases_by_state_df %>% 
  left_join(deaths_by_state_df) %>% 
  mutate(Province.State = as.factor(Province.State),
         Fatality.Rate = round(Deaths/Cases*100, 2),
         region = 'United States') %>% 
  na.omit()

# population by province in 2019
url <- 'https://worldpopulationreview.com/canadian-provinces/'

population <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="recentPopulationEstimate"]/div[1]/div/div/div/div/div/table') %>%
  html_table(fill=T)

population_df <- population[[1]] %>% 
  select(Province.State = Name, Population = `2019 Population`) %>% 
  mutate(Population = Population %>% 
           str_replace_all(',', '') %>% 
           as.integer())

cases_by_province_df <- confirmed_df %>% 
  filter(Country.Region=='Canada',
         Date==max(confirmed_df$Date)) %>% 
  group_by(Province.State) %>%
  summarize(Cases = sum(Cases)) %>% 
  left_join(population_df) %>%
  mutate(Cases.Pop = Cases/(Population/100000))

deaths_by_province_df <- deaths_df %>% 
  filter(Country.Region=='Canada',
         Date==max(confirmed_df$Date)) %>% 
  group_by(Province.State) %>%
  summarize(Deaths = sum(Cases))

summary_by_province_df <- cases_by_province_df %>% 
  left_join(deaths_by_province_df) %>% 
  mutate(Province.State = as.factor(Province.State),
         Fatality.Rate = round(Deaths/Cases*100, 2),
         region = 'Canada') %>% 
  na.omit()

summary_by_province_state_df <- rbind(summary_by_state_df,
                                      summary_by_province_df)

saveRDS(summary_by_province_state_df, 'data/summary_by_province_state_df.rds')

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

current_date <- format(Sys.Date(), '%d.%m.%y') %>% 
  str_replace('\\.0([:digit:])\\.', '\\.\\1\\.')

use_python('C:\\Users/Steph/Anaconda3/python.exe', required=T)
source_python('get_prices.py')

indices_df <- c('dow_jones_global_dow', 'dow_jones', 's&p_500', 'nasdaq_100', 's&p-tsx-60') %>% 
  map(get_prices, query_class='index', start_date='22.1.2020', end_date=current_date) %>% 
  bind_rows() %>% 
  mutate(Index.Name = case_when(Query=='dow_jones_global_dow'~'Global Dow',
                                Query=='dow_jones'~'Dow Jones',
                                Query=='nasdaq_100'~'Nasdaq',
                                Query=='s&p_500'~'S&P 500',
                                Query=='s&p-tsx-60'~'S&P TSX'),
         Date = str_replace_all(Date, '/', '-') %>% 
           as.Date(format='%m-%d-%y'),
         Close = str_replace_all(Close, ',', '') %>% 
           as.numeric()) %>% 
  arrange(Query, Date) %>% 
  group_by(Query) %>% 
  mutate(pc_change = round((Close - first(Close)) / first(Close) * 100, 2))

saveRDS(indices_df, 'data/indices_df.rds')

commodities <- c('gold-price', 'live-cattle-price', 'lumber-price', 'oil-price', 'rice-price')
commodities_df <- commodities %>%
  map(get_prices, query_class='commodities', start_date='22.1.2020', end_date=current_date) %>% 
  bind_rows() %>% 
  mutate(Query = Query %>% 
           str_replace('-price', '') %>% 
           str_replace('-', ' ') %>% 
           toTitleCase(),
         Date = str_replace_all(Date, '/', '-') %>% 
           as.Date(format='%m-%d-%y'),
         Close = str_replace_all(Close, ',', '') %>% 
           as.numeric()) %>% 
  arrange(Query, Date) %>% 
  group_by(Query) %>% 
  arrange(Date) %>% 
  mutate(pc_change = round((Close - first(Close)) / first(Close) * 100, 2))

saveRDS(commodities_df, 'data/commodities_df.rds')


#UNEMPLOYMENT DATA ----
# source: https://data.oecd.org/unemp/unemployment-rate.htm#indicator-chart
emp_df <- fread('data/OECD_unemp.csv') %>% 
  select(Country.Code = LOCATION, Date = TIME, Unemp.Rate = Value) %>% 
  mutate(Date = Date %>% 
           str_c('-01') %>% 
           as.Date(format='%Y-%m-%d'))

saveRDS(emp_df, 'data/emp_df.rds')

ggplot(emp_df %>% filter(Country.Code %in% c('CAN', 'USA')), 
       aes(Date, Unemp.Rate, col=Country.Code)) +
  geom_line()

ggplot(emp_df, aes(Country.Code, Unemp.Rate)) +
  geom_boxplot() +
  geom_point(data = emp3_df %>% filter(Date=='2020-03-01'), col='red')
# ----

# indices during the Great Recession
# past_indices_df <- c('dow_jones', 's&p_500', 'nasdaq_100') %>% 
#   map(get_prices, query_class='index', start_date='1.12.2007', end_date='30.7.2009') %>% 
#   bind_rows() %>% 
#   mutate(Index.Name = case_when(Query=='dow_jones'~'Dow Jones',
#                                 Query=='nasdaq_100'~'Nasdaq',
#                                 Query=='s&p_500'~'S&P 500'),
#          Date = str_replace_all(Date, '/', '-') %>% 
#            as.Date(format='%m-%d-%y'),
#          Close = str_replace_all(Close, ',', '') %>% 
#            as.numeric()) %>% 
#   group_by(Query) %>% 
#   mutate(pc_change = round((Close - first(Close)) / first(Close) * 100, 2))
# 
# past_indices_df %>% group_by(Index.Name) %>% summarize(peak_drop = min(pc_change))

#eco_url = 'http://finmindapi.servebeer.com/api/data'

# Fx to obtain stock time series data
# get_stock_data <- function(stock_id){
#   payload <- list('dataset' = 'USStockPrice',
#                   'stock_id' = stock_id,
#                   'date'='2020-01-22')
#   response <- POST(eco_url, body = payload, encode = "form")
#   print(stock_id)
#   data <- response %>% content
#   
#   df <- do.call('cbind', data$data) %>% 
#     data.table %>% 
#     unnest(cols = colnames(.))
#   
#   return(df)
# }
# 
# stock_data <- c('^GSPC', '^DJI', '^IXIC') %>%
#   map(get_stock_data) %>%
#   bind_rows() %>%
#   select(date, stock_id, Close) %>% 
#   group_by(stock_id) %>% 
#   mutate(pc_change = round((Close - first(Close)) / first(Close) * 100, 2),
#          date = as.Date(date))
# saveRDS(stock_data, 'data/stock_data.rds')
# ----
