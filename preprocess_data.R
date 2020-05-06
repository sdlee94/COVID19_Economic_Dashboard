library(tidyverse)
library(data.table)
library(reticulate)

# MAPS ----
# spatial dataframe of the world
if(!file.exists('data/world_map.rds') | !file.exists('data/country_centroids.rds')){
  world <- getMap(resolution = 'low')
  simple_world <- rgeos::gSimplify(world, tol = 0.5)
  centroids <- rgeos::gCentroid(world, byid=T) %>% 
    as.data.frame() %>% 
    select(Lat = y, Long = x) %>% 
    rownames_to_column('Country.Region') %>% 
    mutate(Country.Region = case_when(Country.Region=='United States of America'~'United States',
                                      Country.Region=='Democratic Republic of the Congo'~'DR Congo',
                                      Country.Region=='Republic of the Congo'~'Congo',
                                      Country.Region=='Guinea Bissau'~'Guinea-Bissau',
                                      Country.Region=='Sao Tome and Principe'~'Sao Tome & Principe',
                                      Country.Region=='Saint Kitts and Nevis'~'Saint Kitts & Nevis',
                                      Country.Region=='Cape Verde'~'Cabo Verde',
                                      Country.Region=='Czech Republic'~'Czech Republic (Czechia)',
                                      Country.Region=='Ivory Coast'~"Cote d'Ivoire",
                                      Country.Region=='The Bahamas'~"Bahamas",
                                      Country.Region=='Myanmar'~"Burma",
                                      Country.Region=='Swaziland'~"Eswatini",
                                      Country.Region=='Republic of Serbia'~"Serbia",
                                      Country.Region=='United Republic of Tanzania'~"Tanzania",
                                      Country.Region=='East Timor'~"Timor-Leste",
                                      Country.Region=='West Bank'~"West Bank and Gaza",
                                      TRUE~Country.Region))
  
  saveRDS(world, 'data/world_map.rds')
  saveRDS(simple_world, 'data/simple_world_map.rds')
  saveRDS(centroids, 'data/country_centroids.rds')
}

# https://eric.clst.org/tech/usgeojson/
if(!file.exists('data/usa_map.rds') | !file.exists('data/usa_centroids.rds')){
  usa <- rgdal::readOGR('data/USA_20m.json')
  usa_centroids <- rgeos::gCentroid(usa, byid=T, id=usa@data$NAME) %>% 
    as.data.frame() %>% 
    select(Lat = y, Long = x) %>% 
    rownames_to_column('Province.State')
  
  usa_simple <- rgeos::gSimplify(usa, tol=0.1)
  
  saveRDS(usa_simple, 'data/usa_map.rds')
  saveRDS(usa_centroids, 'data/usa_centroids.rds')
}

# https://thomson.carto.com/tables/canada_provinces/public/map
if(!file.exists('data/canada_map.rds')){
  canada <- rgdal::readOGR('data/canada_provinces.geojson') %>% 
    rgeos::gSimplify(tol=0.5)
  
  saveRDS(canada, 'data/canada_map.rds')
}
# ----

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

# fx to convert wide to long format & do preprocessing
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

# obtain reported cases from US
confirmed_us_df <- read.csv(confirmed_us_url) %>%
  wide_to_long() %>%
  filter(Cases>0) %>% 
  select(Province.State = Province_State, Country.Region,
         Lat, Long = Long_, Date, Cases)

# obtain reported deaths from US
deaths_us_df <- read.csv(deaths_us_url) %>%
  wide_to_long() %>%
  filter(Cases>0) %>% 
  select(Province.State = Province_State, Country.Region,
         Lat, Long = Long_, Date, Cases)

# obtain reported cases worldwide & combine with US cases
confirmed_df <- read.csv(confirmed_url) %>% 
  wide_to_long() %>% 
  filter(Country.Region != 'United States', Cases>0) %>% 
  rbind(confirmed_us_df) %>% 
  mutate(Cases.Sqrt = round(sqrt(Cases), 2))

# obtain reported deaths worldwide & combine with US cases
deaths_df <- read.csv(deaths_url) %>% 
  wide_to_long() %>% 
  filter(Country.Region != 'United States', Cases>0) %>% 
  rbind(deaths_us_df) %>% 
  mutate(Cases.Sqrt = round(sqrt(Cases), 2))

# obtain reported recovered worldwide (no separate url for US)
recovered_df <- read.csv(recovered_url) %>% 
  wide_to_long() %>%
  filter(Cases>0) %>% 
  mutate(Cases.Sqrt = round(sqrt(Cases), 2))

# fx to get total cases by country
sum_by_region <- function(df, case_type){
  sum_df <- df %>% 
    group_by(Country.Region, Date) %>% 
    summarize(Cases = sum(Cases))
  
  colnames(sum_df) <- c('Country.Region', 'Date', case_type)

  return(sum_df)
}

# total cases (all types) by country
covid_by_country <- confirmed_df %>% 
  sum_by_region('Confirmed') %>% 
  left_join(deaths_df %>% sum_by_region('Deaths')) %>% 
  left_join(recovered_df %>% sum_by_region('Recovered')) %>% 
  # get centroid coordinates for each country
  left_join(centroids) %>% 
  mutate(Province.State='', Map.View='Worldwide') %>% 
  select(Map.View, Country.Region, Province.State, Lat, Long, Date, Confirmed, Deaths, Recovered)

filter_region <- function(df, region){
  case_type <- deparse(substitute(df)) %>% 
    str_remove('_df') %>% 
    tools::toTitleCase()
  
  region_df <- df %>% 
    filter(Country.Region==region) %>% 
    group_by(Province.State, Date) %>% 
    mutate(Cases = sum(Cases), Map.View=region)
  
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

covid_dt <- covid_by_country %>% 
  rename(Region=Country.Region) %>% 
  select(-Province.State) %>% 
  bind_rows(filter_region(confirmed_df, 'United States') %>% 
          left_join(filter_region(deaths_df, 'United States')) %>% 
          # use countrywide recovered since state level data is NA
          left_join(covid_by_country %>% select(Country.Region, Date, Recovered)) %>% 
          # ensure only 1 row has the recovered value for each Date to avoid duplicate
          mutate(Recovered = if_else(Province.State=='Washington', Recovered, as.integer(0))) %>% 
          rename(Region=Province.State) %>% select(-Country.Region)) %>% 
  bind_rows(filter_region(confirmed_df, 'Canada') %>% 
          left_join(filter_region(deaths_df, 'Canada')) %>% 
          # use countrywide recovered since province level data is NA
          left_join(covid_by_country %>% select(Country.Region, Date, Recovered)) %>% 
          mutate(Recovered = if_else(Province.State=='Ontario', Recovered, as.integer(0))) %>% 
          rename(Region=Province.State) %>% select(-Country.Region)) %>%
  ungroup() %>% 
  mutate(Confirmed.Radius = round(Confirmed^(1/4), 2),
         Deaths.Radius = round(Deaths^(1/4), 2),
         Recovered.Radius = round(Recovered^(1/4), 2)) %>% 
  as.data.table()

saveRDS(covid_dt, 'data/covid_dt.rds')

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

saveRDS(cumulative_df %>% as.data.table(), 'data/cumulative_dt.rds')
# ----

# population by country in 2020
country_pop_url <- 'https://www.worldometers.info/world-population/population-by-country/'

country_pop <- country_pop_url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="example2"]') %>%
  html_table()

country_pop_df <- as.data.frame(country_pop[[1]]) %>% 
  select(Country.Region = `Country (or dependency)`, Population = `Population (2020)`) %>% 
  mutate(Population = Population %>% 
           str_replace_all(',', '') %>% 
           as.integer())

# top 10 countries ----
cases_by_country_df <- confirmed_df %>% 
  group_by(Country.Region, Date) %>%
  summarize(Cases = sum(Cases)) %>% 
  left_join(country_pop_df) %>%
  mutate(Cases.Pop = Cases/(Population/100000))

deaths_by_country_df <- deaths_df %>% 
  group_by(Country.Region, Date) %>%
  summarize(Deaths = sum(Cases))

recovered_by_country_df <- recovered_df %>%
  group_by(Country.Region, Date) %>%
  summarize(Recovered = sum(Cases))

covid_summary_df <- cases_by_country_df %>% 
  merge(deaths_by_country_df, all.x=T) %>% 
  merge(recovered_by_country_df, all.x=T) %>% 
  ungroup() %>% 
  mutate(Country.Region = as.factor(Country.Region),
         Mortality.Rate = round(Deaths/Cases*100, 2),
         Recovery.Rate = round(Recovered/Cases*100, 2),
         Map.View = 'Worldwide') %>% 
  select(Region=Country.Region, everything())

saveRDS(covid_summary_df %>% as.data.table(), 'data/covid_summary_dt.rds')
# ----

# population by state in 2019
state_pop_url <- 'https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population'

state_pop <- state_pop_url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill=T)

state_pop_df <- data.frame(state_pop[[1]])[-1,] %>% 
  select(Province.State = State, Population = Census.population) %>% 
  mutate(Population = Population %>% 
           str_replace_all(',', '') %>% 
           as.integer())

cases_by_state_df <- confirmed_df %>% 
  filter(Country.Region=='United States') %>% 
  group_by(Province.State, Date) %>%
  summarize(Cases = sum(Cases)) %>% 
  left_join(state_pop_df) %>%
  mutate(Cases.Pop = Cases/(Population/100000))
            
deaths_by_state_df <- deaths_df %>% 
  filter(Country.Region=='United States') %>% 
  group_by(Province.State, Date) %>%
  summarize(Deaths = sum(Cases))

summary_by_state_df <- cases_by_state_df %>% 
  left_join(deaths_by_state_df) %>% 
  ungroup() %>% 
  mutate(Province.State = as.factor(Province.State),
         Mortality.Rate = round(Deaths/Cases*100, 2),
         Map.View = 'United States') %>% 
  na.omit() %>% 
  mutate(Recovered=NA, Recovery.Rate=NA) %>% 
  select(Region=Province.State, Date, Cases, Population, Cases.Pop, Deaths, 
         Recovered, Mortality.Rate, Recovery.Rate, Map.View)

# population by province in 2019
province_url <- 'https://worldpopulationreview.com/canadian-provinces/'

province_pop <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="recentPopulationEstimate"]/div[1]/div/div/div/div/div/table') %>%
  html_table(fill=T)

province_pop_df <- province_pop[[1]] %>% 
  select(Province.State = Name, Population = `2019 Population`) %>% 
  mutate(Population = Population %>% 
           str_replace_all(',', '') %>% 
           as.integer())

cases_by_province_df <- confirmed_df %>% 
  filter(Country.Region=='Canada') %>% 
  group_by(Province.State, Date) %>%
  summarize(Cases = sum(Cases)) %>% 
  left_join(province_pop_df) %>%
  mutate(Cases.Pop = Cases/(Population/100000))

deaths_by_province_df <- deaths_df %>% 
  filter(Country.Region=='Canada') %>% 
  group_by(Province.State, Date) %>%
  summarize(Deaths = sum(Cases))

summary_by_province_df <- cases_by_province_df %>% 
  left_join(deaths_by_province_df) %>% 
  ungroup() %>% 
  mutate(Province.State = as.factor(Province.State),
         Mortality.Rate = round(Deaths/Cases*100, 2),
         Map.View = 'Canada') %>% 
  na.omit() %>% 
  mutate(Recovered=NA, Recovery.Rate=NA) %>% 
  select(Region=Province.State, Date, Cases, Population, Cases.Pop, Deaths, 
         Recovered, Mortality.Rate, Recovery.Rate, Map.View)

covid_stats_df <- rbind(covid_summary_df,
                        summary_by_state_df %>% mutate(Map.View='United States'),
                        summary_by_province_df %>% mutate(Map.View='Canada'))

saveRDS(covid_stats_df %>% as.data.table(), 'data/covid_stats_dt.rds')

# ECONOMIC DATA ----

# GDP ====

# data obtained from Statistics Canada 
# (https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3610010401)
can_gdp_df <- read_csv('data/CAN_GDP.csv') %>% 
  mutate(Date = str_c(REF_DATE, '-01') %>% as.Date()) %>% 
  filter(Date >= '1999-10-01', 
         Estimates %like% 'Gross domestic product', 
         Prices %like% 'Chained \\(2012\\)') %>% 
  mutate(GDP=VALUE/1e6,
         Growth.Rate = ((GDP/lag(GDP))^4-1)*100,
         Region = 'Canada') %>% 
  select(Date, GDP, Growth.Rate, Region)
  
# manually curated from https://www.bea.gov/data/gdp/gross-domestic-product
us_gdp_df <- read_csv('data/US_GDP.csv') %>% 
  mutate(Date = Date %>% 
           str_replace('^([:digit:]{4})(Q[:digit:])$', '\\1 \\2') %>% 
           as.yearqtr() %>% as.Date(),
         GDP = GDP/1000,
         Region = 'United States')

gdp_dt <- rbind(can_gdp_df[can_gdp_df$Date>='2000-01-01',], us_gdp_df) %>% as.data.table()

saveRDS(gdp_dt, 'data/gdp_dt.rds')
# ====

# Employment ====
# ====
jobless_claim_dates <- seq(as.Date('2020-01-11'), as.Date('2020-04-25'), by='weeks')

# Initial Unemployment Claims (Seasonally Adjusted) manually curated from 
# https://oui.doleta.gov/unemploy/wkclaims/report.asp and https://www.dol.gov/ui/data.pdf
jobless_claims <- c(207000, 220000, 212000, 201000, 204000, 215000, 220000, 217000, 
                    211000, 282000, 3307000, 6867000, 6615000, 5237000, 4442000, 3839000)
  
jobless_claim_df <- data.frame(Date=jobless_claim_dates,
                               Unemp.Insur.Claim=jobless_claims)

ggplot(jobless_claim_df, aes(Date, Unemp.Insur.Claim)) +
  geom_col() +
  my_theme

  
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
emp_dt <- fread('data/OECD_unemp.csv') %>% 
  select(Country.Code = LOCATION, Date = TIME, Unemp.Rate = Value) %>% 
  mutate(Date = Date %>% 
           str_c('-01') %>% 
           as.Date(format='%Y-%m-%d')) %>% 
  as.data.table()

saveRDS(emp_dt, 'data/emp_dt.rds')
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
