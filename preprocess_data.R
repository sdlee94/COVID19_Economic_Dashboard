library(tidyverse)

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
  long_df <- wide_df %>%
    gather(Date, Cases, starts_with('x')) %>%
    mutate(Date = Date %>%
             str_replace('X', '0') %>%
             str_replace_all('\\.', '-') %>%
             as.Date(format='%m-%d-%y'))
  return(long_df)
}

confirmed_us_df <- read.csv(confirmed_us_url) %>%
  wide_to_long() %>%
  filter(Cases>0) %>% 
  select(Province.State = Province_State, Country.Region = Country_Region, 
         Lat, Long = Long_, Date, Confirmed = Cases)

deaths_us_df <- read.csv(deaths_us_url) %>%
  wide_to_long() %>%
  filter(Cases>0) %>% 
  select(Province.State = Province_State, Country.Region = Country_Region, 
         Lat, Long = Long_, Date, Deaths = Cases)

confirmed_df <- read.csv(confirmed_url) %>% 
  wide_to_long() %>% 
  rename(Confirmed = Cases) %>%
  filter(Country.Region != 'US', Confirmed>0) %>% 
  rbind(confirmed_us_df) %>% 
  mutate(Confirmed.Sqrt = sqrt(Confirmed))

deaths_df <- read.csv(deaths_url) %>% 
  wide_to_long() %>% 
  rename(Deaths = Cases) %>%
  filter(Country.Region != 'US', Deaths>0) %>% 
  rbind(deaths_us_df) %>%
  mutate(Deaths.Sqrt = sqrt(Deaths))

# recovered data is reported by country
recovered_df <- read.csv(recovered_url) %>% 
  wide_to_long() %>% 
  rename(Recovered = Cases) %>%
  filter(Recovered>0) %>% 
  mutate(Recovered.Sqrt = sqrt(Recovered))

# save covid data as csv files
write_csv(confirmed_df, 'data/confirmed_df.csv')
write_csv(deaths_df, 'data/deaths_df.csv')
write_csv(recovered_df, 'data/recovered_df.csv')

saveRDS(confirmed_df, 'data/confirmed_df.rds')
saveRDS(deaths_df, 'data/deaths_df.rds')
saveRDS(recovered_df, 'data/recovered_df.rds')
