# to install package run: install.packages('shiny')
# install.packages('leaflet')
library(shiny)
library(tidyverse)
library(leaflet)

parent_url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/'

# urls for csv files
confirmed_url = str_c(parent_url,'time_series_covid19_confirmed_global.csv')
deaths_url = str_c(parent_url,'time_series_covid19_confirmed_global.csv')
recovered_url = str_c(parent_url,'time_series_covid19_confirmed_global.csv')

# fx to convert wide to long format
wide_to_long <- function(wide_df){
  long_df <- wide_df %>% 
    gather(Date, Confirmed.Cases, starts_with('x')) %>% 
    mutate(Date = Date %>% 
             str_replace('X', '0') %>%
             str_replace_all('\\.', '-') %>% 
             as.Date(format='%m-%d-%y'))
  return(long_df)
}

confirmed_df <- read.csv(confirmed_url) %>% wide_to_long()

#covid_df <- read.csv('https://coronadatascraper.com/#timeseries.csv')

df <- confirmed_df %>% 
  gather(Date, Confirmed.Cases, starts_with('x')) %>% 
  mutate(Date = Date %>% 
           str_replace('X', '0') %>%
           str_replace_all('\\.', '-') %>% 
           as.Date(format='%m-%d-%y'))

# Create a color palette with handmade bins.
mybins <- seq(0, max(df$Confirmed.Cases), by=1000)
mypalette <- colorBin(palette="YlOrBr", domain=quakes$mag, na.color="transparent", bins=mybins)

ui <- fluidPage(
  titlePanel('The Impact of COVID19 on the Economy'),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "dateID", "Date:",
        min = as.Date("2020-01-22","%Y-%m-%d"),
        max = as.Date("2020-03-30","%Y-%m-%d"),
        value=as.Date("2020-03-30"),
        timeFormat="%Y-%m-%d")
    ),
    mainPanel(
      leafletOutput("bubblemap")
    )
  )
)

# df <- confirmed_df %>% 
#   filter(Date=="2020-03-29")
# 
# leaflet(df) %>% 
#   addTiles() %>% 
#   setView(lng=10, lat=30, zoom=1) %>% 
#   addProviderTiles("CartoDB.Positron") %>% 
#   addCircles(
#     ~Long, ~Lat,
#     radius = ~sqrt(Confirmed.Cases) * 2000,
#     weight=1,
#     color = 'red',
#     fillColor = 'red'
#   )

server <- function(input, output) {
  df <- confirmed_df %>% 
    filter(Date=="2020-03-29")
  
  output$bubblemap <- renderLeaflet({
    leaflet(df) %>% 
      addTiles() %>% 
      setView(lng=10, lat=30, zoom=1) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addCircles(
        ~Long, ~Lat,
        radius = ~sqrt(Confirmed.Cases) * 2000,
        weight=1,
        color = 'red',
        fillColor = 'red'
      )
  })
}

shinyApp(ui, server)
