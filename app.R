# to install package run: install.packages('shiny')
# install.packages('leaflet')
library(shiny)
library(tidyverse)
library(leaflet)
library(rworldmap)

#covid_df <- read.csv('data/covid_timeseries.csv')

# DATA ----
parent_url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/'

# urls for csv files
confirmed_url = str_c(parent_url,'time_series_covid19_confirmed_global.csv')
deaths_url = str_c(parent_url,'time_series_covid19_deaths_global.csv')
recovered_url = str_c(parent_url,'time_series_covid19_recovered_global.csv')

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

confirmed_df <- read.csv(confirmed_url) %>% 
  wide_to_long() %>% 
  rename(Confirmed = Cases) %>% 
  mutate(Confirmed.Sqrt = sqrt(Confirmed))

deaths_df <- read.csv(deaths_url) %>% 
  wide_to_long() %>% 
  rename(Deaths = Cases) %>% 
  mutate(Deaths.Sqrt = sqrt(Deaths))

recovered_df <- read.csv(recovered_url) %>% 
  wide_to_long() %>% 
  rename(Recovered = Cases) %>% 
  mutate(Recovered.Sqrt = sqrt(Recovered))

# COVID_df <- confirmed_df %>% 
#   left_join(deaths_df %>% select(Lat, Long, Date, Deaths, Deaths.Sqrt), 
#             by=c('Lat','Long','Date')) %>% 
#   left_join(recovered_df %>% select(Lat, Long, Date, Recovered, Recovered.Sqrt), 
#             by=c('Lat','Long','Date')) %>% 
#   distinct()
# 
# COVID_df[duplicated(COVID_df %>% select(Lat, Long, Date)),]
# 
# corp_debt_spdf <- read.csv('data/corp_debt.csv') %>% 
#   joinCountryData2Map(joinCode = "ISO3", nameJoinColumn = "LOCATION")
# 
# bins <- c(0,4,8,12,16,20)
# mypal <- colorBin("YlGnBu", domain=corp_debt_spdf@data$Value, bins=bins, na.color="transparent")

# ----

# ui <- fluidPage(
#   titlePanel('The Impact of COVID19 on the Economy'),
#   sidebarLayout(
#     sidebarPanel(
#       h2(textOutput("show_date"), align='center'),
#       span(h3(textOutput("n_confirmed")), style='color:orange'),
#       span(h3(textOutput("n_deaths")), style='color:red'),
#       span(h3(textOutput("n_recovered")), style='color:blue'),
#       sliderInput(
#         "date", 
#         label = ("Select Date:"),
#         min = min(confirmed_df$Date),
#         max = max(confirmed_df$Date),
#         value = max(confirmed_df$Date),
#         animate = animationOptions(interval=600, loop=F),
#         timeFormat = "%d %b"
#       )
#     ),
#     mainPanel(
#       leafletOutput("bubblemap")
#     )
#   )
# )

ui <- fluidPage(
  titlePanel('The Impact of COVID19 on the Economy'),
  fluidRow(
    column(
      2,h2(textOutput("show_date"), align='center'),
      span(h3(textOutput("n_confirmed")), style='color:orange'),
      span(h3(textOutput("n_deaths")), style='color:red'),
      span(h3(textOutput("n_recovered")), style='color:blue'),
      sliderInput(
        "date",
        label = ("Select Date:"),
        min = min(confirmed_df$Date),
        max = max(confirmed_df$Date),
        value = max(confirmed_df$Date),
        animate = animationOptions(interval=600, loop=F),
        timeFormat = "%d %b"
      )
    ),
    column(
      6,leafletOutput("bubblemap")
    ),
    column(
      4,h2('Tony')
    )
  )
)

server <- function(input, output) {
  r_confirmed <- reactive({
    confirmed_df %>% 
      filter(Date==input$date & Confirmed>0)
  })
  
  r_deaths <- reactive({
    deaths_df %>% 
      filter(Date==input$date & Deaths>0)
  })
  
  r_recovered <- reactive({
    recovered_df %>% 
      filter(Date==input$date & Recovered>0)
  })
  
  output$show_date <- renderText({ 
    format(input$date,"%d %B %Y")
  })
  
  output$n_confirmed <- renderText({ 
    str_c(format(as.integer(sum(r_confirmed()$Confirmed, na.rm=T)), 
                 big.mark=','), ' Confirmed')
  })
  
  output$n_deaths <- renderText({ 
    str_c(format(as.integer(sum(r_deaths()$Deaths, na.rm=T)), 
                 big.mark=','), ' Deaths')
  })
  
  output$n_recovered <- renderText({ 
    str_c(format(as.integer(sum(r_recovered()$Recovered, na.rm=T)), 
                 big.mark=','), ' Recovered')
  })
  
  output$bubblemap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lng=10, lat=30, zoom=2) %>% 
      addProviderTiles("CartoDB.Positron")
  })
  
  observeEvent(input$date, {
    leafletProxy('bubblemap') %>% 
      clearMarkers() %>% 
      # addPolygons(data = corp_debt_spdf, 
      #             weight = 1,
      #             color = 'black',
      #             fillColor = ~mypal(Value),
      #             fillOpacity = 1) %>% 
      addCircleMarkers(
        data = r_confirmed(),
        ~Long, ~Lat,
        radius = ~Confirmed.Sqrt / 10,
        weight = 1,
        color = 'orange',
        fillColor = 'orange',
        fillOpacity = 0.4,
        label = sprintf(
          '<strong>%s</strong><br/>%d Confirmed<br/>',
          r_confirmed()$Country.Region, 
          r_confirmed()$Confirmed) %>% lapply(htmltools::HTML)
      ) %>%
      addCircleMarkers(
        data = r_recovered(),
        ~Long, ~Lat,
        radius = ~Recovered.Sqrt / 10,
        weight = 1,
        color = 'blue',
        fillColor = 'blue',
        fillOpacity = 0.3
      ) %>% 
      addCircleMarkers(
        data = r_deaths(),
        ~Long, ~Lat,
        radius = ~Deaths.Sqrt / 10,
        weight = 1,
        color = 'red',
        fillColor = 'red',
        fillOpacity = 0.5
      )
  })
}

shinyApp(ui, server)
