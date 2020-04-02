# load packages
library(tidyverse)
library(data.table)
library(shiny)
library(shinyWidgets)
library(leaflet)
library(rworldmap)
library(httr)
library(jsonlite)
library(anytime)

# COVID DATA ----
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
# ----

# STOCKS DATA ----
eco_url = 'http://finmindapi.servebeer.com/api/data'

# Data Download
payload <- list('dataset' = 'USStockPrice',
                'stock_id' = '^GSPC',
                'date'='2020-01-22')
response = POST(eco_url,body = payload, encode = "form")
gspc_data = response %>% content
gspc_data = do.call('cbind', gspc_data$data) %>% data.table

payload <- list('dataset' = 'USStockPrice',
                'stock_id' = '^DJI',
                'date'='2020-01-22' )
response = POST(eco_url,body = payload, encode = "form")
dji_data = response %>% content
dji_data = do.call('cbind', dji_data$data) %>% data.table

payload <- list('dataset' = 'USStockPrice',
                'stock_id' = '^IXIC',
                'date' = '2020-01-22' )
response = POST(eco_url,body = payload, encode = "form")
ixic_data = response %>% content
ixic_data = do.call('cbind', ixic_data$data) %>% data.table

gspc_data$Close <- as.character(gspc_data$Close)
gspc_data$date <- anytime::anydate(as.character(gspc_data$date))
gspc_data$stock_id <- as.character(gspc_data$stock_id)

dji_data$Close <- as.character(dji_data$Close)
dji_data$date <- anytime::anydate(as.character(dji_data$date))
dji_data$stock_id <- as.character(dji_data$stock_id)

ixic_data$Close <- as.character(ixic_data$Close)
ixic_data$date <- anytime::anydate(as.character(ixic_data$date))
ixic_data$stock_id <- as.character(ixic_data$stock_id)
# ----

# Aesthetics ----
my_theme <- theme(
  panel.background = element_rect(fill = '#293535'),
  panel.grid.major = element_line(linetype = 'dashed', color = '#4d6a66'),
  panel.grid.minor = element_line(color = '#293535'),
  text = element_text(size = 16)
)
# ----

# COVID_df <- confirmed_df %>% 
#   left_join(deaths_df %>% select(Lat, Long, Date, Deaths, Deaths.Sqrt), 
#             by=c('Lat','Long','Date')) %>% 
#   left_join(recovered_df %>% select(Lat, Long, Date, Recovered, Recovered.Sqrt), 
#             by=c('Lat','Long','Date')) %>% 
#   distinct()
# 
# COVID_df[duplicated(COVID_df %>% select(Lat, Long, Date)),]
# 
corp_debt_spdf <- read.csv('data/corp_debt.csv') %>% 
  joinCountryData2Map(joinCode = "ISO3", nameJoinColumn = "LOCATION")
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
  setBackgroundColor(
    color = "#1A1A1A"
  ),
  tags$head(
    tags$style(
      'body {
        color:#fffacd;
        font-family:Verdana;}',
      HTML(".leaflet-container { background: #293535; }")
    )
  ),
  titlePanel('The Impact of COVID19 on the Economy'),
  fluidRow(
    column(
      2,h2(textOutput("show_date"), align='center'),
      span(h3(textOutput("n_confirmed")), style='color:#d4af37'),
      span(h3(textOutput("n_recovered")), style='color:#79cdcd'),
      span(h3(textOutput("n_deaths")), style='color:#cd5555'),
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
      4,plotOutput('coolplot')
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
      addPolygons(data = corp_debt_spdf, 
                  weight = 1,
                  color = '#293535',
                  fillColor = '#4d6a66',
                  fillOpacity = 1) %>% 
      #addTiles(options = tileop) %>% 
      setView(lng=10, lat=30, zoom=2) #%>% 
      #addProviderTiles("CartoDB.DarkMatter")
  })
  
  observeEvent(input$date, {
    leafletProxy('bubblemap') %>% 
      clearMarkers() %>%
      addCircleMarkers(
        data = r_confirmed(),
        ~Long, ~Lat,
        radius = ~Confirmed.Sqrt / 10,
        weight = 1,
        color = '#d4af37',
        fillColor = '#d4af37',
        fillOpacity = 0.6,
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
        color = '#79cdcd',
        fillColor = '#79cdcd',
        fillOpacity = 0.5
      ) %>% 
      addCircleMarkers(
        data = r_deaths(),
        ~Long, ~Lat,
        radius = ~Deaths.Sqrt / 10,
        weight = 1,
        color = '#cd5555',
        fillColor = '#cd5555',
        fillOpacity = 0.7
      )
  })
  
  output$coolplot <- renderPlot({
    ggplot(dji_data, aes(x=date, y=as.integer(Close))) +
      geom_line(col = 'gold') +
      geom_line(data = gspc_data, col = 'tomato') +
      geom_line(data = ixic_data, col = 'seagreen3') +
      #scale_y_continuous(breaks=c(10000, 50000)) +
      xlab("") + 
      my_theme
  })
}

shinyApp(ui, server)
