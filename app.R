# to install package run: install.packages('shiny')
# install.packages('leaflet')
library(shiny)
library(tidyverse)
library(leaflet)

#covid_df <- read.csv('data/covid_timeseries.csv')

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

confirmed_df <- read.csv(confirmed_url) %>% 
  wide_to_long() %>% 
  mutate(Confirmed.Sqrt = sqrt(Confirmed.Cases))

ui <- fluidPage(
  titlePanel('The Impact of COVID19 on the Economy'),
  sidebarLayout(
    sidebarPanel(
      textOutput("totals"),
      sliderInput(
        "date", 
        label = ("Select Date:"),
        min = as.Date("2020-01-22","%Y-%m-%d"),
        max = as.Date("2020-03-30","%Y-%m-%d"),
        value = as.Date("2020-03-30"),
        timeFormat="%d %b",
        animate=animationOptions(interval=500, loop=F))
    ),
    mainPanel(
      leafletOutput("bubblemap")
    )
  )
)

server <- function(input, output) {
  df <- reactive({
    confirmed_df %>% 
    filter(Date==input$date) %>% 
    mutate(Total.Confirmed=sum(Confirmed.Cases))
  })
  
  output$totals <- renderText({ 
    str_c('Total Confirmed:', format(as.integer(sum(df()$Confirmed.Cases)), big.mark=','))
  })
  
  # zoom <- reactive({
  #   ifelse(is.null(input$bubblemap_zoom), 2, input$bubblemap_zoom)
  # })
  # 
  # center <- reactive({
  #   
  #   if(is.null(input$bubblemap_center)){
  #     return(c(179.462, -20.64275))
  #   } else {
  #     return(input$bubblemap_center)
  #   }
  # })
  
  output$bubblemap <- renderLeaflet({
    leaflet(confirmed_df) %>% 
      addTiles() %>% 
      #setView(lng=10, lat=30, zoom=2) %>% 
      addProviderTiles("CartoDB.Positron") #%>% 
      # addCircles(
      #   ~Long, ~Lat,
      #   radius = ~Confirmed.Sqrt * 2500,
      #   weight=1,
      #   color = 'red',
      #   fillColor = 'red'
      # )
  })
  
  observeEvent(input$date, {
    leafletProxy('bubblemap') %>% 
      clearShapes() %>% 
      addCircles(
        data=df(),
        ~Long, ~Lat,
        radius = ~Confirmed.Sqrt * 2500,
        weight=1,
        color = 'red',
        fillColor = 'red'
      )
  })
}

shinyApp(ui, server)
