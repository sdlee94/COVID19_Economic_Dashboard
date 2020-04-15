# load packages
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(leaflet)
library(rworldmap)

# COVID DATA ----
confirmed_df <- readRDS('data/confirmed_df.rds')
deaths_df <- readRDS('data/deaths_df.rds')
recovered_df <- readRDS('data/recovered_df.rds')
# ----

# import maps
world <- readRDS('data/world_map.rds')
usa <- readRDS('data/usa_map.rds')
canada <- readRDS('data/canada_map.rds')

# import stock data
stock_data <- readRDS('data/stock_data.rds')
# ----

# ggplot Aesthetics ----
my_theme <- theme(
  plot.background = element_rect(fill = '#293535', color = '#293535'),
  plot.margin = unit(c(1.5,1.5,1.5,1.5), 'cm'),
  panel.background = element_rect(fill = '#293535'),
  panel.grid.major = element_line(linetype = 'dashed', color = '#4d6a66'),
  panel.grid.minor = element_line(color = '#293535'),
  text = element_text(size = 18, color = '#fffacd'),
  axis.text = element_text(size = 18, color = '#fffacd'),
  axis.title.y = element_text(margin = margin(t=0, r=20, b=0, l=0)),
  legend.background = element_rect(fill = '#4d6a66', color = '#4d6a66')
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

# ----

ui <- fluidPage(
  
  # background color
  setBackgroundColor(
    color = "#1A1A1A"
  ),
  
  # text styling and background color for map
  tags$head(
    tags$style(
      'body {
        color:#fffacd;
        font-family:Verdana;}',
      HTML(".leaflet-container { background: #293535; }")
    )
  ),
  
  # main title
  titlePanel('The Impact of COVID19 on the Economy'),
  
  # 3 column layout
  fluidRow(
    column(
      2,selectInput('map_view', label = NULL, choices = c('Worldwide', 'Canada', 'USA')),
      h2(textOutput("show_date"), align='center'),
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
    column(6, 
      conditionalPanel(
        condition = "input.map_view == 'Worldwide'",
        leafletOutput("world_map")
      ), 
      conditionalPanel(
        condition = "input.map_view == 'Canada'",
        leafletOutput("canada_map")
      ),
      conditionalPanel(
        condition = "input.map_view == 'USA'",
        leafletOutput("usa_map")
      )
    ),
    column(
      4,plotOutput('coolplot')
    )
  )
)

# leaflet(options = leafletOptions(minZoom=3, maxZoom=6)) %>% 
#   addPolygons(data = world,
#               weight = 1,
#               color = '#293535',
#               fillColor = '#1D2626',
#               fillOpacity = 1) %>% 
#   addPolygons(data = canada,
#               weight = 1,
#               color = '#293535',
#               fillColor = '#4d6a66',
#               fillOpacity = 1) %>% 
#   setView(lng=-100, lat=60, zoom=3) %>% 
#   setMaxBounds(lng1=-130, lng2=-70, lat1=30, lat2=90) %>% 
#   addCircleMarkers(data = confirmed_df,
#                    ~Long, ~Lat,
#                    radius = ~Confirmed.Sqrt / 10,
#                    weight = 1,
#                    color = '#d4af37',
#                    fillColor = '#d4af37',
#                    fillOpacity = 0.6)

# a <- confirmed_df %>% 
#   filter(Date==max(confirmed_df$Date) & Confirmed>0)
# 
# str_c(format(as.integer(sum(a$Confirmed, na.rm=T)), 
#              big.mark=','), ' Confirmed')
# 
# b <- deaths_df %>% 
#   filter(Date==max(deaths_df$Date) & Deaths>0)
# 
# str_c(format(as.integer(sum(b$Deaths, na.rm=T)), 
#              big.mark=','), ' Confirmed')

server <- function(input, output) {
  r_confirmed <- reactive({
    if (input$map_view == 'Worldwide') {
      confirmed_df %>% 
        filter(Date==input$date)
    } else if (input$map_view == 'Canada') {
      confirmed_df %>% 
        filter(Date==input$date & Country.Region=='Canada')
    } else if (input$map_view == 'USA') {
      confirmed_df %>% 
        filter(Date==input$date & Country.Region=='US')
    }
  })
  
  r_deaths <- reactive({
    if (input$map_view == 'Worldwide') {
      deaths_df %>% 
        filter(Date==input$date)
    } else if (input$map_view == 'Canada') {
      deaths_df %>% 
        filter(Date==input$date & Country.Region=='Canada')
    } else if (input$map_view == 'USA') {
      deaths_df %>% 
        filter(Date==input$date & Country.Region=='US')
    }
  })
  
  r_recovered <- reactive({
    recovered_df %>% 
      filter(Date==input$date)
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
  
  output$world_map <- renderLeaflet({
    leaflet(world) %>% 
      addPolygons(weight = 1,
                  color = '#293535',
                  fillColor = '#4d6a66',
                  fillOpacity = 1) %>% 
      setView(lng=10, lat=30, zoom=2)
  })
  
  output$canada_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom=3, maxZoom=6)) %>% 
      addPolygons(data = world,
                  weight = 1,
                  color = '#293535',
                  fillColor = '#1D2626',
                  fillOpacity = 1) %>% 
      addPolygons(data = canada,
                  weight = 1,
                  color = '#293535',
                  fillColor = '#4d6a66',
                  fillOpacity = 1) %>%
      setView(lng=-100, lat=60, zoom=3) %>% 
      setMaxBounds(lng1=-130, lng2=-70, lat1=30, lat2=90)
  })
  
  output$usa_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom=3, maxZoom=6)) %>% 
      addPolygons(data = world,
                  weight = 1,
                  color = '#293535',
                  fillColor = '#1D2626',
                  fillOpacity = 1) %>% 
      addPolygons(data = usa,
                  weight = 1,
                  color = '#293535',
                  fillColor = '#4d6a66',
                  fillOpacity = 1) %>% 
      setView(lng=-170, lat=50, zoom=3) %>% 
      setMaxBounds(lng1=-170, lng2=-40, lat1=10, lat2=70)
  })
  
  update_map <- function(leaflet_map) {
    if (input$map_view == 'Worldwide') {
      leafletProxy(leaflet_map) %>% 
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
            '<strong>%s</strong>, %s<br/>%s Confirmed<br/>',
            r_confirmed()$Country.Region,
            r_confirmed()$Province.State,
            format(r_confirmed()$Confirmed, big.mark=',')) %>% lapply(htmltools::HTML)
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
    } else {
      leafletProxy(leaflet_map) %>% 
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
            '<strong>%s</strong>, %s<br/>%s Confirmed<br/>',
            r_confirmed()$Country.Region,
            r_confirmed()$Province.State,
            format(r_confirmed()$Confirmed, big.mark=',')) %>% lapply(htmltools::HTML)
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
    }
  }
  
  observeEvent({
    input$date
    input$map_view
  }, {
    if (input$map_view == 'Worldwide') {
      update_map('world_map')
    } else if (input$map_view == 'Canada') {
      update_map('canada_map')
    } else if (input$map_view == 'USA') {
      update_map('usa_map')
    }
  })
# 
#   observeEvent(input$date, {
#     leafletProxy('world_map') %>% 
#       clearMarkers() %>%
#       addCircleMarkers(
#         data = r_confirmed(),
#         ~Long, ~Lat,
#         radius = ~Confirmed.Sqrt / 10,
#         weight = 1,
#         color = '#d4af37',
#         fillColor = '#d4af37',
#         fillOpacity = 0.6,
#         label = sprintf(
#           '<strong>%s</strong><br/>%d Confirmed<br/>',
#           r_confirmed()$Country.Region, 
#           r_confirmed()$Confirmed) %>% lapply(htmltools::HTML)
#       ) %>%
#       addCircleMarkers(
#         data = r_recovered(),
#         ~Long, ~Lat,
#         radius = ~Recovered.Sqrt / 10,
#         weight = 1,
#         color = '#79cdcd',
#         fillColor = '#79cdcd',
#         fillOpacity = 0.5
#       ) %>% 
#       addCircleMarkers(
#         data = r_deaths(),
#         ~Long, ~Lat,
#         radius = ~Deaths.Sqrt / 10,
#         weight = 1,
#         color = '#cd5555',
#         fillColor = '#cd5555',
#         fillOpacity = 0.7
#       )}, {
#     leafletProxy('canada_map') %>% 
#       clearMarkers() %>%
#       addCircleMarkers(
#         data = r_confirmed(),
#         ~Long, ~Lat,
#         radius = ~Confirmed.Sqrt / 10,
#         weight = 1,
#         color = '#d4af37',
#         fillColor = '#d4af37',
#         fillOpacity = 0.6,
#         label = sprintf(
#           '<strong>%s</strong><br/>%d Confirmed<br/>',
#           r_confirmed()$Country.Region, 
#           r_confirmed()$Confirmed) %>% lapply(htmltools::HTML)
#       ) %>%
#       addCircleMarkers(
#         data = r_recovered(),
#         ~Long, ~Lat,
#         radius = ~Recovered.Sqrt / 10,
#         weight = 1,
#         color = '#79cdcd',
#         fillColor = '#79cdcd',
#         fillOpacity = 0.5
#       ) %>% 
#       addCircleMarkers(
#         data = r_deaths(),
#         ~Long, ~Lat,
#         radius = ~Deaths.Sqrt / 10,
#         weight = 1,
#         color = '#cd5555',
#         fillColor = '#cd5555',
#         fillOpacity = 0.7
#       )
#   })
  
  output$coolplot <- renderPlot({
    ggplot(stock_data, aes(x=date, y=Close, col=stock_id)) +
      geom_line() +
      scale_color_manual(values = c('^GSPC'='gold', '^DJI'='tomato', '^IXIC'='seagreen3')) +
      #scale_y_continuous(breaks=c(10000, 50000)) +
      labs(x=NULL, col=NULL) + 
      my_theme +
      theme(legend.position = 'top')
  })
}

shinyApp(ui, server)
