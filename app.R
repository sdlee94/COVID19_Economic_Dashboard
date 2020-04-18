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

# import maps
world <- readRDS('data/world_map.rds')
usa <- readRDS('data/usa_map.rds')
canada <- readRDS('data/canada_map.rds')

# import stock data
stock_data <- readRDS('data/stock_data.rds')
# ----

# % change from previous day 
pc_diff_df <- readRDS('data/pc_diff_df.rds')

# cumulative cases
cumulative_df <- readRDS('data/cumulative_df.rds')

# covid summary
covid_summary_df <- readRDS('data/covid_summary_df.rds')

# ----

# ggplot Aesthetics ----
my_theme <- theme(
  #plot.background = element_rect(fill = '#293535', color = '#293535'),
  plot.margin = unit(c(1,1,1,1), 'cm'),
  plot.title = element_text(size = 20, face = 'bold', hjust = 0.5),
  panel.background = element_rect(fill = 'white'),
  panel.grid.major = element_line(linetype = 'dashed', color = 'gainsboro'),
  #panel.grid.minor = element_line(color = '#293535'),
  text = element_text(size = 18),
  axis.line = element_line(colour = 'black'),
  axis.text = element_text(size = 18),
  axis.title.y = element_text(margin = margin(t=0, r=20, b=0, l=0)),
  axis.title.x = element_text(margin = margin(t=10, r=00, b=0, l=0))
  #legend.background = element_rect(fill = '#4d6a66', color = '#4d6a66')
)
# ----

# new page layout with tabs at the top
ui <- navbarPage(title = "COVID-19 | EFFECTS", theme = "styles.css",
                 
  # first tab of the layout, recorded cases and world map
  tabPanel("Recorded Cases", 
    column(8, style='padding-left:50px;',
      fluidRow(
        column(3, #style='padding-left:50px;',
          tags$head(tags$style('#diff * {display:inline;}')),
          tags$div(
            class = "sidebar-container",
            tags$div(class = "sidebar-title",
                     h4("Confirmed Cases")
            ),
            span(h3(textOutput("n_confirmed")), style='color:#d4af37'),
            div(id='diff', h4(textOutput("confirmed_diff"), "↑"), style='color:green')
            #span(h4(textOutput("confirmed_diff"), style='display:inline'), "↑",  style='color:green')
            #tags$p(class = "sidebar-percentage", "##%")
          ),
          tags$div(
            class = "sidebar-container",
            tags$div(class = "sidebar-title", 
                     h4("Recovered") 
              ),
            span(h3(textOutput("n_recovered")), style='color:#79cdcd'),
            div(id='diff', h4(textOutput("deaths_diff"), "↑"), style='color:green')
            #tags$p(class = "sidebar-percentage", "##%")
          ),
            tags$div(class = "sidebar-container", 
            tags$div(class = "sidebar-title", 
                     h4("Deaths") 
            ),
            span(h3(textOutput("n_deaths")), style='color:#cd5555'),
            div(id='diff', h4(textOutput("recovered_diff"), "↑"), style='color:green')
            #tags$p(class = "sidebar-percentage", "##%")
          ),
          tags$footer(class = "sidebar-date-container", 
            tags$p(class = "sidebar-date", textOutput("show_date"))
          )
        ),
        column(9,
          tags$div(class = "map-select", 
            selectInput('map_view', label = NULL, 
                        choices = c('Worldwide', 'Canada', 'United States'), width = "30%")
          ),
          conditionalPanel(
            condition = "input.map_view == 'Worldwide'",
            leafletOutput("world_map", height = 450)
          ), 
          conditionalPanel(
            condition = "input.map_view == 'Canada'",
            leafletOutput("canada_map", height = 450)
          ),
          conditionalPanel(
            condition = "input.map_view == 'United States'",
            leafletOutput("usa_map", height = 450)
          )
        )
      ),
      
      # slider input
      fluidRow(
        column(12, 
          tags$div(
            sliderInput("date",
              label = ("Date"),
              min = min(confirmed_df$Date),
              max = max(confirmed_df$Date),
              value = max(confirmed_df$Date),
              animate = animationOptions(interval=600, loop=F),
              timeFormat = "%d %b",
              width = "100%"
            )
          )
        )
      )
    ),
    column(4, style='padding-right:50px;',
      # tabsetPanel(id='trend_tab',
      #   tabPanel('Cumulative', plotOutput('growth_curve', height = 300)),
      #   tabPanel('Daily Cases', plotOutput('growth_curve', height = 300))
      # ),
      actionButton('cumulative', 'Cumulative'),
      actionButton('daily_cases', 'Daily Cases'),
      # selectInput('Covid_Trend', label = NULL,
      #             choices = c('Cumulative', 'Daily Cases'), width = "30%"),
      plotOutput('covid_trend', height = 300),
      selectInput('top10_stat', label = NULL,
                  choices = c('Absolute Cases', 
                              'Cases per 100k Pop.', 
                              'Fatality Rate', 
                              'Recovery Rate')
                  ),
      plotOutput('top10_countries', height = 300)
    )
  ),
  
  # second tab of the layout, economy data and chart
  tabPanel("Economy", 
    fluidRow(
      column(1),
      column(3,
        span(h3("DJI"), style='color:#000000'),
        span(h3("GSPC"), style='color:#000000'),
        span(h3("IXIC"), style='color:#000000')
      ),
      column(7,
        plotOutput('stock_plot')
      ),
      column(1)
    )
  ),
  
  # third tab of the layout, placeholder for commodities data
  tabPanel("Commodities", 
    fluidRow(
      column(1),
      column(3,
        span(h3("Natural Gas"), style='color:#d4af37'),
        span(h3("Gold"), style='color:#79cdcd'),
        span(h3("Cotton"), style='color:#cd5555')
      ),
      column(
        7, "placeholder"
      )
    )
  )
)

server <- function(input, output) {
  r_confirmed <- reactive({
    if (input$map_view == 'Worldwide') {
      confirmed_df %>% 
        filter(Date==input$date)
    } else if (input$map_view == 'Canada') {
      confirmed_df %>% 
        filter(Date==input$date & Country.Region=='Canada')
    } else if (input$map_view == 'United States') {
      confirmed_df %>% 
        filter(Date==input$date & Country.Region=='United States')
    }
  })
  
  r_deaths <- reactive({
    if (input$map_view == 'Worldwide') {
      deaths_df %>% 
        filter(Date==input$date)
    } else if (input$map_view == 'Canada') {
      deaths_df %>% 
        filter(Date==input$date & Country.Region=='Canada')
    } else if (input$map_view == 'United States') {
      deaths_df %>% 
        filter(Date==input$date & Country.Region=='United States')
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
    str_c(format(as.integer(sum(r_confirmed()$Cases, na.rm=T)),
                 big.mark=','))
  })
  
  output$confirmed_diff <- renderText({
    str_c(cumulative_df[cumulative_df$region==input$map_view &
                        cumulative_df$Date==input$date &
                        cumulative_df$case_type=='Confirmed',]$pc_change, "% ")
  })
  
  output$n_deaths <- renderText({ 
    str_c(format(as.integer(sum(r_deaths()$Cases, na.rm=T)), 
                 big.mark=','))
  })
  
  output$deaths_diff <- renderText({
    str_c(cumulative_df[cumulative_df$region==input$map_view &
                          cumulative_df$Date==input$date &
                          cumulative_df$case_type=='Deaths',]$pc_change, "% ")
  })
  
  output$n_recovered <- renderText({ 
    str_c(format(as.integer(sum(r_recovered()$Cases, na.rm=T)), 
                 big.mark=','))
  })
  
  output$recovered_diff <- renderText({
    str_c(cumulative_df[cumulative_df$region==input$map_view &
                          cumulative_df$Date==input$date &
                          cumulative_df$case_type=='Recovered',]$pc_change, "% ")
  })
  
  output$world_map <- renderLeaflet({
    leaflet(world, height=25) %>% 
      addPolygons(weight = 1,
                  color = '#f2f2f2',
                  fillColor = '#cccccc',
                  fillOpacity = 1) %>% 
      setView(lng=10, lat=30, zoom=2)
  })
  
  output$canada_map <- renderLeaflet({
    leaflet(height=25, options = leafletOptions(minZoom=3, maxZoom=6)) %>% 
      addPolygons(data = world,
                  weight = 1,
                  color = '#f2f2f2',
                  fillColor = '#1D2626',
                  fillOpacity = 1) %>% 
      addPolygons(data = canada,
                  weight = 1,
                  color = '#f2f2f2',
                  fillColor = '#cccccc',
                  fillOpacity = 1) %>%
      setView(lng=-100, lat=60, zoom=3) %>% 
      setMaxBounds(lng1=-130, lng2=-70, lat1=30, lat2=90)
  })
  
  output$usa_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom=3, maxZoom=6)) %>% 
      addPolygons(data = world,
                  weight = 1,
                  color = '#f2f2f2',
                  fillColor = '#1D2626',
                  fillOpacity = 1) %>% 
      addPolygons(data = usa,
                  weight = 1,
                  color = '#f2f2f2',
                  fillColor = '#cccccc',
                  fillOpacity = 1) %>% 
      setView(lng=-170, lat=50, zoom=3) %>% 
      setMaxBounds(lng1=-170, lng2=-40, lat1=10, lat2=70)
  })
  
  update_map <- function(leaflet_map) {
    
    # fx to add bubbles onto map
    add_bubbles <- function(map, df, color){
      new_map <- map %>% 
        addCircleMarkers(
          #show bubbles if more 10 or more cases
          data = df %>% filter(Cases>=10),
          ~Long, ~Lat,
          radius = ~Cases.Sqrt / 10,
          weight = 1,
          color = color,
          fillColor = color,
          fillOpacity = 0.6
          # label = sprintf(
          #   '<strong>%s</strong>, %s<br/>%s Confirmed<br/>',
          #   r_confirmed()$Country.Region,
          #   r_confirmed()$Province.State,
          #   format(r_confirmed()$Cases, big.mark=',')) %>% lapply(htmltools::HTML)
        )
      return(new_map)
    }
    
    if (input$map_view == 'Worldwide') {
      leafletProxy(leaflet_map) %>% 
        clearMarkers() %>%
        add_bubbles(df=r_confirmed(), color='#d4af37') %>% 
        add_bubbles(df=r_recovered(), color='#79cdcd') %>% 
        add_bubbles(df=r_deaths(), color='#cd5555')
    } else {
      leafletProxy(leaflet_map) %>% 
        clearMarkers() %>%
        add_bubbles(df=r_confirmed(), color='#d4af37') %>% 
        add_bubbles(df=r_deaths(), color='#cd5555')
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
    } else if (input$map_view == 'United States') {
      update_map('usa_map')
    }
  })
  
  observeEvent({
    input$cumulative
    input$map_view
  }, {
    output$covid_trend <- renderPlot({
      ggplot(cumulative_df %>% filter(region==input$map_view),
             aes(Date, Total/1e6, col=case_type)) +
        geom_line(size=2, alpha=0.7) +
        scale_color_manual(values = c('Confirmed'='#d4af37',
                                      'Deaths'='#cd5555',
                                      'Recovered'='#79cdcd')) +
        labs(x=NULL, y='Cases\n(Millions)', col=NULL) +
        my_theme + 
        theme(legend.position = 'bottom')
    })
  }, ignoreNULL = FALSE)
  
  observeEvent({
    input$daily_cases
    input$map_view
  }, {
    output$covid_trend <- renderPlot({
      ggplot(cumulative_df %>% filter(region==input$map_view),
             aes(Date, Daily/1000, fill=case_type)) +
        geom_col(width=0.7) +
        scale_fill_manual(values = c('Confirmed'='#d4af37', 
                                      'Deaths'='#cd5555', 
                                      'Recovered'='#79cdcd')) +
        labs(x=NULL, y='Cases\n(Thousands)', fill=NULL) +
        my_theme + 
        theme(legend.position = 'bottom')
    })
  })
  
  observeEvent(input$top10_stat, {
    column_name <- case_when(input$top10_stat=='Absolute Cases'~'Cases',
                             input$top10_stat=='Cases per 100k Pop.'~'Cases.Pop',
                             input$top10_stat=='Fatality Rate'~'Fatality.Rate',
                             input$top10_stat=='Recovery Rate'~'Recovery.Rate')

    output$top10_countries <- renderPlot({
      ggplot(covid_summary_df %>% arrange(-!!as.symbol(column_name)) %>% head(10), 
             aes(x = reorder(Country.Region, !!as.symbol(column_name)), 
                 y = !!as.symbol(column_name))) +
        geom_col(size=2, width=0.7) +
        coord_flip() +
        labs(x=NULL, y=input$top10_stat) +
        my_theme
    })
  })

  output$stock_plot <- renderPlot({
    ggplot(stock_data, aes(x=date, y=Close, col=stock_id)) +
      geom_line(size = 2) +
      scale_color_manual(values = c('^GSPC'='gold', '^DJI'='tomato', '^IXIC'='seagreen3')) +
      #scale_y_continuous(breaks=c(10000, 50000)) +
      labs(x=NULL, col=NULL) + 
      my_theme +
      theme(legend.position = 'top')
  })
}

shinyApp(ui, server)