# load packages
library(tidyverse)
library(data.table)
library(shiny)
library(shinyWidgets)
library(leaflet)
library(rworldmap)

# DATA ----

# import maps
world <- readRDS('data/world_map.rds')
simple_world <- readRDS('data/simple_world_map.rds')
usa <- readRDS('data/usa_map.rds')
canada <- readRDS('data/canada_map.rds')

# import covid data
covid_dt <- readRDS('data/covid_dt.rds')

# import cumulative cases
cumulative_dt <- readRDS('data/cumulative_dt.rds')

# import covid summaries (for top10 plot)
covid_summary_dt <- readRDS('data/covid_summary_dt.rds')
summary_by_province_state_dt <- readRDS('data/summary_by_province_state_dt.rds')
covid_stats_dt <- readRDS('data/covid_stats_dt.rds')

# import indices data
indices_df <- readRDS('data/indices_df.rds')

# import commodities data
commodities_df <- readRDS('data/commodities_df.rds')

# import unemployment data
emp_df <- readRDS('data/emp_df.rds')

# ----

# ggplot Aesthetics ----
my_theme <- theme(
  #plot.background = element_rect(fill = '#293535', color = '#293535'),
  plot.margin = unit(c(0.5,1,1,1), 'cm'),
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
          ),
          tags$a(href='https://github.com/CSSEGISandData/COVID-19', 
                 'Powered by John Hopkins Data')
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
              min = min(covid_dt$Date),
              max = max(covid_dt$Date),
              value = max(covid_dt$Date),
              animate = animationOptions(interval=1000, loop=F),
              timeFormat = "%d %b",
              width = "100%"
            )
          )
        )
      )
    ),
    column(4, style='padding-right:50px;',
      tabsetPanel(id='trend_tab',
         tabPanel('Cumulative', plotOutput('cumulative', height = 300)),
         tabPanel('Daily Cases', plotOutput('daily_cases', height = 300))
      ),
      selectInput('top10_stat', label = NULL,
                  choices = c('Cases', 
                              'Cases by Populace', 
                              'Mortality Rate', 
                              'Recovery Rate')
                  ),
      plotOutput('top10_plot', height = 300)
    )
  ),
  
  # second tab of the layout, economy data and chart
  tabPanel("Economy",
    column(8, style='padding-left:50px;',
      column(3, style='padding-left:50px;',
        span(h3("DJI"), style='color:#000000'),
        span(h3("GSPC"), style='color:#000000'),
        span(h3("IXIC"), style='color:#000000')
      ),
      column(9,
        plotOutput('index_plot'),
        plotOutput('commodities_plot')
      )
    ),
    column(4, style='padding-rightt:50px;',
      plotOutput('unemp_plot')
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
  ),
  
  #prevent reactive elements from flickering
  tags$style(type="text/css", ".recalculating { opacity: 1.0; }")
)

server <- function(input, output) {
  output$show_date <- renderText({ 
    format(input$date,"%d %B %Y")
  })
  
  r_covid <- reactive({
    covid_dt[Map.View==input$map_view & 
               Date==input$date]
  })
  
  output$n_confirmed <- renderText({ 
    r_covid()[Case.Type=='Confirmed']$Cases %>% 
      sum(na.rm=T) %>% 
      as.integer() %>% 
      format(big.mark=',')
  })
  
  output$n_recovered <- renderText({ 
    r_covid()[Case.Type=='Recovered']$Cases %>%
      sum(na.rm=T) %>% 
      as.integer() %>% 
      format(big.mark=',')
  })
  
  output$n_deaths <- renderText({ 
    r_covid()[Case.Type=='Deaths']$Cases %>% 
      sum(na.rm=T) %>% 
      as.integer() %>% 
      format(big.mark=',')
  })
  
  output$confirmed_diff <- renderText({
    cumulative_dt[region==input$map_view &
                    Date==input$date &
                    case_type=='Confirmed',]$pc_change %>%
      str_c(' %')
  })
  
  output$deaths_diff <- renderText({
    cumulative_dt[region==input$map_view &
                    Date==input$date &
                    case_type=='Deaths',]$pc_change %>%
      str_c(' %')
  })
  
  output$recovered_diff <- renderText({
    cumulative_dt[region==input$map_view &
                    Date==input$date &
                    case_type=='Recovered',]$pc_change %>%
      str_c(' %')
  })
  
  output$world_map <- renderLeaflet({
    leaflet(simple_world, height=25) %>% 
      addPolygons(weight = 1,
                  color = '#f2f2f2',
                  fillColor = '#cccccc',
                  fillOpacity = 1) %>% 
      setView(lng=10, lat=30, zoom=2)
  })
  
  output$canada_map <- renderLeaflet({
    leaflet(height=25, options = leafletOptions(minZoom=3, maxZoom=6)) %>% 
      addPolygons(data = simple_world,
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
      addPolygons(data = simple_world,
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
    add_bubbles <- function(map, dt, color){
      new_map <- map %>% 
        addCircleMarkers(
          data = dt,
          ~Long, ~Lat,
          radius = ~Cases.Radius,
          weight = 1,
          color = color,
          fillColor = color,
          fillOpacity = 0.6
          # label = sprintf(
          #   '<strong>%s</strong>, %s<br/>%s Confirmed<br/>',
          #   dt$Country.Region,
          #   dt$Province.State,
          #   format(dt$Cases, big.mark=',')) %>% lapply(htmltools::HTML)
        )
      return(new_map)
    }
    
    if (input$map_view == 'Worldwide') {
      leafletProxy(leaflet_map) %>% 
        clearMarkers() %>%
        # show bubble if at least 100 cases (for performance speed)
        add_bubbles(r_covid()[Case.Type=='Confirmed' & Cases>=100], color='#d4af37') %>% 
        add_bubbles(r_covid()[Case.Type=='Recovered' & Cases>=100], color='#79cdcd') %>% 
        add_bubbles(r_covid()[Case.Type=='Deaths' & Cases>=100], color='#cd5555')
    } else {
      leafletProxy(leaflet_map) %>% 
        clearMarkers() %>%
        add_bubbles(r_covid()[Case.Type=='Confirmed' & Cases>=100], color='#d4af37') %>% 
        add_bubbles(r_covid()[Case.Type=='Deaths' & Cases>=100], color='#cd5555')
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
    input$date
    input$map_view
  }, {
    output$cumulative <- renderPlot({
      ggplot(cumulative_dt[region==input$map_view & Date<=input$date,],
             aes(Date, Total/1e6, col=case_type)) +
        geom_line(size=2, alpha=0.7) +
        scale_color_manual(values = c('Confirmed'='#d4af37',
                                      'Deaths'='#cd5555',
                                      'Recovered'='#79cdcd')) +
        labs(x=NULL, y='Cases\n(Millions)', col=NULL) +
        my_theme + 
        theme(legend.position = 'bottom')
    })
  })
  
  observeEvent({
    input$date
    input$map_view
  }, {
    output$daily_cases <- renderPlot({
      ggplot(cumulative_dt[region==input$map_view & Date<=input$date,],
             aes(Date, Daily/1000)) +
        geom_col(aes(fill=case_type)) +
        geom_smooth(aes(col=case_type), se=F, method='loess', size=1.5, show.legend = F) +
        scale_fill_manual(values = c('Confirmed'='#d4af37',
                                     'Deaths'='#cd5555',
                                     'Recovered'='#79cdcd')) +
        scale_color_manual(values = c('Confirmed'='#B48E2D',
                                      'Deaths'='#B24545',
                                      'Recovered'='#62A9A9')) +
        labs(x=NULL, y='Cases\n(Thousands)', fill=NULL) +
        my_theme +
        theme(legend.position = 'bottom')
    })
  })
  
  observeEvent({
    input$map_view
    input$date
    input$top10_stat
  }, {
    column_name <- case_when(input$top10_stat=='Cases'~'Cases',
                             input$top10_stat=='Cases by Populace'~'Cases.Pop',
                             input$top10_stat=='Mortality Rate'~'Mortality.Rate',
                             input$top10_stat=='Recovery Rate'~'Recovery.Rate')
    
    dt <- covid_stats_dt[Map.View==input$map_view &
                           Date==input$date & 
                           !is.na(column_name)]
    
    output$top10_plot <- renderPlot({
      ggplot(dt[order(-dt[[column_name]])] %>% head(10), 
             aes(x = reorder(Region, !!as.symbol(column_name)), 
                 y = !!as.symbol(column_name))) +
        geom_col(size=2, width=0.1) +
        geom_point(size=3, color='red') +
        coord_flip() +
        scale_y_continuous(
          labels = ifelse(!!as.symbol(column_name) %like% 'Cases', 
                          scales::comma, function(x) paste0(x, "%"))) +
        labs(x=NULL, y=input$top10_stat) +
        my_theme
    })
  })

  output$index_plot <- renderPlot({
    ggplot(indices_df %>% filter(Index.Name %in% c('Dow Jones', 'Nasdaq', 'S&P 500')),
           aes(x=Date, y=pc_change, col=Index.Name)) +
      geom_line(size = 1.5) +
      #scale_color_manual(values = c('^GSPC'='gold', '^DJI'='tomato', '^IXIC'='seagreen3')) +
      scale_x_date(date_labels='%b-%d', date_breaks="1 week") +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(x=NULL, y='% Change', col=NULL) + 
      my_theme +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            legend.position = 'top')
  })
  
  output$commodities_plot <- renderPlot({
    ggplot(commodities_df,
           aes(x=Date, y=pc_change, col=Query)) +
      geom_line(size = 1.5) +
      scale_x_date(date_labels='%b-%d', date_breaks="1 week") +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(x=NULL, y='% Change', col=NULL) + 
      my_theme +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            legend.position = 'top')
  })
  
  output$unemp_plot <- renderPlot({
    ggplot(emp_df %>% filter(Country.Code %in% c('CAN', 'USA')), 
           aes(Date, Unemp.Rate, col=Country.Code)) +
      geom_line(size = 1.5) +
      labs(x=NULL, y='Unemployment Rate', col=NULL) +
      my_theme +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            legend.position = 'top')
  })
}

shinyApp(ui, server)