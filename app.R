# load packages
library(tidyverse)
library(data.table)
library(shiny)
library(shinyWidgets)
library(leaflet)
library(rworldmap)
library(patchwork)

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
covid_stats_dt <- readRDS('data/covid_stats_dt.rds')

# import GDP data
gdp_dt <- readRDS('data/gdp_dt.rds')

# import unemployment data
emp_dt <- readRDS('data/emp_dt.rds')
job_loss_dt <- readRDS('data/job_loss_dt.rds')
jobless_claim_dt <- readRDS('data/jobless_claim_dt.rds')

# import indices data
indices_dt <- readRDS('data/indices_dt.rds')

# import commodities data
#commodities_df <- readRDS('data/commodities_df.rds')
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
ui <- navbarPage(title = "COVID-19 | ECONOMIC DASHBOARD", theme = "styles.css",
                 
  # first tab of the layout, recorded cases and world map
  tabPanel("Pandemic", 
           
    #prevent reactive elements from flickering
    tags$style(type="text/css", ".recalculating { opacity: 1.0; }"),
           
    column(8, style='padding-left:50px;',
      fluidRow(
        column(3,
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
          # tags$footer(class = "sidebar-date-container", 
          #   tags$p(class = "sidebar-date", textOutput("show_date"))
          # ),
          "Powered by ", br(),
          tags$a(href='https://github.com/CSSEGISandData/COVID-19', 'John Hopkins Data'), 
          br(), br(),
          tags$a(href='https://github.com/sdlee94/COVID19_Economic_Dashboard', 'Github Repo')
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
          ),
          absolutePanel(
            bottom=5, left=20, width=300, draggable=T,
            div(
              style= 'font-weight:bold',
              h2(textOutput('show_date')))
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
      column(6, textOutput('top10__by')),
      column(6, align='center',
        selectInput('top10_stat', label=NULL,
          choices = c('Cases', 
                      'Cases by Populace', 
                      'Mortality Rate')
        )
      ),
      plotOutput('top10_plot', height = 280)
    )
  ),
  
  # impact of COVID-19 on GDP
  tabPanel('GDP',
    fluidRow(
      column(3, style='padding-left:50px;',
        tags$style('#tab_select { font-size: 18px;}'),
        div(id='tab_select',
          selectInput('gdp_region', label = NULL, 
                      choices = c('United States', 'Canada'))
        ),
        br(), br(),
        tags$style('#gdp_about { font-size: 16px; font-weight:300;}'),
        div(id='gdp_about',
          tags$b("GDP Growth Rate"), "is an indicator of a nation's economic health.\
          US GDP Growth Rate dropped to ", tags$b("-4.8%"), "in the first quarter of 2020 \
          (2020 Q1), the lowest it has been since the 2008 financial crisis. \
          The 2020 Q1 GDP for Canada is yet to be released, but ",
          tags$a(href="https://www150.statcan.gc.ca/n1/daily-quotidien/200415/dq200415a-eng.htm", "estimates"), 
          "also project a decline of significant scale.",
          br(), br(),
          h4('Data Sources:'),
          tags$a(href="https://www.bea.gov/data/gdp/gross-domestic-product", "Bureau of Economic Analysis"), br(),
          tags$a(href="https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3610010401", "Statistics Canada"), br()
        )
      ),
      column(9, style='padding-right:50px;',
        plotOutput('gdp_growth_plot', height=600)
      )
    )
  ),
  
  # impact of COVID-19 on employment
  tabPanel('Employment',
    fluidRow(
      column(3, style='padding-left:50px;',
        tags$style('#tab_select { font-size: 18px;}'),
        div(id='tab_select',
           selectInput('emp_region', label = NULL, 
                       choices = c('United States', 'Canada'))
        ), 
        br(), br(),
        tags$style('#emp_about { font-size: 16px; font-weight:300;}'),
        div(id='emp_about',
          "Canada's unemployment rate reached ", tags$b("13%"), "in April, a level not seen since ",
          tags$b("December, 1982."), "The US unemployment rate hit ", tags$b('14.7%'), 
          " in April, the highest it has been since the ", tags$b("Great Depression."), br(), br(),
          "As of April 2020, the total number of jobs lost since January is ", tags$b("~2.9 million"), 
          "and", tags$b("~25.4 million"), "for Canada and the US, respectively.", br(), br(),
          h4('Data Sources:'),
          tags$a(href="https://www.bls.gov/bls/unemployment.htm", "US Bureau of Labor Statistics"), 
          br(),
          tags$a(hred="https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1410028701", "Statistics Canada")
        )
      ),
      column(9, style='padding-right:50px;',
             plotOutput('unemp_plot', height=600),
             absolutePanel(style='font-size:18px',
               top=70, left=150, width=300, draggable=F,
               radioButtons('emp_plot_sel', label='Display:', 
                            choices=c('Unemployment Rate', 'Job Loss'))
             )
      )
    )
  ),
  
  # impact of COVID-19 on the Stock Market
  tabPanel('Stock Market',
    fluidRow(
     column(3, style='padding-left:50px;',
            tags$style('#tab_select { font-size: 18px;}'),
            div(id='tab_select',
                selectInput('index_region', label = NULL, 
                            choices = c('United States', 'Canada', 'Global'))
            ), 
            br(), br(),
            tags$style('#index_about { font-size: 16px; font-weight:300;}'),
            div(id='index_about',
                "The global and North American stock market indices have dropped ", 
                tags$b("~20-35%"), " from the new year in late March, soon after\
                COVID-19 was declared a pandemic. However, stock markets have rebounded\
                substantially since then, despite a lack of concrete signs of recovery\
                regarding the pandemic and the job market.",
                br(), br(),
                h4('Data Sources:'),
                tags$a(href='https://markets.businessinsider.com/indices', 'Markets Insider')
            )
     ),
     column(9, style='padding-right:50px;',
            plotOutput('index_plot', height=600)
     )
    )
  )
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
    r_covid()$Confirmed %>% 
      sum(na.rm=T) %>% 
      as.integer() %>% 
      format(big.mark=',')
  })
  
  output$n_recovered <- renderText({ 
    r_covid()$Recovered %>%
      sum(na.rm=T) %>% 
      as.integer() %>% 
      format(big.mark=',')
  })
  
  output$n_deaths <- renderText({ 
    r_covid()$Deaths %>% 
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
      # bubble_label <- ifelse(!is_empty(dt$Map.View), sprintf(
      #   '<strong>%s</strong> <br/>%s Confirmed<br/> %s Recovered<br/> %s Deaths<br/>',
      #   Region,
      #   format(dt$Confirmed, big.mark=','),
      #   format(dt$Recovered, big.mark=','),
      #   format(dt$Deaths, big.mark=',')) %>% lapply(htmltools::HTML), F)
      
      new_map <- map %>% 
        addCircleMarkers(
          data = dt,
          ~Long, ~Lat,
          radius = ~Cases.Radius,
          weight = 1,
          color = color,
          fillColor = color,
          fillOpacity = 0.6#,
          #label = bubble_label
        )
      return(new_map)
    }
    
    if (input$map_view == 'Worldwide') {
      leafletProxy(leaflet_map) %>% 
        clearMarkers() %>%
        add_bubbles(r_covid() %>% 
                      rename(Cases.Radius=Confirmed.Radius), color='#d4af37') %>% 
        add_bubbles(r_covid() %>%
                      rename(Cases.Radius=Recovered.Radius), color='#79cdcd') %>%
        add_bubbles(r_covid() %>%
                      rename(Cases.Radius=Deaths.Radius), color='#cd5555')
    } else {
      leafletProxy(leaflet_map) %>% 
        clearMarkers() %>%
        add_bubbles(r_covid() %>% 
                      rename(Cases.Radius=Confirmed.Radius), color='#d4af37') %>% 
        add_bubbles(r_covid() %>% 
                      rename(Cases.Radius=Deaths.Radius), color='#cd5555')
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
        theme(legend.position = 'top',
              legend.key = element_rect(fill='white'))
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
        #geom_smooth(aes(col=case_type), se=F, method='loess', size=1.5, show.legend = F) +
        scale_fill_manual(values = c('Confirmed'='#d4af37',
                                     'Deaths'='#cd5555',
                                     'Recovered'='#79cdcd')) +
        scale_color_manual(values = c('Confirmed'='#B48E2D',
                                      'Deaths'='#B24545',
                                      'Recovered'='#62A9A9')) +
        labs(x=NULL, y='Cases\n(Thousands)', fill=NULL) +
        my_theme +
        theme(legend.position = c(0.2, 0.85))
    })
  })
  
  output$top10__by <- renderText({
    if(input$map_view=='Worldwide'){
    '  Top 10 Countries by:'
  } else if(input$map_view=='United States'){
    '  Top 10 States by:'
  } else if(input$map_view=='Canada'){
    '  Top 10 Provinces by:'
  }})
  
  observeEvent({
    input$map_view
    input$date
    input$top10_stat
  }, {
    column_name <- case_when(input$top10_stat=='Cases'~'Cases.Millions',
                             input$top10_stat=='Cases by Populace'~'Cases.Pop',
                             input$top10_stat=='Mortality Rate'~'Mortality.Rate')
    
    x_axis_label <- case_when(input$top10_stat=='Cases'~'Cases (Millions)',
                              input$top10_stat=='Cases by Populace'~'Cases per 100,000 People',
                              input$top10_stat=='Mortality Rate'~'Mortality.Rate')
    
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
                          function(x) paste0(x), function(x) paste0(x, "%"))) +
        labs(x=NULL, y=x_axis_label) +
        my_theme
    })
  })
  
  output$gdp_growth_plot <- renderPlot({
    ggplot(gdp_dt[gdp_dt$Region==input$gdp_region], aes(Date, Growth.Rate)) +
      geom_col(aes(fill=Growth.Rate>0)) +
      scale_fill_manual(values=c('TRUE'='limegreen', 'FALSE'='#D41159')) +
      scale_x_date(date_labels='%Y', 
                   breaks = seq(as.Date('2000-01-01'), as.Date('2020-01-01'), by='years')) +
      labs(title='Growth Rate of Real GDP from Previous Quarter',
           x=NULL, y='GDP Growth Rate') +
      my_theme +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            legend.position='none')
  })
  
  output$unemp_plot <- renderPlot({
    if(input$emp_plot_sel=='Job Loss'){
      ggplot(job_loss_dt[Region==input$emp_region], 
             aes(Date, Job.Loss)) +
        geom_col(width=10) +
        #geom_hline(yintercept=0.661, color='red', size=1.5, linetype='dashed') +
        scale_x_date(date_labels='%b') +
        labs(title='Monthly Job Loss in 2020', x=NULL, y='Jobs Lost (Millions)') +
        my_theme +
        theme(axis.text.x = element_text(angle=45, hjust=1))
    } else if(input$emp_plot_sel=='Unemployment Rate') {
      ggplot(emp_dt[Region==input$emp_region], aes(Date, Unemp.Rate)) +
        geom_line(col='dodgerblue', size=1.5) +
        scale_x_date(date_labels='%Y', 
                     breaks=seq(min(emp_dt$Date), max(emp_dt$Date), by='2 years')) +
        labs(title='Monthly Unemployment Rate Over the 21st Century',
             x=NULL, y='Unemployment Rate', col=NULL) +
        my_theme +
        theme(axis.text.x = element_text(angle=45, hjust=1),
              legend.position = 'top')
    }
  })
  
  output$index_plot <- renderPlot({
    ggplot(indices_dt[Region==input$index_region],
           aes(x=Date, y=pc_change, col=Index.Name)) +
      geom_line(size = 1.5) +
      #scale_color_manual(values = c('^GSPC'='gold', '^DJI'='tomato', '^IXIC'='seagreen3')) +
      scale_x_date(date_labels='%b-%d', date_breaks="1 week") +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(title = '% Change in Stock Market Indices from Jan 1, 2020', 
           x=NULL, y='% Change from Jan 1', col=NULL) + 
      my_theme +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            legend.position = c(0.1, 0.15),
            legend.key = element_rect(fill='white'),
            legend.text = element_text(size=18))
  })
  
  # output$commodities_plot <- renderPlot({
  #   ggplot(commodities_df,
  #          aes(x=Date, y=pc_change, col=Query)) +
  #     geom_line(size = 1.5) +
  #     scale_x_date(date_labels='%b-%d', date_breaks="1 week") +
  #     scale_y_continuous(labels = function(x) paste0(x, "%")) +
  #     labs(x=NULL, y='% Change', col=NULL) + 
  #     my_theme +
  #     theme(axis.text.x = element_text(angle=45, hjust=1),
  #           legend.position = 'top')
  # })
}

shinyApp(ui, server)