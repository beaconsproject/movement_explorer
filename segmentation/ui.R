#-------------------------------------------------
# 2. User interface
#-------------------------------------------------

ui = dashboardPage(skin='green',

  #-------------------------------------------------
  # 2.1 Dashboard header
  #-------------------------------------------------

  dashboardHeader(title = 'Seasonal Explorer'),

  #-------------------------------------------------
  # 2.2 Dashboard sidebar
  #-------------------------------------------------

  dashboardSidebar(
    sidebarMenu(id='tabs',
      menuItem('Home', tabName='home', icon=icon('th')),
      menuItem('Select data', tabName='data', icon=icon('th')),
      menuItem('Define segments', tabName = 'segments', icon = icon('th'))
    ),
    conditionalPanel(
      condition="input.tabs=='data'",
      hr(),
      fileInput('gpkg', 'Movement data (csv):', accept='.csv'),
      fileInput('csv', 'Segmentation data (csv):', accept='.csv'),
      br(),
      actionButton('getButton', 'Load data')
    ),
    conditionalPanel(
      condition="input.tabs=='segments'",
      hr(),
      selectInput('caribou', 'Select individual:', choices=NULL, multiple=FALSE),
      selectInput('season', 'Select season:', choices=NULL),
      sliderInput('daterange', 'Select year(s):', min=2021, max=2024, value=c(2024,2024), sep=''),
      #selectInput('stat', 'Select plot type:', choices=c('nsd', 'speed')),
      hr(),
      actionButton('goButton', 'Run', style='color: #000'),
      #hr(),
      #downloadButton('downloadData', 'Save segmentation data', style='color: #000')
    )
  ),

  #-------------------------------------------------
  # 2.3 Dashboard body
  #-------------------------------------------------
  
  dashboardBody(
    tabItems(
      tabItem(tabName='home',
        fluidRow(
          tabBox(id = 'one', width='12',
            tabPanel('Overview', includeMarkdown('docs/overview.md')),
            tabPanel('User guide', includeMarkdown('docs/user_guide.md')),
            tabPanel('Dataset', includeMarkdown('docs/datasets.md'))
          )
        )
      ),
      tabItem(tabName='data',
        fluidRow(
          tabBox(id = 'one', width='12',
            tabPanel('Movement data', DTOutput('gps_data')),
            tabPanel('Segmentation data', DTOutput('seg_data1')),
            #tabPanel('Sampling duration', plotOutput('duration')),
            tabPanel('Sampling rates', DTOutput('sampling_rates')),
            tabPanel('Help', includeMarkdown('docs/select_data.md'))
          )
        )
      ),
      tabItem(tabName='segments',
        fluidRow(
          tabBox(id='three', width='12',
            tabPanel('Segmentation plots',
              #sliderInput('segments', 'Define date range:', min=1, max=366, step=1, value=segments[['FallMigration']], width=1200),
              sliderInput('segments', 'Define date range:', min=1, max=366, step=1, value=c(0,366), width=1200),
              plotOutput('segmentPlot', height=750)),
            tabPanel('Segmentation table', DTOutput('seg_data2')),
            tabPanel('Help', includeMarkdown('docs/define_segments.md'))
          )
        )
      )        
    )
  )
)
