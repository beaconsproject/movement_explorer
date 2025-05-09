#-------------------------------------------------
# 2. User interface
#-------------------------------------------------

ui = dashboardPage(skin="black",

  #-------------------------------------------------
  # 2.1 Dashboard header
  #-------------------------------------------------

  title = "Segmentation Explorer",
  dashboardHeader(title = tags$div(
   tags$img(
     src = "logoblanc.png",  # Replace with your logo file name
     height = "50px",   # Adjust the height of the logo
     style = "margin-right: 10px;"  # Add some spacing around the logo
   ),"BEACONs Segmentation Explorer"), titleWidth = 417,
   # Add Reload Button Next to Sidebar Toggle
   tags$li(
     class = "dropdown",
     actionButton(
       "reload_btn",
       label = "Reload",
       icon = icon("refresh"),
       style = "color: black; background-color: orange; border: none; font-size: 16px;"
     ),
     style = "position: absolute; left: 50px; top: 10px;"  # Adjust margin for placement next to the toggle
   ),
   tags$li(
     class = "dropdown",  # Required for dropdown functionality
     dropdownMenu(
       type = "tasks", 
       badgeStatus = NULL,
       icon = icon("life-ring"),  # Life-ring icon triggering dropdown
       headerText = "",  # No header text in dropdown
       menuItem("Website", href = "https://beaconsproject.ualberta.ca/", icon = icon("globe")),
       menuItem("GitHub", href = "https://github.com/beaconsproject/", icon = icon("github")),
       #menuItem("Contact us", href = "mailto: beaconsproject@ualberta.ca", icon = icon("address-book"))
       tags$li(
         class = "treeview",
         tags$a(href = "mailto:beacons@ualberta.ca", icon("address-book"), "Contact us")
       )
     ),
     # Plain Text "About Us" Positioned Next to Dropdown
     tags$span(
       "About Us", 
       style = "font-size: 16px; position: relative; top: 15px; right: 10px; white-space: nowrap; color: white;"
     )
   )
  ),


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
      radioButtons("selectInput", "Select source dataset:",
        choices = list("Use demo dataset" = "usedemo", 
                       "Upload your own data" = "usegpkg"),
        selected = character(0), 
        inline = FALSE),
      conditionalPanel(
        condition='input.selectInput=="usegpkg"',
      fileInput('gpkg', 'Movement data (csv):', accept='.csv'),
      fileInput('csv', 'Segmentation data (csv):', accept='.csv')
      ),
      #br(),
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
    useShinyjs(),
    # Link to custom CSS for the orange theme
    tags$head(tags$link(rel = "icon", type = "image/png", href = "logoblanc.png"),
      tags$link(rel = "stylesheet", type = "text/css", href = "green-theme.css")),
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
