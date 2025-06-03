ui = dashboardPage(skin="black",

  #-------------------------------------------------
  # Dashboard header
  #-------------------------------------------------

  title = "Movement Explorer",
  dashboardHeader(title = tags$div(
   tags$img(
     src = "logoblanc.png",  # Replace with your logo file name
     height = "50px",   # Adjust the height of the logo
     style = "margin-right: 10px;"  # Add some spacing around the logo
   ),"BEACONs Movement Explorer"), titleWidth = 387,
   
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
       menuItem("Contact us", href = "mailto: beaconsproject@ualberta.ca", icon = icon("address-book"))
     ),
     # Plain Text "About Us" Positioned Next to Dropdown
     tags$span(
       "About Us", 
       style = "font-size: 16px; position: relative; top: 15px; right: 10px; white-space: nowrap; color: white;"
     )
   )
  ),

  #-------------------------------------------------
  # Dashboard sidebar
  #-------------------------------------------------

  dashboardSidebar(
    sidebarMenu(id="tabs",
      menuItem("Welcome", tabName="home", icon=icon("th")),
      menuItem("Select study area", tabName="select", icon=icon("arrow-pointer")),
      menuItem("Define segments", tabName = "segments", icon=icon("arrow-pointer")),
      menuItem("Movement paths", tabName = "paths", icon=icon("arrow-pointer")),
      menuItem("Download data", tabName = "download", icon = icon("th")),
      hr()
    ),
    conditionalPanel(
      condition="input.tabs=='select'",
      radioButtons("selectInput", "Select source dataset:",
        choices = list("Use demo dataset" = "usedemo", 
                       "Upload your own data" = "usecsv"),
        selected = character(0), 
        inline = FALSE),
      conditionalPanel(
        condition="input.selectInput=='usecsv'",
        fileInput("csv1", "Movement data (csv):", accept=".csv"),
        fileInput("csv2", "Segmentation data (csv):", accept=".csv")
      ),
      actionButton("getButton", "Load data")
    ),
    conditionalPanel(
      condition="input.tabs=='segments'",
      selectInput("caribou", "Select individual:", choices=NULL, multiple=FALSE),
      selectInput("season", "Select season:", choices=NULL),
      sliderInput("daterange", "Select year(s):", min=2021, max=2024, value=c(2024,2024), sep=""),
      textInput("day1", "Start of year:", value = NULL),
      hr(),
      actionButton("goButton", "Run", style="color: #000"),
    ),
    conditionalPanel(
      condition="input.tabs=='paths'",
      selectInput("caribou2", "Select individual:", choices=NULL, multiple=TRUE),
      selectInput("season2", "Movement period:", choices=NULL),
      sliderInput("daterange2", "Select year(s):", min=2020, max=2025, value=c(2024,2024), sep=""),
      actionButton("goButton2", "Map Corridor", style="color: #000"),
    ),
    conditionalPanel(
      condition='input.tabs=="download"',
      #div(style="position:relative; left:calc(6%);", downloadButton("downloadData", "Save segmentation table", style='color: #000')),
      #br(),
      div(style="position:relative; left:calc(6%);", downloadButton("downloadData2", "Save movement paths", style='color: #000'))
    )
  ),

  #-------------------------------------------------
  # Dashboard body
  #-------------------------------------------------
  
  dashboardBody(
    useShinyjs(),
    # Link to custom CSS for the orange theme
    tags$head(tags$link(rel = "icon", type = "image/png", href = "logoblanc.png"),
              tags$link(rel = "stylesheet", type = "text/css", href = "green-theme.css")),
   tabItems(
      tabItem(tabName="home",
        fluidRow(
          tabBox(id = "one", width="12",
            tabPanel("Overview", includeMarkdown("docs/overview.md")),
            tabPanel("User guide", includeMarkdown("docs/user_guide.md")),
            tabPanel("Datasets", includeMarkdown("docs/datasets.md"))
          )
        )
      ),
      tabItem(tabName="select",
        fluidRow(
          tabBox(id = "one", width="12",
            tabPanel("Movement data", DTOutput("gps_data")),
            tabPanel("Segmentation data", DTOutput("seg_data1")),
            tabPanel("Sampling duration", plotOutput("duration")),
            tabPanel("Sampling rates", DTOutput("sampling_rates")),
            tabPanel("Mapview", leafletOutput("map1", height=750) |> withSpinner()),
            #tabPanel("Test output", verbatimTextOutput("test_output")),
            tabPanel("Help", includeMarkdown("docs/select_data.md"))
          )
        )
      ),
      tabItem(tabName="segments",
        fluidRow(
          tabBox(id="three", width="12",
            tabPanel("Segmentation plots",
              sliderInput("segments", "Define date range:", min=1, max=365, step=1, value=c(1,365), width=1200),
              plotOutput("segmentPlot", height=700)),
            tabPanel("Segmentation table", 
              DTOutput("seg_data2"),
              br(),
              downloadButton("downloadData", "Save segmentation table")),
            tabPanel("Help", includeMarkdown("docs/define_segments.md"))
          )
        )
      ),
      tabItem(tabName="paths",
        fluidRow(
          tabBox(id="three", width="9",
            tabPanel("Corridors", leafletOutput("map2", height=750) |> withSpinner()),
            tabPanel("Test output", verbatimTextOutput("hr_output"))
            #tabPanel("Help", includeMarkdown("docs/home_ranges.md"))
          )
          #tabBox(
          #  id = "five", width="3",
          #  tabPanel("Statistics", tableOutput("tab1"))#,
            #tabPanel("Help", includeMarkdown("docs/home_ranges.md"))
          #),
          #tabBox(
          #  id = "four", width="3",
          #  tabPanel("Statistics", tableOutput("tab1"))
          #)
        )
      )     
    )
  )
)
