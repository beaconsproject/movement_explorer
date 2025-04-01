#-------------------------------------------------
# 2. User interface
#-------------------------------------------------

ui = dashboardPage(skin="green",

  #-------------------------------------------------
  # 2.1 Dashboard header
  #-------------------------------------------------

  dashboardHeader(title = "Home Range Explorer"),

  #-------------------------------------------------
  # 2.2 Dashboard sidebar
  #-------------------------------------------------

  dashboardSidebar(
    sidebarMenu(id="tabs",
      menuItem("Home", tabName="home", icon=icon("th")),
      menuItem("Select data", tabName="data", icon=icon("th")),
      menuItem("Home ranges", tabName="hr", icon=icon("th"))
    ),
    conditionalPanel(
      condition='input.tabs=="data"',
      hr(),
      fileInput("gps_data", "Movement data (csv):", accept=".csv"),
      fileInput("seg_data", "Segmentation data (csv):", accept=".csv"),
      #fileInput("dist_data", "Disturbance data (gpkg):", accept=".gpkg"),
      br(),
      actionButton("getButton", "Load data")
    ),
    conditionalPanel(
      condition='input.tabs=="hr"',
      hr(),
      selectInput("caribou", "Select individual:", choices=NULL, multiple=FALSE),
      selectInput("season", "Select season:", choices=NULL),
      sliderInput("daterange", "Select year(s):", min=2020, max=2025, value=c(2024,2024), sep=""),
      hr(),
      selectInput("hr", "Estimator:", choices=c("MCP", "KDE", "aKDE", "LoCoH", "OD")),
      sliderInput("levels", "Select isopleth levels:", min=0.5, max=1, value=0.95),
      hr(),
      actionButton("goButton", "Map", style="color: #000"),
    )
  ),

  #-------------------------------------------------
  # 2.3 Dashboard body
  #-------------------------------------------------
  
  dashboardBody(
    tabItems(
      tabItem(tabName="home",
        fluidRow(
          tabBox(id = "one", width="12",
            tabPanel("Overview", includeMarkdown("docs/overview.md")),
            tabPanel("User guide", includeMarkdown("docs/user_guide.md")),
            tabPanel("Dataset", includeMarkdown("docs/datasets.md"))
          )
        )
      ),
      tabItem(tabName="data",
        fluidRow(
          tabBox(id = "two", width="12",
            tabPanel("Movement data", DTOutput("gps_data")),
            tabPanel("Segmentation data", DTOutput("seg_data1")),
            tabPanel("Help", includeMarkdown("docs/select_data.md"))
          )
        )
      ),
      tabItem(tabName="hr",
        fluidRow(
          tabBox(id="three", width="9",
            tabPanel("Home ranges", leafletOutput("map1", height=900) |> withSpinner())
            #tabPanel("Help", includeMarkdown("docs/home_ranges.md"))
          ),
          tabBox(
            id = "five", width="3",
            tabPanel("Help", includeMarkdown("docs/home_ranges.md"))
          ),
          tabBox(
            id = "four", width="3",
            tabPanel("Statistics", tableOutput("tab1"))
          )
        )
      )     
    )
  )
)
