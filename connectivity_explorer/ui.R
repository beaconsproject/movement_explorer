#-------------------------------------------------
# 2. User interface
#-------------------------------------------------

ui = dashboardPage(skin="black",

  #-------------------------------------------------
  # 2.1 Dashboard header
  #-------------------------------------------------

  title = "Connectivity Explorer",
  dashboardHeader(title = tags$div(
   tags$img(
     src = "logoblanc.png",  # Replace with your logo file name
     height = "50px",   # Adjust the height of the logo
     style = "margin-right: 10px;"  # Add some spacing around the logo
   ),"BEACONs Connectivity Explorer"), titleWidth = 403,
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
    sidebarMenu(id="tabs",
      menuItem("Overview", tabName="home", icon=icon("th")),
      menuItem("Select data", tabName="data", icon=icon("arrow-pointer")),
      menuItem("Movement paths", tabName="hr", icon=icon("arrow-pointer")),
      menuItem("Download data", tabName = "download", icon = icon("th"))
    ),
    conditionalPanel(
      condition='input.tabs=="data"',
      hr(),
      radioButtons("selectInput", "Select source dataset:",
        choices = list("Use demo dataset" = "usedemo", 
                       "Upload your own data" = "usegpkg"),
        selected = character(0), 
        inline = FALSE),
      conditionalPanel(
        condition='input.selectInput=="usegpkg"',
        fileInput("gps_data", "Movement data (csv):", accept=".csv"),
        fileInput("seg_data", "Segmentation data (csv):", accept=".csv"),
        fileInput("upload_gpkg", "Disturbance data (gpkg):", accept=".gpkg")
      ),
      actionButton("getButton", "Load data", class = "btn-warning", style='color: #000')
      #actionButton("getButton", "Load data")
    ),
    conditionalPanel(
      condition='input.tabs=="hr"',
      hr(),
      selectInput("caribou", "Select individual:", choices=NULL, multiple=TRUE),
      selectInput("season", "Movement period:", choices=NULL),
      sliderInput("daterange", "Select year(s):", min=2020, max=2025, value=c(2024,2024), sep=""),
      actionButton("goButton", "Map Corridor", style="color: #000"),
      #hr(),
      #selectInput("hr", "Estimator for HR1:", choices=c("MCP", "KDE", "aKDE", "LoCoH", "OD"), selected="MCP"),
      #sliderInput("levels", "Isopleth levels:", min=0.5, max=1, value=c(0.5, 0.95)),
      #numericInput("h", "KDE bandwidth:", 2, min=0, max=10),
    ),
    conditionalPanel(
      condition='input.tabs=="download"',
      hr(),
      div(style="position:relative; left:calc(6%);", downloadButton("downloadData", "Download HRs", style='color: #000'))
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
      tabItem(tabName="home",
        fluidRow(
          tabBox(id = "one", width="12",
            tabPanel("Overview", includeMarkdown("docs/overview.md")),
            tabPanel("User guide", includeMarkdown("docs/user_guide.md")),
            tabPanel("Datasets", includeMarkdown("docs/datasets.md"))
          )
        )
      ),
      tabItem(tabName="data",
        fluidRow(
          tabBox(id = "two", width="12",
            tabPanel("Movement data", DTOutput("gps_data")),
            tabPanel("Segmentation data", DTOutput("seg_data1")),
            tabPanel("Glimpse data", verbatimTextOutput("dist_data1")),
            tabPanel("Sampling rates", DTOutput("sampling_rates"))
          )
        )
      ),
      tabItem(tabName="hr",
        fluidRow(
          tabBox(id="three", width="9",
            tabPanel("Corridors", leafletOutput("map1", height=900) |> withSpinner()),
            tabPanel("Test output", verbatimTextOutput("hr_output")),
            tabPanel("Help", includeMarkdown("docs/home_ranges.md"))
          ),
          tabBox(
            id = "five", width="3",
            tabPanel("Statistics", tableOutput("tab1"))#,
            #tabPanel("Help", includeMarkdown("docs/home_ranges.md"))
          #),
          #tabBox(
          #  id = "four", width="3",
          #  tabPanel("Statistics", tableOutput("tab1"))
          )
        )
      )     
    )
  )
)
