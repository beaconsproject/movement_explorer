ui = dashboardPage(skin="black",

  title = "Movement Explorer",
  dashboardHeader(title = tags$div(
   tags$img(
     src = "logoblanc.png",  # Replace with your logo file name
     height = "50px",   # Adjust the height of the logo
     style = "margin-right: 10px;"  # Add some spacing around the logo
   ),"BEACONs Movement Explorer"), titleWidth = 387,
   tags$li( # Add Reload Button Next to Sidebar Toggle
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
       menuItem("GitHub", href = "https://github.com/beaconsproject/movement_explorer/", icon = icon("github")),
       menuItem("Contact us", href = "mailto: beacons@ualberta.ca", icon = icon("address-book"))
     ),
     tags$span( # Plain Text "About Us" Positioned Next to Dropdown
       "About Us", 
       style = "font-size: 16px; position: relative; top: 15px; right: 10px; white-space: nowrap; color: white;"
     )
   )
  ),

  dashboardSidebar(
    sidebarMenu(id="tabs",
      menuItem("Welcome", tabName="home", icon=icon("th")),
      menuItem("Select data", tabName="select", icon=icon("arrow-pointer")),
      menuItem("Explore data", tabName="explore", icon=icon("arrow-pointer")),
      menuItem("Estimate ranges", tabName="ranges", icon=icon("arrow-pointer")),
      menuItem("Identify corridors", tabName="corridors", icon=icon("arrow-pointer")),
      menuItem("Download data", tabName="download", icon=icon("arrow-pointer")),
      hr()
    ),
    conditionalPanel(
      condition="input.tabs=='select'",
      radioButtons("selectInput", "Select source dataset:",
        choices = list("Use demo dataset" = "usedemo", "Upload your own data" = "usedata"),
        selected = character(0), 
        inline = FALSE),
      conditionalPanel(
        condition="input.selectInput=='usedata'",
        fileInput("csv1", "Movement data (csv):", accept=".csv"),
      ),
      actionButton("getButton", "Load data")
      #br(),
      #conditionalPanel(
      #  condition="input.selectInput=='usedemo'",
      #  div(style="position:relative; left:calc(6%);", downloadButton("saveDemoData", "Save demo dataset", style='color: #000')),
      #),
    ),
    conditionalPanel(
      condition="input.tabs=='explore'",
      selectInput("id", "Select individual:", choices=NULL, multiple=FALSE),
      selectInput("season", "Select season:", choices=NULL),
      sliderInput("daterange", "Select year(s):", min=2020, max=2025, value=c(2020,2025), sep=""),
      actionButton("runButton1", "Map data"),
    ),
    conditionalPanel(
      condition="input.tabs=='ranges'",
      actionButton("runButton2", "Map ranges"),
      br(),
      actionButton("saveRanges", "Save ranges")
      #div(style="position:relative; left:calc(6%);", downloadButton("downloadRanges", "Save ranges", style='color: #000'))
    ),
    conditionalPanel(
      condition="input.tabs=='corridors'",
      actionButton("runButton3", "Map corridors"),
      br(),
      actionButton("savePaths", "Save corridors")
      #div(style="position:relative; left:calc(6%);", downloadButton("downloadPaths", "Save corridors", style='color: #000')),
    ),
    conditionalPanel(
      condition="input.tabs=='download'",
      div(style="position:relative; left:calc(6%);", downloadButton("saveData", "Save data", style='color: #000')),
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(tags$link(rel = "icon", type = "image/png", href = "logoblanc.png"),
              tags$link(rel = "stylesheet", type = "text/css", href = "green-theme.css")),
   tabItems(
      welcome,
      selectData,
      exploreData,
      estimateRanges,
      identifyCorridors
    )
  )
)

