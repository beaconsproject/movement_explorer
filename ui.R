ui = dashboardPage(skin="black",

  title = "BEACONs Movement Explorer",
  
  dashboardHeader(
    title = tags$div(tags$img(src = "logoblanc.png", height = "50px", style = "margin-right: 10px;"), "BEACONs Movement Explorer"), 
    titleWidth = 387,
    tags$li(
      class = "dropdown", 
      actionButton("reload_btn", label = "Reload", icon = icon("refresh"), style = "color: black; background-color: orange; border: none; font-size: 16px;"),
      style = "position: absolute; left: 50px; top: 10px;"),
   #tags$li(
   #  class = "dropdown",
   #  actionButton("screen_btn", label = "Screen capture"),
   #  style = "position: absolute; right: 120px; top: 10px;"), 
   tags$li(
     class = "dropdown",
     dropdownMenu(type = "tasks", badgeStatus = NULL, icon = icon("life-ring"), headerText = "",
       menuItem("Website", href = "https://beaconsproject.ualberta.ca/", icon = icon("globe")),
       menuItem("GitHub", href = "https://github.com/beaconsproject/movement_explorer/", icon = icon("github")),
       menuItem("Contact us", href = "mailto: beacons@ualberta.ca", icon = icon("address-book"))),
     tags$span("About Us", style = "font-size: 16px; position: relative; top: 15px; right: 10px; white-space: nowrap; color: white;")
   )
  ),

  dashboardSidebar(
    sidebarMenu(id="tabs",
      menuItem("Welcome", tabName="home", icon=icon(name ="fas fa-home")),
      menuItem("Select data", tabName="select", icon = icon(name = "fas fa-upload", lib = "font-awesome")),
      menuItem("Explore data", tabName="explore", icon = icon(name = "fas fa-globe")),
      menuItem("Estimate ranges", tabName="ranges", icon=icon(name = "fas fa-draw-polygon")),
      menuItem("Identify corridors", tabName="corridors", icon=icon(name = "fas fa-code-branch")),
      menuItem("Download data", tabName="download", icon=icon(name = "fas fa-download")),
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
      actionButton("saveRanges", "Save ranges"),
      hr(),
      actionButton("range1", "Map1 screenshot"),
      br(),
      actionButton("range2", "Map2 screenshot"),
      #sliderInput("range_size", "Screen shot resolution", min=1, max=10, value=1)
    ),
    conditionalPanel(
      condition="input.tabs=='corridors'",
      actionButton("runButton3", "Map corridors"),
      br(),
      actionButton("savePaths", "Save corridors"),
      hr(),
      actionButton("path1", "Map1 screenshot"),
      br(),
      actionButton("path2", "Map2 screenshot"),
      #sliderInput("path_size", "Screen shot resolution", min=1, max=10, value=1)
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

