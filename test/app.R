library(shiny)
library(DT)
library(dplyr)
library(lubridate)

ui <- fluidPage(
  titlePanel("CSV Viewer with Slider Update"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File:",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(), # Horizontal line for separation
      selectInput("season", "Select season:", choices=c("Annual","Early winter","Late winter","Summer","Fall rut")),
      p("Adjust the slider to multiply a numeric column."),
      sliderInput("segments", "Define date range:", min=1, max=365, step=1, value=c(1,365))
      #sliderInput("segments", "Multiplier for Numeric Column:", min = 0, max = 5, value = 1, step = 0.1)
    ),

    mainPanel(
      DTOutput("csvTable")
    )
  )
)

server <- function(input, output, session) {

  day1 <- reactive({
    x <- 32
  })

  gps_csv <- eventReactive(input$selectInput, {
   req(input$selectInput)  # Ensure `selectInput` is not NULL
   readr::read_csv('www/demo_gps.csv') |>
     mutate(year=year(time), yday=yday(time))
  })

  # Reactive value to store the uploaded data
  # This makes sure the data reacts to file input changes
  data <- reactive({
    req(input$file) # Require a file to be uploaded
    df <- read.csv(input$file$datapath, header = TRUE, stringsAsFactors = FALSE)
    x <- df |> mutate(start_doy=yday(as.Date(start, "%b-%d")), end_doy=yday(as.Date(end, "%b-%d")))
    x <- x |> mutate(start_doy = ifelse(start_doy>=day1() & start_doy<=365, start_doy-day1()+1, 365-day1()+1+start_doy),
        end_doy = ifelse(end_doy>=day1() & end_doy<=365, end_doy-day1()+1, 365-day1()+1+end_doy))
    ids <- unique(gps_csv()$id)
    y <- tibble(
      id=rep(ids, each=nrow(x)), 
      season=rep(x$season, length(ids)), 
      start=rep(x$start, length(ids)), 
      end=rep(x$end, length(ids)),
      start_doy=rep(x$start_doy, length(ids)),
      end_doy=rep(x$end_doy, length(ids)))
    return(y)
  })

  # Reactive value for the data that will be displayed and updated
  # This will be `data()` initially, and then updated by the slider
  display_data <- reactiveVal(NULL)

  # Initialize display_data when a file is uploaded
  observeEvent(data(), {
    display_data(data())
  })

  # Initialize display_data when a file is uploaded
  observeEvent(input$season,{
    x <- data()
    #start <- x$start_doy[x$season=="Annual"]
    #end <- x$end_doy[x$season=="Annual"]
    start <- x$start_doy[x$season==input$season]
    end <- x$end_doy[x$season==input$season]
  #  y <- seg_csv_expand()
  #  start=yday(as.Date(y$start[y$id==input$caribou & y$season==input$season], "%b-%d"))
  #  end=yday(as.Date(y$end[y$id==input$caribou & y$season==input$season], "%b-%d"))
    updateSliderInput(session, 'segments', value=c(start, end))
  })

  # Observe changes in the slider and update a numeric column
  observeEvent(input$segments, {
    current_df <- display_data()
    #if (!is.null(current_df)) {
        initial_df <- data() # Get the original data before any previous multiplications
        # Reset to original values, then apply new multiplier. This prevents compounding multipliers
        current_df$start_doy[current_df$season==input$season] <- input$segments[1]
        current_df$end_doy[current_df$season==input$season] <- input$segments[2]
      display_data(current_df) # Update the reactive value
    #}
  })

  # Render the DT table
  output$csvTable <- renderDT({
    req(display_data()) # Ensure display_data is available
    datatable(display_data(), options = list(pageLength = 10, scrollX = TRUE), editable = TRUE)
  })
}

shinyApp(ui, server)