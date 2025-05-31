server = function(input, output, session) {

  # RELOAD
  observeEvent(input$reload_btn, {
    session$reload()
  })

  ##############################################################################
  # Read input data
  ##############################################################################

  # Read gps movement data
  gps_csv <- eventReactive(input$selectInput, {
   req(input$selectInput)  # Ensure `selectInput` is not NULL
   if (input$selectInput == "usedemo") {
      readr::read_csv('www/demo_gps.csv') |>
          mutate(year=year(time), yday=yday(time))
    } else if (input$selectInput == "usecsv") {
      req(input$csv1)
      readr::read_csv(input$csv1$datapath) |>
        mutate(year=year(time), yday=yday(time))
    }
  })

  # Read seasons and migration periods data
  seg_csv <- eventReactive(input$selectInput, {
    if (input$selectInput == "usedemo") {
      readr::read_csv('www/demo_segments.csv')
    } else if (input$selectInput == "usecsv") {
      req(input$csv2)
      readr::read_csv(input$csv2$datapath) 
    }
  })

  ##############################################################################
  # UPDATE UI
  ##############################################################################

  # Update choices for caribou individuals input based on input movement data
  observeEvent(c(input$selectInput, input$csv1), {
    x <- gps_csv()
    ids <- as.character(sort(unique(x$id)))
    updateSelectInput(session, "caribou", choices=ids, selected=ids[1])
  })

  # Update choices for seasons/migration periods based on input segmentation data
  observeEvent(c(input$selectInput, input$csv2), {
    x <- seg_csv()
    seasons <- x$season
    updateSelectInput(session, "season", choices=c("Annual",seasons), selected="Annual")
  })

  # Update seasonal/migration periods slider input based on selected input
  observeEvent(c(input$caribou,input$season),{
    x <- seg_csv()
    x1 <- x$start[x$season=="Annual"]
    x2 <- x$end[x$season=="Annual"]
    y <- seg_csv_expand()
    start=y$start_doy[y$id==input$caribou & y$season==input$season]
    end=y$end_doy[y$id==input$caribou & y$season==input$season]
    updateSliderInput(session, 'segments', label=paste0("Define date range (",x1,"-",x2,")"), 
      value=c(start, end))
  })

  ##############################################################################
  # CREATE TRACKS
  ##############################################################################

  # Create tracks using amt package
  trk_all <- eventReactive(input$getButton, {
    x <- gps_csv() |>
      make_track(.x=long, .y=lat, .t=time, id = id, long=long, lat=lat, crs = 4326) |>
      transform_coords(crs_to = 3578)
    x |> mutate(sl_ = step_lengths(x), 
      speed = speed(x),
      yday = yday(t_),
      year = year(t_)) |>
      #mutate(nsd = nsd(x)) |>
      mutate(yday = ifelse(yday>=day1() & yday<=366, yday-day1()+1, 366-day1()+1+yday), 
        nsd = nsd(x))
  })
  
  # Select tracks for one individual
  trk_one <- reactive({
    start <- input$segments[1]
    end <- input$segments[2]
    x <- trk_all() |> 
      filter(id %in% input$caribou) |>
      filter(year >= input$daterange[1] & year <= input$daterange[2]) |> 
      mutate(year=as.factor(year)) |>
      mutate(selected=ifelse(yday>=start & yday<=end, 1, 0))
  })

  ##############################################################################
  # SUMMARY TABLES & PLOTS
  ##############################################################################

  # Output segments data to table
  output$seg_data1 <- renderDT({
    req(input$getButton)
    x <- seg_csv()
    datatable(x)
  })

  # Output 'GPS data' to table
  output$gps_data <- renderDT({
    req(input$getButton)
    datatable(gps_csv())
  })

  # Output 'Sampling duration' to plot
  output$duration <- renderPlot({
   x <- gps_csv() |>
      mutate(id = as.factor(id), year = year(time))
    ggplot(data=x, aes(x=time, y=id)) +
      geom_path(size=1) +
      xlab('Time') + ylab('Collar ID') +
      theme(legend.position = 'none') +
      theme(axis.title = element_text(size = 15)) +
      theme(axis.text = element_text(size = 13))
  }, height=600)

  # Output 'Sampling rates' to table
  output$sampling_rates <- renderDT({
    trk_all() |> summarize_sampling_rate_many(cols='id') |>
      datatable() |>
      formatRound(columns=c('min','q1','median','mean','q3','max','sd'), digits=2)
  })

  ##############################################################################
  # DEFINE SEGMENTS
  ##############################################################################

  # Update start of year
  observe({
    x <- seg_csv()
    x1 <- x$start[x$season=="Annual"]
    updateTextInput(session, "day1", value=x1)
  })

  # First day of year - doesn't have to be Jan-01
  day1 <- reactive({
    x <- seg_csv()
    yday(as.Date(x$start[x$season=="Annual"], "%b-%d"))
  })

  # Expand segmentation data
  seg_csv_expand <- reactive({
    x <- seg_csv() |> mutate(start_doy=yday(as.Date(start, "%b-%d")), end_doy=yday(as.Date(end, "%b-%d")))
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
  })

  # Reactive data that can be updated
  display_data <- reactiveVal(NULL)

  # Initialize display_data when a file is uploaded
  observeEvent(seg_csv_expand(), {
    display_data(seg_csv_expand())
  })

  # Update the table when the sliders are moved
  observeEvent(c(input$caribou,input$season,input$segments), {
    x <- display_data()
    #x <- seg_csv_expand()
    x$start[x$id==input$caribou & x$season==input$season] <- format(as.Date(input$segments[1]-1), "%b-%d")
    x$end[x$id==input$caribou & x$season==input$season] <- format(as.Date(input$segments[2]-1), "%b-%d")
    display_data(x)
  })
  
  # Render expanded table
  output$seg_data2 <- renderDT({
    datatable(display_data())
  })
  
  # Render expanded table (test widget)
  output$test_output <- renderPrint({
    #print(trk_one())
    y <- seg_csv_expand()
    start=yday(as.Date(y$start[y$id==input$caribou & y$season==input$season], "%b-%d"))
    end=yday(as.Date(y$end[y$id==input$caribou & y$season==input$season], "%b-%d"))
    print(start)
    print(end)
  })

  # Output three plots for segmentation
  output$segmentPlot <- renderPlot({
    req(input$goButton)
    trk_data <- trk_one() |>
      mutate(selected = as.factor(selected))
    p1 <- ggplot(trk_data, aes(long, lat, color=selected))
    if (length(input$caribou)==1) {
      p1 <- p1 + geom_path(color='grey') # only add lines for one individual otherwise too busy
    }
    p1 <- p1 + geom_point() +
      scale_color_manual(values=c('grey65', 'red')) +
      ylab('Latitude') + xlab('Longitude')
    p2 <- ggplot(trk_data) +
      geom_line(aes(yday, long, color=year)) +
      ylab('Longitude') + xlab('Day of year') +
      geom_vline(xintercept=c(input$segments[1],input$segments[2]))
    p3 <- ggplot(trk_data) +
      geom_line(aes(yday, lat, color=year)) + 
      ylab('Latitude') + xlab('Day of year') +
      geom_vline(xintercept=c(input$segments[1],input$segments[2]))
    p4 <- ggplot(trk_data) + 
        geom_line(aes(yday, nsd, color=year)) + 
        ylab('NSD') + xlab('Day of year') +
        geom_vline(xintercept=c(input$segments[1],input$segments[2]))
    p1 | (p2/p3/p4)
  })

  # Download expanded and updated segmentation table
  output$downloadData <- downloadHandler(
    filename = function() { paste("movement_explorer-", Sys.Date(), ".csv", sep="") },
    content = function(file) { readr::write_csv(seg_csv_expand(), file) }
  )

  # Convert input segments data to expanded table (id by season)
  #seg_csv_list <- reactive({
  #  req(input$getButton)
  #  x <- seg_csv() |>
  #    mutate(start=yday(as.Date(start, "%b-%d")), end=yday(as.Date(end, "%b-%d"))) |>
  #    mutate(start = ifelse(start>=day1() & start<=365, start-day1()+1, 365-day1()+1+start),
  #      end = ifelse(end>=day1() & end<=365, end-day1()+1, 365-day1()+1+end))
  #  seg_list <- list(c(0,366))
  #  for (i in 1:nrow(x)) {
  #    seg_list[[i+1]] <- c(x$start[i], x$end[i])
  #  }
  #  names(seg_list) <- c('Annual', x$season)
  #  return(seg_list)
  #})

  # Update seasonal/migration periods slider input based on selected input
  #observe({
  #  segments <- seg_csv_list()
  #  x <- seg_csv()
  #  x1 <- x$start[x$season=="Annual"]
  #  x2 <- x$end[x$season=="Annual"]
  #  updateSliderInput(session, 'segments', label=paste0("Define date range (",x1,"-",x2,")"), 
  #    value=c(segments[[input$season]][1],segments[[input$season]][2]))
  #})

}
