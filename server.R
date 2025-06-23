server = function(input, output, session) {

  # RELOAD
  observeEvent(input$reload_btn, {
    session$reload()
  })

  ##############################################################################
  # READ INPUT DATA
  ##############################################################################

  # Read gps movement data
  gps_csv <- eventReactive(list(input$selectInput,input$csv1), {
    req(input$selectInput)  # Ensure `selectInput` is not NULL
    if (input$selectInput == "usedemo") {
      readr::read_csv('www/demo_gps.csv') |>
          mutate(year=year(time), yday=yday(time))
    } else if (input$selectInput == "usedata") {
      req(input$csv1)
      readr::read_csv(input$csv1$datapath) |>
        mutate(year=year(time), yday=yday(time))
    }
  })

  gps1 <- reactive({
    x <- gps_csv() |>
      group_by(id) |>
      arrange(time) |>
      slice(1) |>
      ungroup() #|>
      #st_as_sf(coords=c("long", "lat"), crs=4326)
    #x <- x |> mutate(long=st_coordinates(x)[,1], lat=st_coordinates(x)[,2])
  })

  # Read seasons and migration periods data
  seg_csv <- eventReactive(list(input$selectInput,input$csv2), {
    req(input$selectInput)  # Ensure `selectInput` is not NULL
    if (input$selectInput == "usedemo") {
      readr::read_csv('www/demo_segments.csv')
    } else if (input$selectInput == "usedata") {
      req(input$csv2)
      readr::read_csv(input$csv2$datapath) 
    }
  })

  # Read and expand seasons data
  initial_seasons_data <- eventReactive(seg_csv(),{
    if (length(names(seg_csv()))==3) {
      x <- seg_csv() |> mutate(start_doy=yday(as.Date(start, "%b-%d")), end_doy=yday(as.Date(end, "%b-%d")))
      x <- x |> mutate(
        #start_doy = ifelse(start_doy>=day1() & start_doy<=365, start_doy-day1()+1, 365-day1()+1+start_doy),
        #end_doy = ifelse(end_doy>=day1() & end_doy<=365, end_doy-day1()+1, 365-day1()+1+end_doy))
        start_doy = ifelse(start_doy>=day1() & start_doy<=365, start_doy-day1()+1, 365-day1()+1+start_doy),
        end_doy = ifelse(end_doy>=day1() & end_doy<=365, end_doy-day1()+1, 365-day1()+1+end_doy))
      ids <- unique(gps_csv()$id)
      y <- tibble(
        id=rep(ids, each=nrow(x)), 
        season=rep(x$season, length(ids)), 
        start=rep(x$start, length(ids)), 
        end=rep(x$end, length(ids)),
        start_doy=rep(x$start_doy, length(ids)),
        end_doy=rep(x$end_doy, length(ids)),
        start_doy_new=start_doy,
        end_doy_new=end_doy)
    } else {
      y <- seg_csv()
    }
  })

  # Reactive value to store and manage the seasons_data
  r_seasons_data <- reactiveVal(NULL)

  # Initialize display_data when a file is uploaded
  observeEvent(initial_seasons_data(), {
    r_seasons_data(initial_seasons_data())
  })

  studyarea <- eventReactive(list(input$selectInput, input$gpkg),{
    #req(input$getButton)
    if (input$selectInput == "usedemo") {
      st_read('www/demo_data.gpkg', 'studyarea', quiet = TRUE) |>
        st_transform(4326)
    } else if (input$selectInput == "usedata") {
      st_read(input$gpkg$datapath, 'studyarea', quiet = TRUE) |>
        st_transform(4326)
    }
  })

  #line <- eventReactive(input$selectInput,{
  #  #req(input$getButton)
  #  if (input$selectInput == "usedemo") {
  #    st_read('www/demo_data.gpkg', 'linear_disturbance', quiet = TRUE) |>
  #      st_transform(4326)
  #  } else if (input$selectInput == "usedata") {
  #    st_read(input$gpkg$datapath, 'linear_disturbance', quiet = TRUE) |>
  #      st_transform(4326)
  #  }
  #})
  
  #poly <- eventReactive(input$selectInput,{
  #  #req(input$getButton)
  #  if (input$selectInput == "usedemo") {
  #    st_read('www/demo_data.gpkg', 'areal_disturbance', quiet = TRUE) |>
  #      st_transform(4326)
  #  } else if (input$selectInput == "usedata") {
  #    st_read(input$gpkg$datapath, 'areal_disturbance', quiet = TRUE) |>
  #      st_transform(4326)
  #  }
  #})

  #fire <- eventReactive(input$selectInput,{
  #  #req(input$getButton)
  #  if (input$selectInput == "usedemo") {
  #    st_read('www/demo_data.gpkg', 'fires', quiet = TRUE) |>
  #      st_transform(4326)
  #  } else if (input$selectInput == "usedata") {
  #    st_read(input$gpkg$datapath, 'fires', quiet = TRUE) |>
  #      st_transform(4326)
  #  }
  #})

  #foot <- eventReactive(input$selectInput,{
  #  #req(input$getButton)
  #  if (input$selectInput == "usedemo") {
  #    st_read('www/demo_data.gpkg', 'footprint_500m', quiet = TRUE) |>
  #      st_transform(4326)
  #  } else if (input$selectInput == "usedata") {
  #    st_read(input$gpkg$datapath, 'footprint_500m', quiet = TRUE) |>
  #      st_transform(4326)
  #  }
  #})

  #pca <- eventReactive(input$selectInput,{
  #  #req(input$getButton)
  #  if (input$selectInput == "usedemo") {
  #    st_read('www/demo_data.gpkg', 'protected_areas', quiet = TRUE) |>
  #      st_transform(4326)
  #  } else if (input$selectInput == "usedata") {
  #    st_read(input$gpkg$datapath, 'protected_areas', quiet = TRUE) |>
  #      st_transform(4326)
  #  }
  #})

  ##############################################################################
  # UPDATE UI
  ##############################################################################

  # Update start of year in textInput
  #observe({
  #  x <- seg_csv()
  #  if ("Annual" %in% unique(x$season)) {
  #    x1 <- x$start[x$season=="Annual"][1]
  #  } else {
  #    x1 <- "Jan-01"
  #  }
  #  updateTextInput(session, "day1", value=x1)
  #})

  # First day of year - doesn't have to be Jan-01
  day1 <- reactive({
    #x <- seg_csv()
    #if ("Annual" %in% unique(x$season)) {
    #  yday(as.Date(x$start[x$season=="Annual"], "%b-%d"))
    #} else {
      yday(as.Date("Jan-01", "%b-%d"))
    #}
  })

  first_year <- reactive({
    min(gps_csv()$year)
  })

  last_year <- reactive({
    max(gps_csv()$year)
  })

  output$tab2 <- renderText({ start_date() })

  start_date <- reactive({
    as.Date("Jan-01", "%b-%d")
  })

  end_date <- reactive({
    as.Date("Dec-31", "%b-%d")
  })

  # Update choices for caribou individuals input based on input movement data
  observeEvent(c(input$selectInput, input$csv1), {
    x <- gps_csv()
    ids <- as.character(sort(unique(x$id)))
    updateSelectInput(session, "caribou", choices=ids, selected=ids[1])
    updateSelectInput(session, "caribou2", choices=ids, selected=ids[1])
    updateSelectInput(session, "caribou3", choices=ids, selected=ids[1])
    updateSliderInput(session, "daterange", min=first_year(), max=last_year(), value=c(first_year()+1,first_year()+1))
    updateSliderInput(session, "daterange2", min=first_year(), max=last_year(), value=c(first_year()+1,last_year()-1))
    updateSliderInput(session, "daterange3", min=first_year(), max=last_year(), value=c(first_year()+1,last_year()-1))
  })

  # Update choices for seasons/migration periods based on input segmentation data
  observeEvent(c(input$selectInput, input$csv2), {
    x <- seg_csv()
    seasons <- x$season
    updateSelectInput(session, "season", choices=c("Annual",seasons), selected="Annual")
    updateSelectInput(session, "season2", choices=c("Annual","Early winter","Late winter","Summer","Fall rut"), selected="Annual")
    updateSelectInput(session, "season3", choices=c("Spring migration","Fall migration"), selected="Spring migration")
  })

  # Observe changes in ID or Season selection to update the slider's current value
  observeEvent(list(input$caribou, input$season), {
    req(input$caribou, input$season) # Ensure both are selected
    # Find the original start and end for the selected combination
    # This part determines what the slider is INITIALLY set to when you pick an ID/Season
    selected_row_for_slider <- initial_seasons_data() |>
      filter(id == as.numeric(input$caribou) & season == input$season)
    if (nrow(selected_row_for_slider) == 1) {
      updateSliderInput(session, "segments_date", label=paste0("Define date range:"),
        value = c(as.Date(selected_row_for_slider$start[1], format="%b-%d"), as.Date(selected_row_for_slider$end[1], format="%b-%d")),
        timeFormat="%b-%d")
    }
  }, ignoreNULL = TRUE, ignoreInit = FALSE) # ignoreInit = FALSE to run on app startup

  # Observe changes in the slider to update start_new and end_new for the selected row
  observeEvent(input$segments_date, {
    req(input$caribou, input$season, input$segments_date)
    current_data_snapshot <- r_seasons_data() # Get current state of the reactive data
    selected_id_num <- as.numeric(input$caribou)
    # Find the index of the row to update
    row_index <- which(current_data_snapshot$id == selected_id_num & 
                       current_data_snapshot$season == input$season)
    if (length(row_index) == 1) {
      current_data_snapshot$start_doy_new[row_index] <- yday(input$segments_date[1])
      current_data_snapshot$end_doy_new[row_index] <- yday(input$segments_date[2])
      r_seasons_data(current_data_snapshot) # Update the reactive data
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE) # ignoreInit = TRUE: only fire if user changes slider
  
  ##################################################################################################
  # Using day-of-year not real dates
  # Observe changes in ID or Season selection to update the slider's current value
  #observeEvent(list(input$caribou, input$season), {
  #  req(input$caribou, input$season) # Ensure both are selected
  #  # Find the original start and end for the selected combination
  #  # This part determines what the slider is INITIALLY set to when you pick an ID/Season
  #  selected_row_for_slider <- initial_seasons_data() %>%
  #    filter(id == as.numeric(input$caribou) & season == input$season)
  #  if (nrow(selected_row_for_slider) == 1) {
  #    updateSliderInput(session, "segments", label=paste0("Define date range:"),
  #      value = c(selected_row_for_slider$start_doy[1], selected_row_for_slider$end_doy[1]))
  #  }
  #}, ignoreNULL = TRUE, ignoreInit = FALSE) # ignoreInit = FALSE to run on app startup

  # Observe changes in the slider to update start_new and end_new for the selected row
  #observeEvent(input$segments, {
  #  req(input$caribou, input$season, input$segments)
  #  current_data_snapshot <- r_seasons_data() # Get current state of the reactive data
  #  selected_id_num <- as.numeric(input$caribou)
  #  # Find the index of the row to update
  #  row_index <- which(current_data_snapshot$id == selected_id_num & 
  #                     current_data_snapshot$season == input$season)
  #  if (length(row_index) == 1) {
  #    current_data_snapshot$start_doy_new[row_index] <- input$segments[1]
  #    current_data_snapshot$end_doy_new[row_index] <- input$segments[2]
  #    r_seasons_data(current_data_snapshot) # Update the reactive data
  #  }
  #}, ignoreNULL = TRUE, ignoreInit = TRUE) # ignoreInit = TRUE: only fire if user changes slider
  ##################################################################################################

  ##############################################################################
  # CREATE TRACKS
  ##############################################################################

  # Create tracks using amt package
  trk_all <- eventReactive(input$getButton, {
    x <- gps_csv() |>
      make_track(.x=long, .y=lat, .t=time, id = id, long=long, lat=lat, elev=elev, crs = 4326) #|>
      #transform_coords(crs_to = 3578)
    x |> mutate(sl_ = step_lengths(x), 
      speed = speed(x),
      yday = yday(t_),
      year = year(t_)) |>
      mutate(yday = ifelse(yday>=day1() & yday<=366, yday-day1()+1, 366-day1()+1+yday), 
        nsd = nsd(x))
  })
  
  # Select tracks for one individual
  trk_one <- reactive({
    start <- yday(input$segments_date[1])
    end <- yday(input$segments_date[2])
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

  # Render the DT table
  output$seg_data2 <- renderDT({
    datatable(r_seasons_data(), 
              editable = FALSE, # Table itself is not directly editable here
              rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE))
  })

  # Leaflet map with locations, home ranges, and disturbances
  output$map1 <- renderLeaflet({
    if (input$getButton) {
      years <- unique(gps_csv()$year)
      caribou_pal <- colorFactor(topo.colors(25), gps_csv()$id)
      cols <- col_yrs6[1:length(years)]
      year_pal <- colorNumeric(palette=cols, domain=years)
      gps1 <- gps1()
      m <- leaflet(options = leafletOptions(attributionControl=FALSE)) |>
        addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
        addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
        addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") |>
        addCircles(data=gps1, ~long, ~lat, fill=T, stroke=T, weight=12, color="black", 
          fillColor="black", fillOpacity=1, group="Capture location", popup=paste0("Caribou ",gps1$id, " collared on ", gps1$time))
        groups <- NULL
        trk_all <- mutate(trk_all(), year=as.double(year))
        for (i in sort(unique(trk_all$id))) {
          id1 <- trk_all |> filter(id==i)
          groups <- c(groups, paste0("id_",i))
          m <- m |> addCircles(data=id1, ~x_, ~y_, fill=T, stroke=T, weight=2, color=~year_pal(year), 
            fillColor=~year_pal(year), fillOpacity=1, group=paste0("id_",i), popup=id1$t_)
        }
        m <- m |> 
          addLegend("topleft", colors=cols, labels=years, title="Year") |>
          addScaleBar(position="bottomright") |>
          addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
            overlayGroups = c("Capture location", groups),
            options = layersControlOptions(collapsed = FALSE)) |>
          hideGroup(c(groups[2:7], "Capture location"))
      m
    }
  })

  ##############################################################################
  # SEGMENTATION PLOTS
  ##############################################################################

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
      geom_vline(xintercept=c(yday(input$segments_date)[1],yday(input$segments_date)[2]))
    p3 <- ggplot(trk_data) +
      geom_line(aes(yday, lat, color=year)) + 
      ylab('Latitude') + xlab('Day of year') +
      geom_vline(xintercept=c(yday(input$segments_date)[1],yday(input$segments_date)[2]))
    p4 <- ggplot(trk_data) + 
        geom_line(aes(yday, nsd, color=year)) + 
        ylab('NSD') + xlab('Day of year') +
        geom_vline(xintercept=c(yday(input$segments_date)[1],yday(input$segments_date)[2]))
    p5 <- ggplot(trk_data) + 
        geom_line(aes(yday, elev, color=year)) + 
        ylab('Elevation') + xlab('Day of year') +
        geom_vline(xintercept=c(yday(input$segments_date)[1],yday(input$segments_date)[2]))
    p1 | (p2/p3/p4/p5)
  })

  ##############################################################################
  # HOME RANGES
  ##############################################################################

  # Select tracks for one individual
  trk_one2 <- reactive({
    r <- r_seasons_data() |>
      filter(id %in% input$caribou2 & season==input$season2)
    start <- r$start_doy_new
    end <- r$end_doy_new
    x <- trk_all() |> 
      filter(id %in% input$caribou2) |>
      filter(year >= input$daterange2[1] & year <= input$daterange2[2]) |> 
      filter(yday >= start & yday <= end)
  })

  # Estimate home range
  hr1 <- reactive({
    if (input$hr=="MCP") {
      x <- hr_mcp(trk_one2(), levels=input$levels)
    } else if (input$hr=="KDE") {
      lvl <- input$levels
      if (lvl[2]==1) {lvl[2]=0.999}
      if (input$h > 0) {
        x <- hr_kde(trk_one2(), levels=lvl, h=input$h)
      } else {
        x <- hr_kde(trk_one2(), levels=lvl)
      }
    } else if (input$hr=="aKDE") {
      lvl <- input$levels
      #if (lvl==1) {lvl=0.999}
      x <- hr_akde(trk_one2(), levels=lvl)
    } else if (input$hr=="LoCoH") {
      lvl <- input$levels
      #if (lvl==1) {lvl=0.999}
      x <- hr_locoh(trk_one2(), levels=lvl)
    }
    hr_isopleths(x)
  })

  # Leaflet map with locations, home ranges, and disturbances
  output$mapRange <- renderLeaflet({
    if (input$goRange) {
      years <- unique(gps_csv()$year)
      caribou_pal <- colorFactor(topo.colors(25), gps_csv()$id)
      cols <- col_yrs6[1:length(years)]
      year_pal <- colorNumeric(palette=cols, domain=years)
      trk_one <- mutate(trk_one2(), year=as.double(year))
      m <- leaflet(options = leafletOptions(attributionControl=FALSE)) |>
        addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
        addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
        addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap")
        groups <- NULL
        m <- m |> addPolygons(data=hr1(), color="blue", fill=F, weight=2, group="Home ranges")
        #for (i in sort(unique(trk_one$year))) {
        #  yr1 <- trk_one |> filter(year==i)
        #  groups <- c(groups, paste0("Track ",i))
        #  m <- m |> addPolylines(data=yr1, lng=~x_, lat=~y_, color=col_yrs6[1], weight=2, group=paste0("Track ",i))
        #}
        m <- m |> 
          addCircles(data=trk_one, ~x_, ~y_, fill=T, stroke=T, weight=2, color=~year_pal(year), fillColor=~year_pal(year), fillOpacity=1, group="Locations", popup=trk_one()$t_) |>
          addPolygons(data=studyarea(), color="black", weight=2, fill=FALSE, group="Study area") |>
          #addPolylines(data=line(), color="black", weight=2, group="Linear disturbance") |>
          #addPolygons(data=poly(), color="black", weight=1, fill=TRUE, group="Areal disturbance") |>
          #addPolygons(data=foot(), color="black", weight=1, fill=TRUE, fillOpacity=0.5, group="Footprint 500m") |>
          #addPolygons(data=fire(), color="darkred", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires") |>
          #addPolygons(data=pca(), color="darkblue", weight=1, fill=TRUE, fillOpacity=0.5, group="Conservation areas") |>
          addLegend("topleft", colors=cols, labels=years, title="Year") |>
          addScaleBar(position="bottomright") |>
          addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
            #overlayGroups = c("Locations", groups, "Areal disturbance","Linear disturbance","Footprint 500m","Fires","Conservation areas", "Home ranges"),
            overlayGroups = c("Study area", "Locations", "Home ranges"),
            options = layersControlOptions(collapsed = FALSE)) |>
          #hideGroup(c(groups,"Areal disturbance","Linear disturbance","Footprint 500m","Fires","Conservation areas"))
          hideGroup("")
      m
    }
  })

  ##############################################################################
  # MOVEMENT PATHS
  ##############################################################################

  # Select tracks for one individual
  trk_one3 <- reactive({
    r <- r_seasons_data() |>
      filter(id %in% input$caribou3 & season==input$season3)
    start <- r$start_doy_new
    end <- r$end_doy_new
    x <- trk_all() |> 
      filter(id %in% input$caribou3) |>
      filter(year >= input$daterange3[1] & year <= input$daterange3[2]) |> 
      filter(yday >= start & yday <= end)
  })

  # Test widget
  output$text2 <- renderPrint({
    r <- r_seasons_data() |>
      filter(id %in% input$caribou2 & season==input$season2)
    start <- r$start_doy_new
    end <- r$end_doy_new
    cat(input$season2, ":\n", sep="")
    startdate <- as.Date(start - 1, origin = paste0(input$daterange2[1], "-01-01"))
    enddate <- as.Date(end - 1, origin = paste0(input$daterange2[2], "-01-01"))
    cat(as.character(startdate), "-", as.character(enddate), "\n")
  })

  # Test widget
  output$text3 <- renderPrint({
    r <- r_seasons_data() |>
      filter(id %in% input$caribou3 & season==input$season3)
    start <- r$start_doy_new
    end <- r$end_doy_new
    cat(input$season3, ":\n", sep="")
    startdate <- as.Date(start - 1, origin = paste0(input$daterange3[1], "-01-01"))
    enddate <- as.Date(end - 1, origin = paste0(input$daterange3[2], "-01-01"))
    cat(as.character(startdate), "-", as.character(enddate), "\n")
  })

  # Convert track to sf linestring
  path1 <- reactive({
    st_as_sf(trk_one3(), coords = c("x_", "y_"), crs = 4326) |>
      #group_by(id, year) |> 
      summarize(do_union=FALSE) |> 
      st_cast("LINESTRING") |>
      st_buffer(500) |>
      st_union() |> 
      st_sf()
  })

  # Estimate home range
  #hr1 <- reactive({
  #  hr_mcp(trk_one(), levels=input$levels)
  #})

  # Estimate home range
  od1 <- reactive({
    od <- od(trk_one3(), model = fit_ctmm(trk_one3(), "bm"), trast = make_trast(trk_one3()))
    iso <- hr_isopleths(od, levels=0.95) |> st_transform(4326)
    #corridor <- st_union(iso, st_buffer(path1(), 500)) |> st_sf()
  })

  od1path1 <- reactive({
    corridor <- st_union(od1(), path1()) |> 
      st_sf()
  })

  # Leaflet map with locations, home ranges, and disturbances
  output$mapPath <- renderLeaflet({
    if (input$goPath) {
      years <- unique(gps_csv()$year)
      caribou_pal <- colorFactor(topo.colors(25), gps_csv()$id)
      cols <- col_yrs6[1:length(years)]
      year_pal <- colorNumeric(palette=cols, domain=years)
      m <- leaflet(options = leafletOptions(attributionControl=FALSE)) |>
        addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
        addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
        addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap")
        groups <- NULL
        trk_one <- mutate(trk_one3(), year=as.double(year))
        m <- m |> addPolygons(data=od1(), color="blue", fill=T, weight=2, fillOpacity=0.5, group="Occurrence distribution") |>
          addPolygons(data=path1(), color="green", fill=T, weight=2, fillOpacity=0.5, group="Buffered tracks") |>
          addPolygons(data=od1path1(), color="red", fill=T, weight=2, fillOpacity=0.5, group="Movement corridor")
        for (i in sort(unique(trk_one$year))) {
          yr1 <- trk_one |> filter(year==i)
          groups <- c(groups, paste0("Track ",i))
          m <- m |> addPolylines(data=yr1, lng=~x_, lat=~y_, color=col_yrs6[1], weight=2, group=paste0("Track ",i))
        }
        m <- m |> 
          addCircles(data=trk_one, ~x_, ~y_, fill=T, stroke=T, weight=2, color=~year_pal(year), fillColor=~year_pal(year), fillOpacity=1, group="Locations", popup=trk_one()$t_) |>
          addPolygons(data=studyarea(), color="black", weight=2, fill=FALSE, group="Study area") |>
          #addPolylines(data=line(), color="black", weight=2, group="Linear disturbance") |>
          #addPolygons(data=poly(), color="black", weight=1, fill=TRUE, group="Areal disturbance") |>
          #addPolygons(data=foot(), color="black", weight=1, fill=TRUE, fillOpacity=0.5, group="Footprint 500m") |>
          #addPolygons(data=fire(), color="darkred", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires") |>
          #addPolygons(data=pca(), color="darkblue", weight=1, fill=TRUE, fillOpacity=0.5, group="Conservation areas") |>
          addLegend("topleft", colors=cols, labels=years, title="Year") |>
          addScaleBar(position="bottomright") |>
          addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
            #overlayGroups = c("Locations", groups, "Areal disturbance","Linear disturbance","Footprint 500m","Fires","Conservation areas", "Occurrence distribution", "Buffered tracks", "Movement corridor"),
            overlayGroups = c("Study area", "Locations", groups, "Occurrence distribution", "Buffered tracks", "Movement corridor"),
            options = layersControlOptions(collapsed = FALSE)) |>
          hideGroup(c(groups, "Buffered tracks", "Movement corridor"))
      m
    }
  })

  ##############################################################################
  # DOWNLOAD DATA
  ##############################################################################

  # Download updated seasons data
  output$downloadSegments <- downloadHandler(
    filename = function() {
      paste0("seasons_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(r_seasons_data(), file, row.names = FALSE)
    }
  )

  # Download estimated home ranges
  output$downloadRanges <- downloadHandler(
    filename = function() {
      paste0("home_ranges_", Sys.Date(), ".gpkg")
    },
    content = function(file) {
      st_write(hr1(), file, layer=paste0(input$hr,"-",input$levels[1],"-",input$levels[2]), append=TRUE)
    }
  )

  # Download estimated movement paths
  output$downloadPaths <- downloadHandler(
    filename = function() {
      paste0("movement_paths_", Sys.Date(), ".gpkg")
    },
    content = function(file) {
      st_write(od1(), file, layer=input$season3, append=TRUE)
    }
  )

}
