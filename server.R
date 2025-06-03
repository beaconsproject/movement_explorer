server = function(input, output, session) {

  # RELOAD
  observeEvent(input$reload_btn, {
    session$reload()
  })

  ##############################################################################
  # READ INPUT DATA
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

  # Read and expand seasons data
  initial_seasons_data <- eventReactive(input$selectInput, {
    if (input$selectInput == "usedemo") {
      seg_csv <- readr::read_csv('www/demo_segments.csv')
    } else if (input$selectInput == "usecsv") {
      req(input$csv2)
      seg_csv <- readr::read_csv(input$csv2$datapath) 
    }
    x <- seg_csv |> mutate(start_doy=yday(as.Date(start, "%b-%d")), end_doy=yday(as.Date(end, "%b-%d")))
    x <- x |> mutate(start_doy = ifelse(start_doy>=day1() & start_doy<=365, start_doy-day1()+1, 365-day1()+1+start_doy),
        end_doy = ifelse(end_doy>=day1() & end_doy<=365, end_doy-day1()+1, 365-day1()+1+end_doy))
    ids <- unique(gps_csv()$id)
    y <- tibble(
      id=rep(ids, each=nrow(x)), 
      season=rep(x$season, length(ids)), 
      start=rep(x$start, length(ids)), 
      end=rep(x$end, length(ids)),
      start_doy=rep(x$start_doy, length(ids)),
      end_doy=rep(x$end_doy, length(ids)),
      start_doy_new=0,
      end_doy_new=0)
  })

  # Reactive value to store and manage the seasons_data
  r_seasons_data <- reactiveVal(NULL)

  # Initialize display_data when a file is uploaded
  observeEvent(initial_seasons_data(), {
    r_seasons_data(initial_seasons_data())
  })

  studyarea <- eventReactive(list(input$selectInput, input$upload_gpkg),{
    #req(input$getButton)
    if (input$selectInput == "usedemo") {
      st_read('www/demo_data.gpkg', 'studyarea', quiet = TRUE) |>
        st_transform(4326)
    } else if (input$selectInput == "usegpkg") {
      st_read(input$upload_gpkg$datapath, 'studyarea', quiet = TRUE) |>
        st_transform(4326)
    }
  })

  line <- eventReactive(input$selectInput,{
    #req(input$getButton)
    if (input$selectInput == "usedemo") {
      st_read('www/demo_data.gpkg', 'linear_disturbance', quiet = TRUE) |>
        st_transform(4326)
    } else if (input$selectInput == "usegpkg") {
      st_read(input$upload_gpkg$datapath, 'linear_disturbance', quiet = TRUE) |>
        st_transform(4326)
    }
  })
  
  poly <- eventReactive(input$selectInput,{
    #req(input$getButton)
    if (input$selectInput == "usedemo") {
      st_read('www/demo_data.gpkg', 'areal_disturbance', quiet = TRUE) |>
        st_transform(4326)
    } else if (input$selectInput == "usegpkg") {
      st_read(input$upload_gpkg$datapath, 'areal_disturbance', quiet = TRUE) |>
        st_transform(4326)
    }
  })

  fire <- eventReactive(input$selectInput,{
    #req(input$getButton)
    if (input$selectInput == "usedemo") {
      st_read('www/demo_data.gpkg', 'fires', quiet = TRUE) |>
        st_transform(4326)
    } else if (input$selectInput == "usegpkg") {
      st_read(input$upload_gpkg$datapath, 'fires', quiet = TRUE) |>
        st_transform(4326)
    }
  })

  foot <- eventReactive(input$selectInput,{
    #req(input$getButton)
    if (input$selectInput == "usedemo") {
      st_read('www/demo_data.gpkg', 'footprint_500m', quiet = TRUE) |>
        st_transform(4326)
    } else if (input$selectInput == "usegpkg") {
      st_read(input$upload_gpkg$datapath, 'footprint_500m', quiet = TRUE) |>
        st_transform(4326)
    }
  })

  pca <- eventReactive(input$selectInput,{
    #req(input$getButton)
    if (input$selectInput == "usedemo") {
      st_read('www/demo_data.gpkg', 'protected_areas', quiet = TRUE) |>
        st_transform(4326)
    } else if (input$selectInput == "usegpkg") {
      st_read(input$upload_gpkg$datapath, 'protected_areas', quiet = TRUE) |>
        st_transform(4326)
    }
  })

  ##############################################################################
  # UPDATE UI
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

  # Update choices for caribou individuals input based on input movement data
  observeEvent(c(input$selectInput, input$csv1), {
    x <- gps_csv()
    ids <- as.character(sort(unique(x$id)))
    updateSelectInput(session, "caribou", choices=ids, selected=ids[1])
    updateSelectInput(session, "caribou2", choices=ids, selected=ids[1])
  })

  # Update choices for seasons/migration periods based on input segmentation data
  observeEvent(c(input$selectInput, input$csv2), {
    x <- seg_csv()
    seasons <- x$season
    updateSelectInput(session, "season", choices=c("Annual",seasons), selected="Annual")
    updateSelectInput(session, "season2", choices=c("Spring migration","Fall migration"), selected="Spring migration")
  })

  # Observe changes in ID or Season selection to update the slider's current value
  observeEvent(list(input$caribou, input$season), {
    req(input$caribou, input$season) # Ensure both are selected
    # Find the original start and end for the selected combination
    # This part determines what the slider is INITIALLY set to when you pick an ID/Season
    selected_row_for_slider <- initial_seasons_data() %>%
      filter(id == as.numeric(input$caribou) & season == input$season)
    if (nrow(selected_row_for_slider) == 1) {
      updateSliderInput(session, "segments", label=paste0("Define date range (year starts on ",input$day1,")"),
                        value = c(selected_row_for_slider$start_doy[1], selected_row_for_slider$end_doy[1]))
    }
  }, ignoreNULL = TRUE, ignoreInit = FALSE) # ignoreInit = FALSE to run on app startup

  # Observe changes in the slider to update start_new and end_new for the selected row
  observeEvent(input$segments, {
    req(input$caribou, input$season, input$segments)
    current_data_snapshot <- r_seasons_data() # Get current state of the reactive data
    selected_id_num <- as.numeric(input$caribou)
    # Find the index of the row to update
    row_index <- which(current_data_snapshot$id == selected_id_num & 
                       current_data_snapshot$season == input$season)
    if (length(row_index) == 1) {
      current_data_snapshot$start_doy_new[row_index] <- input$segments[1]
      current_data_snapshot$end_doy_new[row_index] <- input$segments[2]
      r_seasons_data(current_data_snapshot) # Update the reactive data
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE) # ignoreInit = TRUE: only fire if user changes slider

  ##############################################################################
  # CREATE TRACKS
  ##############################################################################

  # Create tracks using amt package
  trk_all <- eventReactive(input$getButton, {
    x <- gps_csv() |>
      make_track(.x=long, .y=lat, .t=time, id = id, long=long, lat=lat, crs = 4326) #|>
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

  # Render the DT table
  output$seg_data2 <- renderDT({
    datatable(r_seasons_data(), 
              editable = FALSE, # Table itself is not directly editable here
              rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE))
  })

  # Render expanded table (test widget)
  output$test_output <- renderPrint({
    trk_all <- mutate(trk_all(), year=as.double(year))
    print(trk_all)
  })


  # Leaflet map with locations, home ranges, and disturbances
  output$map1 <- renderLeaflet({
    if (input$getButton) {
      years <- unique(gps_csv()$year)
      caribou_pal <- colorFactor(topo.colors(25), gps_csv()$id)
      year_pal <- colorNumeric(palette=col_yrs6, domain=years)
      m <- leaflet(options = leafletOptions(attributionControl=FALSE)) |>
        addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
        addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
        addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap")
        groups <- NULL
        trk_all <- mutate(trk_all(), year=as.double(year))
        for (i in sort(unique(trk_all$id))) {
          id1 <- trk_all |> filter(id==i)
          groups <- c(groups, paste0("id_",i))
          m <- m |> addCircles(data=id1, ~x_, ~y_, fill=T, stroke=T, weight=2, color=~year_pal(year), 
            fillColor=~year_pal(year), fillOpacity=1, group=paste0("id_",i), popup=id1$t_)
        }
        m <- m |> 
          addLegend("topleft", colors=col_yrs6, labels=years, title="Year") |>
          addScaleBar(position="bottomright") |>
          addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
            overlayGroups = groups,
            options = layersControlOptions(collapsed = FALSE)) |>
          hideGroup("")
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

  ##############################################################################
  # MOVEMENT PATHS
  ##############################################################################

  # Select tracks for one individual
  trk_one2 <- reactive({
    start <- input$segments[1]
    end <- input$segments[2]
    x <- trk_all() |> 
      filter(id %in% input$caribou2) |>
      filter(year >= input$daterange2[1] & year <= input$daterange2[2]) |> 
      mutate(year=as.factor(year)) |>
      mutate(selected=ifelse(yday>=start & yday<=end, 1, 0))
  })

  # Convert track to sf linestring
  path1 <- reactive({
    st_as_sf(trk_one2(), coords = c("x_", "y_"), crs = 4326) |>
      group_by(id, year) |> 
      summarize(do_union=FALSE) |> 
      st_cast("LINESTRING")
  })

  # Estimate home range
  #hr1 <- reactive({
  #  hr_mcp(trk_one(), levels=input$levels)
  #})

  # Estimate home range
  od1 <- reactive({
    od <- od(trk_one2(), model = fit_ctmm(trk_one2(), "bm"), trast = make_trast(trk_one2()))
    iso <- hr_isopleths(od, levels=0.95) |> st_transform(4326)
    corridor <- st_union(iso, st_buffer(path1(), 500)) |> st_sf()
  })

  # Output HR results
  output$hr_output <- renderPrint({
    #cat("Area:", round(od1()$area/1000000,0), "\n")
    trk_one <- mutate(trk_one(), year=as.double(year))
    print(trk_one)
  })

  # Leaflet map with locations, home ranges, and disturbances
  output$map2 <- renderLeaflet({
    if (input$goButton2) {
      years <- unique(gps_csv()$year)
      caribou_pal <- colorFactor(topo.colors(25), gps_csv()$id)
      year_pal <- colorNumeric(palette=col_yrs6, domain=years)
      m <- leaflet(options = leafletOptions(attributionControl=FALSE)) |>
        addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
        addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
        addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap")
        groups <- NULL
        trk_one <- mutate(trk_one2(), year=as.double(year))
        m <- m |> addPolygons(data=od1(), color="#386cb0", fill=T, weight=2, fillOpacity=0.5, group="Movement corridor")
        for (i in sort(unique(trk_one$year))) {
          yr1 <- trk_one |> filter(year==i)
          groups <- c(groups, paste0("Track ",i))
          m <- m |> addPolylines(data=yr1, lng=~x_, lat=~y_, color=col_yrs6[1], weight=2, group=paste0("Track ",i))
        }
        m <- m |> 
          addCircles(data=trk_one, ~x_, ~y_, fill=T, stroke=T, weight=2, color=~year_pal(year), fillColor=~year_pal(year), fillOpacity=1, group="Locations", popup=trk_one()$t_) |>
          addPolygons(data=studyarea(), color="black", weight=2, fill=FALSE, group="Study area") |>
          addPolylines(data=line(), color="black", weight=2, group="Linear disturbance") |>
          addPolygons(data=poly(), color="black", weight=1, fill=TRUE, group="Areal disturbance") |>
          addPolygons(data=foot(), color="black", weight=1, fill=TRUE, fillOpacity=0.5, group="Footprint 500m") |>
          addPolygons(data=fire(), color="darkred", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires") |>
          addPolygons(data=pca(), color="darkblue", weight=1, fill=TRUE, fillOpacity=0.5, group="Conservation areas") |>
          addLegend("topleft", colors=col_yrs6, labels=years, title="Year") |>
          addScaleBar(position="bottomright") |>
          addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
            overlayGroups = c("Locations", groups, "Study area", "Areal disturbance","Linear disturbance","Footprint 500m","Fires","Conservation areas", "Movement corridor"),
            options = layersControlOptions(collapsed = FALSE)) |>
          hideGroup(c(groups,"Areal disturbance","Linear disturbance","Footprint 500m","Fires","Conservation areas"))
      m
    }
  })

  ##############################################################################
  # DOWNLOAD UPDATED SEASONS DATA
  ##############################################################################

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("seasons_data_updated_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(r_seasons_data(), file, row.names = FALSE)
    }
  )

  output$downloadData2 <- downloadHandler(
    filename = function() { 
      paste("HR_",input$caribou2,"_",input$season2,"_",input$hr,input$levels,"_",input$hr2,input$levels2," (", Sys.Date(), ").gpkg", sep="") 
    },
    content = function(file) {
        showModal(modalDialog("Downloading...", footer=NULL))
        on.exit(removeModal())
        if (input$goButton) {
          st_write(hr_isopleths(hr1()), dsn=file, layer=paste0('HR1_',input$hr,input$levels), append=TRUE)
          st_write(gps_subset(), dsn=file, layer=paste0('GPS_', input$season), append=TRUE)
       }
    }
  )

}
