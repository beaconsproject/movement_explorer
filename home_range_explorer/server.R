#-------------------------------------------------
# 3. Server functions
#-------------------------------------------------

server = function(input, output, session) {

  # RELOAD
  observeEvent(input$reload_btn, {
    session$reload()
  })

  #-------------------------------------------------
  # 3.1 Section: Select data
  #-------------------------------------------------

  # UPDATE UI
  # ---------

  # Update choices for caribou individuals input based on input movement data
  observeEvent(c(input$selectInput,input$gps_data), {
    if (input$selectInput == "usedemo") {
      x <- readr::read_csv('www/demo_gps.csv')
    } else if (input$selectInput == "usegpkg") {
      req(input$gps_data)
      x <- st_read(input$gps_data$datapath)
    }
    ids <- as.character(sort(unique(x$id)))
    updateSelectInput(session, "caribou", choices=ids, selected=ids[1])
  })

  # Update choices for seasons/migration periods based on input segmentation data
  observeEvent(c(input$selectInput,input$seg_data), {
    if (input$selectInput == "usedemo") {
      x <- readr::read_csv('www/demo_segments.csv')
    } else if (input$selectInput == "usegpkg") {
      req(input$seg_data)
      x <- st_read(input$seg_data$datapath)
    }
    seasons <- x$season
    updateSelectInput(session, "season", choices=c("Annual",seasons), selected="Annual")
  })

  # READ INPUT FILES
  # ----------------

  # Read gps movement data
  gps_df <- eventReactive(input$selectInput, {
    #req(input$getButton)
    if (input$selectInput == "usedemo") {
      readr::read_csv('www/demo_gps.csv') |>
          mutate(year=year(time), yday=yday(time))
    } else if (input$selectInput == "usegpkg") {
      readr::read_csv(input$gps_data$datapath) |>
        mutate(year=year(time), yday=yday(time))
    }
  })

  # Read seasons and migration periods data
  seg_df <- eventReactive(input$selectInput, {
    #req(input$getButton)
    if (input$selectInput == "usedemo") {
      readr::read_csv('www/demo_segments.csv')
    } else if (input$selectInput == "usegpkg") {
      readr::read_csv(input$seg_data$datapath) 
    }
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

  # PROCESS DATA
  # ------------
  
  # Identify day 1 of the year e.g., Feb-01
  day1 <- reactive({
    x <- seg_df()
    day1 <- x$start[x$season=="Annual"]
    day1 <- yday(as.Date(day1, "%b-%d"))
  })
  
  # Convert segment table dates to day of year
  seg_df_yday <- reactive({
    day1 <- day1()
    seg_df() |> mutate(start=yday(as.Date(start, "%b-%d")), end=yday(as.Date(end, "%b-%d"))) |>
      mutate(start = ifelse(start>=day1 & start<=366, start-day1+1, 366-day1+1+start),
        end = ifelse(end>=day1 & end<=366, end-day1+1, 366-day1+1+end)) # adjust year to day1
  })

  # For all individuals, filter by selected season and date range
  gps_subset <- reactive({
    x <- seg_df_yday()
    day1 <- day1()
    gps_df() |>
      mutate(year=year(time), yday=yday(time)) |>
      mutate(yday = ifelse(yday>=day1 & yday<=366, yday-day1+1, 366-day1+1+yday)) |> # adjust year to day1
      filter(year >= input$daterange[1] & year <= input$daterange[2]) |>
      filter(yday>=x$start[x$season==input$season] & yday<=x$end[x$season==input$season])
  })    

  # Create tracks for all individuals (these are already filtered by season and date range)
  trk_all <- reactive({
    gps_subset() |>
      make_track(.x=long, .y=lat, .t=time, id = id, long=long, lat=lat, all_cols=TRUE, crs = 4326) # |> transform_coords(crs_to = 3578)
  })

  # Select tracks for one individual (these are already filtered by season and date range)
  trk_one <- reactive({
    trk_all() |> filter(id %in% input$caribou)
  })

  # GENERATE OUTPUTS
  # ----------------

  # Output "GPS data" to table
  output$gps_data <- renderDT({
    req(input$getButton)
    datatable(gps_df())
  })

  # Output segments data to table
  output$seg_data1 <- renderDT({
    req(input$getButton)
    x <- seg_df()
    datatable(x)
  })

  # Output disturbance data to table
  output$dist_data1 <- renderPrint({
    req(input$getButton)
    #x <- st_layers(input$upload_gpkg$datapath)
    cat("Movement data\n")
    glimpse(gps_df())
    cat("\nSegmentation data\n")
    glimpse(seg_df())
    cat("\nStudy area\n")
    glimpse(studyarea())
    cat("\nLinear disturbance\n")
    glimpse(line())
    cat("\nAreal disturbance\n")
    glimpse(poly())
    cat("\nFires\n")
    glimpse(fire())
    cat("\nFootprint (500m)\n")
    glimpse(foot())
    cat("\nConservation areas\n")
    glimpse(pca())
  })

  # Output sampling rates to table
  output$sampling_rates <- renderDT({
    trk_all() |> summarize_sampling_rate_many(cols='id') |>
      datatable() |>
      formatRound(columns=c('min','q1','median','mean','q3','max','sd'), digits=2)
  })

  #-------------------------------------------------
  # 3.3 Section: Home Ranges
  #-------------------------------------------------

  # PROCESS INPUTS
  # --------------

  # Convert gps table to sf object
  gps_sf <- reactive({
    gps_df() |>
      st_as_sf(coords = c("long", "lat")) |>
      st_set_crs(4326) |>
      mutate(year = year(time))
  })

  # Estimate home range
  hr1 <- reactive({
    if (input$hr=="MCP") {
      hr_mcp(trk_one(), levels=input$levels)
    } else if (input$hr=="KDE") {
      lvl <- input$levels
      if (lvl[2]==1) {lvl[2]=0.999}
      hr_kde(trk_one(), levels=lvl)
    } else if (input$hr=="aKDE") {
      lvl <- input$levels
      #if (lvl==1) {lvl=0.999}
      hr_akde(trk_one(), levels=lvl)
    } else if (input$hr=="LoCoH") {
      lvl <- input$levels
      #if (lvl==1) {lvl=0.999}
      hr_locoh(trk_one(), levels=lvl)
    } else if (input$hr=="OD") {
      lvl <- input$levels
      #if (lvl==1) {lvl=0.999}
      hr_od(trk_one(), levels=lvl)
    }      
  })

  # Output HR results
  output$hr_output <- renderPrint({
    req(input$caribou)
    #print(hr_isopleths(hr1()[1]))
    hr1_m2 <- as.numeric(hr_area(hr1())['area'][[1]][1])
    foot <- foot() |> st_transform(3578)
    hr <- hr_isopleths(hr1()) |>
      st_transform(3578)
    hrxfoot <- st_intersection(hr, foot)
    if (nrow(hrxfoot)>0) {
      hrxfoot_pct <- round(st_area(hrxfoot)/hr1_m2*100,2)
    } else {
      hrxfoot_pct <- 0
    }
    print(hrxfoot_pct)
    print(hrxfoot_pct[1])
    print(hrxfoot_pct[2])
  })

  # GENERATE OUTPUTS
  # ----------------
  
  # Leaflet map with locations, home ranges, and disturbances
  output$map1 <- renderLeaflet({
    if (input$goButton) {
      years <- unique(gps_sf()$year)
      caribou_pal <- colorFactor(topo.colors(25), gps_sf()$id)
      year_pal <- colorNumeric(palette=col_yrs6, domain=years)
      m <- leaflet(options = leafletOptions(attributionControl=FALSE)) |>
        addTiles(google, group="Google.Imagery") |>
        addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
        addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
        addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap")

              trk_one <- mutate(trk_one(), year=as.double(year))
        trk2020 <- trk_one |> filter(year==2020)
        trk2021 <- trk_one |> filter(year==2021)
        trk2022 <- trk_one |> filter(year==2022)
        trk2023 <- trk_one |> filter(year==2023)
        trk2024 <- trk_one |> filter(year==2024)
        trk2025 <- trk_one |> filter(year==2025)
        m <- m |> addPolygons(data=hr_isopleths(hr1()), color="blue", fill=F, weight=2, group="Home ranges")
        groups <- NULL
        if (nrow(trk2020)>=1) {
          m <- m |> addPolylines(data=trk2020, lng=~x_, lat=~y_, color=col_yrs6[1], weight=2, group="Track 2020")
          groups <- c(groups, "Track 2020")
        }
        if (nrow(trk2021)>=1) {
          m <- m |> addPolylines(data=trk2021, lng=~x_, lat=~y_, color=col_yrs6[2], weight=2, group="Track 2021")
          groups <- c(groups, "Track 2021")
        }
        if (nrow(trk2022)>=1) {
          m <- m |> addPolylines(data=trk2022, lng=~x_, lat=~y_, color=col_yrs6[3], weight=2, group="Track 2022")
           groups <- c(groups, "Track 2022")
        }
        if (nrow(trk2023)>=1) {
          m <- m |> addPolylines(data=trk2023, lng=~x_, lat=~y_, color=col_yrs6[4], weight=2, group="Track 2023")
          groups <- c(groups, "Track 2023")
        }
        if (nrow(trk2024)>=1) {
          m <- m |> addPolylines(data=trk2024, lng=~x_, lat=~y_, color=col_yrs6[5], weight=2, group="Track 2024")
          groups <- c(groups, "Track 2024")
        }
        if (nrow(trk2025)>=1) {
          m <- m |> addPolylines(data=trk2025, lng=~x_, lat=~y_, color=col_yrs6[6], weight=2, group="Track 2025")
          groups <- c(groups, "Track 2025")
        }
        m <- m |> 
          #addPolygons(data=herd4326, color="green", weight=1, fill=TRUE, fillOpacity=0.5, group="YG herd boundary") |>
          addCircles(data=trk_one, ~x_, ~y_, fill=T, stroke=T, weight=2, color=~year_pal(year), fillColor=~year_pal(year), fillOpacity=1, group="Locations", popup=trk_one()$t_) |>
          addPolygons(data=studyarea(), color="black", weight=2, fill=FALSE, group="Study area") |>
          addPolylines(data=line(), color="black", weight=2, group="Linear disturbance") |>
          addPolygons(data=poly(), color="black", weight=1, fill=TRUE, group="Areal disturbance") |>
          addPolygons(data=foot(), color="black", weight=1, fill=TRUE, fillOpacity=0.5, group="Footprint 500m") |>
          addPolygons(data=fire(), color="darkred", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires") |>
          addPolygons(data=pca(), color="darkblue", weight=1, fill=TRUE, fillOpacity=0.5, group="Conservation areas") |>
          #addPolygons(data=ipca4326, color="darkgreen", weight=1, fill=TRUE, fillOpacity=0.5, group="Proposed IPCAs") |>
          addLegend("topleft", colors=col_yrs6, labels=years, title="Year") |>
          addScaleBar(position="bottomright") |>
          addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Google.Imagery","Esri.WorldGrayCanvas"),
            overlayGroups = c("Locations",groups,"Study area","Areal disturbance","Linear disturbance","Footprint 500m","Fires","Conservation areas","Home ranges"),
            options = layersControlOptions(collapsed = FALSE)) |>
          hideGroup(c(groups,"Areal disturbance","Linear disturbance","Footprint 500m","Fires","Conservation areas"))
      m
    }
  })

  # Summary statistics based on mapped features
  output$tab1 <- renderTable({
    if (input$goButton) {
      #if (input$caribou=='All caribou') {
      #  x <- trk_all()
      #} else {
        x <- trk_one()
      #}
      # Calculate a few statistics
      pca <- pca() |> st_transform(3578) |>
        st_union()
      foot <- foot() |> st_transform(3578)
      bnd <- studyarea() |> st_transform(3578)
      bnd_m2 <- st_area(bnd)
      bnd_km2 <- units::set_units(bnd_m2, km^2)
      foot_pct <- round(st_area(foot)/bnd_m2*100,2)
      pca_m2 <- st_area(pca)
      pca_km2 <- units::set_units(pca_m2, km^2)

      # Estimator 1
      #hr_m2 <- as.numeric(hr_area(hr1())['area'])
      #hr_km2 <- round(hr_m2/1000000,2)
      hr1_m2 <- as.numeric(hr_area(hr1())['area'][[1]][1])
      hr1_km2 <- round(hr1_m2/1000000,2)
      hr2_m2 <- as.numeric(hr_area(hr1())['area'][[1]][2])
      hr2_km2 <- round(hr2_m2/1000000,2)
      hr <- hr_isopleths(hr1()) |>
        st_transform(3578)
      hrxfoot <- st_intersection(hr, foot)
      if (nrow(hrxfoot)>0) {
        hrxfoot_pct <- round(st_area(hrxfoot)/hr1_m2*100,2)
      } else {
        hrxfoot_pct <- 0
      }
      hrxpca <- st_intersection(hr, pca)
      if (nrow(hrxpca)>0) {
        hrxpca_pct <- round(st_area(hrxpca)/hr1_m2*100,2)
      } else {
        hrxpca_pct <- 0
      }
      tibble(
        Statistic=c("Locations", "Study area (km2)", "Study area disturbed (%)", "HR95 (km2)", 
                    "HR50 (km2)", "HR95 disturbed (%)", "HR50 disturbed (%)", "HR95 in PCAs (%)", "HR50 in PCAs (%)"), 
        Value=c(nrow(x), bnd_km2, foot_pct, hr1_km2, hr2_km2, hrxfoot_pct[1], hrxfoot_pct[2], hrxpca_pct[1], hrxpca_pct[2] ))
    }
  })

  output$downloadData <- downloadHandler(
    filename = function() { paste("HR_",input$caribou,"_",input$season,"_",input$hr,input$levels,"_",input$hr2,input$levels2," (", Sys.Date(), ").gpkg", sep="") },
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
