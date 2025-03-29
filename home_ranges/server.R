#-------------------------------------------------
# 3. Server functions
#-------------------------------------------------

server = function(input, output, session) {

  #-------------------------------------------------
  # 3.1 Section: Select data
  #-------------------------------------------------

  # UPDATE UI
  # ---------

  # Update choices for caribou individuals input based on input movement data
  observeEvent(input$gps_data, {
    file <- input$gps_data$datapath
    gpkg <- st_read(file)
    lyrs <- st_layers(file)$name
    ids <- as.character(sort(unique(gpkg$id)))
    updateSelectInput(session, "caribou", choices=c("All caribou", ids), selected=ids[1])
  })

  # Update choices for seasons/migration periods based on input segmentation data
  observeEvent(input$seg_data, {
    file <- input$seg_data$datapath
    csv <- st_read(file)
    seasons <- csv$season
    updateSelectInput(session, "season", choices=c("Annual",seasons), selected="Annual")
  })

  # READ INPUT FILES
  # ----------------

  # Read gps movement data
  gps_df <- eventReactive(input$gps_data, {
    req(input$getButton)
    file <- input$gps_data$datapath
    ext <- tools::file_ext(file)
    if(ext == "csv"){
      readr::read_csv(file) |>
        mutate(year=year(time),
          yday=yday(time))
    } else {
      showNotification("Wrong file type, must be CSV file (.csv)", type = "error")
    }
  })

  # Read seasons and migration periods data
  seg_df <- eventReactive(input$seg_data, {
    req(input$getButton)
    file <- input$seg_data$datapath
    ext <- tools::file_ext(file)
    if(ext == "csv"){
      readr::read_csv(file) 
    } else {
      showNotification("Wrong file type, must be CSV file (.csv)", type = "error")
    }
  })

  # Read linear disturbance data
  line <- eventReactive(input$dist_data, {
    req(input$getButton)
    file <- input$dist_data$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      st_read(file, "linear_disturbance", quiet=TRUE)
    } else {
      showNotification("Wrong file type, must be GPKG file (.gpkg)", type = "error")
    }
  })

  # Read areal disturbance data
  poly <- eventReactive(input$dist_data, {
    req(input$getButton)
    file <- input$dist_data$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      st_read(file, "areal_disturbance", quiet=TRUE)
    } else {
      showNotification("Wrong file type, must be GPKG file (.gpkg)", type = "error")
    }
  })

  # Read fire data
  fire <- eventReactive(input$dist_data, {
    req(input$getButton)
    file <- input$dist_data$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      st_read(file, "fires", quiet=TRUE)
    } else {
      showNotification("Wrong file type, must be GPKG file (.gpkg)", type = "error")
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

  # mcp home range
  mcp <- reactive({
    if (input$caribou=="All caribou") {
      hr_mcp(trk_all(), levels=input$levels) #|> st_transform(4326)
    } else {
      hr_mcp(trk_one(), levels=input$levels) #|> st_transform(4326)
    }
  })

  # kde home range
  kde <- reactive({
    lvl <- input$levels
    if (lvl==1) {lvl=0.999}
    if (input$caribou=="All caribou") {
      hr_kde(trk_all(), levels=lvl) #|> st_transform(4326)
    } else {
      hr_kde(trk_one(), levels=lvl) #|> st_transform(4326)
    }
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
        addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap")

      if (input$caribou=="All caribou") {
        trk_all <- mutate(trk_all(), year=as.double(year)) |> group_by(id, year) |> arrange(id, year)
        #trk2020 <- trk_all |> filter(year==2020)
        #trk2021 <- trk_all |> filter(year==2021)
        #trk2022 <- trk_all |> filter(year==2022)
        #trk2023 <- trk_all |> filter(year==2023)
        #trk2024 <- trk_all |> filter(year==2024)
        #trk2025 <- trk_all |> filter(year==2025)
        m <- m |> addPolygons(data=hr_isopleths(mcp()), color="black", fill=F, weight=2, group="HR MCP (black)")
        m <- m |> addPolygons(data=hr_isopleths(kde()), color="blue", fill=F, weight=2, group="HR KDE (blue)")
        #groups <- NULL
        #if (nrow(trk2020)>0) {
        #  m <- m |> addPolylines(data=trk2020, lng=~x_, lat=~y_, color=col_yrs6[1], weight=2, group="Track 2020")
        #  groups <- c(groups, "Track 2020")
        #}
        #if (nrow(trk2021)>0) {
        #  m <- m |>addPolylines(data=trk2021, lng=~x_, lat=~y_, color=col_yrs6[2], weight=2, group="Track 2021")
        #  groups <- c(groups, "Track 2021")
        #}
        #if (nrow(trk2022)>0) {
        #  m <- m |>addPolylines(data=trk2022, lng=~x_, lat=~y_, color=col_yrs6[3], weight=2, group="Track 2022")
        #   groups <- c(groups, "Track 2022")
        #}
        #if (nrow(trk2023)>0) {
        #  m <- m |> addPolylines(data=trk2023, lng=~x_, lat=~y_, color=col_yrs6[4], weight=2, group="Track 2023")
        #  groups <- c(groups, "Track 2023")
        #}
        #if (nrow(trk2024)>0) {
        #  m <- m |> addPolylines(data=trk2024, lng=~x_, lat=~y_, color=col_yrs6[5], weight=2, group="Track 2024")
        #  groups <- c(groups, "Track 2024")
        #}
        #if (nrow(trk2025)>0) {
        #  m <- m |> addPolylines(data=trk2025, lng=~x_, lat=~y_, color=col_yrs6[6], weight=2, group="Track 2025")
        #  groups <- c(groups, "Track 2025")
        #}
        m <- m |> 
          addCircles(data=trk_all, ~x_, ~y_, fill=T, stroke=T, weight=2, color=~year_pal(year), fillColor=~year_pal(year), fillOpacity=1, 
            group="Locations", popup=trk_all()$t_) |>
          addPolylines(data=line(), color="black", weight=3, group="Linear disturbance") |>
          addPolygons(data=poly(), color="black", weight=1, fill=TRUE, group="Areal disturbance") |>
          addPolygons(data=fire(), color="red", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires") |>
          addLegend("topleft", colors=col_yrs6, labels=years, title="Year") |>
          addScaleBar(position="bottomright") |>
          addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Google.Imagery"),
            overlayGroups = c("Locations","Areal disturbance","Linear disturbance","Fires","HR MCP (black)","HR KDE (blue)"),
            options = layersControlOptions(collapsed = FALSE)) |>
          hideGroup(c("Areal disturbance","Linear disturbance","Fires","HR MCP (black)","HR KDE (blue)"))

      } else {
        trk_one <- mutate(trk_one(), year=as.double(year))
        trk2020 <- trk_one |> filter(year==2020)
        trk2021 <- trk_one |> filter(year==2021)
        trk2022 <- trk_one |> filter(year==2022)
        trk2023 <- trk_one |> filter(year==2023)
        trk2024 <- trk_one |> filter(year==2024)
        trk2025 <- trk_one |> filter(year==2025)
        m <- m |> addPolygons(data=hr_isopleths(mcp()), color="black", fill=F, weight=2, group="HR MCP (black)")
        m <- m |> addPolygons(data=hr_isopleths(kde()), color="blue", fill=F, weight=2, group="HR KDE (blue)")
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
          addCircles(data=trk_one, ~x_, ~y_, fill=T, stroke=T, weight=2, color=~year_pal(year), fillColor=~year_pal(year), fillOpacity=1, 
            group="Locations", popup=trk_one()$t_) |>
          addPolylines(data=line(), color="black", weight=2, group="Linear disturbance") |>
          addPolygons(data=poly(), color="black", weight=1, fill=TRUE, group="Areal disturbance") |>
          addPolygons(data=fire(), color="red", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires") |>
          addLegend("topleft", colors=col_yrs6, labels=years, title="Year") |>
          addScaleBar(position="bottomright") |>
          addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Google.Imagery"),
            overlayGroups = c("Locations",groups,"Areal disturbance","Linear disturbance","Fires","HR MCP (black)","HR KDE (blue)"),
            options = layersControlOptions(collapsed = FALSE)) |>
          hideGroup(c(groups,"Areal disturbance","Linear disturbance","Fires","HR MCP (black)","HR KDE (blue)"))
      }
      m
    }
  })

  # Summary statistics based on mapped features
  output$tab1 <- renderTable({
    if (input$goButton) {
      if (input$caribou=='All caribou') {
        x <- trk_all()
      } else {
        x <- trk_one()
      }
      mcp_km2 <- as.integer(hr_area(mcp())['area']/1000000)
      kde_km2 <- as.integer(hr_area(kde())['area']/1000000)
      tibble(Statistic=c("Locations", "MCP home range (km2)", "MCP intact (%)", "MCP in IPCA (%)",
                                      "KDE home range (km2)", "KDE intact (%)", "KDE in IPCA (%)"), 
             Value=c(nrow(x), mcp_km2, 0, 0, kde_km2, 0, 0))
    }
  })

  # Summary statistics based on mapped features
  output$tab2 <- renderTable({
    if (input$goButton) {
      if (input$caribou=='All caribou') {
        x <- trk_all()
      } else {
        x <- trk_one()
      }
      mcp_km2 <- as.integer(hr_area(mcp())['area']/1000000)
      kde_km2 <- as.integer(hr_area(kde())['area']/1000000)
      tibble(Statistic=c("Locations", "MCP home range (km2)", "MCP intact (%)", "MCP in IPCA (%)",
                                      "KDE home range (km2)", "KDE intact (%)", "KDE in IPCA (%)"), 
             Value=c(nrow(x), mcp_km2, 0, 0, kde_km2, 0, 0))
    }
  })

}
