identifyCorridors <- tabItem(tabName = "corridors",
  fluidRow(
    tabBox(width=3,
      tabPanel("Map 1 parameters",
        selectInput("id3a", "Select individual:", choices=NULL, multiple=FALSE),
        selectInput("season3a", "Select migration period:", choices=NULL),
        sliderInput("daterange3a", "Select year(s):", min=2020, max=2025, value=c(2020,2025), sep=""),
        selectInput("hr3a", "Estimator method:", choices=c("Line buffer", "Brownian bridge", "Mixed approach")),
        sliderInput("buffer3a", "Buffer size (m):", min=0, max=750, value=c(250), step=50, sep=""),
        sliderInput("min3a", "Min individuals for corridor:", min=1, max=20, value=3, step=1, sep=""),
        sliderInput("crumbs3a", "Drop patches smaller than (km2):", min=1, max=1000, value=100, step=50, sep=""),
        sliderInput("holes3a", "fill holes small than (km2):", min=1, max=1000, value=200, step=50, sep="")),
      tabPanel("Map 2 parameters",
        selectInput("id3b", "Select individual:", choices=NULL, multiple=FALSE),
        selectInput("season3b", "Select migration period:", choices=NULL),
        sliderInput("daterange3b", "Select year(s):", min=2020, max=2025, value=c(2020,2025), sep=""),
        selectInput("hr3b", "Estimator method:", choices=c("Line buffer", "Brownian bridge", "Mixed approach")),
        sliderInput("buffer3b", "Buffer size (m):", min=0, max=750, value=c(250), step=50, sep=""),
        sliderInput("min3b", "Min individuals for corridor:", min=1, max=20, value=3, step=1, sep=""),
        sliderInput("crumbs3b", "Drop patches smaller than (km2):", min=1, max=1000, value=100, step=50, sep=""),
        sliderInput("holes3b", "Fill holes smaller than (km2):", min=1, max=1000, value=200, step=50, sep="")),
      ),
    tabBox(width=9,
      tabPanel("Seasonal Migration Corridors",
        p("Map 1"),
        div(style = "position: relative;",  # allows layering inside
        leafletOutput("map3a", height = 500) |> withSpinner(),
        tags$img(src = "legend.png", style = "position: absolute; bottom: 15px; left: 15px; width: 150px; opacity: 0.9; z-index: 9999;")),
        br(),
        p("Map 2"),
        div(style = "position: relative;",  # allows layering inside
        leafletOutput("map3b", height = 500) |> withSpinner(),
        tags$img(src = "legend.png", style = "position: absolute; bottom: 15px; left: 15px; width: 150px; opacity: 0.9; z-index: 9999;"))),
      #tabPanel("Test output",
      #  verbatimTextOutput("text_output"))
    )
  )
)

identifyCorridorsServer <- function(input, output, session, project, rv){

  lock <- reactiveVal(FALSE)

  #######################################
  ## observe on pop analysis sliders
  #######################################
  observe({
    if(input$id3a != "ALL"){
      disable("min3a")
      disable("crumbs3a")
      disable("holes3a")
    }else{
      updateSelectInput(session, "hr3a", choices=c("Line buffer"), selected="Line buffer")
      enable("min3a")
      enable("crumbs3a")
      enable("holes3a")
    }
  })
  
  ## observe on pop analysis
  observe({
    if(input$id3b != "ALL"){
      disable("min3b")
      disable("crumbs3b")
      disable("holes3b")
    }else{
      updateSelectInput(session, "hr3b", choices=c("Line buffer"), selected="Line buffer")
      enable("min3b")
      enable("crumbs3b")
      enable("holes3b")
    }
  })
  ###########################
  
  # Update choices for inputs based on movement data
  observeEvent(input$getButton, {
    x <- rv$gps_data()
    ids <- as.character(sort(unique(x$id)))
    #seasons <- unique(x$season); seasons <- seasons[!is.na(seasons)]
    migration_val <- rv$migration()
    
    updateSelectInput(session, "id3a", choices=c("Please select", "ALL",ids), selected=43141) # selected="Please select"
    updateSelectInput(session, "season3a", choices=c(migration_val), selected=migration_val[1])
    updateSliderInput(session, "daterange3a", min=min(x$year), max=max(x$year), value=c(min(x$year),max(x$year)))
    updateSelectInput(session, "id3b", choices=c("Please select", "ALL",ids), selected=43141) # selected="Please select"
    updateSelectInput(session, "season3b", choices=c(migration_val), selected=migration_val[2])
    updateSliderInput(session, "daterange3b", min=min(x$year), max=max(x$year), value=c(min(x$year),max(x$year)))
  })

  observeEvent(input$path1, {
    screenshot(id="map3a", scale=1, filename="corridor_plot1")
  })

  observeEvent(input$path2, {
    screenshot(id="map3b", scale=1, filename="corridor_plot2")
  })

  observeEvent(input$app_corridor, {
    screenshot()
  })
  
  savedRanges <<-list()
    
  # Select tracks for one individual
  trk_one3a <- reactive({
    req(trk_all())
    if(input$id3a != "Please select"){
      if (input$id3a=="ALL" & input$season3a=="ALL") {
        trk_all() |> filter(year>=input$daterange3a[1] & year<=input$daterange3a[2])
      } else if (input$id3a=="ALL" & !input$season3a=="ALL") {
        trk_all() |> filter(migration==input$season3a & (year>=input$daterange3a[1] & year<=input$daterange3a[2]))
      } else if (!input$id3a=="ALL" & input$season3a=="ALL") {
        trk_all() |> filter(id==input$id3a & (year>=input$daterange3a[1] & year<=input$daterange3a[2]))
      } else {
        trk_all() |> filter(id==input$id3a & migration==input$season3a & (year>=input$daterange3a[1] & year<=input$daterange3a[2]))
      }
    }
  })

  #output$text_output <- renderPrint({
  #  trk_one3a()
  #})

  trk_one3b <- reactive({
    req(trk_all())
    if(input$id3b != "Please select"){
      if (input$id3b=="ALL" & input$season3b=="ALL") {
        trk_all() |> filter(year>=input$daterange3b[1] & year<=input$daterange3b[2])
      } else if (input$id3b=="ALL" & !input$season3b=="ALL") {
        trk_all() |> filter(migration==input$season3b & (year>=input$daterange3b[1] & year<=input$daterange3b[2]))
      } else if (!input$id3b=="ALL" & input$season3b=="ALL") {
        trk_all() |> filter(id==input$id3b & (year>=input$daterange3b[1] & year<=input$daterange3b[2]))
      } else {
        trk_all() |> filter(id==input$id3b & migration==input$season3b & (year>=input$daterange3b[1] & year<=input$daterange3b[2]))
      }
    }
  })

  path3a <- reactive({
    st_as_sf(trk_one3a(), coords = c("x_", "y_"), crs = 4326) |>
      group_by(id, year) |> 
      summarize(do_union=FALSE) |> 
      st_cast("LINESTRING")
  })

  path3b <- reactive({
    st_as_sf(trk_one3b(), coords = c("x_", "y_"), crs = 4326) |>
      group_by(id, year) |> 
      summarize(do_union=FALSE) |> 
      st_cast("LINESTRING")
  })

  # Rasterized range
  rhr3a <- reactive({
    hr <- hr_kde(trk_one3a(), levels=0.999) |> 
      hr_isopleths() |>
      mutate(one=1) |>
      dplyr::select(one) |>
      st_transform(3578)
    rhr <- st_rasterize(hr, dx=100, dy=100) |> 
      rast()
  })

  # Rasterized range
  rhr3b <- reactive({
    hr <- hr_kde(trk_one3b(), levels=0.999) |> 
      hr_isopleths() |>
      mutate(one=1) |>
      dplyr::select(one) |>
      st_transform(3578)
    rhr <- st_rasterize(hr, dx=100, dy=100) |> 
      rast()
  })

  # Line buffer method
  path3buffa <- reactive({

    # Population-level
    if (input$id3a=="ALL") {

      # Buffer tracks by some amount
      buf <- st_as_sf(trk_one3a(), coords = c("x_", "y_"), crs = 4326) |>
        st_transform(3578) |>
        group_by(id, year) |> 
        summarize(do_union=FALSE) |> 
        st_cast("LINESTRING") |>
        st_buffer(input$buffer3a)    

      # Create cumulative raster (value = 1 to 25 caribou)
      for (i in unique(buf$id)) {
        b1 <- buf |> filter(id==i)|>
          summarize(id=mean(id)) # this dissolves boundaries
        rb1 <- rasterize(b1, rhr3a(), background=0)
        if (i==unique(buf$id)[1]) {
          rall <- rb1
        } else {
          rall <- rall + rb1
        }
      }

      # Old method
      #r <- subst(rall, 0, NA)
      #rcl <- r > input$min3a
      #rcl <- subst(rcl, 0, NA)
      #rp <- patches(rcl)
      #v <- as.polygons(rp)
      #vp <- v[expanse(v, unit="m") > input$patch3a * 1000000, ] # drop small polygons
      #vp <- st_as_sf(vp) |>
      #  st_transform(4326)

      # Reclass cumulative raster using user-defined threshold (e.g., 2 means >2)
      r <- classify(rall, rcl = c(-Inf, input$min3a, Inf)) |> as.numeric()

      # Convert to polygons
      v <- ifel(r > 0, r, NA) |> 
        as.polygons() |> 
        st_as_sf()

      # Drop small polygons based on user-defined threshold (e.g., 101 km2)
      #v <- drop_crumbs(v, units::set_units(input$crumbs3a, km^2))
      v <- drop_crumbs(v, input$crumbs3a * 1000000)

      # Fill holes based on user-defined threshold (e.g., 201 km2)
      #v <- fill_holes(v, units::set_units(input$holes3a, km^2))
      v <- fill_holes(v, input$holes3a * 1000000)
      
      # Smooth boundary
      vp <- smooth(v, method = "ksmooth", smoothness=1) |> 
        st_transform(4326)

    # Individual-level
    } else {
      vp <- st_as_sf(trk_one3a(), coords = c("x_", "y_"), crs = 4326) |>
        st_transform(3578) |>
        group_by(id, year) |> 
        summarize(do_union=FALSE) |> 
        st_cast("LINESTRING") |>
        st_buffer(input$buffer3a) |>
        st_union() |> 
        st_sf() |>
        st_transform(4326)
    }
    vp
  })

  # Line buffer method
  path3buffb <- reactive({

    # Population-level
    if (input$id3b=="ALL") {

       # Buffer tracks by some amount
       buf <- st_as_sf(trk_one3b(), coords = c("x_", "y_"), crs = 4326) |>
        st_transform(3578) |>
        group_by(id, year) |> 
        summarize(do_union=FALSE) |> 
        st_cast("LINESTRING") |>
        st_buffer(input$buffer3b)    

      # Create cumulative raster (value = 1 to 25 caribou)
      for (i in unique(buf$id)) {
        b1 <- buf |> filter(id==i)|>
          summarize(id=mean(id))
        rb1 <- rasterize(b1, rhr3b(), background=0)
        if (i==unique(buf$id)[1]) {
          rall <- rb1
        } else {
          rall <- rall + rb1
        }
      }

      #r <- subst(rall, 0, NA)
      #rcl <- r > input$min3b
      #rcl <- subst(rcl, 0, NA)
      #rp <- patches(rcl)
      #v <- as.polygons(rp)
      #vp <- v[expanse(v, unit="m") > input$patch3b * 1000000, ] # drop small polygons
      #vp <- st_as_sf(vp) |>
      #  st_transform(4326)

      # Reclass cumulative raster using user-defined threshold (e.g., 2 means >2)
      r <- classify(rall, rcl = c(-Inf, input$min3b, Inf)) |> as.numeric()

      # Convert to polygons
      v <- ifel(r > 0, r, NA) |> 
        as.polygons() |> 
        st_as_sf()

      # Drop small polygons based on user-defined threshold (e.g., 101 km2)
      #v <- drop_crumbs(v, units::set_units(input$crumbs3b, km^2))
      v <- drop_crumbs(v, input$crumbs3b * 1000000)

      # Fill holes based on user-defined threshold (e.g., 201 km2)
      #v <- fill_holes(v, units::set_units(input$holes3b, km^2))
      v <- fill_holes(v, input$holes3b * 1000000)

      # Smooth boundary
      vp <- smooth(v, method = "ksmooth", smoothness=1) |> 
        st_transform(4326)

    # Individual-level
    } else {
      vp <- st_as_sf(trk_one3b(), coords = c("x_", "y_"), crs = 4326) |>
        st_transform(3578) |>
        group_by(id, year) |> 
        summarize(do_union=FALSE) |> 
        st_cast("LINESTRING") |>
        st_buffer(input$buffer3b) |>
        st_union() |> 
        st_sf() |>
        st_transform(4326)
    }
    vp
  })

  # Brownian bridge method
  od3a <- reactive({
    if (input$id3a=="ALL") {
      m <- 1
      for (i in sort(unique(trk_one3a()$id))) {
        n <- 1
        for (j in unique(trk_one3a()$year)) {
          cat(i, "in", j, "...\n"); flush.console()
          x <- trk_one3a() |> filter(id==i & year==j)
          if (nrow(x) > 1) {
            od <- od(x, model = fit_ctmm(x, "bm"), trast=rhr3a())
            names(od) <- paste0(i,"_",j)
            if (n==1) {
              od_all <- od
            } else {
              od_all <- c(od_all, od)
            }
            n <- n + 1
          }
        }
        od_mean <- mean(od_all)
        names(od_mean) <- i
        iso95 <- hr_isopleths(od_mean, levels=0.95) |> mutate(id=i)
        if (m==1) {
          od_mean_all <- od_mean
          iso_all_95 <- iso95
        } else {
          od_mean_all <- c(od_mean_all, od_mean)
          iso_all_95 <- rbind(iso_all_95, iso95)
        }
        m <- m + 1
      }
      for (i in iso_all_95$id) {
        r <- iso_all_95 |> 
          filter(id==i) |>
          select(id) |>
          rasterize(rhr3a()) |>
          subst(NA, 0)
        if (i==iso_all_95$id[1]) {
          rsum <- r
        } else {
          rsum <- rsum + r
        }
      }
      rsum_class <- classify(rsum, rcl = c(-Inf, 2, Inf)) |> # e.g., 2 means >2
        as.numeric() |>
        subst(0, NA)
      iso <- hr_isopleths(od, levels=0.99) |> st_transform(4326)
    } else {
      od <- od(trk_one3a(), model = fit_ctmm(trk_one3a(), "bm"), trast = make_trast(trk_one3a()))
      iso <- hr_isopleths(od, levels=0.95) |> st_transform(4326)
    }
  })

  # Brownian bridge method
  od3b <- reactive({
    if (input$id3b=="ALL") {
      m <- 1
      for (i in sort(unique(trk_one3b()$id))) {
        n <- 1
        for (j in unique(trk_one3b()$year)) {
          cat(i, "in", j, "...\n"); flush.console()
          x <- trk_one3b() |> filter(id==i & year==j)
          if (nrow(x) > 1) {
            od <- od(x, model = fit_ctmm(x, "bm"), trast=rhr3b())
            names(od) <- paste0(i,"_",j)
            if (n==1) {
              od_all <- od
            } else {
              od_all <- c(od_all, od)
            }
            n <- n + 1
          }
        }
        od_mean <- mean(od_all)
        names(od_mean) <- i
        iso95 <- hr_isopleths(od_mean, levels=0.95) |> mutate(id=i)
        if (m==1) {
          od_mean_all <- od_mean
          iso_all_95 <- iso95
        } else {
          od_mean_all <- c(od_mean_all, od_mean)
          iso_all_95 <- rbind(iso_all_95, iso95)
        }
        m <- m + 1
      }
      for (i in iso_all_95$id) {
        r <- iso_all_95 |> 
          filter(id==i) |>
          select(id) |>
          rasterize(rhr3b()) |>
          subst(NA, 0)
        if (i==iso_all_95$id[1]) {
          rsum <- r
        } else {
          rsum <- rsum + r
        }
      }
      rsum_class <- classify(rsum, rcl = c(-Inf, 2, Inf)) |> # e.g., 2 means >2
        as.numeric() |>
        subst(0, NA)
      iso <- hr_isopleths(od, levels=0.99) |> st_transform(4326)
    } else {
      od <- od(trk_one3b(), model = fit_ctmm(trk_one3b(), "bm"), trast = make_trast(trk_one3b()))
      iso <- hr_isopleths(od, levels=0.95) |> st_transform(4326)
    }
  })

  corridor3a <- reactive({
    if (input$hr3a=="Line buffer") {
      path3buffa()
    } else if (input$hr3a=="Brownian bridge") {
      od3a()
    } else if (input$hr3a=="Mixed approach") {
      st_union(od3a(), path3buffa()) |> st_sf()
    }
  })

  corridor3b <- reactive({
    if (input$hr3b=="Line buffer") {
      path3buffb()
    } else if (input$hr3b=="Brownian bridge") {
      od3b()
    } else if (input$hr3b=="Mixed approach") {
      st_union(od3b(), path3buffb()) |> st_sf()
    }
  })

  output$map3a <- renderLeaflet({
    map3a <- leaflet(options = leafletOptions(attributionControl=FALSE)) |>
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
      addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap")
  
    layers <- rv$layers_4326()
    
    if (input$getButton) {
      map3a <- map3a |>
        addPolygons(data=studyarea(), color="black", fill=F, weight=2, group="Study area") 
      
      if(isMappable(layers$linear_disturbance)){
        map3a <- map3a |> addPolylines(data=layers$linear_disturbance, color="#CC3333", weight=2, group="Linear disturbance")
      }
      if(isMappable(layers$areal_disturbance)){
        map3a <- map3a |> addPolygons(data=layers$areal_disturbance, color="#660000", weight=1, fill=TRUE, group="Areal disturbance")
      }
      if(isMappable(layers$footprint_500m)){
        map3a <- map3a |> addPolygons(data=layers$footprint_500m, color="#663399", weight=1, fill=TRUE, fillOpacity=0.5, group="Footprint 500m")
      }
      if(isMappable(layers$fires)){
        map3a <- map3a |> addPolygons(data=layers$fires, color="#996633", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires")
      }
      if(isMappable(layers$Intact_FL_2000)){
        map3a <- map3a |> addPolygons(data=layers$Intact_FL_2000, color="#3366FF", weight=1, fill=TRUE, fillOpacity=0.5, group="Intact FL 2000")
      }
      if(isMappable(layers$Intact_FL_2020)){
        map3a <- map3a |> addPolygons(data=layers$Intact_FL_2020, color="#000066", weight=1, fill=TRUE, fillOpacity=0.5, group="Intact FL 2020")
      }
      if(isMappable(layers$protected_areas)){
        map3a <- map3a |> addPolygons(data=layers$protected_areas, color="#699999", weight=1, fill=TRUE, fillOpacity=0.5, group="Protected areas") 
      }
      if(isMappable(layers$Placer_Claims)){
        map3a <- map3a |> addPolygons(data=layers$Placer_Claims, color='#666666', fill=T, weight=1, group="Placer Claims")
      }
      if(isMappable(layers$Quartz_Claims)){
        map3a <- map3a |> addPolygons(data=layers$Quartz_Claims, color='#CCCCCC', fill=T, weight=1, group="Quartz Claims")
      }  
        
      map3a <- map3a |> addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
                         overlayGroups = c("Study area", rv$mappedLayer()),
                         options = layersControlOptions(collapsed = TRUE)) |>
        hideGroup(rv$mappedLayer())
      }
      map3a
  })
  
  observeEvent(input$runButton3, {
    req(trk_one3a(), rv$gps_data(), corridor3a(), path3a())
    
    gps <- isolate(trk_one3a())
    years <- isolate(unique(rv$gps_data()$year))
    corridor3a <- isolate(corridor3a())
    path3a <- isolate(path3a())
    
    cols <- col_yrs6[1:length(years)]
    year_pal <- colorNumeric(palette=col_yrs6[1:length(years)], domain=years)
    
    leafletProxy("map3a") |>
      clearGroup("Points")|>
      clearGroup("Tracks")|>
      clearGroup("Corridors")|>
      clearControls() |>
      addCircles(data=gps, ~x_, ~y_, fill=T, stroke=T, weight=1, color=~year_pal(year), 
                 fillColor=~year_pal(year), fillOpacity=1, group="Points", popup=gps$t_) |>
      addPolylines(data=path3a(), color="blue", weight=2, group=paste0("Tracks")) |>
      addPolygons(data=corridor3a(), color="red", fill=T, weight=2, fillOpacity=0.5, group="Corridors") |>
      addLegend("topleft", colors=cols, labels=years, title="Year") |>
      addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE)) |>
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
                       overlayGroups = c("Study area", "Points", "Tracks", "Corridors", rv$mappedLayer()),
                       options = layersControlOptions(collapsed = TRUE)) |>
      hideGroup(c("Points", "Tracks", rv$mappedLayer()))
    })
  
  output$map3b <- renderLeaflet({
    map3b <- leaflet(options = leafletOptions(attributionControl=FALSE)) |>
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
      addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap")
    
    layers <- rv$layers_4326()
    
    if (input$getButton) {
      map3b <- map3b |>
        addPolygons(data=studyarea(), color="black", fill=F, weight=2, group="Study area") 
     
      if(isMappable(layers$linear_disturbance)){
        map3b <- map3b |> addPolylines(data=layers$linear_disturbance, color="#CC3333", weight=2, group="Linear disturbance")
      }
      if(isMappable(layers$areal_disturbance)){
        map3b <- map3b |> addPolygons(data=layers$areal_disturbance, color="#660000", weight=1, fill=TRUE, group="Areal disturbance")
      }
      if(isMappable(layers$footprint_500m)){
        map3b <- map3b |> addPolygons(data=layers$footprint_500m, color="#663399", weight=1, fill=TRUE, fillOpacity=0.5, group="Footprint 500m")
      }
      if(isMappable(layers$fires)){
        map3b <- map3b |> addPolygons(data=layers$fires, color="#996633", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires")
      }
      if(isMappable(layers$Intact_FL_2000)){
        map3b <- map3b |> addPolygons(data=layers$Intact_FL_2000, color="#3366FF", weight=1, fill=TRUE, fillOpacity=0.5, group="Intact FL 2000")
      }
      if(isMappable(layers$Intact_FL_2020)){
        map3b <- map3b |> addPolygons(data=layers$Intact_FL_2020, color="#000066", weight=1, fill=TRUE, fillOpacity=0.5, group="Intact FL 2020")
      }
      if(isMappable(layers$protected_areas)){
        map3b <- map3b |> addPolygons(data=layers$protected_areas, color="#699999", weight=1, fill=TRUE, fillOpacity=0.5, group="Protected areas") 
      }
      if(isMappable(layers$Placer_Claims)){
        map3b <- map3b |> addPolygons(data=layers$Placer_Claims, color='#666666', fill=T, weight=1, group="Placer Claims")
      }
      if(isMappable(layers$Quartz_Claims)){
        map3b <- map3b |> addPolygons(data=layers$Quartz_Claims, color='#CCCCCC', fill=T, weight=1, group="Quartz Claims")
      }  
      
      map3b <- map3b |> addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
                         overlayGroups = c("Study area", rv$mappedLayer()),
                         options = layersControlOptions(collapsed = TRUE)) |>
        hideGroup(rv$mappedLayer())
      }
    map3b
  })
  
  observeEvent(input$runButton3, {
    req(trk_one3b(), rv$gps_data(), corridor3b(), path3b())

    gps <- isolate(trk_one3b())
    years <- isolate(unique(rv$gps_data()$year))
    corridor3a <- isolate(corridor3b())
    path3b <- isolate(path3b())
    
    cols <- col_yrs6[1:length(years)]
    year_pal <- colorNumeric(palette=col_yrs6[1:length(years)], domain=years)

    leafletProxy("map3b") |>
      clearGroup("Points")|>
      clearGroup("Tracks")|>
      clearGroup("Corridors")|>
      clearControls() |>    
      addCircles(data=gps, ~x_, ~y_, fill=T, stroke=T, weight=1, color=~year_pal(year), 
                 fillColor=~year_pal(year), fillOpacity=1, group="Points", popup=gps$t_) |>
      addPolylines(data=path3b(), color="blue", weight=2, group=paste0("Tracks")) |>
      addPolygons(data=corridor3b(), color="red", fill=T, weight=2, fillOpacity=0.5, group="Corridors") |>
      addLegend("topleft", colors=cols, labels=years, title="Year") |>
      addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE)) |>
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
                       overlayGroups = c("Study area", "Points", "Tracks", "Corridors", rv$mappedLayer()),
                       options = layersControlOptions(collapsed = TRUE)) |>
      hideGroup(c("Points", "Tracks", rv$mappedLayer()))
  })

  # Observe changes in map3a and update map3b
  observeEvent(input$map3a_bounds, {
    req(input$sync_maps3)
    bounds <- input$map3a_bounds
    leafletProxy("map3b") |> fitBounds(
      lng1 = bounds$west, lat1 = bounds$south,
      lng2 = bounds$east, lat2 = bounds$north
    )
  })
  
  # Optionally, do the reverse too
  observeEvent(input$map3b_bounds, {
    req(input$sync_maps3)
    bounds <- input$map3b_bounds
    leafletProxy("map3a") |> fitBounds(
      lng1 = bounds$west, lat1 = bounds$south,
      lng2 = bounds$east, lat2 = bounds$north
    )
  })
  
  observeEvent(input$map3a_bounds, {
    req(input$sync_maps3)
    req(!lock())
    lock(TRUE)
    bounds <- input$map3a_bounds
    leafletProxy("map3b") |> fitBounds(bounds$west, bounds$south, bounds$east, bounds$north)
    lock(FALSE)
  })
  
  # Save ranges
  observeEvent(input$savePaths, {
    req(input$savePaths)
    updateActionButton(session, "savePaths", label = "Saved!")
    saved <- rv$savedPaths()
    
    corridor3a <- corridor3a()
    name3a <- paste0("path_", input$id3a, "_", input$season3a, "_", input$daterange3a[1], "-", input$daterange3a[2])
    saved[[name3a]] <- corridor3a
    
    corridor3b <- corridor3b() 
    name3b <- paste0("path_", input$id3b, "_", input$season3b, "_", input$daterange3b[1], "-", input$daterange3b[2])
    saved[[name3b]] <- corridor3b
    
    rv$savedPaths(saved)
    
    later::later(function() {
      updateActionButton(session, "savePaths", label = "Save segments")
    }, 2)
  })
}