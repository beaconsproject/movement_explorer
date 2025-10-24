estimateRanges <- tabItem(tabName = "ranges",
  fluidRow(
    tabBox(width=3,
      tabPanel("Map 1 parameters",
        selectInput("id2a", "Select individual:", choices=NULL, multiple=FALSE),
        selectInput("season2a", "Select season:", choices=NULL),
        sliderInput("daterange2a", "Select year(s):", min=2020, max=2025, value=c(2020,2025), sep=""),
        selectInput("hr2a", "Estimator method:", choices=c("MCP", "KDE", "aKDE", "LoCoH"), selected="KDE"),
        sliderInput("levels2a", "Isopleth levels:", min=0.5, max=1, value=c(0.5, 0.95)),
        sliderInput("h2a", "KDE bandwidth (0 = estimated by app):", min=0, max=1, value=c(0), step=0.01)),
      tabPanel("Map 2 parameters",
        selectInput("id2b", "Select individual:", choices=NULL, multiple=FALSE),
        selectInput("season2b", "Select season:", choices=NULL),
        sliderInput("daterange2b", "Select year(s):", min=2020, max=2025, value=c(2020,2025), sep=""),
        selectInput("hr2b", "Estimator method:", choices=c("MCP", "KDE", "aKDE", "LoCoH"), selected="KDE"),
        sliderInput("levels2b", "Isopleth levels:", min=0.5, max=1, value=c(0.5, 0.95)),
        sliderInput("h2b", "KDE bandwidth (0 = estimated by app):", min=0, max=1, value=c(0), step=0.01))
    ),
    tabBox(width = 9,
      tabPanel("Seasonal and Home Ranges",
        p("Map 1"),
        div(style = "position: relative;",  # allows layering inside
        leafletOutput("map2a", height = 500) |> withSpinner(),
        #tags$img(src = "legend.png", style = "position: absolute; bottom: 15px; left: 15px; width: 150px; opacity: 0.9; z-index: 9999;")
        uiOutput("legendUIrange1")),
        br(),
        p("Map 2"),
        div(style = "position: relative;",  # allows layering inside
        leafletOutput("map2b", height = 500) |> withSpinner(),
        uiOutput("legendUIrange2")
        #tags$img(src = "legend.png", style = "position: absolute; bottom: 15px; left: 15px; width: 150px; opacity: 0.9; z-index: 9999;")
        )
      )
      tabPanel("User guide", uiOutput("estimateRanges_md")),
    )
  )
)


estimateRangesServer <- function(input, output, session, project, rv){

  output$estimateRanges_md <- renderUI({
    md_text <- get_markdown_content(estimateRanges_url)
    if(md_text=="# Error\nCould not load markdown file from GitHub.") {
      includeMarkdown("docs/estimateRanges.md")
    } else {
      tmp_file <- tempfile(fileext = ".md")
      writeLines(md_text, tmp_file)
      includeMarkdown(tmp_file)
    }
  })
  
  lock <- reactiveVal(FALSE)

  # Update choices for inputs based on movement data
  observeEvent(input$getButton, {
    season_val <- rv$season()
    
    x <- rv$gps_data()
    ids <- as.character(sort(unique(x$id)))
    #seasons <- unique(x$season); seasons <- seasons[!is.na(seasons)]
    updateSelectInput(session, "id2a", choices=c("Please select", "ALL", ids), selected=43141) # selected="Please select"
    updateSelectInput(session, "season2a", choices=c("ALL", season_val), selected=season_val[1])
    updateSliderInput(session, "daterange2a", min=min(x$year), max=max(x$year), value=c(min(x$year),max(x$year)))
    updateSelectInput(session, "id2b", choices=c("Please select", "ALL", ids), selected=43141) # selected="Please select"
    updateSelectInput(session, "season2b", choices=c("ALL",season_val), selected=season_val[2])
    updateSliderInput(session, "daterange2b", min=min(x$year), max=max(x$year), value=c(min(x$year),max(x$year)))
  })

  observeEvent(input$range1, {
    screenshot(id="map2a", scale=1, filename="range_plot1")
  })

  observeEvent(input$range2, {
    screenshot(id="map2b", scale=1, filename="range_plot2")
  })

  observeEvent(input$app_range, {
    screenshot()
  })

  savedRanges <<- list()
  
  trk_one2a <- reactive({
    req(trk_all())
    if(input$id2a != "Please select"){
      if (input$id2a=="ALL" & input$season2a=="ALL") {
        trk_all() |> filter(year>=input$daterange2a[1] & year<=input$daterange2a[2])
      } else if (input$id2a=="ALL" & !input$season2a=="ALL") {
        trk_all() |> filter(season==input$season2a & (year>=input$daterange2a[1] & year<=input$daterange2a[2]))
      } else if (!input$id2a=="ALL" & input$season2a=="ALL") {
        trk_all() |> filter(id==input$id2a & (year>=input$daterange2a[1] & year<=input$daterange2a[2]))
      } else {
        trk_all() |> filter(id==input$id2a & season==input$season2a & (year>=input$daterange2a[1] & year<=input$daterange2a[2]))
      }
    } else{
      return(NULL)
    }
  })

  trk_one2b <- reactive({
    req(trk_all())
    if(input$id2b != "Please select"){
      if (input$id2b=="ALL" & input$season2b=="ALL") {
        trk_all() |> filter(year>=input$daterange2b[1] & year<=input$daterange2b[2])
      } else if (input$id2b=="ALL" & !input$season2b=="ALL") {
        trk_all() |> filter(season==input$season2b & (year>=input$daterange2b[1] & year<=input$daterange2b[2]))
      } else if (!input$id2b=="ALL" & input$season2b=="ALL") {
        trk_all() |> filter(id==input$id2b & (year>=input$daterange2b[1] & year<=input$daterange2b[2]))
      } else {
        trk_all() |> filter(id==input$id2b & season==input$season2b & (year>=input$daterange2b[1] & year<=input$daterange2b[2]))
      }
    } else{
      return(NULL)
    }
  })

  # Create sf linestrings for mapping
  path2a <- reactive({
    req(trk_one2a())
    st_as_sf(trk_one2a(), coords = c("x_", "y_"), crs = 4326) |>
      group_by(id, year) |> 
      summarize(do_union=FALSE) |> 
      st_cast("LINESTRING")
  })

  path2b <- reactive({
    req(trk_one2b())
    st_as_sf(trk_one2b(), coords = c("x_", "y_"), crs = 4326) |>
      group_by(id, year) |> 
      summarize(do_union=FALSE) |> 
      st_cast("LINESTRING")
  })

  # Estimate home range
  hr2a <- reactive({
    req(trk_one2a())
    if (input$hr2a=="MCP") {
      x <- hr_mcp(trk_one2a(), levels=input$levels2a)
    } else if (input$hr2a=="KDE") {
      lvl <- input$levels2a
      if (lvl[2]==1) {lvl[2]=0.999}
      if (input$h2a > 0) {
        x <- hr_kde(trk_one2a(), levels=lvl, h=input$h2a)
      } else {
        x <- hr_kde(trk_one2a(), levels=lvl)
      }
    } else if (input$hr2a=="aKDE") {
      lvl <- input$levels2a
      #if (lvl==1) {lvl=0.999}
      x <- hr_akde(trk_one2a(), levels=lvl)
    } else if (input$hr2a=="LoCoH") {
      lvl <- input$levels2a
      #if (lvl==1) {lvl=0.999}
      x <- hr_locoh(trk_one2a(), levels=lvl)
    }
    hr_isopleths(x)
  })

  # Estimate home range
  hr2b <- reactive({
    req(trk_one2b())
    if (input$hr2b=="MCP") {
      x <- hr_mcp(trk_one2b(), levels=input$levels2b)
    } else if (input$hr2b=="KDE") {
      lvl <- input$levels2b
      if (lvl[2]==1) {lvl[2]=0.999}
      if (input$h2b > 0) {
        x <- hr_kde(trk_one2b(), levels=lvl, h=input$h2b)
      } else {
        x <- hr_kde(trk_one2b(), levels=lvl)
      }
    } else if (input$hr2b=="aKDE") {
      lvl <- input$levels2b
      #if (lvl==1) {lvl=0.999}
      x <- hr_akde(trk_one2b(), levels=lvl)
    } else if (input$hr2b=="LoCoH") {
      lvl <- input$levels2b
      #if (lvl==1) {lvl=0.999}
      x <- hr_locoh(trk_one2b(), levels=lvl)
    }
    hr_isopleths(x)
  })


  # Leaflet map with locations, home ranges, and disturbances
  output$map2a <- renderLeaflet({
    map2a <- leaflet(options = leafletOptions(attributionControl=FALSE)) |>
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
      addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap")
    
    layers <- rv$layers_4326()
  
    if(input$getButton){
      map2a <- map2a |>
        addPolygons(data=studyarea(), color="black", fill=F, weight=2, group="Study area") 
      
      if(isMappable(layers$linear_disturbance)){
        map2a <- map2a |> addPolylines(data=layers$linear_disturbance, color="#CC3333", weight=2, group="Linear disturbance")
      }
      if(isMappable(layers$areal_disturbance)){
        map2a <- map2a |> addPolygons(data=layers$areal_disturbance, color="#660000", weight=1, fill=TRUE, group="Areal disturbance")
      }
      if(isMappable(layers$footprint_500m)){
        map2a <- map2a |> addPolygons(data=layers$footprint_500m, color="#663399", weight=1, fill=TRUE, fillOpacity=0.5, group="Footprint 500m")
      }
      if(isMappable(layers$fires)){
        map2a <- map2a |> addPolygons(data=layers$fires, color="#996633", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires")
      }
      if(isMappable(layers$Intact_FL_2000)){
        map2a <- map2a |> addPolygons(data=layers$Intact_FL_2000, color="#3366FF", weight=1, fill=TRUE, fillOpacity=0.5, group="Intact FL 2000")
      }
      if(isMappable(layers$Intact_FL_2020)){
        map2a <- map2a |> addPolygons(data=layers$Intact_FL_2020, color="#000066", weight=1, fill=TRUE, fillOpacity=0.5, group="Intact FL 2020")
      }
      if(isMappable(layers$protected_areas)){
        map2a <- map2a |> addPolygons(data=layers$protected_areas, color="#699999", weight=1, fill=TRUE, fillOpacity=0.5, group="Protected areas") 
      }
      if(isMappable(layers$Placer_Claims)){
        map2a <- map2a |> addPolygons(data=layers$Placer_Claims, color='#666666', fill=T, weight=1, group="Placer Claims")
      }
      if(isMappable(layers$Quartz_Claims)){
        map2a <- map2a |> addPolygons(data=layers$Quartz_Claims, color='#CCCCCC', fill=T, weight=1, group="Quartz Claims")
      }  
      
      map2a <- map2a |>  addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
                         overlayGroups = c("Study area", rv$mappedLayer()),
                         options = layersControlOptions(collapsed = TRUE)) |>
        hideGroup(rv$mappedLayer())
    }
     map2a 
     
  })

  output$legendUIrange1 <- renderUI({
    req(rv)  # ensure rv exists
    if (!is.null(rv$mappedLayer())) {
      tags$img(
        src = "legend.png",
        style = "position: absolute; bottom: 15px; right: 15px; width: 200px; opacity: 0.9; z-index: 9999;"
      )
    } else {
      NULL
    }
  })
  
  observeEvent(input$runButton2, {
    req(trk_one2a(), rv$gps_data(), path2a(), hr2a())
    
    # isolate() ensures the map uses the current inputs at button press only
    gps <- isolate(trk_one2a())
    years <- isolate(unique(rv$gps_data()$year))
    path <- isolate(path2a())
    hr <- isolate(hr2a())
    
    cols <- isolate(col_yrs6[1:length(years)])
    year_pal <- isolate(colorNumeric(palette = col_yrs6[1:length(years)], domain = years))
    
    leafletProxy("map2a") |>
      clearGroup("Points")|>
      clearGroup("Tracks")|>
      clearGroup("Ranges")|>
      clearControls() |>
      addCircles(data=gps, ~x_, ~y_, fill=T, stroke=T, weight=1, color=~year_pal(year), 
          fillColor=~year_pal(year), fillOpacity=1, group="Points", popup=gps$t_) |>
      addPolylines(data=path2a(), color="black", weight=1, group=paste0("Tracks")) |>
      addPolygons(data=hr2a(), color="blue", fill=F, weight=2, group="Ranges") |>
      #addPolygons(data=hr2(), stroke=TRUE, color="red", opacity=1, weight=2, fillColor=hr2()$level, fillOpacity=0.5, group="Ranges") |>
      #addPolygons(data=hr2(), stroke=TRUE, color="red", opacity=1, weight=2, fillColor=pal(hr2()$level), fillOpacity=input$alpha, group="Ranges") |>
      addLegend("topleft", colors=cols, labels=years, title="Year") |>
      addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE)) |>
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
                       overlayGroups = c("Study area", "Points", "Tracks", "Ranges", rv$mappedLayer()),
                       options = layersControlOptions(collapsed = TRUE)) |>
      hideGroup(c("Tracks", rv$mappedLayer()))
  })

  output$map2b <- renderLeaflet({
    map2b <- leaflet(options = leafletOptions(attributionControl=FALSE)) |>
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
      addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap")
    
    layers <- rv$layers_4326()
  
    if(input$getButton){
      map2b <- map2b |>
        addPolygons(data=studyarea(), color="black", fill=F, weight=2, group="Study area") 
      
      if(isMappable(layers$linear_disturbance)){
        map2b <- map2b |> addPolylines(data=layers$linear_disturbance, color="#CC3333", weight=2, group="Linear disturbance")
      }
      if(isMappable(layers$areal_disturbance)){
        map2b <- map2b |> addPolygons(data=layers$areal_disturbance, color="#660000", weight=1, fill=TRUE, group="Areal disturbance")
      }
      if(isMappable(layers$footprint_500m)){
        map2b <- map2b |> addPolygons(data=layers$footprint_500m, color="#663399", weight=1, fill=TRUE, fillOpacity=0.5, group="Footprint 500m")
      }
      if(isMappable(layers$fires)){
        map2b <- map2b |> addPolygons(data=layers$fires, color="#996633", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires")
      }
      if(isMappable(layers$Intact_FL_2000)){
        map2b <- map2b |> addPolygons(data=layers$Intact_FL_2000, color="#3366FF", weight=1, fill=TRUE, fillOpacity=0.5, group="Intact FL 2000")
      }
      if(isMappable(layers$Intact_FL_2020)){
        map2b <- map2b |> addPolygons(data=layers$Intact_FL_2020, color="#000066", weight=1, fill=TRUE, fillOpacity=0.5, group="Intact FL 2020")
      }
      if(isMappable(layers$protected_areas)){
        map2b <- map2b |> addPolygons(data=layers$protected_areas, color="#699999", weight=1, fill=TRUE, fillOpacity=0.5, group="Protected areas") 
      }
      if(isMappable(layers$Placer_Claims)){
        map2b <- map2b |> addPolygons(data=layers$Placer_Claims, color='#666666', fill=T, weight=1, group="Placer Claims")
      }
      if(isMappable(layers$Quartz_Claims)){
        map2b <- map2b |> addPolygons(data=layers$Quartz_Claims, color='#CCCCCC', fill=T, weight=1, group="Quartz Claims")
      }  
        
      map2b <- map2b |> addLayersControl(position = "topright",
                           baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
                           overlayGroups = c("Study area", rv$mappedLayer()),
                           options = layersControlOptions(collapsed = TRUE)) |>
          hideGroup(rv$mappedLayer())
    }
    map2b
  })
  
  output$legendUIrange2 <- renderUI({
    req(rv)  # ensure rv exists
    if (!is.null(rv$mappedLayer())) {
      tags$img(
        src = "legend.png",
        style = "position: absolute; bottom: 15px; right: 15px; width: 200px; opacity: 0.9; z-index: 9999;"
      )
    } else {
      NULL
    }
  })
  
  observeEvent(input$runButton2, {
    req(trk_one2b(), rv$gps_data(), path2b(), hr2b())
    
    gps <- isolate(trk_one2b())
    years <- isolate(unique(rv$gps_data()$year))
    path <- isolate(path2b())
    hr <- isolate(hr2b())
      
    cols <- col_yrs6[1:length(years)]
    year_pal <- colorNumeric(palette=col_yrs6[1:length(years)], domain=years)
      
    leafletProxy("map2b") |>
      clearGroup("Points")|>
      clearGroup("Tracks")|>
      clearGroup("Ranges")|>
      clearControls() |>
      addCircles(data=gps, ~x_, ~y_, fill=T, stroke=T, weight=1, color=~year_pal(year), 
          fillColor=~year_pal(year), fillOpacity=1, group="Points", popup=gps$t_) |>
      addPolylines(data=path2b(), color="black", weight=1, group=paste0("Tracks")) |>
      addPolygons(data=hr2b(), color="blue", fill=F, weight=2, group="Ranges") |>
      #addPolygons(data=hr2(), stroke=TRUE, color="red", opacity=1, weight=2, fillColor=hr2()$level, fillOpacity=0.5, group="Ranges") |>
      #addPolygons(data=hr2(), stroke=TRUE, color="red", opacity=1, weight=2, fillColor=pal(hr2()$level), fillOpacity=input$alpha, group="Ranges") |>
      addLegend("topleft", colors=cols, labels=years, title="Year") |>
      addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))|>
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
                       overlayGroups = c("Study area", "Points", "Tracks", "Ranges", rv$mappedLayer()),
                       options = layersControlOptions(collapsed = TRUE)) |>
      hideGroup(c("Tracks", rv$mappedLayer()))
  })

  
  # Observe changes in map2a and update map2b
  observeEvent(input$map2a_bounds, {
    req(input$sync_maps2)
    bounds <- input$map2a_bounds
    leafletProxy("map2b") |> fitBounds(
      lng1 = bounds$west, lat1 = bounds$south,
      lng2 = bounds$east, lat2 = bounds$north
    )
  })
  
  # Optionally, do the reverse too
  observeEvent(input$map2b_bounds, {
    req(input$sync_maps2)
    bounds <- input$map2b_bounds
    leafletProxy("map2a") |> fitBounds(
      lng1 = bounds$west, lat1 = bounds$south,
      lng2 = bounds$east, lat2 = bounds$north
    )
  })

  observeEvent(input$map2a_bounds, {
    req(input$sync_maps2)
    req(!lock())
    lock(TRUE)
    bounds <- input$map2a_bounds
    leafletProxy("map2b") |> fitBounds(bounds$west, bounds$south, bounds$east, bounds$north)
    lock(FALSE)
  })
  
  
  # Save ranges
  observeEvent(input$saveRanges, {
    req(input$saveRanges)
    
    updateActionButton(session, "saveRanges", label = "Saved!")
    
    saved <- rv$savedRanges()
    
    hr2a <- hr2a() |> filter(level==0.95)
    name2a <- paste0("range_", input$id2a, "_", input$season2a, "_", input$daterange2a[1], "-", input$daterange2a[2])
    saved[[name2a]] <- hr2a

    hr2b <- hr2b() |> filter(level==0.95)
    name2b <- paste0("range_", input$id2b, "_", input$season2b, "_", input$daterange2b[1], "-", input$daterange2b[2])
    saved[[name2b]] <- hr2b
    
    rv$savedRanges(saved)
    
    later::later(function() {
      updateActionButton(session, "saveRanges", label = "Save segments")
    }, 2)
    
  })
  
}
