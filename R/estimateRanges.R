estimateRanges <- tabItem(tabName = "ranges",
  fluidRow(
     box(width=3,
        selectInput("id2a", "Select individual:", choices=NULL, multiple=FALSE),
        selectInput("season2a", "Select season:", choices=NULL),
        sliderInput("daterange2a", "Select year(s):", min=2020, max=2025, value=c(2020,2025), sep=""),
        selectInput("hr2a", "Estimator method:", choices=c("MCP", "KDE", "aKDE", "LoCoH"), selected="KDE"),
        sliderInput("levels2a", "Isopleth levels:", min=0.5, max=1, value=c(0.5, 0.95)),
        sliderInput("h2a", "KDE bandwidth (0 = estimated by app):", min=0, max=1, value=c(0), step=0.01)
      ),
      box(width=9,
          leafletOutput("map2a", height=550) |> withSpinner()
      ),
      box(width=3,
        selectInput("id2b", "Select individual:", choices=NULL, multiple=FALSE),
        selectInput("season2b", "Select season:", choices=NULL),
        sliderInput("daterange2b", "Select year(s):", min=2020, max=2025, value=c(2020,2025), sep=""),
        selectInput("hr2b", "Estimator method:", choices=c("MCP", "KDE", "aKDE", "LoCoH"), selected="KDE"),
        sliderInput("levels2b", "Isopleth levels:", min=0.5, max=1, value=c(0.5, 0.95)),
        sliderInput("h2b", "KDE bandwidth (0 = estimated by app):", min=0, max=1, value=c(0), step=0.01)
      ),
      box(width=9,
            leafletOutput("map2b", height=550) |> withSpinner()
      )
  )
)


estimateRangesServer <- function(input, output, session, project, rv){
  
  lock <- reactiveVal(FALSE)
  
  # Update choices for inputs based on movement data
  observeEvent(c(input$selectInput, input$csv1), {
    x <- gps_csv()
    ids <- as.character(sort(unique(x$id)))
    seasons <- unique(x$season); seasons <- seasons[!is.na(seasons)]
    updateSelectInput(session, "id2a", choices=c("Please select", "ALL", ids), selected="Please select")
    updateSelectInput(session, "season2a", choices=c("ALL","Summer range","Winter range"), selected="Summer range")
    updateSliderInput(session, "daterange2a", min=min(x$year), max=max(x$year), value=c(min(x$year),max(x$year)))
    updateSelectInput(session, "id2b", choices=c("Please select", "ALL", ids), selected="Please select")
    updateSelectInput(session, "season2b", choices=c("ALL","Summer range","Winter range"), selected="Winter range")
    updateSliderInput(session, "daterange2b", min=min(x$year), max=max(x$year), value=c(min(x$year),max(x$year)))
  })

  observeEvent(input$range1, {
    screenshot(id="map2a", scale=1, filename="range_plot1")
  })

  observeEvent(input$range2, {
    screenshot(id="map2b", scale=1, filename="range_plot2")
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
      st_transform(3578) |>
      group_by(id, year) |> 
      summarize(do_union=FALSE) |> 
      st_cast("LINESTRING") |>
      st_transform(4326)
  })

  path2b <- reactive({
    req(trk_one2b())
    st_as_sf(trk_one2b(), coords = c("x_", "y_"), crs = 4326) |>
      st_transform(3578) |>
      group_by(id, year) |> 
      summarize(do_union=FALSE) |> 
      st_cast("LINESTRING") |>
      st_transform(4326)
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
        addPolygons(data=studyarea(), color="black", fill=F, weight=2, group="Study area") |>
        addPolylines(data=layers$linear_disturbance, color="black", weight=2, group="Linear disturbance") |>
        addPolygons(data=layers$areal_disturbance, color="black", weight=1, fill=TRUE, group="Areal disturbance") |>
        addPolygons(data=layers$footprint_500m, color="black", weight=1, fill=TRUE, fillOpacity=0.5, group="Footprint 500m") |>
        addPolygons(data=layers$fires, color="darkred", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires") |>
        addPolygons(data=layers$ifl_2000, color="darkgreen", weight=1, fill=TRUE, fillOpacity=0.5, group="Intact FL 2000") |>
        addPolygons(data=layers$ifl_2020, color="darkgreen", weight=1, fill=TRUE, fillOpacity=0.5, group="Intact FL 2020") |>
        addPolygons(data=layers$protected_areas, color="green", weight=1, fill=TRUE, fillOpacity=0.5, group="Protected areas") |>
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
                         overlayGroups = c("Study area", "Linear disturbance", "Areal disturbance", "Fires",
                                           "Footprint 500m", "Intact FL 2000", "Intact FL 2020", "Protected areas"),
                         options = layersControlOptions(collapsed = FALSE)) |>
        hideGroup(c("Linear disturbance", "Areal disturbance", "Fires",
                    "Footprint 500m", "Intact FL 2000", "Intact FL 2020", "Protected areas"))
    }
     map2a 
  })

  observeEvent(input$runButton2, {
    req(trk_one2a(), gps_csv(), path2a(), hr2a())
    
    # isolate() ensures the map uses the current inputs at button press only
    gps <- isolate(trk_one2a())
    years <- isolate(unique(gps_csv()$year))
    path <- isolate(path2a())
    hr <- isolate(hr2a())
    
    cols <- isolate(col_yrs6[1:length(years)])
    year_pal <- isolate(colorNumeric(palette = col_yrs6[1:length(years)], domain = years))
    
    leafletProxy("map2a") |>
      clearGroup("Points")|>
      clearGroup("Tracks")|>
      clearGroup("Ranges")|>
      clearControls() |>
      addCircles(data=gps, ~x_, ~y_, fill=T, stroke=T, weight=gps$first_obs, color=~year_pal(year), 
          fillColor=~year_pal(year), fillOpacity=1, group="Points", popup=gps$t_) |>
      addPolylines(data=path2a(), color="black", weight=1, group=paste0("Tracks")) |>
      addPolygons(data=hr2a(), color="blue", fill=F, weight=2, group="Ranges") |>
      #addPolygons(data=hr2(), stroke=TRUE, color="red", opacity=1, weight=2, fillColor=hr2()$level, fillOpacity=0.5, group="Ranges") |>
      #addPolygons(data=hr2(), stroke=TRUE, color="red", opacity=1, weight=2, fillColor=pal(hr2()$level), fillOpacity=input$alpha, group="Ranges") |>
      addLegend("topleft", colors=cols, labels=years, title="Year") |>
      addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE)) |>
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
                       overlayGroups = c("Study area", "Points", "Tracks", "Ranges", "Linear disturbance", "Areal disturbance", "Fires",
                                         "Footprint 500m", "Intact FL 2000", "Intact FL 2020", "Protected areas"),
                       options = layersControlOptions(collapsed = FALSE)) |>
      hideGroup(c("Tracks",  "Linear disturbance", "Areal disturbance", "Fires",
                  "Footprint 500m", "Intact FL 2000", "Intact FL 2020", "Protected areas"))
  })

  output$map2b <- renderLeaflet({
    
    layers <- rv$layers_4326()
    
   map2b <- leaflet(options = leafletOptions(attributionControl=FALSE)) |>
     addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
     addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
     addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") |>
     addPolygons(data=studyarea(), color="black", fill=F, weight=2, group="Study area") |>
     addPolylines(data=layers$linear_disturbance, color="black", weight=2, group="Linear disturbance") |>
     addPolygons(data=layers$areal_disturbance, color="black", weight=1, fill=TRUE, group="Areal disturbance") |>
     addPolygons(data=layers$footprint_500m, color="black", weight=1, fill=TRUE, fillOpacity=0.5, group="Footprint 500m") |>
     addPolygons(data=layers$fires, color="darkred", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires") |>
     addPolygons(data=layers$ifl_2000, color="darkgreen", weight=1, fill=TRUE, fillOpacity=0.5, group="Intact FL 2000") |>
     addPolygons(data=layers$ifl_2020, color="darkgreen", weight=1, fill=TRUE, fillOpacity=0.5, group="Intact FL 2020") |>
     addPolygons(data=layers$protected_areas, color="green", weight=1, fill=TRUE, fillOpacity=0.5, group="Protected areas") |>
     addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
                         overlayGroups = c("Study area", "Linear disturbance", "Areal disturbance", "Fires",
                                           "Footprint 500m", "Intact FL 2000", "Intact FL 2020", "Protected areas"),
                         options = layersControlOptions(collapsed = FALSE)) |>
        hideGroup(c("Linear disturbance", "Areal disturbance", "Fires",
                    "Footprint 500m", "Intact FL 2000", "Intact FL 2020", "Protected areas"))
    map2b
  })
  
  observeEvent(input$runButton2, {
    req(trk_one2b(), gps_csv(), path2b(), hr2b())
    
    gps <- isolate(trk_one2b())
    years <- isolate(unique(gps_csv()$year))
    path <- isolate(path2b())
    hr <- isolate(hr2b())
      
    cols <- col_yrs6[1:length(years)]
    year_pal <- colorNumeric(palette=col_yrs6[1:length(years)], domain=years)
      
    leafletProxy("map2b") |>
      clearGroup("Points")|>
      clearGroup("Tracks")|>
      clearGroup("Ranges")|>
      clearControls() |>
      addCircles(data=gps, ~x_, ~y_, fill=T, stroke=T, weight=gps$first_obs, color=~year_pal(year), 
          fillColor=~year_pal(year), fillOpacity=1, group="Points", popup=gps$t_) |>
      addPolylines(data=path2b(), color="black", weight=1, group=paste0("Tracks")) |>
      addPolygons(data=hr2b(), color="blue", fill=F, weight=2, group="Ranges") |>
      #addPolygons(data=hr2(), stroke=TRUE, color="red", opacity=1, weight=2, fillColor=hr2()$level, fillOpacity=0.5, group="Ranges") |>
      #addPolygons(data=hr2(), stroke=TRUE, color="red", opacity=1, weight=2, fillColor=pal(hr2()$level), fillOpacity=input$alpha, group="Ranges") |>
      addLegend("topleft", colors=cols, labels=years, title="Year") |>
      addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))|>
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
                       overlayGroups = c("Study area", "Points", "Tracks", "Ranges", "Linear disturbance", "Areal disturbance", "Fires",
                                         "Footprint 500m", "Intact FL 2000", "Intact FL 2020", "Protected areas"),
                       options = layersControlOptions(collapsed = FALSE)) |>
      hideGroup(c("Tracks", "Linear disturbance", "Areal disturbance", "Fires",
                  "Footprint 500m", "Intact FL 2000", "Intact FL 2020", "Protected areas"))
  })

  
  # Observe changes in map2a and update map2b
  observeEvent(input$map2a_bounds, {
    req(input$sync_maps)
    bounds <- input$map2a_bounds
    leafletProxy("map2b") |> fitBounds(
      lng1 = bounds$west, lat1 = bounds$south,
      lng2 = bounds$east, lat2 = bounds$north
    )
  })
  
  # Optionally, do the reverse too
  observeEvent(input$map2b_bounds, {
    req(input$sync_maps)
    bounds <- input$map2b_bounds
    leafletProxy("map2a") |> fitBounds(
      lng1 = bounds$west, lat1 = bounds$south,
      lng2 = bounds$east, lat2 = bounds$north
    )
  })
  observeEvent(input$map2a_bounds, {
    req(input$sync_maps)
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
