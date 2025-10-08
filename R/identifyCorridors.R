identifyCorridors <- tabItem(tabName = "corridors",
  fluidRow(
     box(width=3,
        selectInput("id3a", "Select individual:", choices=NULL, multiple=FALSE),
        selectInput("season3a", "Select season:", choices=NULL),
        sliderInput("daterange3a", "Select year(s):", min=2020, max=2025, value=c(2020,2025), sep=""),
        selectInput("hr3a", "Estimator method:", choices=c("Line buffer", "Brownian bridge", "Mixed approach")),
        sliderInput("buffer3a", "Buffer size (m):", min=0, max=750, value=c(250), step=50, sep=""),
        sliderInput("min3a", "Min individuals:", min=1, max=20, value=3, step=1, sep=""),
        sliderInput("patch3a", "Min patch size (km2):", min=0, max=5, value=1, step=0.1, sep="")
      ),
      box(width=9,
        leafletOutput("map3a", height=625) |> withSpinner()
      ),
      box(width=3,
        selectInput("id3b", "Select individual:", choices=NULL, multiple=FALSE),
        selectInput("season3b", "Select season:", choices=NULL),
        sliderInput("daterange3b", "Select year(s):", min=2020, max=2025, value=c(2020,2025), sep=""),
        selectInput("hr3b", "Estimator method:", choices=c("Line buffer", "Brownian bridge", "Mixed approach")),
        sliderInput("buffer3b", "Buffer size (m):", min=0, max=750, value=c(250), step=50, sep=""),
        sliderInput("min3b", "Min individuals:", min=1, max=20, value=3, step=1, sep=""),
        sliderInput("patch3b", "Min patch size (km2):", min=0, max=5, value=1, step=0.1, sep=""),
      ),
      box(width=9,
        leafletOutput("map3b", height=625) |> withSpinner()
      )
  )
)

identifyCorridorsServer <- function(input, output, session, project, rv){

  # Update choices for inputs based on movement data
  observeEvent(c(input$selectInput, input$csv1), {
    x <- gps_csv()
    ids <- as.character(sort(unique(x$id)))
    seasons <- unique(x$season); seasons <- seasons[!is.na(seasons)]
    updateSelectInput(session, "id3a", choices=c("ALL",ids), selected="43141")
    updateSelectInput(session, "season3a", choices=c("Spring migration","Fall migration"), selected="Spring migration")
    updateSliderInput(session, "daterange3a", min=min(x$year), max=max(x$year), value=c(min(x$year),max(x$year)))
    updateSelectInput(session, "id3b", choices=c("ALL",ids), selected="43141")
    updateSelectInput(session, "season3b", choices=c("Spring migration","Fall migration"), selected="Fall migration")
    updateSliderInput(session, "daterange3b", min=min(x$year), max=max(x$year), value=c(min(x$year),max(x$year)))
  })

  observeEvent(input$path1, {
    screenshot(id="map3a", scale=1, filename="corridor_plot1")
  })

  observeEvent(input$path2, {
    screenshot(id="map3b", scale=1, filename="corridor_plot2")
  })
  
  savedRanges <<-list()
    
  # Select tracks for one individual
  trk_one3a <- reactive({
    if (input$id3a=="ALL" & input$season3a=="ALL") {
      trk_all() |> filter(year>=input$daterange3a[1] & year<=input$daterange3a[2])
    } else if (input$id3a=="ALL" & !input$season3a=="ALL") {
      trk_all() |> filter(season==input$season3a & (year>=input$daterange3a[1] & year<=input$daterange3a[2]))
    } else if (!input$id3a=="ALL" & input$season3a=="ALL") {
      trk_all() |> filter(id==input$id3a & (year>=input$daterange3a[1] & year<=input$daterange3a[2]))
    } else {
      trk_all() |> filter(id==input$id3a & season==input$season3a & (year>=input$daterange3a[1] & year<=input$daterange3a[2]))
    }
  })

  trk_one3b <- reactive({
    if (input$id3b=="ALL" & input$season3b=="ALL") {
      trk_all() |> filter(year>=input$daterange3b[1] & year<=input$daterange3b[2])
    } else if (input$id3b=="ALL" & !input$season3b=="ALL") {
      trk_all() |> filter(season==input$season3b & (year>=input$daterange3b[1] & year<=input$daterange3b[2]))
    } else if (!input$id3b=="ALL" & input$season3b=="ALL") {
      trk_all() |> filter(id==input$id3b & (year>=input$daterange3b[1] & year<=input$daterange3b[2]))
    } else {
      trk_all() |> filter(id==input$id3b & season==input$season3b & (year>=input$daterange3b[1] & year<=input$daterange3b[2]))
    }
  })

  path3a <- reactive({
    st_as_sf(trk_one3a(), coords = c("x_", "y_"), crs = 4326) |>
      st_transform(3578) |>
      group_by(id, year) |> 
      summarize(do_union=FALSE) |> 
      st_cast("LINESTRING") |>
      st_transform(4326)
  })

  path3b <- reactive({
    st_as_sf(trk_one3b(), coords = c("x_", "y_"), crs = 4326) |>
      st_transform(3578) |>
      group_by(id, year) |> 
      summarize(do_union=FALSE) |> 
      st_cast("LINESTRING") |>
      st_transform(4326)
  })

  # Line buffer method
  path3buffa <- reactive({
    if (input$id3a=="ALL") {

      hr <- hr_kde(trk_all(), levels=0.999) |> 
        hr_isopleths() |>
        mutate(one=1) |>
        dplyr::select(one) |>
        st_transform(3578)
      rhr <- st_rasterize(hr, dx=100, dy=100) |> 
        rast()

      buf <- st_as_sf(trk_one3a(), coords = c("x_", "y_"), crs = 4326) |>
        st_transform(3578) |>
        group_by(id, year) |> 
        summarize(do_union=FALSE) |> 
        st_cast("LINESTRING") |>
        st_buffer(input$buffer3a)    

      for (i in unique(buf$id)) {
        b1 <- buf |> filter(id==i)|>
          summarize(id=mean(id))
        rb1 <- rasterize(b1, rhr, background=0)
        if (i==unique(buf$id)[1]) {
          rall <- rb1
        } else {
          rall <- rall + rb1
        }
      }

      r <- subst(rall, 0, NA)
      rcl <- r > input$min3a
      rcl <- subst(rcl, 0, NA)
      rp <- patches(rcl)
      v <- as.polygons(rp)
      vp <- v[expanse(v, unit="m") > input$patch3a * 1000000, ] # drop small polygons
      vp <- st_as_sf(vp) |>
        st_transform(4326)

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
    if (input$id3b=="ALL") {

      hr <- hr_kde(trk_all(), levels=0.999) |> 
        hr_isopleths() |>
        mutate(one=1) |>
        dplyr::select(one) |>
        st_transform(3578)
      rhr <- st_rasterize(hr, dx=100, dy=100) |> 
        rast()

      buf <- st_as_sf(trk_one3b(), coords = c("x_", "y_"), crs = 4326) |>
        st_transform(3578) |>
        group_by(id, year) |> 
        summarize(do_union=FALSE) |> 
        st_cast("LINESTRING") |>
        st_buffer(input$buffer3b)    

      for (i in unique(buf$id)) {
        b1 <- buf |> filter(id==i)|>
          summarize(id=mean(id))
        rb1 <- rasterize(b1, rhr, background=0)
        if (i==unique(buf$id)[1]) {
          rall <- rb1
        } else {
          rall <- rall + rb1
        }
      }

      r <- subst(rall, 0, NA)
      rcl <- r > input$min3b
      rcl <- subst(rcl, 0, NA)
      rp <- patches(rcl)
      v <- as.polygons(rp)
      vp <- v[expanse(v, unit="m") > input$patch3b * 1000000, ] # drop small polygons
      vp <- st_as_sf(vp) |>
        st_transform(4326)

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
    od <- od(trk_one3a(), model = fit_ctmm(trk_one3a(), "bm"), trast = make_trast(trk_one3a()))
    iso <- hr_isopleths(od, levels=0.95) |> st_transform(4326)
  })

  # Brownian bridge method
  od3b <- reactive({
    od <- od(trk_one3b(), model = fit_ctmm(trk_one3b(), "bm"), trast = make_trast(trk_one3b()))
    iso <- hr_isopleths(od, levels=0.95) |> st_transform(4326)
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
    leaflet(options = leafletOptions(attributionControl=FALSE)) |>
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
      addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") |>
      addPolygons(data=studyarea(), color="black", fill=F, weight=3, group="Study area") |>
      addPolylines(data=line_sf(), color="black", weight=2, group="Linear disturbance") |>
      addPolygons(data=poly_sf(), color="black", weight=1, fill=TRUE, group="Areal disturbance") |>
      addPolygons(data=fire(), color="darkred", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires") |>
      addPolygons(data=fp500(), color="black", weight=1, fill=TRUE, fillOpacity=0.5, group="Footprint 500m") |>
      addPolygons(data=fire(), color="darkred", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires") |>
      addPolygons(data=ifl2000(), color="darkgreen", weight=1, fill=TRUE, fillOpacity=0.5, group="IFL 2000") |>
      addPolygons(data=ifl2020(), color="darkgreen", weight=1, fill=TRUE, fillOpacity=0.5, group="IFL 2020") |>
      addPolygons(data=pa(), color="green", weight=1, fill=TRUE, fillOpacity=0.5, group="Protected areas") |>
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
                       overlayGroups = c("Study area", "Points", "Tracks", "Corridors", "Linear disturbance", "Areal disturbance", "Fires",
                                         "Footprint 500m", "IFL 2000", "IFL 2020", "Protected areas"),
                       options = layersControlOptions(collapsed = FALSE)) |>
      hideGroup(c("Points", "Tracks", "Linear disturbance", "Areal disturbance", "Fires",
                  "Footprint 500m", "IFL 2000", "IFL 2020", "Protected areas"))
  })
  
  observeEvent(input$runButton3, {
    req(trk_one3a(), gps_csv(), corridor3a(), path3a())
    
    gps <- isolate(trk_one3a())
    years <- isolate(unique(gps_csv()$year))
    corridor3a <- isolate(corridor3a())
    path3a <- isolate(path3a())
    
    cols <- col_yrs6[1:length(years)]
    year_pal <- colorNumeric(palette=col_yrs6[1:length(years)], domain=years)
    
    leafletProxy("map3a") |>
      clearGroup("Points")|>
      clearGroup("Tracks")|>
      clearGroup("Corridors")|>
      clearControls() |>
      addCircles(data=gps, ~x_, ~y_, fill=T, stroke=T, weight=gps$first_obs, color=~year_pal(year), 
                 fillColor=~year_pal(year), fillOpacity=1, group="Points", popup=gps$t_) |>
      addPolylines(data=path3a(), color="blue", weight=2, group=paste0("Tracks")) |>
      addPolygons(data=corridor3a(), color="red", fill=T, weight=2, fillOpacity=0.5, group="Corridors") |>
      addLegend("topleft", colors=cols, labels=years, title="Year") |>
      addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))
    })
  
  output$map3b <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl=FALSE)) |>
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
      addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") |>
      addPolygons(data=studyarea(), color="black", fill=F, weight=3, group="Study area") |>
      addPolylines(data=line_sf(), color="black", weight=2, group="Linear disturbance") |>
      addPolygons(data=poly_sf(), color="black", weight=1, fill=TRUE, group="Areal disturbance") |>
      addPolygons(data=fire(), color="darkred", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires") |>
      addPolygons(data=fp500(), color="black", weight=1, fill=TRUE, fillOpacity=0.5, group="Footprint 500m") |>
      addPolygons(data=fire(), color="darkred", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires") |>
      addPolygons(data=ifl2000(), color="darkgreen", weight=1, fill=TRUE, fillOpacity=0.5, group="IFL 2000") |>
      addPolygons(data=ifl2020(), color="darkgreen", weight=1, fill=TRUE, fillOpacity=0.5, group="IFL 2020") |>
      addPolygons(data=pa(), color="green", weight=1, fill=TRUE, fillOpacity=0.5, group="Protected areas") |>
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
                       overlayGroups = c("Study area", "Points", "Tracks", "Corridors", "Linear disturbance", "Areal disturbance", "Fires",
                                         "Footprint 500m", "IFL 2000", "IFL 2020", "Protected areas"),
                       options = layersControlOptions(collapsed = FALSE)) |>
      hideGroup(c("Points", "Tracks", "Linear disturbance", "Areal disturbance", "Fires",
                  "Footprint 500m", "IFL 2000", "IFL 2020", "Protected areas"))
  })
  
  observeEvent(input$runButton3, {
    req(trk_one3b(), gps_csv(), corridor3b(), path3b())

    gps <- isolate(trk_one3b())
    years <- isolate(unique(gps_csv()$year))
    corridor3a <- isolate(corridor3b())
    path3b <- isolate(path3b())
    
    cols <- col_yrs6[1:length(years)]
    year_pal <- colorNumeric(palette=col_yrs6[1:length(years)], domain=years)

    leafletProxy("map3b") |>
      clearGroup("Points")|>
      clearGroup("Tracks")|>
      clearGroup("Corridors")|>
      clearControls() |>    
      addCircles(data=gps, ~x_, ~y_, fill=T, stroke=T, weight=gps$first_obs, color=~year_pal(year), 
                 fillColor=~year_pal(year), fillOpacity=1, group="Points", popup=gps$t_) |>
      addPolylines(data=path3b(), color="blue", weight=2, group=paste0("Tracks")) |>
      addPolygons(data=corridor3b(), color="red", fill=T, weight=2, fillOpacity=0.5, group="Corridors") |>
      addLegend("topleft", colors=cols, labels=years, title="Year") |>
      addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))
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