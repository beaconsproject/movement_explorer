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

estimateRangesServer <- function(input, output, session, project){

  # Update choices for inputs based on movement data
  observeEvent(c(input$selectInput, input$csv1), {
    x <- gps_csv()
    ids <- as.character(sort(unique(x$id)))
    seasons <- unique(x$season); seasons <- seasons[!is.na(seasons)]
    updateSelectInput(session, "id2a", choices=c("ALL",ids), selected="43141")
    updateSelectInput(session, "season2a", choices=c("ALL","Summer range","Winter range"), selected="Summer range")
    updateSliderInput(session, "daterange2a", min=min(x$year), max=max(x$year), value=c(min(x$year),max(x$year)))
    updateSelectInput(session, "id2b", choices=c("ALL",ids), selected="43141")
    updateSelectInput(session, "season2b", choices=c("ALL","Summer range","Winter range"), selected="Winter range")
    updateSliderInput(session, "daterange2b", min=min(x$year), max=max(x$year), value=c(min(x$year),max(x$year)))
  })

  line <- eventReactive(input$selectInput,{
    req(input$getButton)
    if (input$selectInput == "usedemo") {
      st_read('www/little_rancheria.gpkg', 'linear_disturbance', quiet = TRUE)
    } else if (input$selectInput == "usedata") {
      st_read(input$gpkg$datapath, 'linear_disturbance', quiet = TRUE) |>
        st_transform(4326)
    }
  })
  
  poly <- eventReactive(input$selectInput,{
    #req(input$getButton)
    if (input$selectInput == "usedemo") {
      st_read('www/little_rancheria.gpkg', 'areal_disturbance', quiet = TRUE)
    } else if (input$selectInput == "usedata") {
      st_read(input$gpkg$datapath, 'areal_disturbance', quiet = TRUE) |>
        st_transform(4326)
    }
  })

  trk_one2a <- reactive({
    if (input$id2a=="ALL" & input$season2a=="ALL") {
      trk_all() |> filter(year>=input$daterange2a[1] & year<=input$daterange2a[2])
    } else if (input$id2a=="ALL" & !input$season2a=="ALL") {
      trk_all() |> filter(season==input$season2a & (year>=input$daterange2a[1] & year<=input$daterange2a[2]))
    } else if (!input$id2a=="ALL" & input$season2a=="ALL") {
      trk_all() |> filter(id==input$id2a & (year>=input$daterange2a[1] & year<=input$daterange2a[2]))
    } else {
      trk_all() |> filter(id==input$id2a & season==input$season2a & (year>=input$daterange2a[1] & year<=input$daterange2a[2]))
    }
  })

  trk_one2b <- reactive({
    if (input$id2b=="ALL" & input$season2b=="ALL") {
      trk_all() |> filter(year>=input$daterange2b[1] & year<=input$daterange2b[2])
    } else if (input$id2b=="ALL" & !input$season2b=="ALL") {
      trk_all() |> filter(season==input$season2b & (year>=input$daterange2b[1] & year<=input$daterange2b[2]))
    } else if (!input$id2b=="ALL" & input$season2b=="ALL") {
      trk_all() |> filter(id==input$id2b & (year>=input$daterange2b[1] & year<=input$daterange2b[2]))
    } else {
      trk_all() |> filter(id==input$id2b & season==input$season2b & (year>=input$daterange2b[1] & year<=input$daterange2b[2]))
    }
  })

  # Create sf linestrings for mapping
  path2a <- reactive({
    st_as_sf(trk_one2a(), coords = c("x_", "y_"), crs = 4326) |>
      st_transform(3578) |>
      group_by(id, year) |> 
      summarize(do_union=FALSE) |> 
      st_cast("LINESTRING") |>
      st_transform(4326)
  })

  path2b <- reactive({
    st_as_sf(trk_one2b(), coords = c("x_", "y_"), crs = 4326) |>
      st_transform(3578) |>
      group_by(id, year) |> 
      summarize(do_union=FALSE) |> 
      st_cast("LINESTRING") |>
      st_transform(4326)
  })

  # Estimate home range
  hr2a <- reactive({
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
    if (input$runButton2) {
      gps <- trk_one2a()
      years <- unique(gps_csv()$year)
      cols <- col_yrs6[1:length(years)]
      year_pal <- colorNumeric(palette=col_yrs6[1:length(years)], domain=years)
      #pal <- colorFactor(c("#ff9d9a","#77aadd"), levels = levels(hr2()$level))
      leaflet(options = leafletOptions(attributionControl=FALSE)) |>
        addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
        addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
        addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") |>
        addPolygons(data=studyarea(), color="black", fill=F, weight=2, group="Study area") |>
        addPolylines(data=line(), color="black", weight=2, group="Linear disturbance") |>
        addPolygons(data=poly(), color="black", weight=1, fill=TRUE, group="Areal disturbance") |>
        addPolygons(data=fire(), color="darkred", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires") |>
        addPolygons(data=fp500(), color="black", weight=1, fill=TRUE, fillOpacity=0.5, group="Footprint 500m") |>
        addPolygons(data=fire(), color="darkred", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires") |>
        addPolygons(data=ifl2000(), color="darkgreen", weight=1, fill=TRUE, fillOpacity=0.5, group="IFL 2000") |>
        addPolygons(data=ifl2020(), color="darkgreen", weight=1, fill=TRUE, fillOpacity=0.5, group="IFL 2020") |>
        addPolygons(data=pa(), color="green", weight=1, fill=TRUE, fillOpacity=0.5, group="Protected areas") |>
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
          "Footprint 500m", "IFL 2000", "IFL 2020", "Protected areas"),
          options = layersControlOptions(collapsed = FALSE)) |>
        hideGroup(c("Tracks", "Linear disturbance", "Areal disturbance", "Fires",
          "Footprint 500m", "IFL 2000", "IFL 2020", "Protected areas"))
    }
  })

  output$map2b <- renderLeaflet({
    if (input$runButton2) {
      gps <- trk_one2b()
      years <- unique(gps_csv()$year)
      cols <- col_yrs6[1:length(years)]
      year_pal <- colorNumeric(palette=col_yrs6[1:length(years)], domain=years)
      #pal <- colorFactor(c("#ff9d9a","#77aadd"), levels = levels(hr2()$level))
      leaflet(options = leafletOptions(attributionControl=FALSE)) |>
        addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
        addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
        addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") |>
        addPolygons(data=studyarea(), color="black", fill=F, weight=2, group="Study area") |>
        addPolylines(data=line(), color="black", weight=2, group="Linear disturbance") |>
        addPolygons(data=poly(), color="black", weight=1, fill=TRUE, group="Areal disturbance") |>
        addPolygons(data=fire(), color="darkred", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires") |>
        addPolygons(data=fp500(), color="black", weight=1, fill=TRUE, fillOpacity=0.5, group="Footprint 500m") |>
        addPolygons(data=fire(), color="darkred", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires") |>
        addPolygons(data=ifl2000(), color="darkgreen", weight=1, fill=TRUE, fillOpacity=0.5, group="IFL 2000") |>
        addPolygons(data=ifl2020(), color="darkgreen", weight=1, fill=TRUE, fillOpacity=0.5, group="IFL 2020") |>
        addPolygons(data=pa(), color="green", weight=1, fill=TRUE, fillOpacity=0.5, group="Protected areas") |>
        addCircles(data=gps, ~x_, ~y_, fill=T, stroke=T, weight=gps$first_obs, color=~year_pal(year), 
          fillColor=~year_pal(year), fillOpacity=1, group="Points", popup=gps$t_) |>
        addPolylines(data=path2b(), color="black", weight=1, group=paste0("Tracks")) |>
        addPolygons(data=hr2b(), color="blue", fill=F, weight=2, group="Ranges") |>
        #addPolygons(data=hr2(), stroke=TRUE, color="red", opacity=1, weight=2, fillColor=hr2()$level, fillOpacity=0.5, group="Ranges") |>
        #addPolygons(data=hr2(), stroke=TRUE, color="red", opacity=1, weight=2, fillColor=pal(hr2()$level), fillOpacity=input$alpha, group="Ranges") |>
        addLegend("topleft", colors=cols, labels=years, title="Year") |>
        addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE)) |>
        addLayersControl(position = "topright",
          baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
          overlayGroups = c("Study area", "Points", "Tracks", "Ranges", "Linear disturbance", "Areal disturbance", "Fires",
          "Footprint 500m", "IFL 2000", "IFL 2020", "Protected areas"),
          options = layersControlOptions(collapsed = FALSE)) |>
        hideGroup(c("Tracks", "Linear disturbance", "Areal disturbance", "Fires",
          "Footprint 500m", "IFL 2000", "IFL 2020", "Protected areas"))
    }
  })

  # Download ranges
  output$downloadRanges <- eventReactive(input$downloadRanges,{
    req(input$downloadRanges)
    file <- paste0("C:/Users/pierr/OneDrive/Desktop/demo_", Sys.Date(), ".gpkg")
    hr2a <- hr2a() |> filter(level==0.95)
    st_write(hr2a, file, paste0("range_", input$hr2a, "_", input$id2a, "_", input$season2a, "_", input$daterange2a[1], "_", input$daterange2a[2]), append=TRUE)
    hr2b <- hr2b() |> filter(level==0.95)
    st_write(hr2b, file, paste0("range_", input$hr2b, "_", input$id2b, "_", input$season2b, "_", input$daterange2b[1], "_", input$daterange2b[2]), append=TRUE)
  })

}
