exploreData <- tabItem(
  tabName = "explore",
  fluidRow(
    tabBox(
      id = "one", width = "12",
      
      tabPanel("Mapview",
               div(style = "position: relative;",
                 leafletOutput("map1", height = 800) |> withSpinner(),
                 tags$img(src = "legend.png", style = "position: absolute; bottom: 15px; right: 15px; width: 200px; opacity: 0.9; z-index: 9999;")
               )
      ),
      
      tabPanel("Help", includeMarkdown("docs/exploreData.md"))
    )
  )
)

exploreDataServer <- function(input, output, session, project, rv){
  
  observeEvent(input$getButton, {
    layers <- rv$layers_4326()
    
    # Leaflet map with locations, home ranges, and disturbances
    output$map1 <- renderLeaflet({
      map1 <- leaflet(options = leafletOptions(attributionControl=FALSE)) |>
        addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
        addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
        addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") |>
        addPolygons(data=studyarea(), color="black", fill=F, weight=3, group="Study area") 
      
      if(isMappable(layers$linear_disturbance)){
        map1 <- map1 |> addPolylines(data=layers$linear_disturbance, color="#CC3333", weight=2, group="Linear disturbance")
      }
      if(isMappable(layers$areal_disturbance)){
        map1 <- map1 |> addPolygons(data=layers$areal_disturbance, color="#660000", weight=1, fill=TRUE, group="Areal disturbance")
      }
      if(isMappable(layers$footprint_500m)){
        map1 <- map1 |> addPolygons(data=layers$footprint_500m, color="#663399", weight=1, fill=TRUE, fillOpacity=0.5, group="Footprint 500m")
      }
      if(isMappable(layers$fires)){
        map1 <- map1 |> addPolygons(data=layers$fires, color="#996633", weight=1, fill=TRUE, fillOpacity=0.5, group="Fires")
      }
      if(isMappable(layers$Intact_FL_2000)){
        map1 <- map1 |> addPolygons(data=layers$Intact_FL_2000, color="#3366FF", weight=1, fill=TRUE, fillOpacity=0.5, group="Intact FL 2000")
      }
      if(isMappable(layers$Intact_FL_2020)){
        map1 <- map1 |> addPolygons(data=layers$Intact_FL_2020, color="#000066", weight=1, fill=TRUE, fillOpacity=0.5, group="Intact FL 2020")
      }
      if(isMappable(layers$protected_areas)){
        map1 <- map1 |> addPolygons(data=layers$protected_areas, color="#699999", weight=1, fill=TRUE, fillOpacity=0.5, group="Protected areas") 
      }
      if(isMappable(layers$Placer_Claims)){
        map1 <- map1 |> addPolygons(data=layers$Placer_Claims, color='#666666', fill=T, weight=1, group="Placer Claims")
      }
      if(isMappable(layers$Quartz_Claims)){
        map1 <- map1 |> addPolygons(data=layers$Quartz_Claims, color='#CCCCCC', fill=T, weight=1, group="Quartz Claims")
      }  
      
      map1 <- map1 |> addLayersControl(position = "topright",
                                               baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
                                               overlayGroups = c("Study area", rv$mappedLayer()),
                                               options = layersControlOptions(collapsed = FALSE)) |>
        hideGroup(rv$mappedLayer())
  
      map1
    })
    
  })
  
  # Update choices for inputs based on movement data
  observeEvent(input$getButton, {
    x <- rv$gps_data()
    season_val <- rv$season()
    migration_val <- rv$migration()
    ids <- as.character(sort(unique(x$id)))
    updateSelectInput(session, "id", choices=c("Please select","All",ids), selected="Please select")
    updateSelectInput(session, "season", choices=c("All", season_val, migration_val), selected="All")
    updateSliderInput(session, "daterange", min=min(x$year), max=max(x$year), value=c(min(x$year),max(x$year)))
  })

  # Select tracks based on filters
  trk_one <- reactive({
    req(trk_all())
    season_val <- rv$season()
    migration_val <- rv$migration()
    if(input$id != "Please select"){
      if (input$id=="All" & input$season=="All") {
        trk_all() |> filter(year>=input$daterange[1] & year<=input$daterange[2])
      } else if (input$id=="All" & !input$season=="All") {
        if (input$season %in% season_val) {
          trk_all() |> filter(season==input$season & (year>=input$daterange[1] & year<=input$daterange[2]))
        } else {
          trk_all() |> filter(migration==input$season & (year>=input$daterange[1] & year<=input$daterange[2]))
        }
      } else if (!input$id=="All" & input$season=="All") {
        trk_all() |> filter(id==input$id & (year>=input$daterange[1] & year<=input$daterange[2]))
      } else {
        if (input$season %in% season_val) {
          trk_all() |> filter(id==input$id & season==input$season & (year>=input$daterange[1] & year<=input$daterange[2]))
        } else {
          trk_all() |> filter(id==input$id & migration==input$season & (year>=input$daterange[1] & year<=input$daterange[2]))
        }
      }
    }
  })

  # Create sf linestrings for mapping
  path <- reactive({
    st_as_sf(trk_one(), coords = c("x_", "y_"), crs = 4326) |>
      group_by(id, year) |> 
      summarize(do_union=FALSE) |> 
      st_cast("LINESTRING")
  })
 
  observeEvent(input$runButton1, {
    req(trk_one())
      years <- unique(rv$gps_data()$year)
      cols <- col_yrs6[1:length(years)]
      year_pal <- colorNumeric(palette=col_yrs6[1:length(years)], domain=years)
      gps <- trk_one() |> 
        mutate(first_obs = c(12, rep(2,nrow(trk_one())-1))) |>
        group_by(id, year)
      leafletProxy("map1") |>
        clearGroup("Points")|>
        clearGroup("Tracks")|>
        clearControls() |>
        addCircles(data=gps, ~x_, ~y_, fill=T, stroke=T, weight=gps$first_obs, color=~year_pal(year), 
          fillColor=~year_pal(year), fillOpacity=1, group="Points", popup=gps$t_) |>
        addPolylines(data=path(), color="blue", weight=2, group=paste0("Tracks")) |>
        addLegend("topleft", colors=cols, labels=years, title="Year") |>
        addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))|>
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
                         overlayGroups = c("Study area", "Points", "Tracks", rv$mappedLayer()),
                         options = layersControlOptions(collapsed = FALSE)) |>
        hideGroup(c("Tracks", rv$mappedLayer()))
      
  })

}
