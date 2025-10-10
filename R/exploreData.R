exploreData <- tabItem(tabName = "explore",
  fluidRow(
    tabBox(id = "one", width="12",
      tabPanel("Mapview", leafletOutput("map1", height=800) |> withSpinner()),
      tabPanel("Help", includeMarkdown("docs/exploreData.md"))
    )
  )
)

exploreDataServer <- function(input, output, session, project, rv){
  
  observeEvent(input$getButton, {
    layers <- rv$layers_4326()
    
    # Leaflet map with locations, home ranges, and disturbances
    output$map1 <- renderLeaflet({
      leaflet(options = leafletOptions(attributionControl=FALSE)) |>
        addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
        addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
        addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") |>
        addPolygons(data=studyarea(), color="black", fill=F, weight=3, group="Study area") |>
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
    })
  })
  
  # Update choices for inputs based on movement data
  observeEvent(c(input$selectInput, input$csv1), {
    x <- gps_csv()
    ids <- as.character(sort(unique(x$id)))
    seasons <- unique(x$season); seasons <- seasons[!is.na(seasons)]
    updateSelectInput(session, "id", choices=c("Please select","All",ids), selected="Please select")
    updateSelectInput(session, "season", choices=c("All", seasons), selected="All")
    updateSliderInput(session, "daterange", min=min(x$year), max=max(x$year), value=c(min(x$year),max(x$year)))
  })

  # Select tracks based on filters
  trk_one <- reactive({
    req(trk_all())
    if(input$id != "Please select"){
      if (input$id=="All" & input$season=="All") {
        trk_all() |> filter(year>=input$daterange[1] & year<=input$daterange[2])
      } else if (input$id=="All" & !input$season=="All") {
        trk_all() |> filter(season==input$season & (year>=input$daterange[1] & year<=input$daterange[2]))
      } else if (!input$id=="All" & input$season=="All") {
        trk_all() |> filter(id==input$id & (year>=input$daterange[1] & year<=input$daterange[2]))
      } else {
        trk_all() |> filter(id==input$id & season==input$season & (year>=input$daterange[1] & year<=input$daterange[2]))
      }
    }
  })

  # Create sf linestrings for mapping
  path <- reactive({
    st_as_sf(trk_one(), coords = c("x_", "y_"), crs = 4326) |>
      st_transform(3578) |>
      group_by(id, year) |> 
      summarize(do_union=FALSE) |> 
      st_cast("LINESTRING") |>
      st_transform(4326)
  })
 
  observeEvent(input$runButton1, {
    req(trk_one())
      years <- unique(gps_csv()$year)
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
                         overlayGroups = c("Study area", "Points", "Tracks", "Linear disturbance", "Areal disturbance", "Fires",
                                           "Footprint 500m", "IFL 2000", "IFL 2020", "Protected areas"),
                         options = layersControlOptions(collapsed = FALSE)) |>
        hideGroup(c("Tracks", "Linear disturbance", "Areal disturbance", "Fires",
                    "Footprint 500m", "IFL 2000", "IFL 2020", "Protected areas"))
      
  })

}
