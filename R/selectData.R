selectData <- tabItem(tabName = "select",
  fluidRow(
    tabBox(id = "one", width="12",
      tabPanel("Movement data", DTOutput("gps_data")),
      tabPanel("Sampling duration", plotOutput("duration")),
      tabPanel("Sampling rates", DTOutput("sampling_rates"))
    )
  )
)

selectDataServer <- function(input, output, session, project, rv){
 
  # Read gps movement data
  gps_csv <<- eventReactive(list(input$selectInput,input$csv1), {
    req(input$selectInput)  # Ensure `selectInput` is not NULL
    if (input$selectInput == "usedemo") {
      
      readr::read_csv('www/little_rancheria_season_migration.csv') |>
          mutate(year=year(timestamp), yday=yday(timestamp))
    } else if (input$selectInput == "usedata") {
      req(input$csv1)
      readr::read_csv(input$csv1$datapath) |>
        mutate(year=year(timestamp), yday=yday(timestamp))
    }
  })

  
  observeEvent(c(input$selectInput == "usedata", gps_csv()), {
    x <- gps_csv()
    updateSelectInput(session, "season_col", choices= colnames(x), selected=colnames(x)[1]) # selected="Please select"
    updateSelectInput(session, "mig_col", choices= colnames(x), selected=colnames(x)[1])
  })
  
  observeEvent(gps_csv(), {
    f <- gps_csv()
    req(f)
    
    if(input$selectInput == "usedemo"){
      season <- f$season |> unique() |> na.omit() |> sort()
      rv$season(season)
      migration <- f$migration |> unique() |> na.omit() |> sort()
      rv$migration(migration)
    }else{
      req(input$season_col, input$mig_col)
      season <- f[[input$season_col]] |> unique() |> na.omit() |> sort()
      rv$season(season)
      migration <- f[[input$mig_col]] |> unique() |> na.omit() |> sort()
      rv$migration(migration)
    }
  }) 
    
  # Create study area boundary based on KDE
  studyarea <<- reactive({
    trk <- gps_csv() |>
      make_track(.x=longitude, .y=latitude, crs = 4326)
    aoi <- hr_kde(trk, levels=0.9999) |> hr_isopleths()
  })

  observeEvent(input$getButton, {
    req(input$selectInput)
    name_rv <- rv$mappedLayer()
    new_name <- c()
    
    namelist <- c("linear_disturbance", "areal_disturbance", "Intact_FL_2000", "Intact_FL_2020", "protected_areas","footprint_500m", "fires","Quartz_Claims", "Placer_Claims")
    if (input$selectInput == "usedemo") {
      li <- st_layers('www/little_rancheria.gpkg')$name
      available <- intersect(namelist, li)
      
      for(name in available){
        i<- st_read('www/little_rancheria.gpkg', name, quiet = TRUE)
        new_name <- c(new_name, name)
        j<- i |>  st_transform(4326)
        
        current_layers <- rv$layers()
        current_layers[[name]] <- i
        rv$layers(current_layers)
        pj_layers <- rv$layers_4326()
        pj_layers[[name]] <- j
        rv$layers_4326(pj_layers)
      }
    } else if (input$selectInput == "usedata") {
      req(input$gpkg1)
      
      gpkg_path <- file.path(tempdir(), paste0("uploaded_", input$gpkg1$name))
      file.copy(input$gpkg1$datapath, gpkg_path, overwrite = TRUE)
      li <- st_layers(gpkg_path)$name
      available <- intersect(namelist, li)
      for(name in available){
        i<- st_read(gpkg_path, name, quiet = TRUE)
        new_name <- c(new_name, name)
        j<- i |>  st_transform(4326)

        current_layers <- rv$layers()
        current_layers[[name]] <- i
        rv$layers(current_layers)
        pj_layers <- rv$layers_4326()
        pj_layers[[name]] <- j
        rv$layers_4326(pj_layers)
      }
    }
    legend_names <- c("Linear disturbance", "Areal disturbance", "Intact FL 2000", "Intact FL 2020",
                      "Protected areas", "Footprint 500m", "Fires", "Quartz Claims", "Placer Claims")
    mapped_names <- legend_names[match(new_name, namelist)]
    rv$mappedLayer(mapped_names)
    
  })

  
  # Create tracks using amt package
  trk_all <<- eventReactive(input$getButton, {
    gps_csv() |> make_track(.x=longitude, .y=latitude, .t=timestamp, all_cols=TRUE, crs = 4326)
  })
  
  # Output 'GPS data' to table
  output$gps_data <- renderDT({
    req(input$getButton)
    datatable(gps_csv())
  })

  # Output 'Sampling duration' to plot
  output$duration <- renderPlot({
   x <- gps_csv() |>
      mutate(id = as.factor(id), year = year(timestamp))
    ggplot(data=x, aes(x=timestamp, y=id)) +
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
}
