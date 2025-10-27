selectData <- tabItem(tabName = "select",
  fluidRow(
    tabBox(id = "one", width="12",
      tabPanel("Movement data", DTOutput("gps_data")),
      tabPanel("Sampling duration", plotOutput("duration")),
      tabPanel("Sampling rates", DTOutput("sampling_rates")),
      tabPanel("User guide", uiOutput("selectData_md"))
    )
  )
)

selectDataServer <- function(input, output, session, project, rv){

  output$selectData_md <- renderUI({
    md_text <- get_markdown_content(selectData_url)
    if(md_text=="# Error\nCould not load markdown file from GitHub.") {
      includeMarkdown("docs/selectData.md")
    } else {
      tmp_file <- tempfile(fileext = ".md")
      writeLines(md_text, tmp_file)
      includeMarkdown(tmp_file)
    }
  })

 
  # Read gps movement data
  #gps_csv <<- eventReactive(list(input$selectInput,input$csv1), {
  #  req(input$selectInput)  # Ensure `selectInput` is not NULL
  #  
  #  if (input$selectInput == "usedemo") {
   #   
  #    readr::read_csv('www/little_rancheria_season_migration.csv') |>
 #         mutate(year=year(timestamp), yday=yday(timestamp))
  #  } else if (input$selectInput == "usedata") {
 #     req(input$csv1)
 #     readr::read_csv(input$csv1$datapath) |>
 #       mutate(year=year(timestamp), yday=yday(timestamp))
 #   }
  #})

  observeEvent(list(input$selectInput, input$csv1), {
    req(input$selectInput)
    
    if (input$selectInput == "usedemo") {
      f <- readr::read_csv('www/little_rancheria_season_migration.csv') |>
        mutate(year = year(timestamp), yday = yday(timestamp))
    } else if (input$selectInput == "usedata") {
      req(input$csv1)
      f <- readr::read_csv(input$csv1$datapath) |>
        mutate(year = year(timestamp), yday = yday(timestamp))
    }
    
    rv$gps_data(f)  # store the dataframe in reactiveVal
  })
  
  segment_csv <<- eventReactive(input$csv2, {
    req(input$csv2)
    i <- readr::read_csv(input$csv2$datapath) |>
      mutate(yday_start = yday(ymd(paste(year(today()), start, sep="-"))),
             yday_end = yday(ymd(paste(year(today()), end, sep="-"))))  
  })  
  
  observeEvent(c(input$colIncluded == "incol", rv$gps_data()), {
    x <- rv$gps_data()
    updateSelectInput(session, "season_col", choices= colnames(x), selected="season")
    updateSelectInput(session, "mig_col", choices= colnames(x), selected="migration")
  })
  
  observe({
    req(rv$gps_data())
    f <- rv$gps_data()
    colInc <- input$colIncluded  

    if(input$selectInput == "usedemo"){
      season <- f$season |> unique() |> na.omit() |> sort()
      rv$season(season)
      migration <- f$migration |> unique() |> na.omit() |> sort()
      rv$migration(migration)
    }else if (input$selectInput == "usedata"){
      req(colInc)
      if(colInc == "incol"){
        req(input$season_col, input$mig_col)
        season <- f[[input$season_col]] |> unique() |> na.omit() |> sort()
        rv$season(season)
        migration <- f[[input$mig_col]] |> unique() |> na.omit() |> sort()
        rv$migration(migration)
      }else{
        req(segment_csv())
        x <- segment_csv()
        
        season <- x |> filter(type=="Season") |> pull(name) |> unique() |> na.omit() |> sort()
        rv$season(season)
        migration <- x |> filter(type=="Migration") |> pull(name) |> unique() |> na.omit() |> sort()
        rv$migration(migration)
        f <- f |> mutate(season="", migration="")
        for (i in 1:nrow(x)) {
          if (x$type[i]=="Season") {
            if (x$yday_start[i] < x$yday_end[i]) {   
              f <- f |> mutate(season=ifelse(yday>=x$yday_start[i] & yday<=x$yday_end[i], x$name[i], season))
            } else {
              f <- f |> mutate(season=ifelse(yday>=x$yday_start[i] | yday<=x$yday_end[i], x$name[i], season))
            }
          } else if (x$type[i]=="Migration") {
            if (x$yday_start[i] < x$yday_end[i]) {    
              f <- f |> mutate(migration=ifelse(yday>=x$yday_start[i] & yday<=x$yday_end[i], x$name[i], migration))
            } else {
              f <- f |> mutate(migration=ifelse(yday>=x$yday_start[i] | yday<=x$yday_end[i], x$name[i], migration))
            }
          }
        }
        rv$gps_data(f)
      }
    }
      
  }) 
    
  # Create study area boundary based on KDE
  studyarea <<- reactive({
    trk <- rv$gps_data() |>
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
    rv$gps_data() |> make_track(.x=longitude, .y=latitude, .t=timestamp, all_cols=TRUE, crs = 4326)
  })
  
  # Output 'GPS data' to table
  output$gps_data <- renderDT({
    req(input$getButton)
    datatable(rv$gps_data())
  })

  # Output 'Sampling duration' to plot
  output$duration <- renderPlot({
   x <- rv$gps_data() |>
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
