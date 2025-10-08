dwdDataServer <- function(input, output, session, project, rv){

  # Download disturbance gpkg
  output$saveData <- downloadHandler(
    filename = function() {
      paste0("little_rancheria_", Sys.Date(), ".gpkg")
    },
    content = function(file) {
      
      savedPaths <- rv$savedPaths()
      savedRanges <- rv$savedRanges()
      
      studyarea <- studyarea() |> st_transform(3578)
      line_sf <- line_sf() |> st_transform(3578)
      poly_sf <- poly_sf() |> st_transform(3578)
      fire <- fire() |> st_transform(3578)
      ifl2000 <- ifl2000() |> st_transform(3578)
      ifl2020 <- ifl2020() |> st_transform(3578)
      pa <- pa() |> st_transform(3578)
      
      if(input$selectInput =="usedemo"){
        st_write(studyarea, file, "studyarea", append=TRUE)
        st_write(line_sf, file, "linear_disturbance", append=TRUE)
        st_write(poly_sf, file, "areal_disturbance", append=TRUE)
        st_write(fire, file, "fires", append=TRUE)
        st_write(ifl2000, file, "Intact_FL_2000", append=TRUE)
        st_write(ifl2020, file, "Intact_FL_2020", append=TRUE)
        st_write(pa, file, "protected_areas", append=TRUE)
      } 
      
      for (layer_name in names(savedPaths)) {
        layer <- savedPaths[[layer_name]] |> st_transform(3578)
        sf::st_write(
          obj = layer,
          dsn = file,
          layer = layer_name,
          driver = "GPKG",
          append = TRUE
        )
      }
      
      for (layer_name in names(savedRanges)) {
        layer <- savedRanges[[layer_name]] |> st_transform(3578)
        sf::st_write(
          obj = layer,
          dsn = file,
          layer = layer_name,
          driver = "GPKG",
          append = TRUE
        )
      }
    }
  )

}
