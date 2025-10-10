dwdDataServer <- function(input, output, session, project, rv){

  # Download disturbance gpkg
  output$saveData <- downloadHandler(
    filename = function() {
      paste0("little_rancheria_", Sys.Date(), ".gpkg")
    },
    content = function(file) {
      
      savedPaths <- rv$savedPaths()
      savedRanges <- rv$savedRanges()
      layers <- rv$layers()
      
      studyarea <- studyarea() 
      line_sf <- layers$linear_disturbance 
      poly_sf <- layers$areal_disturbance 
      fire <- layers$fires 
      ifl2000 <- layers$Intact_FL_2000
      ifl2020 <- layers$Intact_FL_2020 
      pa <- layers$protected_areas 
      pj1 <- layers$Placer_Claims
      pj2 <- layers$Quartz_Claims
      fp_500m <- layers$footprint_500m
      
      if(input$selectInput =="usedemo"){
        st_write(studyarea, file, "studyarea", append=TRUE)
        st_write(line_sf, file, "linear_disturbance", append=TRUE)
        st_write(poly_sf, file, "areal_disturbance", append=TRUE)
        st_write(fire, file, "fires", append=TRUE)
        st_write(ifl2000, file, "Intact_FL_2000", append=TRUE)
        st_write(ifl2020, file, "Intact_FL_2020", append=TRUE)
        st_write(pa, file, "protected_areas", append=TRUE)
        st_write(pj1, file, "Placer_Claims", append=TRUE)
        st_write(pj2, file, "Quartz_Claims", append=TRUE)
        st_write(fp_500m, file, "footprint_500m", append=TRUE)
        
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
