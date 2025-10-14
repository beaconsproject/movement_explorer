dwdDataServer <- function(input, output, session, project, rv){

  # Download disturbance gpkg
  output$saveData <- downloadHandler(
    filename = function() {
      paste0("little_rancheria_", Sys.Date(), ".gpkg")
    },
    content = function(file) {
      browser()
      savedPaths <- rv$savedPaths()
      savedRanges <- rv$savedRanges()
      layers <- rv$layers()
      nm_layer <- names(layers)
      st_write(studyarea(), file, "studyarea", append=TRUE)
      
      if(length(layers)>0){
        for(i in nm_layer){
          st_write(layers[[i]], file, i, append=TRUE)
        }
      }
    
      if(length(savedPaths)>0){
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
      } 
      
      if(length(savedRanges)>0){
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
    }
  )

}
