server = function(input, output, session) {

  # Reload app
  observeEvent(input$reload_btn, {
    session$reload()
  })

  observeEvent(input$screen_btn, {
    screenshot(scale=5, filename="movement_explorer")
  })

  reactiveValsList <- list(
    mappedLayer = reactiveVal(),
    savedRanges = reactiveVal(list()),
    savedPaths = reactiveVal(list()), 
    layers = reactiveVal(list()),
    layers_4326 = reactiveVal(list())
  )
  
  # R/selectData.R
  selectDataServer(input, output, session, project, reactiveValsList)

  # R/ExploreData.R
  exploreDataServer(input, output, session, project, reactiveValsList)

  # R/estimateRanges.R
  estimateRangesServer(input, output, session, project, reactiveValsList)

  # R/identifyCorridors.R
  identifyCorridorsServer(input, output, session, project, reactiveValsList)
  
  # R/dwdData.R
  dwdDataServer(input, output, session, project, reactiveValsList)
}
