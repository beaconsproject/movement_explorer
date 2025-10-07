server = function(input, output, session) {

  # Reload app
  observeEvent(input$reload_btn, {
    session$reload()
  })

  # R/selectData.R
  selectDataServer(input, output, session, project)

  # R/ExploreData.R
  exploreDataServer(input, output, session, project)

  # R/estimateRange.R
  estimateRangesServer(input, output, session, project)

  # R/estimateRange.R
  identifyCorridorsServer(input, output, session, project)
}
