download <- tabItem(tabName = "download",
  fluidRow(
    tabBox(id = "one", width="12",
      tabPanel("Movement data", DTOutput("gps_data")),
      tabPanel("Sampling duration", plotOutput("duration")),
      tabPanel("Sampling rates", DTOutput("sampling_rates"))
    )
  )
)

downloadServer <- function(input, output, session, project){

  # Download disturbance gpkg
  output$saveDemoData <- downloadHandler(
    filename = function() {
      paste0("demo_", Sys.Date(), ".gpkg")
    },
    content = function(file) {
      st_write(studyarea(), file, "studyarea", append=TRUE)
      st_write(line(), file, "linear_disturbance", append=TRUE)
      st_write(poly(), file, "areal_disturbance", append=TRUE)
      st_write(fire(), file, "fires", append=TRUE)
      st_write(ifl2000(), file, "ifl_2000", append=TRUE)
      st_write(ifl2020(), file, "ifl_2020", append=TRUE)
      st_write(pa(), file, "protected_areas", append=TRUE)
    }
  )

}
