
library(shiny)
library(dplyr)

ui <- fluidPage(
  titlePanel("Air Quality Analysis"),
  sidebarLayout(
    sidebarPanel(
      # Reactive Source: Dropdown menu for selecting the month
      selectInput("monthInput", "Select Month:",
                  choices = month.abb[5:9],  # Only May to September months
                  selected = "May")
    ),
    mainPanel(
      textOutput("avgTemp"),
      textOutput("avgWind")
    )
  )
)


# Server logic
server <- function(input, output) {
  # Observer for reactivity: Responds to month selection changes
  observe({
    selectedMonth <- match(input$monthInput, month.abb)
    filteredData <- airquality %>% 
      filter(Month == selectedMonth)

    if(nrow(filteredData) == 0) {
      output$summary <- renderText("No data available for the selected month.")
      return()
    }

    summaryResult <- filteredData %>%
      summarize(AvgTemp = mean(Temp, na.rm = TRUE), AvgWind = mean(Wind, na.rm = TRUE),
                MaxTemp = max(Temp, na.rm = TRUE), MinTemp = min(Temp, na.rm = TRUE))

    # Update the summary output
    output$summary <- renderText({
      paste("Avg Temp:", round(summaryResult$AvgTemp, 2), "°F. Avg Wind:", round(summaryResult$AvgWind, 2), 
            "mph. Max Temp:", summaryResult$MaxTemp, "°F. Min Temp:", summaryResult$MinTemp, "°F.")
    })
  })

  # Reactive plot output: Updates based on the month and wind speed selected by the user
  output$airQualityPlot <- renderPlot({
    filteredData <- airquality %>%
      filter(Month == match(input$monthInput, month.abb), 
             if(input$showAllData) TRUE else Wind > input$windSlider)

    ggplot(filteredData, aes(x = Temp, y = Wind)) + 
      geom_point(aes(color = factor(Day)), size = 5) + 
      theme_minimal() +
      labs(x = "Temperature (°F)", y = "Wind Speed (mph)", color = "Day of Month",
           title = paste("Temperature vs Wind Speed in", input$monthInput))
  })
}

shinyApp(ui, server)
