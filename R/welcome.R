welcome <- tabItem(tabName="home",
  fluidRow(
    tabBox(id = "one", width="12",
      tabPanel("Overview", includeMarkdown("docs/overview.md")),
      tabPanel("User guide", includeMarkdown("docs/user_guide.md")),
      tabPanel("Dataset requirements", includeMarkdown("docs/datasets.md")),
      tabPanel("Estimating ranges", includeMarkdown("docs/estimateRanges.md")),
      tabPanel("Identifying corridors", includeMarkdown("docs/identifyCorridors.md")),
    )
  )
)