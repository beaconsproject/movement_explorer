welcome <- tabItem(tabName="home",
  fluidRow(
    tabBox(id = "one", width="12",
      #tabPanel("Overview", includeMarkdown("docs/overview.md")),
      tabPanel("Overview", uiOutput("overview_md")),
      tabPanel("User guide", uiOutput("user_guide_md")),
      tabPanel("Dataset requirements", uiOutput("datasets_md")),
      tabPanel("Estimating ranges", uiOutput("estimateRanges_md")),
      tabPanel("Identifying corridors", uiOutput("identifyCorridors_md")),
    )
  )
)


welcomeServer <- function(input, output, session, project, rv){

  output$overview_md <- renderUI({
    md_text <- get_markdown_content(overview_url)
    tmp_file <- tempfile(fileext = ".md")
    writeLines(md_text, tmp_file)
    includeMarkdown(tmp_file)
  })

  output$user_guide_md <- renderUI({
    md_text <- get_markdown_content(overview_url)
    tmp_file <- tempfile(fileext = ".md")
    writeLines(md_text, tmp_file)
    includeMarkdown(tmp_file)
  })

  output$datasets_md <- renderUI({
    md_text <- get_markdown_content(overview_url)
    tmp_file <- tempfile(fileext = ".md")
    writeLines(md_text, tmp_file)
    includeMarkdown(tmp_file)
  })

  output$estimateRanges_md <- renderUI({
    md_text <- get_markdown_content(overview_url)
    tmp_file <- tempfile(fileext = ".md")
    writeLines(md_text, tmp_file)
    includeMarkdown(tmp_file)
  })
  
  output$identifyCorridors_md <- renderUI({
    md_text <- get_markdown_content(overview_url)
    tmp_file <- tempfile(fileext = ".md")
    writeLines(md_text, tmp_file)
    includeMarkdown(tmp_file)
  })

}  
  