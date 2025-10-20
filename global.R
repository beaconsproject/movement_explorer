# Libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(shinyBS)
library(sf)
library(tidyverse)
library(leaflet)
library(terra)
library(stars)
library(bslib)
library(shinycssloaders)
library(amt)
library(smoothr)
library(markdown)
library(shinyscreenshot)

# Options
options(shiny.maxRequestSize=100*1024^2) 
options(DT.options = list(scrollX = TRUE))

# Color schemes for 2020-2025
col_yrs6 <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33")

isMappable <- function(x) {
  !is.null(x) && inherits(x, "sf") && nrow(x) > 0
}

library(markdown)  # for includeMarkdown
library(httr)      # for downloading from GitHub

# Replace with your actual raw GitHub URL
overview_url <- "https://raw.githubusercontent.com/beaconsproject/movement_explorer/main/docs/overview.md"
user_guide_url <- "https://raw.githubusercontent.com/beaconsproject/movement_explorer/main/docs/user_guide.md"
datasets_url <- "https://raw.githubusercontent.com/beaconsproject/movement_explorer/main/docs/datasets.md"
exploreData_url <- "https://raw.githubusercontent.com/beaconsproject/movement_explorer/main/docs/exploreData.md"
estimateRanges_url <- "https://raw.githubusercontent.com/beaconsproject/movement_explorer/main/docs/estimateRanges.md"
identifyCorridors_url <- "https://raw.githubusercontent.com/beaconsproject/movement_explorer/main/docs/identifyCorridors.md"

# Function to safely fetch markdown content
get_markdown_content <- function(url) {
  res <- try(GET(url), silent = TRUE)
  if (inherits(res, "try-error") || status_code(res) != 200) {
    return("# Error\nCould not load markdown file from GitHub.")
  }
  content(res, "text", encoding = "UTF-8")
}
