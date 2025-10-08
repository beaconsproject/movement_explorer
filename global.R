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
library(markdown)

# Options
options(shiny.maxRequestSize=100*1024^2) 
options(DT.options = list(scrollX = TRUE))

# Color schemes for 2020-2025
col_yrs6 <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33")
