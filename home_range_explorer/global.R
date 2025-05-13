library(sf)
library(DT)
library(amt)
library(dplyr)
library(terra)
library(leaflet)
library(shinyjs)
library(markdown)
library(lubridate)
library(shinydashboard)
library(shinycssloaders)

#-------------------------------------------------
# 1. Options and parameters
#-------------------------------------------------

# Increase memory size for large files
options(shiny.maxRequestSize=100*1024^2) 

# Datatable options
options(DT.options = list(scrollX = TRUE))

# Color schemes for 2020-2025
col_yrs6 <- c("#3288bd","#99d594","#e6f598","#fee08b","#fc8d59","#d53e4f")

# Google imagery web service
google <- "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G"
