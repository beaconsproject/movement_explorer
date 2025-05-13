dependencies <- c("sf","DT","amt","terra","patchwork","leaflet","dplyr",
  "markdown","lubridate", "shinydashboard","shinycssloaders","shinyjs")

for(i in 1:length(dependencies)){
  if(dependencies[i] %in% installed.packages()==FALSE){
    install.packages(dependencies[i])
  }
}

library(sf)
library(DT)
library(amt)
library(dplyr)
library(terra)
library(ggplot2)
library(leaflet)
library(shinyjs)
library(markdown)
library(patchwork)
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

# Segments - these should be uploaded by user
segments <- list(None=c(0,366), Summer=c(167, 257), FallRut=c(258, 293), EarlyWinter=c(31,294), 
  LateWinter=c(32,105), SpringMigration=c(91,181), FallMigration=c(244,304))

# Google imagery web service
google <- "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G"
