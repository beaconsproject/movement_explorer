# Movement explorer app
# Author: Pierre Vernier
# Updated: 2025-03-21

library(sf)
library(DT)
library(amt)
library(dplyr)
library(terra)
library(ggplot2)
library(leaflet)
library(shinyjs)
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
col_yrs6 <- c('#3288bd','#99d594','#e6f598','#fee08b','#fc8d59','#d53e4f')

# When should year start? e.g., Jan 01 = 1, Feb 01 = 32
day1 <- 32

# Google imagery web service
#google = 'https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G'

# Disturbance datasets
#poly <- st_read('data/disturbances.gpkg', 'areal_disturbance', quiet=TRUE)
#line <- st_read('data/disturbances.gpkg', 'linear_disturbance', quiet=TRUE)
#fire <- st_read('data/disturbances.gpkg', 'fires', quiet=TRUE)
