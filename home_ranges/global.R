# Home Range Explorer
# Author: Pierre Vernier
# Updated: 2025-03-31

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
col_yrs6 <- c("#3288bd","#99d594","#e6f598","#fee08b","#fc8d59","#d53e4f")

# Google imagery web service
google <- "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G"

# Read data
line <- st_read("../data/little_rancheria.gpkg", "Linear_disturbances", quiet=TRUE) |>
  st_transform(4326)
poly <- st_read("../data/little_rancheria.gpkg", "Areal_disturbances", quiet=TRUE) |>
  st_transform(4326)
fire <- st_read("../data/little_rancheria.gpkg", "Fires", quiet=TRUE) |>
  st_transform(4326)
foot <- st_read("../data/little_rancheria.gpkg", "Footprint_500m", quiet=TRUE)
foot4326 <- st_transform(foot, 4326)
intact <- st_read("../data/little_rancheria.gpkg", "Intactness_500m", quiet=TRUE)
intact4326 <- st_transform(intact, 4326)
bnd <- st_read("../data/little_rancheria.gpkg", "Study_area", quiet=TRUE) #|>
  #st_buffer(10000)
bnd4326 <- st_transform(bnd, 4326)
ipca <- st_read("../data/little_rancheria.gpkg", "Proposed_IPCAs", quiet=TRUE)
ipca4326 <- st_transform(ipca, 4326)
pa <- st_read("../data/little_rancheria.gpkg", "Protected_areas", quiet=TRUE)
pa4326 <- st_transform(pa, 4326)

# Calculate a few statistics
bnd_m2 <- st_area(bnd)
bnd_km2 <- units::set_units(bnd_m2, km^2)
foot_pct <- round(st_area(foot)/bnd_m2*100,2)
pca <- st_union(st_union(ipca), st_union(pa)) |>
  st_intersection(bnd)
pca_m2 <- st_area(pca)
pca_km2 <- units::set_units(pca_m2, km^2)
