# Non-GUI version of geopackage creator
# PV 2025-10-06

library(sf)
library(terra)
library(tidyverse)

aoi <- st_read("movement_explorer/www/little_rancheria_aoi.gpkg", "studyarea")
gpkg_out <- "movement_explorer/www/little_rancheria.gpkg"
bp <- "D:/github/beaconsproject/geopackage_creator/www/bp_datasets.gpkg"

line <- st_read(bp, "sd_line") |>
  st_intersection(aoi) |>
  st_cast("MULTILINESTRING") |>
  st_transform(4326)

poly <- vect(bp, "sd_poly") |>
  st_as_sf() |>
  st_cast("MULTIPOLYGON") |>
  st_intersection(aoi) |>
  st_cast("MULTIPOLYGON") |>
  st_transform(4326)
  
fires <- vect(bp, "fires") |>
  st_as_sf() |>
  st_cast("MULTIPOLYGON") |>
  st_intersection(aoi) |>
  st_cast("MULTIPOLYGON") |>
  st_transform(4326)

ifl_2000 <- st_read(bp, "ifl_2000") |>
  st_intersection(aoi) |>
  st_cast("MULTIPOLYGON") |>
  st_transform(4326)

ifl_2020 <- st_read(bp, "ifl_2020") |>
  st_intersection(aoi) |>
  st_cast("MULTIPOLYGON") |>
  st_transform(4326)

pa_2021 <- st_read(bp, "protected_areas") |>
  st_intersection(aoi) |>
  st_cast("MULTIPOLYGON") |>
  st_transform(4326)

fp500 <- st_read(bp, "footprint_500m") |>
  st_intersection(aoi) |>
  st_cast("MULTIPOLYGON") |>
  st_transform(4326)

aoi <- aoi |> st_transform(4326)

st_write(aoi, gpkg_out, "studyarea", delete_layer=TRUE)
st_write(line, gpkg_out, "linear_disturbance", delete_layer=TRUE)
st_write(poly, gpkg_out, "areal_disturbance", delete_layer=TRUE)
st_write(fires, gpkg_out, "fires", delete_layer=TRUE)
st_write(ifl_2000, gpkg_out, "ifl_2000", delete_layer=TRUE)
st_write(ifl_2020, gpkg_out, "ifl_2020", delete_layer=TRUE)
st_write(pa_2021, gpkg_out, "protected_areas", delete_layer=TRUE)
st_write(fp500, gpkg_out, "footprint_500m", delete_layer=TRUE)

st_layers(gpkg_out)
