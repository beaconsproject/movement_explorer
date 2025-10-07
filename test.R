library(sf)
library(terra)
library(stars)
library(tidyverse)

gps <- read_csv("movement_explorer/www/little_rancheria.csv")
trk <- gps |> 
  make_track(.x=longitude, .y=latitude, .t=timestamp, all_cols=TRUE, crs = 4326) |>
  filter(season=="Fall migration")
hr <- hr_kde(trk, levels=c(0.5, 0.95)) |> 
  hr_isopleths()

