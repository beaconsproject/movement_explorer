# Merge GPS data with simple season-migration table
# Revised: 2025-10-16
# Author: Pierre Vernier

library(tidyverse)

x <- read_csv("segments.csv") |>
  mutate(yday_start = yday(ymd(paste(year(today()), start, sep="-"))),
         yday_end = yday(ymd(paste(year(today()), end, sep="-"))))
y <- read_csv("caribou.csv") |> mutate(season="", migration="")

for (i in 1:nrow(x)) {
  if (x$type[i]=="Season") {
    if (x$yday_start[i] < x$yday_end[i]) {    
      y <- y |> mutate(season=ifelse(yday>=x$yday_start[i] & yday<=x$yday_end[i], x$name[i], season))
    } else {
      y <- y |> mutate(season=ifelse(yday>=x$yday_start[i] | yday<=x$yday_end[i], x$name[i], season))
    }
  } else if (x$type[i]=="Migration") {
    if (x$yday_start[i] < x$yday_end[i]) {    
      y <- y |> mutate(migration=ifelse(yday>=x$yday_start[i] & yday<=x$yday_end[i], x$name[i], migration))
    } else {
      y <- y |> mutate(migration=ifelse(yday>=x$yday_start[i] | yday<=x$yday_end[i], x$name[i], migration))
    }
  }
}
write_csv(y, "little_rancheria_merged.csv")


################################################################################
# TEST

# Standalone script
z1 <- read_csv("little_rancheria_merged.csv") |> 
  filter(migration=="Fall migration", year==2023)
min(z1$yday)
max(z1$yday)

# Movement Explorer (selectData.R)
z2 <- read_csv("test_output.csv") |>
  filter(migration=="Fall migration", year==2023)
min(z2$yday)
max(z2$yday)
################################################################################
