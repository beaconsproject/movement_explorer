## BEACONs Movement Apps

May 9, 2025

The BEACONs Movement Apps are meant to facilitate the exploration of movement data. Two apps are currently available.

### Segmentation Explorer

The app can also be run from a local machine using the following steps (note, the first 2 steps only need to be run once):

  1. Install R (download from [r-project.org](https://www.r-project.org/) and follow instructions)
  2. Install the following additional packages:

    install.packages(c("sf","DT","amt","terra","patchwork",leaflet","dplyr","lubridate","shinydashboard","shinycssloaders","shinyjs"))

  3. Start the Shiny app:

    shiny::runGitHub("beaconsproject/movement_apps/segmentation_explorer", subdir="home")

### Home Range Explorer

The app can also be run from a local machine using the following steps (note, the first 2 steps only need to be run once):

  1. Install R (download from [r-project.org](https://www.r-project.org/) and follow instructions)
  2. Install the following additional packages:

    install.packages(c("sf","DT","amt","terra","leaflet","dplyr","lubridate","shinydashboard","shinycssloaders","shinyjs"))

  3. Start the Shiny app:

    shiny::runGitHub("beaconsproject/movement_apps/home_range_explorer", subdir="home")

### Movement Explorer


