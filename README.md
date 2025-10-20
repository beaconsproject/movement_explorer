## BEACONs Movement Explorer

October 19, 2025

The BEACONs Movement Explorer is a simple app intended to be used as part of a workflow for exploring movement data. Specifically, the app can help users to:

  - Upload and visualize animal movement data
  - Estimate seasonal and annual home ranges
  - Identify seasonal movement corridors (paths)

Please note that the app has some known issues and is in development, and we would appreciate your feedback on any issues or suggestions you may have.

### Running the app

There are three ways to run the app:

1) The simplest method is simply point your browser to: https://beaconsproject.shinyapps.io/movement_explorer/

2) The app can also be run from a local machine using the following steps (note, the first 2 steps only need to be run once):

  -  Install R (download from [r-project.org](https://www.r-project.org/) and follow instructions)
  -  Install the following additional packages:

```         
install.packages(c("leaflet", "tidyverse", "cli","shinydashboard", "shinycssloaders", "shiny", "shinyjs",
   "markdown", "dplyr", "tidyr", "sf", "shinyMatrix"))
```

    Start the Shiny app:

```         
shiny::runGitHub("beaconsproject/movement_explorer")
```

3) Alternatively, you can download the repository and run it from a local machine:

  - Download and extract this repository to a local drive e.g., "D:/Apps/movement_explorer"
  - Start R or RStudio
  - Run the app from prompt:
    - shiny::runApp("D:/Apps/movement_explorer")
