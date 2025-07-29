## BEACONs Movement Explorer

July 29, 2025

The BEACONs Movement Explorer is a simple app intended to be used as part of a workflow for exploring movement data. Specifically, the app can help users to:

  - Explore movement data
  - Define seasonal and migration periods
  - Estimate seasonal and annual home ranges
  - Identify seasonal movement corridors (paths)

Please note that the app has some known issues and is in development, and we would appreciate your feedback on any issues or suggestions you may have.


### Video introduction to the app

These videos will be updated regularly to reflect recent modifications to the app.

- Video 1 - [Overview](https://drive.google.com/file/d/1B-hoDu5dyd-4hSyTBoIRXnMGgSy8-KV6/view?usp=sharing)
- Video 2 - [Select study area](https://drive.google.com/file/d/1NyA7VCmodqu2RhPWgJnTnngn56zM8Abj/view?usp=sharing)
- Video 3 - [Define seasons](https://drive.google.com/file/d/1LQRAj7jZT-XaN2wrh9yUgSiEsW8HSGPj/view?usp=sharing)
- Video 4 - [Estimate ranges](https://drive.google.com/file/d/1RmLIXZLFExRZqtmYR3AednMeFnKYD2VX/view?usp=sharing)
- Video 5 - [Identify movement paths](https://drive.google.com/file/d/16LC3Ckbe4Akj7KoOkMj1vRnhC0apZhjZ/view?usp=sharing)


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
