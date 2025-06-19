# Check and install packages if missing
required_packages <- c("sf", "DT", "amt", "dplyr", "terra", "ggplot2", "leaflet", "bslib",
  "shinyjs", "markdown", "patchwork", "lubridate", "shinydashboard", "shinycssloaders"
)

# Install any missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load the packages
invisible(lapply(required_packages, library, character.only = TRUE))

# Increase memory size for large files
options(shiny.maxRequestSize=100*1024^2) 

# Datatable options
options(DT.options = list(scrollX = TRUE))

# Color schemes for 2020-2025
#col_yrs6 <- c('#3288bd','#99d594','#e6f598','#fee08b','#fc8d59','#d53e4f')
col_yrs6 <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33')
