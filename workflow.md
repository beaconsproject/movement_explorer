# Movement Analysis Workflow

Updated: April 1, 2025

## 1. Data preparation

The main objective of the data preparation component is to create an analysis-ready database consisting of caribou locations, landscape and climate covariates, and attributes identifying seasons and migration periods.

**Telemetry data:**

- Import data directly from movebank.org (YT)
- Import shapefile data from BC Government [Can we get access to movebank project?]
    - Select individuals and years to use (see below re. overlap)
- For BC and YT data:
    - Acquire, clean, and save caribou location data [Has this been done for either datasets?]
    - Identify and remove duplicate observations
    - Identify and remove outliers
    - Assess telemetry errors
    - Thin data by space and/or time
    - Save as RDS objects, CSV files, and Shapefiles
- Combine BC and YT data [How will this be done and who will do it?]
    - Ensure there is no spatial or temporal overlap
- Define seasons and migration periods
- Which seasons are of interest?
    - Early winter, late winter, fall rut, spring, summer
    - Spring and fall migration
    - Snow and snow-free seasons
- Review segmentation methods for evaluating seasons e.g., expert knowledge, Migration-Mapper
- Apply selected method to define seasons

**Covariate data:**

- Acquire and prepare a set of covariate rasters that have the same resolution, extent, project, and are aligned
    - Topography: elevation, slope, aspect, ruggedness, easting (eastness), northing (northness)
    - Landcover: forest (conifer, deciduous, mixedwood), shrubland, grassland, wetland, barren/snow/ice
    - Anthropogenic: linear and areal disturbances (occurrence, distance to, neighbourhood)
    - Natural disturbances: fire polygons, burn areas
- Acquire and prepare normal and projected bioclimate data from climateNA
    - Bioclimate normals [1990-2020?] and selected scenarios [projected to 2050, 2080?]

**Extract values**

- Extract raster values using caribou location data using `terra` package
- Save as CSV and GPKG files

## 2. Movement analysis

The main objectives of the movement analysis component is to: (i) visualize movement patterns of caribou across individuals, years, and seasons; (ii) estimate the home ranges (range and occurrence distributions) for caribou at the individual and population levels; (iii) identify spring and fall migration corridors.

**Movement patterns**

-ctmm can help us account for telemetry error, autocorrelation and irregular sampling frequencies in animal tracking data
-Convert caribou location data into tracks (`amt`) or telemetry objects (`ctmm`)
-Summarize movement by density patterns
-Select all intersecting units (hierarchical catchments, grids)
-Calculate density for each scale

**Home ranges**

-EstimateRange distributions
-Estimate home range using aKDE (OUF model)
-Compare MCP, LoCoH, KDE, and aKDE
-Create overall HR estimate (5-year population-level)
-Create annual HRs
-Population-level inferences
  - Estimate population home range using PKDE
-Occurrence distributions
  - Individual-level: annual and seasonal

**Migration and movement corridors**

- Determining migratory status and migration tracks
- Assessing environmental drivers of caribou migratory movements
- Habitat selection analyses during migration
- Modelling connectivity between summer and winter ranges

## 3. Habitat analysis

The steps include:

- Develop habitat models: population and individual-levels
- Use models to identify
  - What landscape features do animals seek or avoid during their movements?
  - In which areas of the landscape are animals at risk of predation or disease transmission?

Use models to identify:

- What landscape features do animals seek or avoid during their movements?
- In which areas of the landscape are animals at risk of predation or disease transmission?

Workflow (seasonal range):

- For each individual for each year:
  - Identify spatial and temporal boundaries of each seasonal range
  - Estimate range distribution (MCP, KDE, AKDE) - to limit potential availability
  - Select covariates and view their use/availability histograms
  - Estimate RSF (with home ranging i.e., availability varies based on location)
- Estimate mixed-effects RSF (with home ranging)

Workflow (seasonal migration)

- Identify spatial and temporal boundaries of each seasonal range
- Estimate range distribution (MCP, KDE, AKDE)
- Select covariates and view their use/availability histograms
- Estimate RSF (without home ranging)

Worked examples:

- glm: https://jcoliver.github.io/learn-r/011-species-distribution-models.html
- sdm (raster): https://rspatial.org/raster/sdm/
- sdm (terra): https://rspatial.org/sdm/index.html
- rf: https://rspatial.org/analysis/5-global_regression.html
- gbm: https://bookdown.org/mcwimberly/gdswr-book/application---species-distribution-modeling.html
- interpret: https://easystats.github.io/report/
- validate: https://towardsdatascience.com/model-validation-techniques-explained-a-visual-guide-with-code-examples-eb13bbdc8f88

## 4. Connectivity analysis

The steps include:

- Develop a permeability/resistance surface using the previously fitted habitat model.
- Develop a probability of connectivity map using omniscape
- Identify key (seasonal) movement corridors
- Evaluate ecological representativity and functional connectivity or IPCA network

## 5. Climate change

- Developing climate-based models and using them to evaluate changes in home ranges, core habitat, and movement corridors (matrix permeability) under different climate-change scenarios

## 6. Monitoring

The objective is to develop an app to allow users to design a monitoring study and analyse the results. Components include:

- validation monitoring design
- adaptive monitoring design

## 7. Visualizing results

The objective is to develop a (shiny) dashboard to view the results of the various analyses and allow users to explore the sensitivity of some of the parameters. Functionality includes:

Dashboard:

- View estimated seasonal home ranges (present)
- View seasonal probability of occurrence maps (present, future)
- View potential seasonal movement corridors (present, future)

Shiny app:

- Visualize telemetry data - filter by ID, season, year
- Summarize by grid or catchment - identify key connectivity areas
- Identify road crossings and other barriers
- Identify movement corridors
- Identify refugia
