## Dataset requirements

The Movement Explorer comes with a demo dataset that can be used to explore the functionality of the app. Alternatively, you can upload your own data, which should consist of three input files:

### Input 1: Animal movement data

The animal movement data needs to be in a CSV format with the attributes listed below. The easiest way to create this file is to use BEACONs Movement Mapper to define migration and seasonal segments. See Input two below if you do not have this attribute..

Format:
- Text file - comma separated values (".csv")

Required attributes:
- **id**: Individual identification e.g., collar id
- **timestamp**: a timestamp indicating the year, month, day, hour, minute, second
- **longitude**: longitude (crs:4326)
- **latitude**: latitude (crs:4326)
- **season**: name of season e.g., "Winter range"
- **migration**: name of migration period e.g., "Fall migration"

Optional attributes:
- **elevation**: elevation in metres (not currently required)

Any additional variables will be loaded and can be viewed but will not be used in any of the analyses. 

## Input 2 (optional): Seasonal and migration periods

This input table is required if you do not have "season" and "migration" attributes in your animal movement data. In this case, you will need to create a simple table as described below. This table will then be automatically merged to your movement data. Note: This table will assign the same start and end dates for all individuals and years.

Format:
- Text file - comma separated values (".csv")

Required attributes:
- **type**: can only take one of two values: "Season" or "Migration"
- **name**: name of season or migration period e.g., "Winter range" or "Fall migration"
- **start**: start date in month-day format e.g., Jun-16
- **end**: end date in month-day format e.g., Sep-14

Example table:

<pre>
|type      |name             |start  |end    |
|----------|-----------------|-------|-------|
|Season    |Early winter     |Oct-21 |Jan-31 |
|Season    |Late winter      |Feb-01 |Apr-15 |
|Season    |Summer           |Jun-16 |Sep-14 |
|Season    |Fall rut         |Sep-15 |Oct-20 |
|Migration |Spring migration |Apr-01 |Jun-30 |
|Migration |Fall migration   |Sep-01 |Oct-31 |
</pre>


### Input 3 (optional): Disturbances and conservation areas

The easiest way to create this optional file is to use the BEACONs Geopackage Creator (https://beaconsproject.shinyapps.io/geopackage_creator/). As long as your study area is located within the limits of the underlying database (i.e., all of the Yukon and the northern half of BC), you simply upload your study area, select the layers you want, and run the app. Alternatively, you will need to create your own geopackage comprising one or more of layers described below. For the app to recognize the spatial layers in the GeoPackage, the layer names **must exactly match the expected names shown below.** In addition, all spatial layers must have the same projection.

Format: 
- Geopackage file (".gpkg")

Potential layers:

- **studyarea**: boundary of study region (polygon)
- **linear_disturbance**: anthropogenic linear disturbances (lines)
- **areal_disturbance**: anthropogenic areal disturbances (polygons)
- **fire**: fire polygons (polygons)
- **footprint_500m**: combined linear and areal disturbances buffered by 500m (polygon)
- **protected_areas**: protected and conserved areas, including IPCAs (polygons)
- **quartz_claims**
- **placer_claims**

Any additional layers will not be loaded nor used in any of the analyses.
