## Dataset requirements

Movement Explorer comes with an embedded demo dataset that can be used to explore the functionality of the app. Alternatively, users can upload their own data via three input files:

- Input 1: Animal movement (or location) data defined by longitude and latitude with timestamp
- Input 2: Start and end dates for each season and migration period
- Input 3: Spatial data for exploring ranges and movement such as human disturbance

### Input 1: Animal movement data

Animal movement data is uploaded as a CSV text file with the attributes listed below. This file can be created using [BEACONs Movement Mapper](https://beaconsproject.shinyapps.io/movement_mapper/), for example. 

Format:
- CSV text file (comma separated values, .csv)

Required attributes:
- **id**: Individual identification e.g., collar id
- **timestamp**: a timestamp indicating the year, month, day, hour, minute, second
- **longitude**: longitude (crs:4326)
- **latitude**: latitude (crs:4326)

Required attributes if Input 2 does not exist:
- **season**: name of the season within which the location occurs to e.g., "Winter"
- **migration**: name of the migration period within which the location occurs e.g., "Fall migration"

Optional attributes:
- **elevation**: elevation in metres at which the location occurred

Additional variables can be uploaded for viewing only.

## Input 2: Season and migration periods

  📌 This file is only required if the Animal movement data (Input 1) does not have the "season" and "migration" attributes.

This input table lists the start and end dates for each season and migration period. It is uploaded as a CSV text file with the attributes listed below.  This table is used by the app to assign each movement (or location) point to a season and/or migration period for all individuals and years.  

Format:
- CSV text file (comma separated values, .csv)

Required attributes:
- **type**: can only take one of two values: "Season" or "Migration"
- **name**: name of season or migration period e.g., "Winter" or "Fall migration"
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


### Input 3 (optional): Spatial data for exploring ranges and movement

Spatial layers can be added to the app via a GeoPackage. For the moment, only the layers list below can be uploaded. 

  📌 For the app to recognize the spatial layers in the GeoPackage, the layer names **must exactly match the expected names shown below.** 
  
  📌 All spatial layers must have the same projection.

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

This GeoPackage can be created using [BEACONs Geopackage Creator](https://beaconsproject.shinyapps.io/geopackage_creator/), for example, or manually in QGIS. 
