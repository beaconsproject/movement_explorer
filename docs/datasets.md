## Dataset requirements

The Movement Explorer comes with a demo dataset that can be used to explore the functionality of the app. Alternatively, you can upload your own data, which should consist of two input files:

### Input 1: Animal movement data

The animal movement data needs to be in a CSV format with the attributes listed below. **The functionality described in the next line will be implemented on 2025-10-16**. If the **season** attribute is not in the input file, you can use a dialog box to enter the start and end dates for as many seasons and migration periods as you like. Once this table is filled, the season attribute will automatically be created.

Format:
- Text file - comma separated values (".csv")

Required attributes:
- **id**: Individual identification e.g., collar id
- **timestamp**: a timestamp indicating the year, month, day, hour, minute, second
- **longitude**: longitude (crs:4326)
- **latitude**: latitude (crs:4326)
- **season**: name of season or migration period

Optional attributes:
- **elevation**: elevation in metres (not currently required)

Any additional variables will be loaded and can be viewed but will not be used in any of the analyses. 

### Input 2 (optional): Disturbances and conservation areas

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
