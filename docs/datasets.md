## Dataset requirements

The Movement Explorer comes with a demo dataset that can be used to explore the functionality of the app. Alternatively, you can upload your own data, which should consist of two input files:

### Input 1: Animal movement data

Format:
- Text file - comma separated values (".csv")

Required attributes:
- **id**: Individual identification e.g., collar id
- **timestamp**: a timestamp indicating the year, month, day, hour, minute, second
- **longitude**: longitude (crs:4326)
- **latitude**: latitude (crs:4326)
- **elevation**: elevation in metres
- **season**: name of season or migration period

Any additional variables will be loaded and can be viewed but will not be used in any of the analyses. 

### Input 2: Disturbances and conservation areas

The second input can be generated from the Geopackage Creator app.

Format: 
- Geopackage file (".gpkg")

Required layers:

- **studyarea**: boundary of study region (polygon)
- **linear_disturbance**: anthropogenic linear disturbances (lines)
- **areal_disturbance**: anthropogenic areal disturbances (polygons)
- **fire**: fire polygons (polygons)
- **footprint_500m**: combined linear and areal disturbances buffered by 500m (polygon)
- **protected_areas**: protected and conserved areas, including IPCAs (polygons)
