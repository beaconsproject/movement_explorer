## Datasets

The Movement Explorer app requires three input files: one containing animal movement data, the second identifying seasonal and migration periods, and the third disturbances and conservation areas. All required attribute and layer names are highlighted in bold letters.

### Input 1: Animal movement data

Format:
- Text file - comma separated values (".csv")

Required attributes:
- **id**: Individual identification e.g., collar id
- **time**: a timestamp indicating the year, month, day, hour, minute, second
- **long**: longitude (crs:4326)
- **lat**: latitude (crs:4326)
- **elev**: elevation in metres

Any additional variables will be loaded and can be viewed but will not be used in any of the analyses. 

### Input 2: Seasonal and migration periods

Format:
- Text file - comma separated values (".csv")

Required attributes:
- **season**: name of season or migration period
- **start**: start date in month-day e.g., Jun-16
- **end**: end date in month-day format e.g., Sep-14

Example table:

<pre>
|season           |start  |end    |
|-----------------|-------|-------|
|Annual           |Feb-01 |Jan-31 |
|Early winter     |Oct-21 |Jan-31 |
|Late winter      |Feb-01 |Apr-15 |
|Summer           |Jun-16 |Sep-14 |
|Fall rut         |Sep-15 |Oct-20 |
|Spring migration |Apr-01 |Jun-30 |
|Fall migration   |Sep-01 |Oct-31 |
</pre>

<br>

**Important note:** One of the rows of the table must contain an "Annual" season with start and end dates ranging 365 days. Depending on the study species and the seasons of interest, the start and end dates do not have to coincide with Jan-01 and Dec31, respectively. The idea is to avoid having a particular season or migratory period span 2 calendar years since this will cause issues with the date range slider.

### Input 3: Disturbances and conservation areas

The third input can be generated from the Geopackage Creator app.

Format: 
- Geopackage file (".gpkg")

Required layers:

- **studyarea**: boundary of study region (polygon)
- **linear_disturbance**: anthropogenic linear disturbances (lines)
- **areal_disturbance**: anthropogenic areal disturbances (polygons)
- **fire**: fire polygons (polygons)
- **footprint_500m**: combined linear and areal disturbances buffered by 500m (polygon)
- **protected_areas**: protected and conserved areas, including IPCAs (polygons)
