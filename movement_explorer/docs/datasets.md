## Datasets

The Movement Explorer apps requires two input files: one containing animal movement data and the other identifying seasonal and migration periods.

### Input 1: Animal movement data

Format:
- Text file - comma separated values (".csv")

Required attributes:
- **id**: Individual identification e.g., collar id
- **time**: a timestamp indicating the year, month, day, hour, minute, second
- **long**: longitude (crs:4326)
- **lat**: latitude (crs:4326)

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
