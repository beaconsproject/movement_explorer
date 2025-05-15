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