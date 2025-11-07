## Welcome to Movement Explorer


<br><br>
<center><img src="https://github.com/user-attachments/assets/5415f08d-628b-46dd-a9d9-45dd83944907"></center>
<br><br>

Photo credit: Gabriel Rivest

<br>

📌 Note: **Movement Explorer** is intended to provide users with a simple way to visualize movement data and explore patterns e.g., movement corridors and high use areas. It is not intended to be used for data preparation and cleaning, nor for complex statistical analysis and modelling.

### Input data

**Movement Explorer** requires a text file ("csv" format) containing animal movement data and a GeoPackage ("gpkg" format) containing spatial layers for display and exploration (e.g., human disturbance, fire, etc.). A demo dataset is included with the app for the Little Rancheria caribou herd located in southeast Yukon and northcentral BC. Alternatively, users may upload their own data, provided it follows the structure described in the ***Dataset requirements*** tab.

### Functionality

The app consists of several sections:

#### Select data

- Use the embedded demo dataset or upload custom data (i.e., text file of location data and GeoPackage containing all spatial layers).

📌 Note: All spatial layers in the GeoPackage must have the same projection.
<br>

#### Explore data

- View and explore the animal movement data along with disturbance data (e.g., human-caused disturbances and fires) and other spatial layers (e.g., protected areas and intact forest landscapes). Movement data can be filtered animal Id, Year, and Season.

#### Estimate ranges

- Estimate annual or seasonal ranges (e.g., summer and winter) for individual animals or for the population using one of several methods: Minimum Convex Polygon (MCP), Kernel Density Estimation (KDE), Autocorrelated KDE (aKDE), or Local Convex Hull (LoCoH).

#### Identify corridors

- Identify seasonal movement corridors (e.g., spring and fall) for individual animals or for the population using one of several methods: Line buffer, Brownian bridge movement model (BBMM), or a mixed approach.

#### Download data

- Download a GeoPackage containig the seasonal and home ranges and movement corridor boundaries created by the app, as well as the input spatial layers (e.g., linear and areal disturbances).
