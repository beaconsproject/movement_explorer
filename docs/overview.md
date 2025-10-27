## Welcome to Movement Explorer

Understanding how animals use landscapes is crucial for understanding their response to environmental change and land stewardship. To that end, BEACONs Movement Explorer allows users to visually explore animal locations and identify annual and seasonal ranges, and seasonal movement corridors (paths). A built-in **User guide** tab provides step-by-step instructions and function descriptions, while the **Dataset requirements** tab details data formats and spatial layers needed to run the app.

<br>
<div align="center">

![gabe1](https://github.com/user-attachments/assets/5415f08d-628b-46dd-a9d9-45dd83944907)

Photo credit: Gabriel Rivest

</div>
<br>

📌 Note: **Movement Explorer** is intended to provide users with a simple way to visualize movement data and explore patterns e.g., movement corridors and high use areas. It is not intended to be used for data preparation and cleaning, nor for complex statistical analysis and modelling.

### Input data

**Movement Explorer** requires a text file (csv format) containing animal movement data and a GeoPackage file (gpkg format) containing spatial layers for display and exploration (e.g., human disturbance, fire, *etc.*).  A demo dataset is included with the app for the Little Rancheria caribou herd located in southeast Yukon and northcentral BC. Alternatively, users may upload their own data, provided it follows the structure described in the **Dataset requirements** tab.

### Functionality

The app consists of several sections:

#### Select data

- Use the embedded demo dataset or upload custom data (i.e., csv text file and GeoPackage described above).

#### Explore data

- View and explore the animal movement data along with disturbance data (e.g., human disturbances and fires) and other spatial layers (e.g., protected areas and intact forest landscapes). Movement data can be filtered by animal Id, Year, and Season.

#### Estimate ranges

- Estimate annual or seasonal ranges (e.g., summer and winter) for individual animals or for the population using one of several methods: Minimum Convex Polygon (MCP), Kernel Density Estimation (KDE), Autocorrelated KDE (aKDE), or Local Convex Hull (LoCoH).

#### Identify corridors

- Identify seasonal movement corridors (e.g., spring and fall) for individual animals or for the population using one of several methods: Line buffer, Brownian bridge movement model (BBMM), or a mixed approach.

#### Download data

- Download a GeoPackage of the seasonal and home ranges and movement corridor boundaries created by the app, as well as the input spatial layers (e.g., linear and areal disturbances).
