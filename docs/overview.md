## Welcome to Movement Explorer

Understanding how animals use space and migrate is crucial for conservation and for understanding the effects of environmental changes. To that end, BEACONs **Movement Explorer** was designed to enable users to visualize animal locations and trajectories, estimate annual and seasonal ranges, and identify seasonal movement corridors (paths). A built-in ***User guide*** tab provides step-by-step instructions and function descriptions, while the ***Dataset requirements*** tab details data formats and spatial layers needed to run the app.

<br>
<div align="center">

![gabe1](https://github.com/user-attachments/assets/5415f08d-628b-46dd-a9d9-45dd83944907)

</div>
<br>



📌 Note: **Movement Explorer** is intended to provide users with a simple way to visualize movement data and explore patterns e.g., movement corridors and high use areas. It is not intended to be used for data preparation and cleaning, nor for complex statistical analysis and modelling.

### Input data

**Movement Explorer** relies on several key spatial layers contained within two files: a text file ("csv" format) containing animal movement data and a GeoPackage file ("gpkg" format) containing disturbance and conservation datasets. A demo dataset is included with the app for the Little Rancheria herd located in southeast Yukon and northcentral BC. Alternatively, users may upload their own data, provided it follows the structure described in the ***Dataset requirements*** tab.

### Functionality

The app consists of several sections:

#### Select data

- Use the builtin demo data or upload a custom data ("csv" and "gpkg" extensions) contabuiltin ining all required spatial layers.

📌 Note: All layers in the GeoPackage must have the same projection. Additionally, the study area must capture the full extent of disturbance layers to ensure accurate analysis.
<br>

#### Explore data

- View and explore the animal movement data along with disturbance data (e.g., human-caused disturbances and fires) and conservation data (e.g., protected areas and intact forest landscapes). Movement data can be filtered animal Id, Year, or Season.

#### Estimate ranges

- Estimate annual or seasonal ranges (summer and winter) for individual animals or for the population using one of several methods: Minimum Convex Polygon (MCP), Kernel Density Estimation (KDE), Autocorrelated KDE (aKDE), or Local Convex Hull (LoCoH).

#### Identify corridors

- Identify seasonal movement corridors (spring and fall) for individual animals or for the population using one of several methods: Line buffer, Brownian bridge movement model (BBMM), or a mixed approach.

#### Download data

- Download a GeoPackage of the range and corridor boundaries created by the app, as well as the input spatial layers (e.g., linear and areal disturbances).
