## User guide

The BEACONs **Movement Explorer** app assists users with visualizing and analyzing animal movement data to better understand how animals use the landscape, including movement between seasonal ranges. The app is divided into six sections as listed on the left sidebar, in the order of a typical workflow: select data, explore data visually, estimate annual and seasonal ranges, identify movement corridors, and download results.

The **Welcome** section includes the **Overview** landing page that provides a concise introduction to the app and its functionality. It also includes this **User guide** and a description of the **Datasets requirements** used by Movement Explorer. It is recommended that a user first read these sections, especially the **Dataset requirements** prior to using the app.

### Step 1. Select data

In this step, the user uploads spatial data into **Movement Explorer**. There are two options: 

1. **Use demo data** - This dataset is embedded in the App. The dataset includes multi-year location data with season and migration assignments for 24 individuals, as well as spatial datasets for linear and areal datasets of human disturbance, human footprint, intact forest landscapes, fire, and quartz claims. 

2. **Upload your own data** - If selected, the menu will expand.

    📌 Users are responsible for ensuring that all data satisfies the specification detailed in the **Dataset requirements** tab.

    **Movement data (csv)** (see Dataset requirements - Input 1) - Click on "Browse", navigate to the csv file with the animal movement (or location) data, and click "Open".

    **Source of season/migration data** (see Dataset requirements - Input 2) - If the **Movement data** includes the attributes "season" and "migration", select **Included in uploaded csv**. If not, select **From external file** and browse to **Season and migration csv file** and click "Open". This text file contains the start and end dates for each season and migration period. The App will use this file to assign each location in the **Movement data** to a season and/or migration period.   

    **Disturbance data (gpkg)** (see Dataset requirements - Input 3) - This dataset is optional and contains spatial layers in a GeoPackage. Click on "Browse", navigate to the gpkg file, and click "Open".

    After option 1 or 2 above has been completed, click on the **Load data** button. Once the data are loaded, summary information for the **Movement data** will appear in the three tabs on the right:

    - **Movement data** is a table of the movement data. Most attributes are defined in **Data requirements - Input 1**. **yday** is the day of the year out of 365. For example, April 1 would be the 91st day of the year.
    - **Sampling duration** is a graph plotting the length of time data was collected for each individual in the dataset. 
    - **Sampling rates** displays descriptive statistics of sampling rates for each individual animal such as the average length of time between relocation.

### Step 2. Explore data

The **Explore data** section allows the user to view the movement data interactively along with some optional underlying disturbance and conservation datasets described in the **Datasets requirements** help tab. Three filters are provided that allow the user to select individuals, seasons, and a range of years. The **Mapview** tab includes an interactive map displaying GPS relocations for all animals in the study area.

### Step 3. Estimate ranges

Seasonal and annual home ranges can be estimated for individual or groups of individuals using one of more years of data using the interface elements (Select individual, Select season, Select year(s)) in the sidebar. Three Estimator methods are currently available: minimum convex polygons (MCP), kernel density estimates (KDE), and autocorrelated KDEs. Isopleth levels are set to 0.5 and 0.95 but can be modified using the slider. The KDE bandwidth can only be set for the KDE estimator method. To estimate and visualize the home ranges just click on the **Calculate HRs** button. The map on the right will display two home range boundaries based on the user-selected method and isopleth levels. The underlying points and trajectories can also be viewed. Additionally, disturbances and conservation areas are displayed. Not all layers are turned on, but these can be turned on and off by selecting them in the legend.

You can click on the **Save home ranges** button at any time to download the estimated home ranges. After you click the button, you can select a folder and filename of your choice.

### Step 4. Identify corridors

Movement paths (corridors) can be estimated for individual or groups of individuals using one of more years of data using the interface elements (Select individual, Movement period, Select year(s)) in the sidebar. Currently, only "Spring" and "Fall" migration periods are permitted. By clicking on the **Map corridor** button, the app calculates and displays the likely paths that animals take during their spring and fall migrations. These paths are the estimated movement paths (corridors) used by the selected individual(s) for the selected time period. The computation may take a bit of time depending on the amount of input data or caribou relocations that we have available. Once the computation has completed, several map layers will be shown in the map and associated legend, including the estimated movement paths (corridors) as well as the underlying GPS data and trajectories.

You can click on the **Save movement paths** button at any time to download the estimated home ranges. After you click the button, you can select a folder and filename of your choice.

## Download data

The Download data section consists of a single button located in the left sidebar. By clicking on the button, a dialog will open allowing you to save the range and corridor boundaries that were produced in the previous sections into a single geopackage file. In addition, all disturbance and conservation layers will also be saved into that same file. The geopackage file can then be openned in a GIS (e.g., QGIS) or the BEACONs Disturbance Explorer app for further analysis.
