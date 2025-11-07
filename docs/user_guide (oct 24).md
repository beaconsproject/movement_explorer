## User guide

The BEACONs Movement Explorer app is designed to assist users to visualize and analyze animal movement data, helping to better understand animal space use, including how they move between seasonal ranges. The app is divided into several sections as indicated in the left sidebar. A typical workflow involves 1) selecting data, exploring data visually, estimating home ranges, and identifying and mapping corridors.

The **Welcome** section includes an **Overview** of the app, a **User guide** (this section), a description of the **Datasets requirements** to use the app, and sections describing how to **Estimate ranges** and **Identify corridors**. It is highly recommended that a user first read these sections, especially the **Datasets requirements** section prior to using the app.

### Step 1. Select data

The **Select data** section is where you upload the data that you would like to view and analyse. You have two options: you can use the demo data that comes with the app or you can upload your own data.

Option 1 - To use the demo data, simply select the "Use demo dataset" radiobutton and then click on the "Load data" button.

Option 2 - To use your own data, first make sure that your data is formatted correctly as described in the **Datasets requirements** section. If that's the case, select the "Upload your own data" radiobutton. You will now be presented with a few options.

-   Movement data (required). Click on this button to upload your own animal movement data.

-   Source of season/migration data

    -   Option1. Included in uploaded CSV. Select this option if the "season" and "migration" attributes are already included with the movement data.

    -   Option 2. From external file. Select this option if the "season" and "migration" attributes are not included. Once you select this radiobutton, you will be able to load a simple "Season and migration file" that will be merged to your movement data. The format of this file is described in te **Datasets requirements** section.

-   Disturbance data (optional). Click on the "Browse..." button to upload a geopackage file containing disturbance and conservation data layers. Note, that you can use the [BEACONs Geopackage Creator](https://beaconsproject.shinyapps.io/geopackage_creator/) app to create this dataset.

After the files have been selected, click on the **Load data** button. Once the data are loaded, you can view some summary information by navigating to the three tabs on the right:

-   The **Movement data** tab displays the movement data that was uploaded.
-   The **Sampling duration** tab plots a graph showing, for each individual animal, the length of time that its location was recorded.
-   The **Sampling rates** tab displays descriptive statistics of sampling rates for each individual animal such as the average length of time between relocation.

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