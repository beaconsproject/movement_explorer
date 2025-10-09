## User guide

Understanding how animals move and migrate is important for conservation and for assessing the effects of human activities and climate change. The BEACONs Movement Explorer app is designed to assist users to visualize and analyze animal movement data, helping uncover some of these patterns. The app is divided into 6 sections as indicated in the left sidebar.

### 1. Welcome

The "Welcome" section includes an **Overview** of the app, a **User guide** (this section), a description of the **Datasets requirements** to use the app, and sections describing how to **Estimate ranges** and **Identify corridors**. It is highly recommended that a user first read these sections, especially the **Datasets requirements** section prior to using the app.

### 2. Select data

The "Select data" section is where you upload the data that you would like to view and analyse. As long as your data is formatted correctly as described in the **Datasets** section, uploading it into the app is straightfoward. To usata

e you own dataset, select the "Upload your own data" radiobutton. If, on the other hand, you just want to test the app out without your own data, you can just select "Use demo dataset" radiobutton.

After the files have been selected, just click on the **Load data** button. Once the data are loaded they will be displayed in the three tabs on the right:

#### Movement data tab

Displays the movement data that was uploaded.

#### Sampling duration tab

Plots a graph showing, for each individual animal, the length of time that its location was recorded.

#### Sampling rates tab

Displays descriptive statistics of sampling rates for each individual animal such as the average length of time between relocation.

### 3. Explore data

The **Explore data** section allows the user to view the movement data interactively along with some underlying disturbance and conservation datasets described in the **Datasets requirements** help tab. Three filters are provided allowing the user to select individuals, seasons, and a range of years.

#### Mapview tab

Interactive map displaying GPS relocations for all animals in the study area.

### 4. Estimate ranges

Seasonal and annual home ranges can be estimated for individual or groups of individuals using one of more years of data using the interface elements (**Select individual**, **Select season**, **Select year(s)**) in the sidebar. Three **Estimator methods** are currently available: minimum convex polygons (MCP), kernel density estimates (KDE), and autocorrelated KDEs. **Isopleth levels** are set to 0.5 and 0.95 but can be modified using the slider. The **KDE bandwidth** currently cannot be modified. To estimate and visualize the home ranges just click on the **Calculate HRs** button. The map on the right will display two home range boundaries based on the user-selected method and isopleth levels. The underlying points and trajectories can also be viewed. Additionally, disturbances and conservation areas are displayed. Not all layers are turned on, but these can be turned on and off by selecting them in the legend.

You can click on the **Save home ranges** button at any time to download the estimated home ranges. After you click the button, you can select a folder and filename of your choice.

### 5. Identify corridors

Movement paths (corridors) can be estimated for individual or groups of individuals using one of more years of data using the interface elements (**Select individual**, **Movement period**, **Select year(s)**) in the sidebar. Currently, only "Spring" and "Fall" migration periods are permitted. By clicking on the **Map corridor** button, the app calculates and displays the likely paths that animals take during their spring and fall migrations. These paths are the estimated movement paths (corridors) used by the selected individual(s) for the selected time period. The computation may take a bit of time depending on the amount of input data or caribou relocations that we have available. Once the computation has completed, several map layers will be shown in the map and associated legend, including the estimated movement paths (corridors) as well as the underlying GPS data and trajectories.

You can click on the **Save movement paths** button at any time to download the estimated home ranges. After you click the button, you can select a folder and filename of your choice.

### 6. Download data

The Download data section consists of a single button located in the left sidebar. By clicking on the button, a dialog will open allowing you to save the range and corridor boundaries that were produced in the previous sections into a single geopackage file. In addition, all disturbance and conservation layers will also be saved into that same file. The geopackage file can then be openned in a GIS (e.g., QGIS) or the BEACONs Disturbance Explorer app for further analysis.
