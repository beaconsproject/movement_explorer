## User guide

The Home Range Explorer app is designed to assist users to explore seasonal and annual home ranges using several commonly used estimators, including minimum convex hull (MCP) and kernel density estimation (KDE). The app is divided into 4 sections.

### 1. Home

The "Home" section includes an **Overview** of the app, a **User guide** (this section), and a description of the **Datasets** required to use the app. It is highly recommended that a user first read these sections, especially the **Datasets** section prior to using the app.

### 2. Select data

The "Select data" section is where you upload the data that you would like to analyse. As long as your data is formatted correctly as described in the **Datasets** section, uploading it into the app is straightforward. To use you own dataset, select the "Upload your own data" radiobutton. If, on the other hand, you just want to test the app out without your own data, you can just select "Use demo dataset" radiobutton.

After the files have been selected, just click on the **Load data** button. Once the data are loaded they will be displayed in three tabs on the right:
  - Movement data
  - Segmentation data
  - Glimpse data

### 3. Home ranges

Now that we've loaded our data, we can go to the "Home ranges" section to estimate and visualize seasonal home range boundaries. Here, you will need to select an individual animal (or all animals), a season, and a range of years. You will then need to select two estimators from the list of estimators available, for example MCP and KDE. Finally, select the isopleth levels for both estimators (see Help tab). Now you can click on the "Calculate HRs" button to esimate and view a map showing caribou locations, trajectories, and estimated home ranges. Additionally, disturbances and conservation areas are displayed. Not all layers are turned on, but these can be turned on and off by selecting them in the legend. The "Statistics" box on the right provides statistics such as the number of GPS relocations and the estimated area of the 2 different home range estimators in km2.

### 4. Download data

The "Download data** section include one button ("Download HRs") which allow you to download the home range boundaries that are currently loaded in the map. These will be saved as two separate layers in a geopackage file. After you click the button, you can select a folder and filename of your choice.