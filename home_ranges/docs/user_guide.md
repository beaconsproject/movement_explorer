## User guide

Understanding how animals move and migrate is important for conservation and for assessing the effects of human activities and climate change. The Movement Explorer app is a tool designed to visualize and analyze animal movement data, helping us uncover some of these patterns. The app is divided into 5 sections.

### 1. Home

The "Home" section includes a description of the app, a user guide, and a description of the datasets required to use the app. The user guide provides a step-by-step description on how to use the app while the dataset tab describes the format and attributes of the input files. Currently, there are two main input files. The first is a geopackage file with a minimum of two attributes: an individual id and a timestamp for each GPS relocations. The second file is a simple CSV file that identifies the start and end dates for different seasons and migration periods. We'll see what they both look like in the next section.

### 2. Select data

- Now we're ready to proceed. This "Select data" section is where you upload your data. As long as your data is formatted correctly, uploading it into the app is straightforward. As we just mentioned, the app needs two main files:
  - A file containing the animal's location data at different times, similar to a GPS track. This file should have information that identifies each animal and a timestamp for each location. We can select the GPKG file we want by clicking on the Browse button. Once loaded the box below will list available map layers.
  - The input we need is a file that specifies the start and end dates for different seasons or migration periods. It's a simple CSV file. Again, we can select it using the Browse button.
- Now that we've selected our files, we just need to click on the Load data button. Once the data is loaded it will be displayed in the Movement data and Segmentation data tabs. As you can see the movement data contains the two required fields as well as some additional ones. Likewise, we can preview the segmentation data. Here is contains the start and end dates for four seasons (Early winter, Later winter, Summer, and Fall) as well as two migration periods: Spring and Fall.
- The last two tabs on the right provide information on sampling duration and rates. The Sampling duration tab shows, for each individual caribou, the length of time that it has been monitored. Here, we can see that 14 of the 25 caribou have been monitored continuously from late 2020 to early 2025. The Sampling rates tab provides additional sampling information such as the average length of time between relocation. For these data the median time interval between GPS relocations for all individuals is approximately 6 hours.

### 3. View trajectories

- Now that we've loaded our data, we can go to the "View trajectories" section to visualize the caribou movement data. Here, we need to select an individual caribou (or all caribou) as well as a range of years we want to view. We can then click on the Map button to view a map showing caribou locations, trajectories, and home ranges. Not all layers are turned on, but these can be turned on and off by selecting them in the legend. As you can see there are two types of home range estimators displayed, the MCP estimator and the KDE estimator. Additional layers, such as human disturbances or wildfires, can be also overlaid on the map. The box on the right provides statistics such as the number of GPS relocations and the estimated area of the 2 different home range estimators in km2.

## Future enhancements

Currently, the app focuses on mapping seasonal movement corridors for one individual based on one or more years of data. However, plans are in place to expand its capabilities to analyze movement patterns for entire populations. For now, once segments have been defined for all individuals, this could need to be done outside of the app using R or Python. An example of the output can be seen in the accompanying poster.

### Definitions

**Range distribution**

Two methods are used to estimate range distributions:

- MCP: minimum convex polygon
- KDE: kernel density estimator

**Occurrence distribution**

Two methods are used to estimate occurrence distributions:

- Buffer: Line buffer method (DeCesare et al. 2012)
- BBMM: Brownian bridge movement model
- DBBMM: Dynamic brownian bridge movement model
- Kernel: Bivariate normal kernel

**Migration corridor**

Migration corridors can be estimated using a simple buffering approach or one of the occurrence estimates

**High use areas**

