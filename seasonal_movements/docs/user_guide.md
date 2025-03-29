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
### 4. Define segments

- The "Define segments" section is where you specify when the spring and fall migrations occur for each animal. We first select an individual and a range of years and then click on the Run button. The app provides plots that show the animal's movement speed and direction over time. These plots help identify the start and end points of migration periods. You can use a slider to adjust these points. Initially, they are set to the values that were uploaded in the segmentation data provided by the user.
- The larger plot on the left shows the distribution of relocations for that individual and range of years. The red points highlight those that are within the fall migration period. The three plots on the right provide information of the speed and direction of movement across time, from Jan 1 to Dec 31, measure as day of year from 1 to 365. The top 2 graphs show the relationship between longitude and latitude and day of year. Likewise, the bottom graph plots the net squared displacement (or NSD) over time. Higher NSD values indicate larger distances travelled over 6 hour periods. Together, these plots can help identify start and end times of migration periods. We can adjust the start and end times using the slider and this will change the vertical bars in the plots as well as the points highlighted in red.
- Once we are satisfied with our choices, we can move on to a different individual or the same individual for the Fall migration period.
### 5. Map corridors

- Once you've defined the migration periods, we can move to the "Map corridors" section of the app. By clicking on the Run button, the app calculates and displays the likely paths that animals take during their spring and fall migrations. These paths are the migration corridors. The computation may take a bit of time depending on the amount of input data or caribou relocations that we have available. In our case, it should be relatively quick since we are only looking at one individual in one year. Once the computation has completed, several map layers will be shown in the map and associated legend, including the spring and fall estimated migration corridors as well as the underlying GPS data and trajectories.
- If we want we can return to the previous section to adjust the start and end periods and/or change the range of years we want to use. Once we're satisfied with the results we can save the corridors to a file geopackage by selecting the save button. Now you can return to the previous section and repeat the procedure for another individual.

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

