## User guide

Understanding how animals move and migrate is important for conservation and for assessing the effects of human activities and climate change. The Movement Explorer app is a tool designed to visualize and analyze animal movement data, helping us uncover some of these patterns.The app is divided into 5 sections.

### 1. Home

The "Home" section includes a description of the app, a user guide, and a description of the datasets required to use the app. The user guide provides a step-by-step description on how to use the app while the dataset tab describes the format and attributes of the input files. Currently, there are two main input files. The first is a geopackage file with a minimum of two attributes: an individual id and a timestamp for each GPS relocations. The second file is a simple CSV file that identifies the start and end dates for different seasons and migration periods. We'll see what they both look like in the next section.

### 2. Select data

- Now we're ready to proceed. This "Select data" section is where you upload your data. As long as your data is formatted correctly, uploading it into the app is straightforward. As we just mentioned, the app needs two main files:
  - A file containing the animal's location data at different times, similar to a GPS track. This file should have information that identifies each animal and a timestamp for each location. We can select the GPKG file we want by clicking on the Browse button. Once loaded the box below will list available map layers.
  - The input we need is a file that specifies the start and end dates for different seasons or migration periods. It's a simple CSV file. Again, we can select it using the Browse button.
- Now that we've selected our files, we just need to click on the Load data button. Once the data is loaded it will be displayed in the Movement data and Segmentation data tabs. As you can see the movement data contains the two required fields as well as some additional ones. Likewise, we can preview the segmentation data. Here is contains the start and end dates for four seasons (Early winter, Later winter, Summer, and Fall) as well as two migration periods: Spring and Fall.
- The last two tabs on the right provide information on sampling duration and rates. The Sampling duration tab shows, for each individual caribou, the length of time that it has been monitored. Here, we can see that 14 of the 25 caribou have been monitored continuously from late 2020 to early 2025. The Sampling rates tab provides additional sampling information such as the average length of time between relocation. For these data the median time interval between GPS relocations for all individuals is approximately 6 hours.

### 3. Map corridors

- Once you've defined the migration periods, we can move to the "Map corridors" section of the app. By clicking on the Run button, the app calculates and displays the likely paths that animals take during their spring and fall migrations. These paths are the migration corridors. The computation may take a bit of time depending on the amount of input data or caribou relocations that we have available. In our case, it should be relatively quick since we are only looking at one individual in one year. Once the computation has completed, several map layers will be shown in the map and associated legend, including the spring and fall estimated migration corridors as well as the underlying GPS data and trajectories.
- If we want we can return to the previous section to adjust the start and end periods and/or change the range of years we want to use. Once we're satisfied with the results we can save the corridors to a file geopackage by selecting the save button. Now you can return to the previous section and repeat the procedure for another individual.

### 4. Download data
