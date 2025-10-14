## Identifying and mapping seasonal corridors

Movement paths (corridors) can be estimated for individual or groups of individuals using one of more years of data using the interface elements (Select individual, Movement period, Select year(s)) in the sidebar. Currently, only "Spring" and "Fall" migration periods are permitted. By clicking on the Map corridor button, the app calculates and displays the likely paths that animals take during their spring and fall migrations. These paths are the estimated movement paths (corridors) used by the selected individual(s) for the selected time period. The computation may take a bit of time depending on the amount of input data or caribou relocations that we have available. Once the computation has completed, several map layers will be shown in the map and associated legend, including the estimated movement paths (corridors) as well as the underlying GPS data and trajectories.

### Methods for generating movement paths

Three methods are available to generate movement paths at the individual-level but only the line buffer method can be used at the population-level due to speed constraints. **To do: Provide some R code to run all methods at population-level.**

- **Line buffer method**: This is the simplest and quickest method and simply consists of buffering the movement tracks by a user-selected buffer distance.
- **Brownian bridge movement model (BBMM)**: This is a probabilistic method that incorporates movement path and temporal autocorrelation but can be computationally intensive. It can also result in breaks when relocations are few and far apart.
- **Mixed approach**: As its name suggests, this is a combination of the two methods. Essentially, it is the BBMM method with gaps filled by the line buffer method.

### References

