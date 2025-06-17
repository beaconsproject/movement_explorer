## Help: Movement paths

Movement paths (corridors) can be estimated for individual or groups of individuals using one of more years of data using the interface elements (Select individual, Movement period, Select year(s)) in the sidebar. Currently, only "Spring" and "Fall" migration periods are permitted. By clicking on the Map corridor button, the app calculates and displays the likely paths that animals take during their spring and fall migrations. These paths are the estimated movement paths (corridors) used by the selected individual(s) for the selected time period. The computation may take a bit of time depending on the amount of input data or caribou relocations that we have available. Once the computation has completed, several map layers will be shown in the map and associated legend, including the estimated movement paths (corridors) as well as the underlying GPS data and trajectories.

### Methods for generating movement paths

Currently, one method is used to generate movement paths.

| Method | Type | Key principles | Advantages | Limitations |
| :---- | :---- | :---- | :---- | :---- |
| Brownian Bridge Movement Model (BBMM) | Probabilistic | Incorporates movement path and temporal autocorrelation. | Suitable for GPS data, accounts for movement patterns. | Can be computationally intensive. |
