## Help: Home ranges

The concept of an animal's home range, defined as the area traversed by an individual during its normal activities of food gathering, mating, and caring for young, forms a cornerstone of ecological research. In this section of the app, we provide a simple interface to allow users to estimate seasonal and annual home ranges using several approaches described in the literature (ref).

### Estimators

The following home range estimators are currently available:

- MCP: Minimum convex polygon
- KDE: Kernel density estimator
- aKDE: Autocorrelated KDE

### Isopleth level

- For MCP estimates, indicates the proportion of relocations that should be within the home range. For example, 0.95 would include 95% of relocations.
- For KDE estimates, represents the probability of an animal's location within its home range. For example, 0.95 would indicate that there is a 95% probability of finding the animal at any random point in time within the study period.

### Common isopleth levels and their ecological interpretations

| Isopleth level | Ecological interpretation | Common applications |
| :---- | :---- | :---- |
| 50% | Core Area, Area of Highest Use Intensity | Identifying primary foraging or resting sites, Comparing core area size across individuals or conditions |
| 75% | Secondary Area of High Use | Investigating patterns of resource use beyond the core |
| 90% | Overall Home Range Extent (Alternative) | Estimating the total area regularly used, Assessing habitat connectivity |
| 95% | Overall Home Range Extent (Common Standard) | Defining the boundary of the home range for comparisons, Conservation planning |
| 99% | Maximum Extent of Use, Including Occasional Excursions | Identifying rare or exploratory movements |

### Common methods for generating home range isopleths

| Method | Type | Key principles | Advantages | Limitations |
| :---- | :---- | :---- | :---- | :---- |
| Minimum Convex Polygon (MCP) | Geometric | Creates the smallest convex polygon enclosing all location points. | Simple to calculate and interpret, historically used. | Overestimates home range size, sensitive to outliers, no information on intensity of use. |
| Kernel Density Estimation (KDE) | Non-parametric | Places a kernel function over each location point and sums them to estimate probability density. | Can estimate complex shapes, provides a continuous utilization distribution. | Sensitive to bandwidth selection, potential for overestimation, assumes data independence. |
| Autocorrelated Kernel Density Estimation (aKDE) | Non-parametric | Modifies KDE to account for temporal autocorrelation. | More robust for serially correlated data. | Still requires bandwidth selection. |
| Local Convex Hull (LoCoH) | Non-parametric | Constructs overlapping local convex hulls based on subsets of data points. | Handles complex shapes and boundaries well, potentially more accurate than KDE. | Parameter selection can be influential. |
| Brownian Bridge Movement Model (BBMM) | Probabilistic | Incorporates movement path and temporal autocorrelation. | Suitable for GPS data, accounts for movement patterns. | Can be computationally intensive. |
