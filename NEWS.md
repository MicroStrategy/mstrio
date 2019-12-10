
## mstrio 11.2.1


## mstrio 11.2.0
* optimized downloading speed of filtered Reports
* improved performance when downloading unfiltered Cubes/Reports
* improved performance when filtering by attributes and metrics
* GUI bug fixes

## mstrio 11.1.4
### Major changes
* Cube and Report classes to provide more flexibility when interacting with cubes and reports. These new classes provide the ability to select attributes, metrics, and attribute elements before importing them to R as data frames
* Dataset class that allows defining and creating multi-table cubes from multiple data frames, with improved data upload scalability, and the ability to define the dataset within a specific folder
* Graphical user interface to access the MicroStrategy environment using interactive RStudio add-in
### Bug fixes
* Ensure session cookies are passed when closing the connection

## mstrio 10.11.1
### Bug fixes
* addressed reproducibility of vignette in CRAN build process

## mstrio 10.11.0 
* released to CRAN (2 August 2018)
