## mstrio 11.2.2
### Major changes
* improved performance for downloading Reports/cubes with view filter
* automatically remove the `Row Count` column from the imported datasets
* extend `Connection` class with the `identity_token` param to allow for delegated authentication
* added support for operator `NotIn` in view filter
* added `instance_id` parameter in the `Report` / `Cube` constructors to utilize existing instance on I-Server
* added new methods to `Cube` class: `update` and `save_as`
* refactored `Connection` class into `R6` format
* documented all public methods with Roxygen
* generate R code while importing / exporting Datasets in MicroStrategy for RStudio
* improved overall user experience in the GUI

### Bug fixes
* fixed various UI defects
* fixed the verbosity of the dataset class to allow for better communication

## mstrio 11.2.1
### Major changes
* introduced functionality for updating existing Cubes
* improved fetching performance by up to 50%
* added support for cross-tabbed Reports
* added support for Reports with subtotals
* added basic support for Reports with attribute forms
* extended `Dataset` class with the `certify()` method
* implemented asynchronous download of Cubes and Reports
* applied revamped MicroStrategy REST API import-related endpoints
* reworked GUI’s data modeling functionality

### Bug fixes
* fixed issues with Cube / Report filtering during import
* improved user experience for the GUI's login page
* added handling of various forms of environment's base URL
* resolved issues with importing / exporting datasets containing special characters


## mstrio 11.2.0
* optimized downloading speed for filtered Reports
* improved performance when downloading unfiltered Cubes / Reports
* improved performance when filtering by attributes and metrics
* added `Filter` class for checking the validity of selected objects


## mstrio 11.1.4
### Major changes
* added `Cube` and `Report` classes to provide more flexibility when interacting with Cubes and Reports. These new classes provide the ability to select attributes, metrics, and attribute elements before importing them to R as Data Frames
* added `Dataset` class that allows defining and creating multi-table Cubes from multiple Data Frames, with improved data upload scalability, and the ability to define the Dataset within a specific folder
* introduced graphical user interface to access the MicroStrategy environment using interactive RStudio addin

### Bug fixes
* ensured session cookies are passed when closing the connection


## mstrio 10.11.1
### Bug fixes
* addressed reproducibility of vignette in CRAN build process


## mstrio 10.11.0
* initial CRAN release (2 August 2018)
