# Changelog

**MicroStrategy for RStudio and mstrio for R are no longer supported and developed as of March 2024 and have been archived. You can still use the last available version.**

## mstrio 11.3.5.101

## 11.3.5.101 - 2022/03/25

### Minor changes

* term `application` renamed to `project` in RStudio connector
* enhancement - improved error handling while comparing
`NULL` to value (error messages)

### Bug fixes

* resolved Cube creation issue - added empty cookie handler for Cube

## mstrio 11.3.1

## 11.3.0.1 - 2020/12/18

### Major changes

#### R Code

* certify RStudio Server Open Source Edition 1.2
* added support for **proxy** configuration in `Connection` class

### Bug fixes

* improved GUI stability in Data Modelling
* improved Safari compatibility
* resolved import issues with OLAP cubes
* resolved edge case general issues in import and export

### Deprecated

* `add`, `update`, `upsert` update methods are not supported anymore when
  overwriting cube and will raise an error

## 11.2.2.1 - 2020/08/04

### Bug fixes

* fixed crucial compatibility issue with 11.1.x environments

## 11.2.2 - 2020/06/23

### Major changes

* improved performance for downloading Reports/cubes with view filter
* **generate R code** while importing / exporting Datasets in MicroStrategy for
  RStudio
* automatically remove the `Row Count` column from the imported datasets
* extend `Connection` class with the `identity_token` param to allow for delegated
  authentication
* added support for operator `NotIn` in view filter
* added `instance_id` parameter in the `Report` / `Cube` constructors to utilize
  existing instance on I-Server
* added new methods to `Cube` class: `update` and `save_as`
* refactored `Connection` class into `R6` format
* documented all public methods with Roxygen
* improved overall user experience in the GUI

### Bug fixes

* fixed various UI defects
* fixed the verbosity of the dataset class to allow for better communication

## 11.2.1 - 2020/03/26

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

## 11.2.0 - 2019/12/10

* optimized downloading speed for filtered Reports
* improved performance when downloading unfiltered Cubes / Reports
* improved performance when filtering by attributes and metrics
* added `Filter` class for checking the validity of selected objects

## 11.1.4 - 2019/10/28

### Major changes

* added `Cube` and `Report` classes to provide more flexibility when interacting
  with Cubes and Reports. These new classes provide the ability to select
  attributes, metrics, and attribute elements before importing them to R as Data
  Frames
* added `Dataset` class that allows defining and creating multi-table Cubes from
  multiple Data Frames, with improved data upload scalability, and the ability
  to define the Dataset within a specific folder
* introduced graphical user interface to access the MicroStrategy environment
  using interactive RStudio addin

### Bug fixes

* ensured session cookies are passed when closing the connection

## 10.11.1 - 2018/09/14

### Bug fixes

* addressed reproducibility of vignette in CRAN build process

## 10.11.0 - 2018/08/17

* initial CRAN release (2 August 2018)
