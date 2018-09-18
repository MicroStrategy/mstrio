## Resubmission
This is a resubmission of the package 'mstrio' version 10.11.0. This version, 10.11.1, includes a fix for an error that occurred when the package vignette was re-built from source during periodic code-quality checks on CRAN.

The error was due to the temporary unavailability of a web service which was called by code within the vignette. The vignette has been re-written to call the web service on a demo environment that is permanently available. The underlying issue should thus not re-occur in future code-quality checks by CRAN.


## Test environments
* local OS X install, R 3.4.0
* devtools::build_win(version='R-release')
* devtools::build_win(version='R-devel')

## R CMD check results

There were no ERRORs and no WARNINGs and no NOTEs.


## R CMD check results from devtools::build_win(version='R-devel') and devtools::build_win(version='R-release')

There were no ERRORs and no WARNINGs and 1 NOTE.

The NOTE is related to the aforementioned web service availability issue which has been addressed:

checking CRAN incoming feasibility ... NOTE

New submission

Package was archived on CRAN

