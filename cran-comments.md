## Submission details
This submission contains enhancements to the package and an interactive web application for RStudio that is accessed by the RStudio 'Addins' menu and is rendered within the RStudio 'Viewer' pane.


## Test environments
* local OS macOS 10, R 3.6.1
* local OS Windows 10, R 3.6.1?
* devtools::check_win_devel()
* devtools::check_winoldrelease()
* devtools::check_win_release()


## R CMD check results

0 errors | 0 warnings | 2 notes

There were no errors or warnings. There were 2 notes.

* checking R code for possible problems ... NOTE
  Found the following assignments to the global environment:
  File ‘mstrio/R/utils-fetching.R’:
    assign(x = datasetName, value = dataset, envir = .GlobalEnv)
  File ‘mstrio/R/utils-helpers.R’:
    assign(dataframe_name, stats::setNames(dataset, proper_columns), 
      .GlobalEnv)
  File ‘mstrio/R/utils-update.R’:
    assign(df_name, stats::setNames(get(df_name, .GlobalEnv), new_names), 
      .GlobalEnv)
    assign(df_name, arrange.col(df, instr), .GlobalEnv)

This package contains an RStudio add-in that operates within the RStudio viewer pane. End-users use the add-in to select data frames from the parent R environment and interact with them using the add-in. The functions below provide interactivity between the parent R environment and the RStudio add-in.


* checking installed package size ... NOTE
  installed size is 14.2Mb
  sub-directories of 1Mb or more:
    testdata   4.0Mb
    www        9.8Mb

The directory 'www' contains compiled and minified components needed by the aforementioned RStudio application along with dependencies. Files have been appropriately minified. The directory 'testdata' contains data used for testing and demonstration.


## R CMD check results from devtools::check_win_release(), devtools::check_winoldrelease(), and devtools::check_win_devel():

0 errors | 0 warnings | 2 notes

There were no errors or warnings. There were 2 notes which were identical to the aforementioned ones.
