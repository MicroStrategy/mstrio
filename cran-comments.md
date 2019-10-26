## Submission details
This submission contains enhancements to the package and an interactive web application for RStudio that is accessed by the RStudio 'Addins' menu and is rendered within the RStudio 'Viewer' pane.


## Test environments
* local OS macOS 10, R 3.6.1
* local OS Windows 10, R 3.6.1
* devtools::check_win_devel()
* devtools::check_win_oldrelease()
* devtools::check_win_release()


## R CMD check results

0 errors | 0 warnings | 1 note

There were no errors or warnings. There was 1 note.

* checking installed package size ... NOTE
  installed size is 14.2Mb
  sub-directories of 1Mb or more:
    testdata   4.0Mb
    www        9.8Mb

The directory 'www' contains compiled and minified components needed by the aforementioned RStudio application along with dependencies. Files have been appropriately minified. The directory 'testdata' contains data used for testing and demonstration.


## R CMD check results from devtools::check_win_release(), devtools::check_win_oldrelease(), and devtools::check_win_devel():

0 errors | 0 warnings | 1 notes

There were no errors or warnings. There was 1 note which was identical to the aforementioned one.
