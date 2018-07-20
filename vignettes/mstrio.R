## ----include=FALSE-------------------------------------------------------
library(knitr)
library(mstrio)

## ----tidy=TRUE-----------------------------------------------------------
library(mstrio)

conn <- connect_mstr(base_url = 'https://env-93760.customer.cloud.microstrategy.com/MicroStrategyLibrary/api', 
                     username = 'mstr', 
                     password = 'C7IdyxKLEcgA', 
                     project_name = 'MicroStrategy Tutorial')

## ------------------------------------------------------------------------
cube_data <- get_cube(connection = conn, cube_id = '5E2501A411E8756818A50080EF4524C9')
knitr::kable(head(cube_data))

## ------------------------------------------------------------------------
report_data <- get_report(connection = conn, report_id = '873CD58E11E8772BA1CD0080EF05B984')
knitr::kable(head(cube_data))

## ------------------------------------------------------------------------
dat <- iris[1:50, ]
names(dat) <- gsub("[[:punct:]]", "_", names(dat))  ## mstr doesn't permit "." in column name
dat$ID <- row.names(dat)
newcube <- create_dataset(connection = conn,
                          data_frame = dat, 
                          dataset_name = "IRIS_Upload", 
                          table_name = "IRIS_Upload",
                          to_attribute = c('ID'))
newcube

## ------------------------------------------------------------------------
dat <- iris[51:150, ]
names(dat) <- gsub("[[:punct:]]", "_", names(dat))  ## mstr doesn't permit "." in column name
dat$ID <- row.names(dat)
update_dataset(connection = conn,
               data_frame = dat,
               dataset_id = newcube$datasetID,
               table_name = newcube$name,
               update_policy = 'add')

## ------------------------------------------------------------------------
dat <- iris
names(dat) <- gsub("[[:punct:]]", "_", names(dat))  ## mstr doesn't like "." in column name

dat$integer_attribute <- as.integer(row.names(dat))
dat$integer_metric <- row.names(dat)
str(dat)

newcube <- create_dataset(connection = conn, 
                          data_frame = dat, 
                          dataset_name = "IRIS", 
                          table_name = "IRIS_Upload",
                          to_metric = c("integer_metric"), 
                          to_attribute = c("integer_attribute"))

## ------------------------------------------------------------------------
close(connection = conn)

