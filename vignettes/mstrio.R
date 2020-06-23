## ---- include=FALSE------------------------------------------------------
library(mstrio)

## ---- include=FALSE------------------------------------------------------
username <- "demouser"
password <- "demopass"

## ------------------------------------------------------------------------
library(mstrio)

conn <- Connection$new(base_url = 'https://demo.microstrategy.com/MicroStrategyLibrary/api', 
                     username = username, 
                     password = password, 
                     project_name = 'MobileDossier',
                     login_mode = 8)

## ---- eval=FALSE---------------------------------------------------------
#  cube_data <- get_cube(connection = conn, cube_id = '5E2501A411E8756818A50080EF4524C9')

## ---- eval=FALSE---------------------------------------------------------
#  report_data <- get_report(connection = conn, report_id = '873CD58E11E8772BA1CD0080EF05B984')

## ---- eval=FALSE---------------------------------------------------------
#  dat <- iris[1:50, ]
#  
#  # note: column names in MicroStrategy cannot have a period (".")
#  names(dat) <- gsub("[[:punct:]]", "_", names(dat))
#  
#  # this creates the dataset, and returns the dataset ID and table ID which you'll use to update the dataset later
#  newcube <- create_dataset(connection = conn,
#                            data_frame = dat,
#                            dataset_name = "IRIS_Upload",
#                            table_name = "IRIS_Upload")

## ---- eval=FALSE---------------------------------------------------------
#  dat <- iris[51:150, ]
#  
#  # note: column names in MicroStrategy cannot have a period (".")
#  names(dat) <- gsub("[[:punct:]]", "_", names(dat))
#  
#  # this adds new data to the previously created dataset
#  update_dataset(connection = conn,
#                 data_frame = dat,
#                 dataset_id = newcube$datasetID,
#                 table_name = newcube$name,
#                 update_policy = 'add')

## ---- eval=FALSE---------------------------------------------------------
#  dat <- iris
#  
#  # note: column names in MicroStrategy cannot have a period (".")
#  names(dat) <- gsub("[[:punct:]]", "_", names(dat))
#  
#  # create two new columns - one formatted as a numeric, and another formatted as a string
#  dat$integer_attribute <- as.integer(row.names(dat))
#  dat$integer_metric <- row.names(dat)
#  
#  # create a new cube, but this time, specifically instructs MicroStrategy to reflect these two new columns as a metric and attribute, respectively
#  # you can map multiple columns at once with to_metric = c("A", "B", "C")
#  newcube <- create_dataset(connection = conn,
#                            data_frame = dat,
#                            dataset_name = "IRIS",
#                            table_name = "IRIS_Upload",
#                            to_metric = c("integer_metric"),
#                            to_attribute = c("integer_attribute"))

## ------------------------------------------------------------------------
close(connection = conn)

