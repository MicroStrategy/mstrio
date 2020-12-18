## ---- include=FALSE-----------------------------------------------------------
library(mstrio)

## ---- include=FALSE-----------------------------------------------------------
username <- "demouser"
password <- "demopass"

## ---- eval=FALSE--------------------------------------------------------------
#  library(mstrio)
#  
#  conn <- Connection$new(base_url = 'https://demo.microstrategy.com/MicroStrategyLibrary/api',
#                         username = username,
#                         password = password,
#                         project_name = 'MobileDossier',
#                         login_mode = 8)

## ---- eval=FALSE--------------------------------------------------------------
#  cube <- Cube$new(connection = conn, cube_id = '5E2501A411E8756818A50080EF4524C9')
#  cube$to_dataframe()
#  cube_data <- cube$dataframe

## ---- eval=FALSE--------------------------------------------------------------
#  report <- Report$new(connection = conn, report_id = '873CD58E11E8772BA1CD0080EF05B984')
#  report$to_dataframe()
#  report_data <- report$dataframe

## ---- eval=FALSE--------------------------------------------------------------
#  dat <- iris[1:50, ]
#  
#  # note: column names in MicroStrategy cannot have a period (".")
#  names(dat) <- gsub("[[:punct:]]", "_", names(dat))
#  
#  # this creates the dataset, and returns the dataset object, which you can use to update the dataset later
#  my_dataset <- Dataset$new(connection=conn, name="IRIS_Upload")
#  
#  # add one or more tables to the dataset
#  my_dataset$add_table(name = "IRIS_Upload",
#                       data_frame = dat,
#                       update_policy = "add")
#  my_dataset$create()

## ---- eval=FALSE--------------------------------------------------------------
#  dat <- iris[51:150, ]
#  
#  # note: column names in MicroStrategy cannot have a period (".")
#  names(dat) <- gsub("[[:punct:]]", "_", names(dat))
#  
#  # Initialise the existing dataset using the `id` param.
#  my_dataset <- Dataset$new(connection = conn, id = my_dataset$dataset_id)
#  
#  # update one or more tables to the dataset
#  my_dataset$add_table(name = "IRIS_Upload",
#                       data_frame = dat,
#                       update_policy = "add")
#  
#  # push the new data to the MicroStrategy Environment
#  my_dataset$update(auto_publish=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  dat <- iris
#  
#  # note: column names in MicroStrategy cannot have a period (".")
#  names(dat) <- gsub("[[:punct:]]", "_", names(dat))
#  
#  # create two new columns - one formatted as a numeric, and another formatted as a string
#  dat$integer_attribute <- as.integer(row.names(dat))
#  dat$integer_metric <- row.names(dat)
#  
#  # create a new dataset, but this time, specifically instructs MicroStrategy to reflect these two new columns as a metric and attribute, respectively
#  # you can map multiple columns at once with to_metric = c("A", "B", "C")
#  new_dataset <- Dataset$new(connection=conn, name="IRIS")
#  my_dataset$add_table(name = "IRIS_Upload",
#                       data_frame = dat,
#                       update_policy = "add",
#                       to_metric = c("integer_metric"),
#                       to_attribute = c("integer_attribute"))
#  my_dataset$create()

## ---- eval=FALSE--------------------------------------------------------------
#  conn$close()

