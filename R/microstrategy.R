# microstrategy.R
# Interface for connecting to the MicroStrategy REST API server, creating datasets, updating datasets, and extracting data from reports and cubes

#' @import httr
#' @importFrom jsonlite toJSON
#' @importFrom openssl base64_encode
#' @importFrom utils tail
#' @importFrom methods new

# TODO: Add validity check to connection object before making requests and optionally extend session token

#' @title Connection class
#'
#' @description Base S4 class object containing connection parameters
#' @slot username Username
#' @slot password Password
#' @slot base_url URL for the REST API server
#' @slot project_name Name of the project to connect to (e.g. "MicroStrategy Tutorial")
#' @slot project_id Project ID corresponding to the chosen project name. This is determined when connecting to the project by name.
#' @slot login_mode Authentication option. Standard (1) or LDAP (16).
#' @slot ssl_verify Default TRUE. Attempts to verify SSL certificates with each request.
#' @slot auth_token Token provided by the I-Server after a successful log in.
#' @slot cookies Cookies returned by the I-Server after a successful log in.
#' @rdname connection-class
#' @exportClass connection
.connection <- setClass("connection",
                        slots = c(username = 'character',
                                  password = 'character',
                                  base_url = 'character',
                                  project_name = 'character',
                                  project_id = 'character',
                                  login_mode = 'numeric',
                                  ssl_verify = 'logical',
                                  auth_token = 'character',
                                  cookies = 'character'))


#' @title Create a MicroStrategy REST API connection
#'
#' @description Establishes and creates a connection with the MicroStrategy REST API.
#' @param base_url URL of the MicroStrategy REST API server
#' @param username Username
#' @param password Password
#' @param project_name Name of the project you intend to connect to. Case-sensitive
#' @param login_mode Specifies the authentication mode to use. Supported authentication modes
#'                   are: Standard (1) (default) or LDAP (16)
#' @param ssl_verify If \code{TRUE} (default), verifies the server's SSL certificates with each request
#' @return A connection object to use in subsequent requests
#' @name connect_mstr
#' @rdname connect_mstr
#' @examples
#' \donttest{
#' # Connect to a MicroStrategy environment
#' con <- connect_mstr(base_url = "https://demo.microstrategy.com/MicroStrategyLibrary/api",
#'                     username = "user",
#'                     password = "password",
#'                     project_name = "Financial Reporting")
#'
#' # A good practice is to disconnect once you're done
#' # In case you forget, the server will disconnect the session after some time has passed
#' close(con)
#' }
#' @export
connect_mstr <- function(base_url, username, password, project_name, login_mode=1, ssl_verify=TRUE){

  # Basic error checking for input types
  if(class(base_url) != "character") stop("'base_url' must be a character; try class(base_url)")
  if(class(username) != 'character') stop("'username' must be a character; try class(username)")
  if(class(password) != 'character') stop("'password' must be a character; try class(password)")
  if(class(project_name) != 'character') stop("'project_name' must be a character; try class(project_name)")
  if(!(login_mode %in% c(1, 8, 16))) stop("Invalid login mode. Only '1' (normal), '8' (guest), or '16' (LDAP) are supported.")
  if(class(ssl_verify) != 'logical') stop("'ssl_verify' must be TRUE or FALSE")

  # Creates a new connection object
  con <- .connection(base_url=base_url, username=username, password=password,
                     project_name=project_name, login_mode=login_mode)

  if(!ssl_verify){
    httr::set_config(config(ssl_verifypeer = FALSE))
    con@ssl_verify <- FALSE
  } else {
    con@ssl_verify <- ssl_verify
  }

  # Makes connection
  tmp_con <- connect(connection=con)

  # Add authentication token and cookies to connection object
  con@auth_token <- tmp_con$auth_token
  con@cookies <- tmp_con$cookies

  # Connect to the project and set object's project id property
  con@project_id <- select_project(connection=con)

  # Return connection object
  con
}

# TODO: Document internal-only (non-exported) function and method
setGeneric("connect", function(connection) standardGeneric("connect"))

# TODO: Document internal-only (non-exported) function and method
setMethod("connect", "connection", function(connection){

  # Create session
  response <- login(connection = connection)

  if(response$status_code != 204){
    # Raise server error message
    status <- http_status(response)
    errors <- content(response)
    errmsg <- paste0("Authentication error. Check user credentials or REST API URL and try again.\n",
                     "HTTP ", response$status_code, " ", status$reason, ", ", status$message, "\n",
                     "I-Server Error ", errors$code, ", ", errors$message)
    stop(errmsg)

  }
  return(list(auth_token = as.character(response$headers['x-mstr-authtoken']),
              cookies = as.character(response$headers['set-cookie'])))
})



#' @title Closes a connection with MicroStrategy REST API
#'
#' @description Closes a connection with MicroStrategy REST API.
#' @param connection MicroStrategy REST API connection object returned by \code{connect_mstr()}
#' @name close
#' @rdname close
#' @examples
#' \donttest{
#' # Connect to a MicroStrategy environment
#' con <- connect_mstr(base_url = "https://demo.microstrategy.com/MicroStrategyLibrary/api",
#'                     username = "user",
#'                     password = "password",
#'                     project_name = "Financial Reporting")
#'
#' # A good practice is to disconnect once you're done
#' # However, the server will disconnect the session after some time has passed
#' close(con)
#' }
#' @export close
setGeneric("close", function(connection) standardGeneric("close"))

#' @rdname close
setMethod("close", "connection", function(connection){

  # Terminate the connection
  response <- logout(connection = connection)
  if(response$status_code != 204){
    # Raise server error message
    status <- http_status(response)
    errors <- content(response)
    errmsg <- paste0("Error attempting to terminate session connection. The session may have been terminated by the Intelligence Server.\n",
                     "HTTP ", response$status_code, " ", status$reason, ", ", status$message, "\n",
                     "I-Server Error ", errors$code, ", ", errors$message)
    stop(errmsg)
  }
})


# TODO: Document internal-only (non-exported) function and method
setGeneric("select_project", function(connection) standardGeneric("select_project"))

# TODO: Document internal-only (non-exported) function and method
setMethod("select_project", "connection", function(connection){

  response <- projects(connection=connection)

  if(response$status_code != 200){
    # Raise server error message
    status <- http_status(response)
    errors <- content(response)
    errmsg <- paste0("Error connecting to project '", connection@project_name, "'. Check project name and try again.\n",
                     "HTTP ", response$status_code, " ", status$reason, ", ", status$message, "\n",
                     "I-Server Error ", errors$code, ", ", errors$message)
    stop(errmsg)
  }

  projs <- content(response)
  for(proj in projs){
    if(proj$name == connection@project_name){
      return(proj$id)
    }
  }

  # Project not found
  # Close the session. Assuming the user will attempt to re-authenticate, this prevents excess sessions
  on.exit(close(connection=connection))
  status <- http_status(response)
  errors <- content(response)
  errmsg <- paste0("Project '", connection@project_name, "' not found. Check project name and try again.\n",
                   "HTTP ", response$status_code, " ", status$reason, ", ", status$message, "\n",
                   "I-Server Error ", errors$code, ", ", errors$message)
  stop(errmsg)
})



#' @title Extracts the contents of a report into a R Data.Frame
#'
#' @description Extracts the contents of a MicroStrategy report into a R Data.Frame
#' @param connection MicroStrategy REST API connection object
#' @param report_id Unique ID of the report you wish to extract information from
#' @param offset (optional) To extract all data from the report, use 0 (default)
#' @param limit (optional) Used to control data extract behavior on datasets with a large
#'              number of rows. The default is 1000. As an example, if the dataset has 50,000 rows,
#'              \code{get_report()} will incrementally extract all 50,000 rows in 1,000 row chunks. Depending
#'              on system resources, a higher limit (e.g. 10,000) may reduce the total time
#'              required to extract the entire dataset
#' @return R Data.Frame containing the report contents
#' @name get_report
#' @rdname get_report
#' @examples
#' \donttest{
#' # Extract the contents of a report into an R Data.Frame
#' my_report <- get_report(connection = conn,
#'                         report_id = "5E2501A411E8756818A50080EF4524C9")
#'
#' # Extract the contents in larger 'chunks' using limit.
#' # May require add'l server processing time.
#' # As a rule-of-thumb, aim for a limit setting around 10%
#' # to 20% of the total number of rows in the report.
#' my_report <- get_report(connection = conn,
#'                         report_id = "5E2501A411E8756818A50080EF4524C9",
#'                         limit = 100000)
#'
#' # You can also set limit to -1. Use this only on smaller reports.
#' my_report <- get_report(connection = conn,
#'                         report_id = "5E2501A411E8756818A50080EF4524C9",
#'                         limit = -1)
#' }
#' @export get_report
setGeneric("get_report", function(connection, report_id, offset=0, limit=1000) standardGeneric("get_report"))

#' @rdname get_report
setMethod("get_report", "connection", function(connection, report_id, offset=0, limit=1000){

  # Basic error checking
  if(class(report_id) != 'character') stop("'report_id' must be a character; try class(report_id)")

  # Get results of first report pagination
  response <- report_instance(connection=connection, report_id=report_id, offset=offset, limit=limit)

  if(response$status_code != 200){
    # Raise server error message
    status <- http_status(response)
    errors <- content(response)
    errmsg <- paste0("Error getting report contents.\n",
                     "HTTP ", response$status_code, " ", status$reason, ", ", status$message, "\n",
                     "I-Server Error ", errors$code, ", ", errors$message)
    stop(errmsg)
  }

  # report instance id, used for fetching add'l rows
  response <- content(response)
  instance_id = response$instanceId

  # Gets the pagination totals from the response object
  pagination <- response$result$data$paging

  if(pagination$current != pagination$total){

    # Append first response object to a list
    response_list <- list(parse_json(response = response))

    # Create vector of offset parameters to iterate over
    offsets <- pagination$current
    while(tail(offsets, 1) + limit < pagination$total){
      offsets <- append(offsets, tail(offsets, 1) + limit)
    }

    for(offset_ in offsets){
      # Fetch add'l rows from the report
      response <- report_instance_id(connection=connection, report_id=report_id, instance_id=instance_id, offset=offset_, limit=limit)
      response <- content(response)
      response_list[[length(response_list)+1]] <- parse_json(response = response)
    }
    return(do.call(rbind.data.frame, response_list))
  } else {
    return(parse_json(response=response))
  }
})



#' @title Extract a MicroStrategy cube into a R Data.Frame
#'
#' @description Extracts the contents of a MicroStrategy cube into a R Data.Frame
#' @param connection MicroStrategy REST API connection object
#' @param cube_id Unique ID of the cube you wish to extract information from
#' @param offset (optional) To extract all data from the report, use 0 (default)
#' @param limit (optional) Used to control data extract behavior on datasets with a large
#'              number of rows. The default is 1000. As an example, if the dataset has 50,000 rows,
#'              \code{get_cube()} will incrementally extract all 50,000 rows in 1,000 row chunks. Depending
#'              on system resources, a higher limit (e.g. 10,000) may reduce the total time
#'              required to extract the entire dataset
#' @return R Data.Frame containing the cube contents
#' @name get_cube
#' @rdname get_cube
#' @examples
#' \donttest{
#' # Extract the contents of a cube into an R Data.Frame
#' my_cube <- get_cube(connection = conn,
#'                     cube_id = "5E2501A411E8756818A50080EF4524C9")
#'
#' # Extract the contents in larger 'chunks' using limit.
#' # May require add'l server processing time.
#' # As a rule-of-thumb, aim for a limit setting around 10%
#' # to 20% of the total number of rows in the cube.
#' my_cube <- get_cube(connection = conn,
#'                     cube_id = "5E2501A411E8756818A50080EF4524C9",
#'                     limit = 100000)
#'
#' # You can also set limit to -1. Use this only on smaller reports.
#' my_cube <- get_cube(connection = conn,
#'                     cube_id = "5E2501A411E8756818A50080EF4524C9",
#'                     limit = -1)
#' }
#' @export get_cube
setGeneric("get_cube", function(connection, cube_id, offset=0, limit=1000) standardGeneric("get_cube"))

#' @rdname get_cube
setMethod("get_cube", "connection", function(connection, cube_id, offset=0, limit=1000){

  # Basic error checking
  if(class(cube_id) != 'character') stop("'cube_id' must be a character; try class(cube_id)")

  # Get first cube instance
  response <- cube_instance(connection=connection, cube_id=cube_id, offset=offset, limit=limit)

  if(response$status_code != 200){
    # Raise server error message
    status <- http_status(response)
    errors <- content(response)
    errmsg <- paste0("Error getting cube contents.\n",
                     "HTTP ", response$status_code, " ", status$reason, ", ", status$message, "\n",
                     "I-Server Error ", errors$code, ", ", errors$message)
    stop(errmsg)
  }

  # Gets the pagination totals from the response object
  response <- content(response)
  pagination <- response$result$data$paging

  # cube instance id, used for fetching add'l rows
  instance_id = response$instanceId

  if(pagination$current != pagination$total){

    # Append first response object to a list
    response_list <- list(parse_json(response = response))

    # Create vector of offset parameters to iterate over
    offsets <- pagination$current
    while(tail(offsets, 1) + limit < pagination$total){
      offsets <- append(offsets, tail(offsets, 1) + limit)
    }

    for(offset_ in offsets){
      # Fetch add'l rows from the cube
      response <- cube_instance_id(connection=connection, cube_id=cube_id, instance_id=instance_id, offset=offset_, limit=limit)
      response <- content(response)
      response_list[[length(response_list)+1]] <- parse_json(response = response)
    }
    return(do.call(rbind.data.frame, response_list))
  } else {
    return(parse_json(response=response))
  }
})



#' @title Create an in-memory MicroStrategy dataset
#'
#' @description Creates an in-memory dataset from an R Data.Frame.
#' @param connection MicroStrategy REST API connection object
#' @param data_frame R Data.Frame from which an in-memory dataset will be created
#' @param dataset_name Name of the in-memory dataset
#' @param table_name Name of the table to create within the dataset
#' @param to_metric (optional) A vector of column names from the Data.Frame to format as metrics
#'                  in the dataset. By default, numeric types are formatted as metrics while character and date types are formatted as attributes.
#'                  For example, a column of integer-like strings ("1", "2", "3") would
#'                  appear as an attribute in the newly created dataset. If the intent is to format this data as a metric, provide the
#'                  corresponding column name as \code{to_metric=c('myStringIntegers')}
#' @param to_attribute (optional) Logical opposite of \code{to_metric}. Helpful for formatting an integer-based row identifier as
#'                     a primary key in the dataset
#' @return Unique identifiers of the dataset and table within the newly created dataset. Required for \code{update_dataset()}
#' @name create_dataset
#' @rdname create_dataset
#' @examples
#' \donttest{
#' df <- iris
#'
#' # Create a primary key
#' df$ID <- as.character(row.names(df))
#'
#' # Remove periods and other special characters due to their
#' # special role in MicroStrategy. But, "_" is ok.
#' names(df) <- c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width", "Species", "ID")
#'
#' # Create the dataset
#' mydf <- create_dataset(connection = conn,
#'                        data_frame = df,
#'                        dataset_name = "IRIS",
#'                        table_name = "IRIS")
#'
#' # You can specify special treatment for columns within the data frame.
#' # This will convert the character-formatted row ID's to a MicroStrategy metric
#' mydf <- create_dataset(connection = conn,
#'                        data_frame = df,
#'                        dataset_name = "IRIS",
#'                        table_name = "IRIS",
#'                        to_metric = c("ID"))
#'
#' # This will convert 'Sepal_Length' and 'Sepal_Width' to attributes
#' mydf <- create_dataset(connection = conn,
#'                        data_frame = df,
#'                        dataset_name = "IRIS",
#'                        table_name = "IRIS",
#'                        to_attribute = c("Sepal_Length", "Sepal_Width"))
#' }
#' @export create_dataset
setGeneric("create_dataset", function(connection, data_frame, dataset_name, table_name, to_metric=NULL, to_attribute=NULL) standardGeneric("create_dataset"))

#' @rdname create_dataset
setMethod("create_dataset", "connection", function(connection, data_frame, dataset_name, table_name, to_metric=NULL, to_attribute=NULL){

  # Basic error checking for input types
  if(class(data_frame) != "data.frame") stop("'data_frame' must be a valid R data.frame; try class(data_frame)")
  if(class(dataset_name) != 'character') stop("'dataset_name' must be a character; try class(dataset_name)")
  if(class(table_name) != 'character') stop("'table_name' must be a character; try class(table_name)")
  if(all(!is.null(to_metric), class(to_metric) != 'character')) stop("'to_metric' must be a vector of characters")
  if(all(!is.null(to_attribute), class(to_attribute) != 'character')) stop("'to_attribute' must be a vector of characters")

  # Check column names for presence of non-word characters like ., $, ' ', ; as these will create issues in mstr
  if(any(grepl("\\W", names(data_frame)))) stop("Column names cannot have non-word characters. Try again after eliminating white space and special characters.")

  # Generate microstrategy json template for the data frame
  if(all(is.null(to_metric), is.null(to_attribute))){
    jsonlist <- form_json(df=data_frame, table_name=table_name)
  } else {
    jsonlist <- form_json(df=data_frame, table_name=table_name, to_metric=to_metric, to_attribute=to_attribute)
  }

  # Encode the data in base64 encoded JSON string, required for API to work properly
  data_encoded <- openssl::base64_encode(jsonlite::toJSON(data_frame, raw='base64'))

  # create json string to pass to the server
  json_body <- jsonlite::toJSON(list(name = dataset_name,
                                     tables = list(list(name = table_name,
                                                        columnHeaders = jsonlist$columnHeaders,
                                                        data = data_encoded)),
                                     attributes = jsonlist$attributeList,
                                     metrics = jsonlist$metricList),
                                auto_unbox = TRUE,
                                pretty = TRUE)

  # Make the request
  response <- .create_dataset(connection=connection, json_body=json_body)

  if(response$status_code != 200){
    # Raise server error message
    status <- http_status(response)
    errors <- content(response)
    errmsg <- paste0("Error creating the dataset.\n",
                     "HTTP ", response$status_code, " ", status$reason, ", ", status$message, "\n",
                     "I-Server Error code ", errors$code, ", ", errors$message)
    stop(errmsg)
  }

  req <- content(response, as="parsed", type="application/json")
  return(list(datasetID = req$datasetId,
              name = req$name,
              tableID = req$tables[[1]]$id))
})



#' @title Update a previously created dataset
#'
#' @description Updates a previously created MicroStrategy dataset with an R Data.Frame.
#' @param connection MicroStrategy REST API connection object
#' @param data_frame R Data.Frame to use to update an in-memory dataset
#' @param dataset_id Identifier of the dataset to update, provided by \code{create_dataset()}
#' @param table_id Not used. Identifier of the table to update within the dataset, provided by \code{create_dataset()}
#' @param table_name Name of the table to update within the dataset
#' @param update_policy Update operation to perform. One of 'add' (inserts new, unique rows), 'update'
#'                      (updates data in existing rows and columns), 'upsert' (updates existing
#'                      data and inserts new rows), 'replace' (similar to truncate and load, replaces the existing data with new data)
#' @name update_dataset
#' @rdname update_dataset
#' @examples
#' \donttest{
#' df <- iris
#'
#' # Create a primary key
#' df$ID <- as.character(row.names(df))
#'
#' # Remove periods and other special characters due to their
#' # special role in MicroStrategy. But, "_" is ok.
#' names(df) <- c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width", "Species", "ID")
#'
#' # Create the dataset
#' mydf <- create_dataset(connection = conn,
#'                        data_frame = df,
#'                        dataset_name = "IRIS",
#'                        table_name = "IRIS")
#'
#' # Add new rows to the dataset with update policy "add"
#' df2 <- df[sample(nrow(df), 5), ]
#' df2[, 'ID'] <- as.character(nrow(df) + seq(1:5))
#' update_dataset(connection = conn, data_frame = df2,
#'                dataset_id = mydf$datasetID,
#'                table_id = mydf$tableID,
#'                table_name = mydf$name,
#'                update_policy = 'add')
#'
#' # Update existing data in the dataset with update policy "update"
#' df$Sepal_Length <- df$Sepal_Length + runif(nrow(df))
#' df$Petal_Width <- df$Sepal_Length + rnorm(nrow(df))
#' update_dataset(connection = conn, data_frame = df,
#'                dataset_id = mydf$datasetID,
#'                table_id = mydf$tableID,
#'                table_name = mydf$name,
#'                update_policy = 'update')
#'
#' # Update and add new rows to the dataset with update policy "upsert"
#' df$Sepal_Length <- df$Sepal_Length + runif(nrow(df))
#' df$Petal_Width <- df$Sepal_Length + rnorm(nrow(df))
#' df2 <- df[sample(nrow(df), 5), ]
#' df2[, 'ID'] <- as.character(nrow(df) + seq(1:5))
#' df <- rbind(df, df2)
#' update_dataset(connection = conn,
#'                data_frame = df,
#'                dataset_id = mydf$datasetID,
#'                table_id = mydf$tableID,
#'                table_name = mydf$name,
#'                update_policy = 'upsert')
#'
#' # Truncate and load new data into the dataset with update policy "replace"
#' df[] <- lapply(df, sample)
#' update_dataset(connection = conn, data_frame = df,
#'                dataset_id = mydf$datasetID,
#'                table_id = mydf$tableID,
#'                table_name = mydf$name,
#'                update_policy = 'replace')
#'
#' # It is possible to update a dataset if it wasn't created in this session or by another client.
#' # Simply provide the dataset ID and table IDs to this function as characters.
#' df[] <- lapply(df, sample)  # shuffle contents of the dataframe
#' update_dataset(connection = conn, data_frame = df,
#'                dataset_id = "5E2501A411E8756818A50080EF4524C9",
#'                table_id = "F0DA816816432E448F1105327C119596",
#'                table_name = "IRIS",
#'                update_policy = 'replace')
#' }
#' @export update_dataset
setGeneric("update_dataset", function(connection, data_frame, dataset_id, table_id, table_name, update_policy) standardGeneric("update_dataset"))

#' @rdname update_dataset
setMethod("update_dataset", "connection", function(connection, data_frame, dataset_id, table_name, update_policy){

  # TODO: Implement table_id or table_name

  # Basic error checking for input types
  if(class(data_frame) != "data.frame") stop("'data_frame' must be a valid R data.frame; try class(data_frame)")
  if(class(dataset_id) != 'character') stop("'dataset_id' must be a character; try class(dataset_id)")
  if(class(table_name) != 'character') stop("'table_name' must be a character; try class(table_name)")
  stopifnot(update_policy %in% c("add", "update", "upsert", "replace"))

  # Check column names for presence of non-word characters like ., $, ' ', ; as these will create issues in mstr
  if(any(grepl("\\W", names(data_frame)))) stop("Column names cannot have non-word characters. Try again after eliminating white space and special characters.")

  # encode data
  data_encoded <- openssl::base64_encode(jsonlite::toJSON(data_frame, raw='base64'))

  # Generate microstrategy json template for the data frame
  jsonlist <- form_json(df=data_frame, table_name=table_name)

  # form json body
  json_body <- jsonlite::toJSON(list(name = table_name,
                                     columnHeaders = jsonlist$columnHeaders,
                                     data = data_encoded),
                                auto_unbox = TRUE,
                                pretty = TRUE)

  # make request
  response <- .update_dataset(connection=connection, dataset_id=dataset_id, table_name=table_name,
                              update_policy=update_policy, json_body=json_body)

  if(response$status_code != 200){
    # Raise server error message
    status <- http_status(response)
    errors <- content(response)
    errmsg <- paste0("Error updating the dataset. Check for datatype inconsistencies between the data frame and the MicroStrategy dataset.\n",
                     "HTTP ", response$status_code, " ", status$reason, ", ", status$message, "\n",
                     "I-Server Error ", errors$code, ", ", errors$message)
    stop(errmsg)
  }
})
