# datasets.R
# Create and interact with MicroStrategy datasets

#' @title Create, update, and delete MicroStrategy datasets
#'
#' @description When creating a new dataset, provide a dataset name and an optional description. 
#' When updating a pre-existing dataset, provide the dataset identifier. Tables are added to the 
#' dataset in an iterative manner using `add_table()`.
#' @field connection MicroStrategy connection object
#' @field name Name of the dataset
#' @field description Description of the dataset. Must be less than or equal to 250 characters
#' @field dataset_id Identifier of a pre-existing dataset. Used when updating a pre-existing dataset
#' @field verbose Print API requests to console. Used for debugging
#' @examples
#' \donttest{
#' # Create data frames
#' df1 <- data.frame("id" = c(1, 2, 3, 4, 5),
#'                   "first_name" = c("Jason", "Molly", "Tina", "Jake", "Amy"),
#'                   "last_name" = c("Miller", "Jacobson", "Turner", "Milner", "Cooze"))
#'
#' df2 <- data.frame("id" = c(1, 2, 3, 4, 5),
#'                   "age" = c(42, 52, 36, 24, 73),
#'                   "state" = c("VA", "NC", "WY", "CA", "CA"),
#'                   "salary" = c(50000, 100000, 75000, 85000, 250000))
#'
#' # Create a list of tables containing one or more tables and their names
#' my_dataset <- Dataset$new(connection=conn, name="HR Analysis")
#' my_dataset$add_table("Employees", df1, "add")
#' my_dataset$add_table("Salaries", df2, "add")
#' my_dataset$create()
#' 
#' # By default Dataset$create() will upload the data to the Intelligence Server and publish the 
#'  dataset. 
#' # If you just want to create the dataset but not upload the row-level data, use 
#' Dataset$create(auto_upload=FALSE)
#' 
#' # followed by 
#' Dataset$update()
#' Dataset$publish()
#' 
#' # When the source data changes and users need the latest data for analysis and reporting in 
#' # MicroStrategy, mstrio allows you to update the previously created dataset.
#' 
#' ds <- Dataset$new(connection=conn, dataset_id="...")
#' ds$add_table(name = "Stores", data_frame = stores_df, update_policy = 'update')
#' ds$add_table(name = "Sales", data_frame = stores_df, update_policy = 'upsert')
#' ds$update()
#' ds$publish()
#' 
#' # By default, the raw data is transmitted to the server in increments of 25,000 rows. On very 
#' # large datasets (>1 GB), it is beneficial to increase the number of rows transmitted to the 
#' # Intelligence Server with each request. Do this with the chunksize parameter:
#'
#' ds$update(chunksize = 500000)
#' }
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#' @export
Dataset <- R6Class("Dataset",

  public = list(

    # instance variables
    name = NULL,
    description = NULL,
    folder_id = NULL,
    dataset_id = NULL,
    session_id = NULL,
    upload_body = NULL,
    verbose = NULL,

    VALID_POLICY = c("add", "update", "replace", "upsert"),
    MAX_DESC_LEN = 250,

    initialize = function(connection, name=NULL, description=NULL, dataset_id=NULL, verbose=FALSE) {
      # Initialize dataset constructor

      private$connection <- connection

      if(!is.null(name)) {
        private$check_param_str(name, msg="Dataset name should be a string.")
        private$check_param_len(name, msg="Dataset name should be <= 250 characters.", length=self$MAX_DESC_LEN)
      }
      self$name <- name

      if(!is.null(description)) {
        private$check_param_str(description, msg="Dataset description should be a string.")
        private$check_param_len(description, msg="Dataset description should be <= 250 characters.", length=self$MAX_DESC_LEN)
      }
      self$description <- description

      if(!is.null(dataset_id)) {
        private$check_param_str(dataset_id, msg="Dataset ID should be a string.")
      }
      self$dataset_id <- dataset_id
      self$verbose <- verbose

    },

    add_table = function(name, data_frame, update_policy, to_metric=NULL, to_attribute=NULL) {
      # Add a data.frame to a collection of tables which are later used to update the MicroStrategy dataset

      if(class(data_frame) != "data.frame") {
        stop("data_frame must be a valid R data.frame.")
      }

      if(!update_policy %in% self$VALID_POLICY) {
        stop("Invalid update policy. Only 'add', 'update', 'replace', and 'upsert' are supported.")
      }

      table <- list("table_name" = name,
                    "data_frame" = data_frame,
                    "update_policy" = tolower(update_policy))

      if(!is.null(to_attribute)) {
        if(!all(to_attribute %in% names(data_frame))) {
          stop(paste0("Column name(s) in `to_attribute` were not found in `names(data_frame)`."))
        } else {
          table["to_attribute"] <- to_attribute
        }
      }

      if(!is.null(to_metric)) {
        if(!all(to_metric %in% names(data_frame))) {
          stop(paste0("Column name(s) in `to_metric` were not found in `names(data_frame)`."))
        } else {
          table["to_metric"] <- to_metric
        }
      }

      # add the new dataframe to the list of dataframes
      private$tables <- c(private$tables, list(table))

    },

    create = function(folder_id=NULL, auto_upload=TRUE) {
      # Creates a new dataset

      # Check that tables object contains data
      private$check_tables(private$tables)

      if(!is.null(folder_id)) {
        self$folder_id <- folder_id
      } else {
        self$folder_id <- ""
      }

      # generate model of the dataset
      private$build_model()

      # makes request to create the dataset definition on the server
      response <- create_multitable_dataset(private$connection,
                                            body=private$model_list$json,
                                            verbose=self$verbose)

      if(http_error(response)) {
        stop(private$response_handler(response, msg="Error creating new dataset definition."))
      } else {

        response <- content(response, as="parsed", type="application/json")
        self$dataset_id <- response$id

        if(self$verbose) {
          sprintf("Created dataset %s with ID: %s", self$name, self$dataset_id)
        }
      }

      # if desired, automatically upload and publish the data to the new dataset
      if(auto_upload) {
        self$update()
        self$publish()

        status <- 6
        while(status != 1){
          pub <- publish_status(connection=private$connection,
                                dataset_id=self$dataset_id,
                                session_id=self$session_id,
                                verbose=self$verbose)
          pub <- content(pub, as="parsed", type="application/json")
          status <- pub$status
          if(status == 1){
            break
          }
        }
      }
    },

    update = function(chunksize=100000) {
      # Updates an existing dataset

      # Check that tables object contains data
      private$check_tables(private$tables)

      # form request body and create a session for data uploads
      private$form_upload_body()
      response <- upload_session(connection=private$connection, dataset_id=self$dataset_id,
                                 body=self$upload_body$json, verbose=self$verbose)

      if(http_error(response)) {  # http != 200
        stop(private$response_handler(response, msg="Error creating new data upload session"))
      }

      response <- content(response, as="parsed", type="application/json")
      self$session_id <- response$uploadSessionId

      # upload each table
      for(table in private$tables) {

        # break the data up into chunks
        rows <- 0
        total <- nrow(table$data_frame)

        chunks <- split(table$data_frame, rep(1:ceiling(nrow(table$data_frame)/chunksize),
                                              each=chunksize,
                                              length.out=nrow(table$data_frame)))
        for(i in seq_along(chunks)) {

          # base64 encode the data
          enc <- Encoder$new(chunks[[i]], "multi")
          b64_enc <- enc$encode()

          # form body of the request
          body <- toJSON(list("tableName"=table$table_name,
                              "index"=i,
                              "data"=b64_enc),
                         auto_unbox = TRUE)

          # make request to upload the data
          response <- upload(private$connection, dataset_id=self$dataset_id, session_id=self$session_id, body=body, verbose=self$verbose)

          if(http_error(response)) {  # http != 200
            private$response_handler(response, msg="Error uploading data.")
            publish_cancel(private$connection, self$dataset_id, self$session_id, verbose=self$verbose)
          }

          rows <- rows + nrow(chunks[i])

          if(self$verbose) {
            private$upload_progress(table$table_name, rows, total)
          }
        }
      }
    },

    publish = function() {
      # Publish the uploaded data to the selected dataset

      response <- publish(connection=private$connection,
                          dataset_id=self$dataset_id,
                          session_id=self$session_id,
                          verbose=self$verbose)

      if(http_error(response)) {  # http != 200
        # on error, cancel the previously uploaded data
        private$response_handler(response, msg="Error publishing updated data. Cancelling upload.")
        publish_cancel(private$connection, self$dataset_id, self$session_id, verbose=self$verbose)
      }

      return(response)
    },

    publish_status = function() {
      # Check the status of data that was uploaded to a dataset

      response <- publish_status(connection=private$connection,
                                 dataset_id=self$dataset_id,
                                 session_id=self$session_id,
                                 verbose=self$verbose)

      status <- content(response, as="parsed", type="application/json")

      return(status)

    },

    delete = function() {
      # Delete a dataset that was previously created using the REST API

      response <- delete_dataset(connection=private$connection,
                                 dataset_id=self$dataset_id,
                                 verbose=self$verbose)

      if(http_error(response)) {  # http != 200
        private$response_handler(response, msg=paste("Error deleting dataset with ID:", dataset_id))
      } else {
        print(paste("Successfully deleted dataset with ID:", dataset_id))
      }
    }
   ),

  private = list(

    connection = NULL,
    tables = list(),
    definition = NULL,
    model_list = NULL,

    build_model = function() {
      # generate model of the dataset using Models class
      model <- Model$new(tables=private$tables, name=self$name, description=self$description, folder_id=self$folder_id)
      private$model_list <- model$get_model()

    },

    form_upload_body = function() {
      # Form request body for creating an upload session for data uploads

      body <- list("tables" = lapply(private$tables, function(x) {
        list("name" = x$table_name,
             "updatePolicy" = x$update_policy,
             "columnHeaders" = names(x$data_frame))
      }))
      body_json <- toJSON(body, auto_unbox = TRUE)

      self$upload_body <- list("raw" = body,
                               "json" = body_json)
    },

    load_definition = function() {
      # Load definition of an existing dataset

      response <- dataset_definition(connection=private$connection,
                                     dataset_id=self$dataset_id,
                                     verbose=self$verbose)

      if(http_error(response)) {  # http != 200
        private$response_handler(response=response,
                                 msg="Error loading dataset definition. Check dataset ID.")
      } else {
        private$definition <- content(response, as="parsed", type="application/json")
        self$name <- private$definition$name
        self$dataset_id <- private$definition$id
      }
    },

    upload_progress = function(table_name, rows, total) {
      # Prints status of dataset upload
      sprintf("%s status: %s of %s rows", table_name, round(rows / total, 2) * 100, rows)
    },

    response_handler = function(response, msg) {
      # Generic error message handler for transactions against datasets

      status <- http_status(response)
      errors <- content(response)
      stop(sprintf("%s\n HTTP Error: %s %s %s\n I-Server Error: %s %s",
                   msg, response$status_code, status$reason, status$message, errors$code, errors$message),
           call.=FALSE)

    },

    check_param_len = function(param, msg, length) {
      if(nchar(param) >= length) {
        stop(msg)
      } else {
        return(TRUE)
      }

    },

    check_param_str = function(param, msg) {
      if(class(param) != "character") {
        stop(msg)
      } else {
        return(TRUE)
      }
    },

    check_tables = function(tables) {
      if(length(tables) == 0) {
        stop("No tables have been added to the dataset. Use `Dataset$add_table()` to add a table.")
      }
    }
  )
)
