# datasets.R
# Create and interact with MicroStrategy datasets

#' @title Create, update, delete and certify MicroStrategy datasets
#'
#' @description When creating a new dataset, provide a dataset name and an optional description.
#' When updating a pre-existing dataset, provide the dataset identifier. Tables are added to the
#' dataset in an iterative manner using `add_table()`.
#' @field connection MicroStrategy connection object
#' @field name Name of the dataset
#' @field description Description of the dataset. Must be less than or equal to 250 characters
#' @field folder_id If specified the dataset will be saved in this folder
#' @field dataset_id Identifier of a pre-existing dataset. Used when updating a pre-existing dataset
#' @field owner_id Owner ID
#' @field path Cube path
#' @field modification_time Last modification time, "yyyy-MM-dd HH:mm:ss" in UTC
#' @field size Cube size
#' @field cube_state Cube status,for example, 0=unpublished, 1=publishing, 64=ready
#' @field verbose If True (default), displays additional messages.
#' @examples
#' \dontrun{
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
#' ds$update(auto_publish=TRUE)
#'
#' # By default Dataset$update() will upload the data to the Intelligence Server and publish the
#'  dataset.
#' # If you just want to update the dataset but not publish the row-level data, use
#' Dataset$update(auto_publish=FALSE)
#'
#' # By default, the raw data is transmitted to the server in increments of 100,000 rows. On very
#' # large datasets (>1 GB), it is beneficial to increase the number of rows transmitted to the
#' # Intelligence Server with each request. Do this with the chunksize parameter:
#'
#' ds$update(chunksize = 500000)
#'
#' # If you want to cerfify an existing dataset, use
#' ds$certify()
#' }
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#' @export
Dataset <- R6Class("Dataset",

  public = list(

# instance variables
    connection = NULL,
    name = NULL,
    description = NULL,
    folder_id = NULL,
    dataset_id = NULL,
    owner_id = NULL,
    path = NULL,
    modification_time = NULL,
    size = NULL,
    cube_state = NULL,
    verbose = NULL,

#' @description
#' Interface for creating, updating, and deleting MicroStrategy in-memory datasets.
#' @details
#' When creating a new dataset, provide a dataset name and an optional description. When
#' updating a pre-existing dataset, provide the dataset identifier. Tables are added to the
#' dataset in an iterative manner using `add_table()`.
#' @param connection MicroStrategy connection object returned by `Connection$New()`.
#' @param name (character): Name of the dataset.
#' @param description (character, optional): Description of the dataset. Must be less than or equal to 250 characters.
#' @param dataset_id (character, optional): Identifier of a pre-existing dataset. Used when updating a pre-existing
#' dataset.
#' @param verbose Setting to control the amount of feedback from the I-Server.
#' @return A new `Datasets` object
    initialize = function(connection, name = NULL, description = NULL, dataset_id = NULL, verbose = TRUE) {

      self$connection <- connection
      self$dataset_id <- dataset_id
      self$verbose <- verbose

      if (!is.null(name)) {
        private$check_param_str(name, msg = "Dataset name should be a string.")
        private$check_param_len(name, msg = "Dataset name should be <= 250 characters.",
                                max_length = private$max_desc_len)
      }
      self$name <- name

      if (!is.null(description)) {
        private$check_param_str(description, msg = "Dataset description should be a string.")
        private$check_param_len(description, msg = "Dataset description should be <= 250 characters.",
                                max_length = private$max_desc_len)
      }
      self$description <- description

      if (!is.null(dataset_id)) {
        private$check_param_str(dataset_id, msg = "Dataset ID should be a string.")
        private$load_definition()
      }
    },

#' @description Add a data.frame to a collection of tables which are later used to update the MicroStrategy dataset
#' @details Add tables to the dataset in an iterative manner using `add_table()`.
#' @param name (character): Logical name of the table that is visible to users of the dataset in MicroStrategy.
#' @param data_frame (`data.frame`): R data.frame to add or update.
#' @param update_policy (character): Update operation to perform. One of 'add' (inserts new, unique rows), 'update'
#' (updates data in existing rows and columns), 'upsert' (updates existing data and inserts new rows), or 'replace'
#' (replaces the existing data with new data).
#' @param to_metric (optional, vector): By default, R numeric data types are treated as metrics in
#' the MicroStrategy dataset while character and date types are treated as attributes. For example, a
#' column of integer-like strings ("1", "2", "3") would, by default, be an attribute in the newly created
#' dataset. If the intent is to format this data as a metric, provide the respective column name as
#' a character vector in `to_metric` parameter.
#' @param  to_attribute (optional, vector): Logical opposite of `to_metric`. Helpful for formatting an
#' integer-based row identifier as a primary key in the dataset.
    add_table = function(name, data_frame, update_policy, to_metric = NULL, to_attribute = NULL) {

      if (class(data_frame) != "data.frame") {
        stop("data_frame must be a valid R data.frame.")
      }

      if (!update_policy %in% private$valid_policy) {
        stop("Invalid update policy. Only 'add', 'update', 'replace', and 'upsert' are supported.")
      }

      # info <- check_version(self$connection$base_url, )
      ver <- utils::compareVersion(self$connection$iserver_version, "11.2.0300")
      new_policy <- ver >= 0

      if (is.null(self$dataset_id) && update_policy != 'replace' && new_policy) {
        stop("Update policy has to be 'replace' if a dataset is created or overwritten.")
      }

      table <- list("table_name" = name,
                    "data_frame" = data_frame,
                    "update_policy" = tolower(update_policy))

      if (any(to_attribute %in% to_metric)) {
        stop(paste0("Column name(s) present in `to_attribute` also present in 'to_metric'."))
      }

      if (!is.null(to_attribute)) {
        if (!all(to_attribute %in% names(data_frame))) {
          stop(paste0("Column name(s) in `to_attribute` were not found in `names(data_frame)`."))
        } else {
          table["to_attribute"] <- list(to_attribute)
        }
      }

      if (!is.null(to_metric)) {
        if (!all(to_metric %in% names(data_frame))) {
          stop(paste0("Column name(s) in `to_metric` were not found in `names(data_frame)`."))
        } else {
          table["to_metric"] <- list(to_metric)
        }
      }

      # add the new dataframe to the list of dataframes
      private$tables <- c(private$tables, list(table))

    },

#' @description Create a new dataset.
#' @param folder_id ID of the shared folder that the dataset should be created within. If `None`, defaults to the
#' user's My Reports folder.
#' @param auto_upload (default TRUE)  If True, automatically uploads the data to the I-Server. If False, simply
#' creates the dataset definition but does not upload data to it.
#' @param auto_publish (default TRUE) If True, automatically publishes the data used to create the dataset
#' definition. If False, simply creates the dataset but does not publish it. To publish the dataset, data has to be
#' uploaded first.
#' @param chunksize (int, optional) Number of rows to transmit to the I-Server with each request when uploading.
      create = function(folder_id = NULL, auto_upload = TRUE, auto_publish = TRUE, chunksize = 100000) {

        if (auto_publish && !auto_upload) {
          stop("Data needs to be uploaded to the I-Server before the dataset can be published.")
        }
        # Check that tables object contains data
        private$check_tables(private$tables)

        if (!is.null(folder_id)) {
          self$folder_id <- folder_id
        } else {
          self$folder_id <- ""
        }

        # generate model of the dataset
        private$build_model()

        # makes request to create the dataset definition on the server
        response <- create_multitable_dataset(self$connection,
                                            body = private$model_list$json,
                                            verbose = private$debug)

        response <- content(response, as = "parsed", type = "application/json")
        self$dataset_id <- response$id

        if (self$verbose) {
          print(sprintf("Created dataset %s with ID: %s", self$name, self$dataset_id))
        }

        # if desired, automatically upload and publish the data to the new dataset
        if (auto_upload) {
          self$update(chunksize = chunksize, auto_publish = auto_publish)
        }
      },

#' @description Updates an existing dataset with new data.
#' @param chunksize (int, optional): Number of rows to transmit to the I-Server with each request when uploading.
#' @param auto_publish (default TRUE) If True, automatically publishes the data. If False, data will be uploaded but
#' the cube will not be published
      update = function(chunksize = 100000, auto_publish = TRUE) {
        # Check that tables object contains data
        private$check_tables(private$tables)

        # form request body and create a session for data uploads
        private$form_upload_body()
        response <- upload_session(connection = self$connection, dataset_id = self$dataset_id,
                                 body = private$upload_body$json, verbose = private$debug)

        response <- content(response, as = "parsed", type = "application/json")
        private$session_id <- response$uploadSessionId

        # upload each table
        for (table in private$tables) {

          # break the data up into chunks
          rows <- 0
          total <- nrow(table$data_frame)

          chunks <- split(table$data_frame, rep(1:ceiling(nrow(table$data_frame) / chunksize),
                                              each = chunksize,
                                              length.out = nrow(table$data_frame)))
          for (i in seq_along(chunks)) {

            # base64 encode the data
            enc <- Encoder$new(chunks[[i]], "multi")
            b64_enc <- enc$encode()

            # form body of the request
            body <- toJSON(list("tableName" = table$table_name,
                              "index" = i,
                              "data" = b64_enc),
                         auto_unbox = TRUE)

            # make request to upload the data
            response <- upload(self$connection, dataset_id = self$dataset_id, session_id = private$session_id,
                             body = body, verbose = private$debug)

            if (http_error(response)) {
              # http != 200
              response_handler(response, msg = "Error uploading data.", throw_error = FALSE)
              publish_cancel(self$connection, self$dataset_id, private$session_id, verbose = private$debug)
            }

            rows <- rows + nrow(chunks[[i]])
            if (self$verbose) {
              private$upload_progress(table$table_name, rows, total)
            }
          }
        }
        # if desired, automatically publish the data to the new dataset
        if (auto_publish) {
          self$publish()
        }
      },

#' @description Publish the uploaded data to the selected dataset. A dataset can be published just once.
      publish = function() {
        response <- publish(connection = self$connection,
                              dataset_id = self$dataset_id,
                              session_id = private$session_id,
                              verbose = private$debug)

        if (http_error(response)) {
          # on error, cancel the previously uploaded data
          response <- publish_cancel(self$connection, self$dataset_id, private$session_id, verbose = private$debug)
        } else {
          status <- 6 # default initial status
          while (status != 1) {
            pub <- publish_status(connection = self$connection,
                                dataset_id = self$dataset_id,
                                session_id = private$session_id,
                                verbose = private$debug)

            pub <- content(pub, as = "parsed", type = "application/json")
            status <- pub$status
            if (status == 1 & self$verbose) {
              print(sprintf("Dataset '%s' published successfully.", self$name))
            }
          }
        }
      },

#' @description Check the status of data that was uploaded to a dataset.
#' @return Response status code
      publish_status = function() {

        response <- publish_status(connection = self$connection,
                                 dataset_id = self$dataset_id,
                                 session_id = private$session_id,
                                 verbose = private$debug)

        status <- content(response, as = "parsed", type = "application/json")
        if (self$verbose) {
          print(sprintf("Publish message: %s", status$message))
          print(sprintf("Publish status: %s", status$status))
        } else {
          return(status)
        }
      },

#' @description Delete a dataset that was previously created using the REST API.
#' @return Response object from the Intelligence Server acknowledging the deletion process.
      delete = function() {

        response <- delete_dataset(connection = self$connection,
                                 dataset_id = self$dataset_id,
                                 verbose = private$debug)
        if (self$verbose) {
          print(paste("Successfully deleted dataset with ID:", self$dataset_id))
        } else {
          return(response)
        }
      },

#' @description Certify a dataset that was previously creted using the REST API
#' @return Response object from the Intelligence Server acknowledging the certification process.
      certify = function() {

        response <- toggle_dataset_certification(connection = self$connection,
                                 dataset_id = self$dataset_id,
                                 verbose = private$debug)
        if (self$verbose) {
          print(sprintf("The dataset with ID: %s has been certified.", self$dataset_id))
        } else {
          return(response)
        }
      }
  ),

  private = list(

    tables = list(),
    model_list = NULL,
    upload_body = NULL,
    session_id = NULL,
    valid_policy = c("add", "update", "replace", "upsert"),
    max_desc_len = 250,
    debug = FALSE,

    build_model = function() {
      # generate model of the dataset using Models class
      model <- Model$new(tables = private$tables, name = self$name, description = self$description, folder_id = self$folder_id)
      private$model_list <- model$get_model()

    },

    form_upload_body = function() {
      # Form request body for creating an upload session for data uploads

      body <- list("tables" = lapply(private$tables, function(x) {
        list("name" = x$table_name,
             "updatePolicy" = x$update_policy,
             "columnHeaders" = as.list(names(x$data_frame)))
      }))
      body_json <- toJSON(body, auto_unbox = TRUE)

      private$upload_body <- list("raw" = body,
                               "json" = body_json)
    },

    load_definition = function() {
      # Load definition of an existing dataset

      response <- cube_info(connection = self$connection,
                            cube_id = self$dataset_id,
                            verbose = private$debug)

      response <- content(response, as = "parsed", type = "application/json")
      self$name <- response$cubesInfos[[1]]$cubeName
      self$owner_id <- response$cubesInfos[[1]]$ownerId
      self$path <- response$cubesInfos[[1]]$path
      self$modification_time <- response$cubesInfos[[1]]$modificationTime
      self$size <- response$cubesInfos[[1]]$size
      self$cube_state <- response$cubesInfos[[1]]$status
    },

    upload_progress = function(table_name, rows, total) {
      # Prints status of dataset upload
      print(sprintf("%s status: %s of %s rows. %1.0f%%", table_name, rows, rows, 100 * rows / total))
    },

    check_param_len = function(param, msg, max_length) {
      if (nchar(param) > max_length) {
        stop(msg)
      } else {
        return(TRUE)
      }

    },

    check_param_str = function(param, msg) {
      if (class(param) != "character") {
        stop(msg)
      } else {
        return(TRUE)
      }
    },

    check_tables = function(tables) {
      if (length(tables) == 0) {
        stop("No tables have been added to the dataset. Use `Dataset$add_table()` to add a table.")
      }
    }
  )
      )
