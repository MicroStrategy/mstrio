# utils-model.R
# create and update dataset definitions

#' @title Create the definition of multi-table and single-table datasets
#'
#' @description Create the definition of a dataset containing one or more tables. The definition includes the name and description of the dataset and the name and description of each table, attribute, and metric within the dataset.
#' @field tables List containing lists of data.frames and corresponding table names
#' @field name Name of the dataset
#' @field description Description of the data set. Must be less than or equal to 250 characters
#' @field folder_id ID of the shared folder that the dataset should be created within. If NULL, defaults to the user's My Reports folder
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
#' tables = list(list("table_name" = "employee_id",
#'                     "data_frame" = df1),
#'               list("table_name" = "employee_data",
#'                    "data_frame" = df2))
#'
#' # Generate the data model
#' model <- Model$new(tables=tables, name="Employees", description="Employee Analytics Data")
#' model_info <- model$get_model()
#' }
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#' @export
Model <- R6Class("Model",

  public = list(

    # internal params used for validation / mapping checks
    KEY_TABLE_NAME = "table_name",
    KEY_DATA_FRAME = "data_frame",
    KEY_AS_ATTR = "to_attribute",
    KEY_AS_METR = "to_metric",
    KEY_UPDATE_POL = "update_policy",
    INVALID_COL_CHARS = c(' ', '.'),  # check for invalid characters in column names
    MAX_DESC_LEN = 250,  # max string length for dataset name and description

    # TODO: These should be private...
    name = NULL,
    description = NULL,
    folder_id = NULL,
    tables = NULL,

    initialize = function(tables, name, description=NULL, folder_id=NULL) {
      # Initializes Model with tables, a name, and an optional description

      # check integrity of tables list
      self$tables <- tables
      private$check_table_list(tables=self$tables)

      # check dataset name params
      self$name <- name
      private$check_param_str(self$name,
                              msg="Dataset name should be a string.")
      private$check_param_len(self$name,
                              length=self$MAX_DESC_LEN,
                              msg=paste("Dataset name should be <=", self$MAX_DESC_LEN, "characters."))

      # check dataset description params
      if(is.null(description)) {
        self$description <- ""
      } else {
        self$description <- description
        private$check_param_str(self$description,
                                msg="Dataset name should be a string.")
        private$check_param_len(self$description,
                                length=self$MAX_DESC_LEN,
                                msg=paste("Dataset name should be <=", self$MAX_DESC_LEN, "characters."))
      }

      # check folder_id param
      if(is.null(folder_id)) {
        self$folder_id <- ""
      } else {
        self$folder_id <- folder_id
        private$check_param_str(self$folder_id,
                                msg="Folder ID should be a string.")
      }

      # build the model
      private$build(tables=self$tables)

    },

    get_model = function() {
      # Return the model object
      return(list("raw" = private$model,
                  "json" = private$model_json))
    },
    get_name = function() {
      return(self$name)
    },
    get_description = function() {
      return(self$description)
    },
    get_folder_id = function() {
      return(self$folder_id)
    },
    get_tables = function() {
      return(private$tbls)
    },
    get_attributes = function() {
      return(private$attr)
    },
    get_metrics = function() {
      return(private$metr)
    }
  ),

  private = list(

    # lists of table, attr, and metric objects
    tbls = list(),
    metr = list(),
    attr = list(),

    model = NULL,       # dataset model as a list of R objects
    model_json = NULL,  # dataset model as a JSON string

    build = function(tables) {
      # Generates the data model by mapping attributes and metrics from list of tables
      for(table in tables) {

        # map column names and column types
        col_names <- names(table[[self$KEY_DATA_FRAME]])
        col_types <- lapply(table[[self$KEY_DATA_FRAME]], private$map_data_type)

        # map tables
        private$add_table(name = table[[self$KEY_TABLE_NAME]], col_names = col_names, col_types = col_types)

        # map attributes and metrics
        for(i in seq_along(col_names)) {

          name = col_names[[i]]
          type = col_types[[i]]

          if(private$is_metric(type)) { # attribute
            ## check for metric override
            if(self$KEY_AS_ATTR %in% names(table) && name %in% table[[self$KEY_AS_ATTR]]) {
              private$add_attribute(name, table[[self$KEY_TABLE_NAME]])
            } else {
              private$add_metric(name, table[[self$KEY_TABLE_NAME]])
            }
          } else { # metric
            if(self$KEY_AS_METR %in% names(table) && name %in% table[[self$KEY_AS_METR]]) {
              private$add_metric(name, table[[self$KEY_TABLE_NAME]])
            } else {
              private$add_attribute(name, table[[self$KEY_TABLE_NAME]])
            }
          }
        }
      }

      # set the model object
      private$model <- list("name" = self$name,
                            "description" = self$description,
                            "folderId" = self$folder_id,
                            "tables" = private$tbls,
                            "metrics" = private$metr,
                            "attributes" = private$attr)

      private$model_json <- toJSON(private$model, auto_unbox = TRUE)
    },
    add_metric = function(name, table_name) {
      # Add a metric to a metric list instance
      new_metric <- list(list("name" = name,
                              "expressions" = list(list("tableName" = table_name,
                                                        "columnName" = name))))
      private$metr <- c(private$metr, new_metric)

    },
    add_attribute = function(name, table_name) {
      # Add an attribute to an attribute list instance

      new_attribute <- list(list("name" = name,
                                 "attributeForms" = list(list("category" = "ID",
                                                              "expressions" = list(list("tableName" = table_name,
                                                                                        "columnName" = name))))))

      private$attr <- c(private$attr, new_attribute)

    },
    add_table = function(name, col_names, col_types) {
      # Add a table to a table list instance

      columnHeaders <- lapply(seq_along(col_names), function(x) {
        list(name = col_names[[x]],
             dataType = col_types[[x]])
      })

      new_table <- list(list("name" = name,
                             "columnHeaders" = columnHeaders))

      private$tbls <- c(private$tbls, new_table)

    },
    map_data_type = function(datatype) {
      # Maps a R data type to a MicroStrategy data type

      ## treated as metrics
      if(class(datatype) %in% c('double', 'numeric'))              return('DOUBLE')
      if(class(datatype) %in% c('integer'))                        return('INTEGER')

      ## treated as attributes
      if(class(datatype) %in% c('string', 'factor', 'character'))  return('STRING')
      if(class(datatype) %in% c('logical'))                        return("BOOL")
      if(class(datatype) %in% c('Date'))                           return("DATE")
      if(any(grepl("POSIX", class(datatype))))                     return("DATETIME")

    },
    check_table_list = function(tables) {
      # Check integrity of table list parameter

      # tables must be a list
      if(class(tables) != "list") {
        stop("Elements of 'tables' must be a list of lists.")
      }

      # tables cannot be length 0
      if(length(tables) == 0) {
        stop("No tables have been added to the dataset.")
      }

      # check integrity of each table passed to tables
      for(table in tables) {
        private$check_table(table)
      }

    },
    check_table = function(table) {

      # force each table to be a list
      if(class(table) != "list") {
        stop("Elements of 'tables' must be a list of lists.")
      }

      # check named elements of each table in tables
      if(!all(names(table) %in% c(self$KEY_TABLE_NAME, self$KEY_DATA_FRAME, self$KEY_AS_ATTR, self$KEY_AS_METR, self$KEY_UPDATE_POL))) {
        stop(paste0("Named elements of 'tables' must be '",
                    self$KEY_TABLE_NAME, ", ", self$KEY_DATA_FRAME, ", ", self$KEY_UPDATE_POL, "' and optionally '",
                    self$KEY_AS_ATTR, "' or '", self$KEY_AS_METR, "'"))
      }

      # check that the value of the data frame key is a R data.frame
      if(class(table[[self$KEY_DATA_FRAME]]) != "data.frame") {
        msg <- paste0("R data.frame must be passed as the value in the '", table[[self$KEY_TABLE_NAME]], "' element of the tables list.")
        stop(msg)
      }

      # check for presence of invalid characters in data frame column names
      if(any(names(table[[self$KEY_DATA_FRAME]]) %in% self$INVALID_COL_CHARS)) {
        stop(paste("Column names cannot contain any whitespace or '.'"))
      }

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
    is_metric = function(datatype) {
      # Helper function for determining if the requested datatype is (by default) a metric or attribute
      if(datatype %in% c("DOUBLE", "INTEGER")) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  )
)
