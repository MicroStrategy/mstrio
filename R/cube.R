# cube.R
# Create and interact with MicroStrategy cubes

#' @title Extract a MicroStrategy cube into a R Data.Frame
#'
#' @description Access, filter, publish, and extract data from MicroStrategy in-memory cubes
#' @field connection MicroStrategy connection object
#' @field cube_id Identifier of a cube.
#' @examples
#' \donttest{
#' my_cube <- Cube$new(connection=conn, cube_id="...")
#' df <- my_cube$to_dataframe()
#' # Use object IDs for metrics, attributes, and attribute elements to filter contents of a cube.
#' my_cube$metrics
#' my_cube$attributes
#' my_cube$attr_elements
#' 
#' # Then, choose those elements by passing their IDs to the Cube.apply_filters() method. To see 
#' # the chosen elements, call my_cube.filters and to clear any active filters, 
#' # call my_cube.clear_filters().
#' my_cube$apply_filters(
#'          attributes=list("A598372E11E9910D1CBF0080EFD54D63", "A59855D811E9910D1CC50080EFD54D63"),
#'          metrics=list("B4054F5411E9910D672E0080EFC5AE5B"),
#'          attr_elements=list("A598372E11E9910D1CBF0080EFD54D63:Los Angeles", 
#'          "A598372E11E9910D1CBF0080EFD54D63:Seattle"))
#' df <- my_cube$to_dataframe()
#' }
#' @docType class
#' @importFrom R6 R6Class
#' @export
Cube <- R6Class("Cube",

  public = list(

    #instance variables
    connection = NULL,
    cube_id = NULL,
    name = NULL,
    owner_id = NULL,
    path = NULL,
    last_modified = NULL,
    size = NULL,
    status = NULL,
    attributes = NULL,
    metrics = NULL,
    attr_elements = NULL,
    selected_attributes = NULL,
    selected_metrics = NULL,
    selected_attr_elements = NULL,

    initialize = function(connection, cube_id) {
      # Initialize cube contructor.

      self$connection <- connection
      self$cube_id <- cube_id

      private$load_info()
      private$load_definition()
    },

    get_attr_elements = function(verbose = TRUE) {
      # Load elements of all attributes to be accesibile by Cube$attr_elements.

      if(is.null(self$attr_elements)){
        private$load_attr_elements()
      }

      if(verbose) self$attr_elements
    },

    apply_filters = function(attributes = NULL, metrics = NULL, attr_elements = NULL, body = NULL) {
      # Apply filters on the cube data so only the chosen attributes, metrics, and attribute elements are retrieved from the Intelligence Server.
      if(!is.null(body)){
        private$filters <- body
      } else {
        self$selected_attributes <- attributes
        self$selected_metrics <- metrics
        self$selected_attr_elements <- attr_elements
        private$filters <- full_body(attributes, metrics, attr_elements)
      }
    },

    clear_filters = function() {
      # Clear previously set filters, allowing all attributes, metrics, and attribute elements to be retrieved from the Intelligence Server.

      self$selected_attributes <- NULL
      self$selected_metrics <- NULL
      self$selected_attr_elements <- NULL
      private$filters <- NULL

    },

    to_dataframe = function(offset = 0, limit = 25000, multi_df=FALSE, callback = function(x,y) {}) {
      # Extract contents of a cube into a R Data Frame. Previously `get_cube()`.
      
      # Get first cube instance
      response <- cube_instance(connection=self$connection, cube_id=self$cube_id, offset=offset, limit=limit, body=private$filters)

      if(http_error(response)) {  # http != 200
            private$response_handler(response, "Error getting cube contents.")
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
          response <- cube_instance_id(connection=self$connection, cube_id=self$cube_id, instance_id=instance_id, offset=offset_, limit=limit)
          response <- content(response)
          response_list[[length(response_list)+1]] <- parse_json(response = response)
          callback(offset_, pagination$total)
        }
        callback(pagination$total, pagination$total)
        big_df <- do.call(rbind.data.frame, response_list)
      } else {
        callback(pagination$total, pagination$total)
        big_df <- parse_json(response=response)
      }

      if(isTRUE(multi_df)) {

        # save the multitable_definition response
        tables_columns <- self$multitable_definition()

        # split dataframe to dataframes matching tables in Cube
        list_of_df <- list()

        table_names <- names(tables_columns)
        for (name in table_names) {
          list_of_df[[name]] <- subset(big_df, select = unlist(tables_columns[[name]]))
        }

        return(list_of_df)
      } else {
        return(big_df)
      }
    },

    multitable_definition = function() {
      # Return all tables names and collumns as a list of list

      response <- dataset_definition(connection=self$connection, dataset_id=self$cube_id)

      if(http_error(response)) {  # http != 200
        private$response_handler(response, "Error getting cube information.")
      }
      response <- content(response, as = "parsed", type = "application/json")

      # Create the list of list from response
      table_columns <- list()
      for (table in response$result$definition$availableObjects$tables) {
        column_list <- list()
        for (column in response$result$definition$availableObjects$columns) {
          if (table$name == column$tableName) {
            column_list <- append(column_list, column$columnName)
          }
        }
        # Add another table name and column list to the table_columns list
        table_columns[[table$name]] = column_list
      }
      return(table_columns)
    }
  ),

  private = list(

    filters = NULL,

    load_info = function() {
      # Get metadata for specific cubes. Implements GET /cubes to retrieve basic metadata.

      response <- cube_info(connection=self$connection, cube_id=self$cube_id)

      if(http_error(response)) {  # http != 200
        private$response_handler(response, "Error getting cube information.")
      }

      info <- content(response)$cubesInfos[[1]]

      self$name <- info$cubeName
      self$owner_id <- info$ownerId
      self$path <- info$path
      self$last_modified <- info$modificationTime
      self$size <- info$size
      self$status <- info$status

    },

    load_definition = function() {
      # Get the definition of a cube, including attributes and metrics. Implements GET /cubes/<cube_id>.

      response <- cube(connection = self$connection, cube_id = self$cube_id)

      if(http_error(response)) {  # http != 200
        private$response_handler(response, "Error loading Cube$attributes and Cube$metrics.")
      }

      objects <- content(response)$result$definition$availableObjects

      self$attributes <- lapply(objects$attributes, function(attr) attr$id)
      self$metrics <- lapply(objects$metrics, function(metr) metr$id)
      names(self$attributes) <- lapply(objects$attributes, function(attr) attr$name)
      names(self$metrics) <- lapply(objects$metrics, function(metr) metr$name)

    },

    load_attr_elements = function() {
      # Get the elements of cube attributes. Implements GET /cubes/<cube_id>/attributes/<attribute_id>/elements

      get_single_attr_elements = function(conn, cube_id, attr_id) {
        #helper function extracting attr elements from HTTP response.
        response <- cube_single_attr_elements(conn, cube_id, attr_id)

        if(http_error(response)) {  # http != 200
          private$response_handler(response, "Error loading Cube$attr_elements.")
        }

        elements <- lapply(content(response), function(elem) unlist(elem$id))
        names(elements) <- lapply(content(response), function(elem) unlist(elem$formValues))

        elements
      }

      self$attr_elements <- lapply(self$attributes, function(attr_id) get_single_attr_elements(self$connection, self$cube_id, attr_id))

    },

    response_handler = function(response, msg) {
      # Generic error message handler for transactions against cubes.

      status <- http_status(response)
      errors <- content(response)
      stop(sprintf("%s\n HTTP Error: %s %s %s\n I-Server Error: %s %s",
                   msg, response$status_code, status$reason, status$message, errors$code, errors$message),
           call.=FALSE)

    }
  )
)
