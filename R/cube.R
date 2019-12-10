# cube.R
# Create and interact with MicroStrategy cubes


#' @title Extract a MicroStrategy cube into a R Data.Frame
#'
#' @description Access, filter, publish, and extract data from MicroStrategy in-memory cubes
#' @field connection MicroStrategy connection object
#' @field cube_id Identifier of a cube.
#' @examples
#' \donttest{
#' # Create a connection object.
#' connection = connect_mstr(base_url, username, password, project_name)
#' 
#' # Create a cube object.
#' my_cube <- Cube$new(connection=conn, cube_id="...")
#' 
#' # See attributes and metrics in the report.
#' my_cube$attributes
#' my_cube$metrics
#' my_cube$attr_elements
#' 
#' # Specify attributes and metrics (columns) to be fetched.
#' my_cube$apply_filters(attributes = my_report$attributes[1:2],
#'                          metrics = my_report$metrics[1:2])
#'
#' # See the selection of attributes, metrics and attribute elements.
#' my_cube$selected_attributes
#' my_cube$selected_metrics
#' my_cube$selected_attr_elements
#'
#' # Clear filtering to load a full dataset.
#' my_cube$clear_filters()
#'
#' # Fetch data from the Intelligence Server.
#' my_cube$to_dataframe()
#'
#' # See the dataframe.
#' my_cube$dataframe
#' }
#' @docType class
#' @importFrom R6 R6Class
#' @export
Cube <- R6Class("Cube",

  public = list(

    #instance variables
    connection = NULL,
    cube_id = NULL,
    offset = 0,
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
    dataframe = NULL,
    dataframe_list = NULL,
    table_definition = NULL,
    filters = NULL,

    initialize = function(connection, cube_id) {
      # Initialize cube contructor.

      self$connection <- connection
      self$cube_id <- cube_id

      private$load_info()
      private$load_definition()

      self$filters <- Filter$new(attributes = self$attributes,
                                metrics = self$metrics,
                                attr_elements = NULL)
    },

    to_dataframe = function(limit = 25000, multi_df = FALSE, callback = function(x,y) {}) {
      # Extract contents of a cube into a R Data Frame. Previously `get_cube()`.
      
      # Get first cube instance
      response <- cube_instance(connection=self$connection,
                                cube_id=self$cube_id,
                                offset=self$offset,
                                limit=limit,
                                body=self$filters$filter_body())

      if(http_error(response)) {  # http != 200
            private$response_handler(response, private$err_msg_instance)
      }

      # Get the pagination totals from the response object
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
          response <- cube_instance_id(connection=self$connection,
                                       cube_id=self$cube_id,
                                       instance_id=instance_id,
                                       offset=offset_,
                                       limit=limit)
          response <- content(response)
          response_list[[length(response_list)+1]] <- parse_json(response = response)
          callback(offset_, pagination$total)
        }
        callback(pagination$total, pagination$total)
        self$dataframe <- do.call(rbind.data.frame, response_list)
      } else {
        callback(pagination$total, pagination$total)
        self$dataframe <- parse_json(response=response)
      }
      
      # convert column types
      self$dataframe <- type.convert(self$dataframe, as.is = TRUE)
      
      if(isTRUE(multi_df)) {

        # save the multitable_definition response to the instance
        private$multitable_definition()

        # split dataframe to dataframes matching tables in Cube
        self$dataframe_list <- list()

        table_names <- names(self$table_definition)
        for (name in table_names) {
          self$dataframe_list[[name]] <- subset(self$dataframe, select = unlist(self$table_definition[[name]]))
        }

        return(self$dataframe_list)
      } else if(length(self$dataframe) == 0) {
        stop(print("Data Frame looks empty."))
      } else {
      return(self$dataframe)
    }
    },

    apply_filters = function(attributes = NULL, metrics = NULL, attr_elements = NULL) {
      # Instantiate the filter object and download all attr_elements of the cube.
      # Apply filters on the cube data so only the chosen attributes, metrics, 
      # and attribute elements are retrieved from the Intelligence Server.

      #1st check: if params is null finish
      if(is.null(attributes) & is.null(metrics) & is.null(attr_elements)) {
      }
      else {
        
        #TODO replace with get_attr_elements()
        #2nd check: if self.attr_elem is null
        if(is.null(self$attr_elements) & !is.null(attr_elements)) {
          #load attr_elem and save the result to report instance
          private$load_attr_elements()
        }
        # 3rd check if filter object is created with attr_elem and if report has attr_elem downloaded
        if(is.null(self$filters$attr_elems) & !is.null(attr_elements)) {
          self$filters <- Filter$new(attributes=self$attributes,
                                          metrics=self$metrics,
                                          attr_elements=self$attr_elements)
        }

        # Previous functionality of apply_filters: set filters as specified in apply filters
        if(!is.null(attributes)){
          if(length(attributes)==0){
            self$filters$attr_selected <- list()
          }else{
            self$filters$select(attributes)
          }
        } # Else do nothing

        if(!is.null(metrics)){
          if(length(metrics)==0){
            self$filters$metr_selected <- list()
          }else{
            self$filters$select(metrics)
          }
        } # Else do nothing

        if(length(attr_elements)>0) {
          self$filters$select(attr_elements)
        }

        # Assign filter values in the report instance
        self$selected_attributes <- self$filters$attr_selected
        self$selected_metrics <- self$filters$metr_selected
        self$selected_attr_elements <- self$filters$attr_elem_selected
      }
    },

    clear_filters = function() {
      # Clear previously set filters, allowing all attributes, metrics, and attribute elements to be retrieved from the Intelligence Server.
      self$filters$clear()

      self$selected_attributes <- self$filters$attr_selected
      self$selected_metrics <- self$filters$metr_selected
      self$selected_attr_elements <- self$filters$attr_elem_selected
    },

    get_attr_elements = function(verbose = TRUE) {
      # Load elements of all attributes to be accesibile by Cube$attr_elements.

      if (is.null(self$attr_elements)) {
        private$load_attr_elements()
      }
      if (verbose) self$attr_elements
    }
  ),


  private = list(

    err_msg_information = "Error getting cube information. Check cube ID.",
    err_msg_definition = "Error getting cube definition. Check cube ID.",
    err_msg_instance = "Error getting cube contents.",
    err_msg_elements = "Error loading attribute elements.",

    load_info = function() {
      # Get metadata for specific cubes. Implements GET /cubes to retrieve basic metadata.

      response <- cube_info(connection=self$connection, cube_id=self$cube_id)

      if(http_error(response)) {  # http != 200
        private$response_handler(response, private$err_msg_information)
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
        private$response_handler(response, private$err_msg_definition)
      }

      objects <- content(response)$result$definition$availableObjects

      self$attributes <- lapply(objects$attributes, function(attr) attr$id)
      self$metrics <- lapply(objects$metrics, function(metr) metr$id)
      names(self$attributes) <- lapply(objects$attributes, function(attr) attr$name)
      names(self$metrics) <- lapply(objects$metrics, function(metr) metr$name)

    },

    load_attr_elements = function() {
      # Get the elements of cube attributes. Implements GET /cubes/<cube_id>/attributes/<attribute_id>/elements
      is_empty <- function(x){
      # Helper function to handle empty 'formValues'.
        if(is.na(x) || is.null(x) || nchar(x)==0) return(TRUE)
        else return(FALSE)
      }

      get_single_attr_elements = function(conn, cube_id, attr_id, offset=self$offset, limit=160000) {
        # Helper function extracting attr elements from HTTP response.
        
        # Fetch first chunk to determine what is the total number of elements.
        response <- cube_elements(conn, cube_id, attr_id, offset, limit)

        if(http_error(response)) {  # http != 200
          private$response_handler(response, private$err_msg_elements)
        }
        
        elements <- lapply(content(response), function(elem) unlist(elem$id))
        names(elements) <- lapply(content(response), function(elem){
                              if(is_empty(unlist(elem$formValues))) unlist(elem$id)
                              else unlist(elem$formValues)
                              })
                                  
        total <- as.numeric(response$headers$'x-mstr-total-count')
      
        # Fetch the rest of elements if their number exceeds the limit.
        if(total > limit){
          offsets = seq(from=limit, to=total, by=limit)

          for(offset_ in offsets){
            response <- cube_elements(conn, cube_id, attr_id, offset_, limit)

            if(http_error(response)) {  # http != 200
              private$response_handler(response, private$err_msg_elements)
            }

            chunk <- lapply(content(response), function(elem) unlist(elem$id))
            names(chunk) <- lapply(content(response), function(elem){
                              if(is_empty(unlist(elem$formValues))) unlist(elem$id)
                              else unlist(elem$formValues)
                            })
            elements <- c(elements, chunk)
          }
        }
                                
        elements
      }

      # Fetch elements for all attributes
      self$attr_elements <- lapply(self$attributes, function(attr_id){
        list("id" = attr_id,
             "elements" = get_single_attr_elements(self$connection, self$cube_id, attr_id))
      })

    },

    multitable_definition = function() {
      # Return all tables names and collumns as a list of list

      response <- dataset_definition(connection=self$connection, dataset_id=self$cube_id)

      if(http_error(response)) {  # http != 200
        private$response_handler(response, "Error getting cube information.")
      }
      response <- content(response, as = "parsed", type = "application/json")

      # Create the list of list from response
      self$table_definition <- list()
      for (table in response$result$definition$availableObjects$tables) {
        column_list <- list()
        for (column in response$result$definition$availableObjects$columns) {
          if (table$name == column$tableName) {
            column_list <- append(column_list, column$columnName)
          }
        }
        # Add another table name and column list to the table_definition list
        self$table_definition[[table$name]] <- column_list
      }
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
