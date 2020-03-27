# cube.R
# Create and interact with MicroStrategy cubes

#' @title Extract a MicroStrategy cube into a R Data.Frame
#'
#' @description Access, filter, publish, and extract data from MicroStrategy in-memory cubes
#'
#'      Attributes:
#'        connection: MicroStrategy connection object returned by `microstrategy.Connection()`.
#'        cube_id: Identifier of a pre-existing cube containing the required data.
#'        parallel (bool, optional): If True, use asynchronous requests to download data. If False (default), this
#'          feature will be disabled.
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
#' @rawNamespace import(crul, except = handle)
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
    offsets = NULL,
    cookies = NULL,
    size_limit = 10000000,
    initial_limit = 1000,
    parallel = FALSE,


    initialize = function(connection, cube_id, parallel=FALSE) {
      # Initialize cube contructor.

      self$connection <- connection
      self$cube_id <- cube_id
      self$parallel <- parallel
      private$load_info()
      private$load_definition()

      self$filters <- Filter$new(attributes = self$attributes,
                                metrics = self$metrics,
                                attr_elements = NULL)
    },

    to_dataframe = function(limit = NULL, multi_df = FALSE, callback = function(x, y) { }) {
      # Extract contents of a cube into a R Data Frame.

      #checking if given limit is valid
      auto <- TRUE
      if (is.null(limit)) {
      } else if (limit < 1 & limit != -1) {
        warning("Limit has to be larger than 0, new limit will be set automatically", immediate. = TRUE)
      } else {
        auto <- FALSE
        self$initial_limit <- limit
      }

      body <- self$filters$filter_body()
      if (length(body) == 0) { body <- c() }
      body <- toJSON(body, auto_unbox = TRUE)
      # Get first cube instance
      response <- cube_instance(connection = self$connection,
                                cube_id = self$cube_id,
                                offset = self$offset,
                                limit = self$initial_limit,
                                body = body)

      #getting size of the first response in bytes, to use in auto chunk sizing
      size_bytes <- as.integer(object.size(response))
      # Get the pagination totals from the response object
      response <- content(response)
      pagination <- response$data$paging
      instance_id <- response$instanceId
      callback(0, pagination$total)

      # initialize parser and process first response
      p <- Parser$new(response = response)
      p$parse(response = response)

      if (pagination$current != pagination$total) {
        #auto select chunk limit based on desired chunk size in bytes
        if (auto == TRUE) {
          limit <- max(1000, round(((self$initial_limit*self$size_limit)/size_bytes), digits = 0))
          message(sprintf("Chunk limit set automatically to %s", limit))
        }
        # Create vector of offset parameters to iterate over
        offsets <- pagination$current
        while (tail(offsets, 1) + limit < pagination$total) {
          offsets <- append(offsets, tail(offsets, 1) + limit)
        }
        self$offsets <- offsets

        #asynchronous download of chunks and dataframe creation
        if (isTRUE(self$parallel)) {
          future <- private$fetch_future(self$offsets, limit = limit, conn = self$connection, cookies = self$cookies,
                                      cube_id = self$cube_id, instanceId = instance_id)
          future_responses <- AsyncVaried$new(.list = future)
          future_responses$request()
          failed_chunks_idx <- which("200" != future_responses$status_code())
          future_responses <- future_responses$parse()
          future_responses <- lapply(future_responses, fromJSON, simplifyVector = FALSE,
                                            simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
          if (length(failed_chunks_idx) != 0) { future_responses <- private$retry_chunks(failed_chunks_idx = 
          failed_chunks_idx, future_responses, instance_id, self$offsets, limit) }
          sapply(future_responses, p$parse)
          callback(pagination$total, pagination$total)

        } else {
          #sequential download of chunks and dataframe creation
          sapply(c(self$offsets), private$fetch_chunk, p = p, instance_id = instance_id, limit = limit,
                                                       pagination = pagination, callback = callback)
          callback(pagination$total, pagination$total)
        }
      }
      self$dataframe <- p$to_dataframe()

      if (isTRUE(multi_df)) {

        # save the multitable_definition response to the instance
        private$multitable_definition()

        # split dataframe to dataframes matching tables in Cube
        self$dataframe_list <- list()

        table_names <- names(self$table_definition)
        for (name in table_names) {
          self$dataframe_list[[name]] <- subset(self$dataframe, select = unlist(self$table_definition[[name]]))
        }
        return(self$dataframe_list)
      } else {
        return(self$dataframe)
      }
    },

    apply_filters = function(attributes = NULL, metrics = NULL, attr_elements = NULL) {
      # Instantiate the filter object and download all attr_elements of the cube.
      # Apply filters on the cube data so only the chosen attributes, metrics,
      # and attribute elements are retrieved from the Intelligence Server.

      #1st check: if params is null finish
      if (is.null(attributes) & is.null(metrics) & is.null(attr_elements)) {
      }
      else {
        #2nd check: if self.attr_elem is null
        if (!is.null(attr_elements)) {
          #load attr_elem and save the result to cube instance
          self$get_attr_elements()
        }
        # 3rd check if filter object is created with attr_elem and if cube has attr_elem downloaded
        if (is.null(self$filters$attr_elems) & !is.null(attr_elements)) {
          self$filters <- Filter$new(attributes = self$attributes,
                                          metrics = self$metrics,
                                          attr_elements = self$attr_elements)
        }

        # Previous functionality of apply_filters: set filters as specified in apply filters
        if (!is.null(attributes)) {
          if (length(attributes) == 0) {
            self$filters$attr_selected <- list()
          } else {
            self$filters$select(attributes)
          }
        }
        # Else do nothing

        if (!is.null(metrics)) {
          if (length(metrics) == 0) {
            self$filters$metr_selected <- list()
          } else {
            self$filters$select(metrics)
          }
        }
        # Else do nothing

        if (length(attr_elements) > 0) {
          self$filters$select(attr_elements)
        }

        # Assign filter values in the cube instance
        self$selected_attributes <- self$filters$attr_selected
        self$selected_metrics <- self$filters$metr_selected
        self$selected_attr_elements <- self$filters$attr_elem_selected
      }
    },

    clear_filters = function() {
      # Clear previously set filters, allowing all attributes, metrics, and attribute elements to
      # be retrieved from the Intelligence Server.
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

    load_info = function() {
      # Get metadata for specific cubes. Implements GET /cubes to retrieve basic metadata.

      response <- cube_info(connection = self$connection, cube_id = self$cube_id)

      self$cookies <- paste0('JSESSIONID=', response$cookies$value[[1]], '; iSession=', response$cookies$value[[2]])

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

      objects <- content(response)$definition$availableObjects

      self$attributes <- lapply(objects$attributes, function(attr) attr$id)
      self$metrics <- lapply(objects$metrics, function(metr) metr$id)
      names(self$attributes) <- lapply(objects$attributes, function(attr) attr$name)
      names(self$metrics) <- lapply(objects$metrics, function(metr) metr$name)

    },

    fetch_chunk = function(p, instance_id, offset_, limit, pagination, callback) {
      #fetching chunk for sequential download
      response <- cube_instance_id(connection = self$connection,
                                   cube_id = self$cube_id,
                                   instance_id = instance_id,
                                   offset = offset_,
                                   limit = limit)
      p$parse(response = content(response))
      callback(offset_, pagination$total)
    },

    fetch_future = function(offsets, limit, conn, cookies, cube_id, instanceId) {
      #fetching a set of http requests for async downloading
      future <- list()

      url <- paste0(conn@base_url, "/api/v2/cubes/", cube_id, "/instances/", instanceId)
      all_headers <- list("X-MSTR-AuthToken" = conn@auth_token,
                          "X-MSTR-ProjectID" = conn@project_id,
                          "Cookie" = cookies)

      for (offset_ in offsets) {
        respons <- HttpRequest$new(url = url,
                                   headers = all_headers)
        response <- respons$get(query = list(offset = format(offset_, scientific=FALSE, trim=TRUE),
                                limit = format(limit, scientific = FALSE, trim = TRUE)))
        future <- append(future, response)
      }
      return(future)
    },

    retry_chunks = function(failed_chunks_idx, future_responses, instance_id, offsets, limit) {
      #retrying failed chunks
      for (chunk in failed_chunks_idx) {
        response <- cube_instance_id(connection = self$connection,
                                     cube_id = self$cube_id,
                                     instance_id = instance_id,
                                     offset = offsets[[chunk]],
                                     limit = limit)
        future_responses[[chunk]] <- content(response)
      }
      return(future_responses)
    },

    load_attr_elements = function() {
      # Get the elements of cube attributes. Implements GET /cubes/<cube_id>/attributes/<attribute_id>/elements

      is_empty <- function(x) {
        # Helper function to handle empty 'formValues'.
        if (is.na(x) || is.null(x) || nchar(x) == 0) return(TRUE)
        else return(FALSE)
      }

      get_single_attr_elements = function(response=NULL, conn, cube_id, attr_id, limit) {

        if (is.null(response)) {
          response <- cube_elements(connection = self$connection,
                                       cube_id = self$cube_id,
                                       attribute_id = attr_id,
                                       offset = 0,
                                       limit = limit) 
          total <- as.numeric(response$headers$'x-mstr-total-count')
          response <- content(response)
        }
        else if (response[["status_code"]] != 200) {
          response <- cube_elements(connection = self$connection,
                                       cube_id = self$cube_id,
                                       attribute_id = attr_id,
                                       offset = 0,
                                       limit = limit) 
          total <- as.numeric(response$headers$'x-mstr-total-count')
          response <- content(response)
        } else if (response[["status_code"]] == 200){
          total <- as.numeric(response[["response_headers"]][["x-mstr-total-count"]])
          response <- response$parse(encoding='UTF-8')
          response <- fromJSON(response, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
       }

        elements <- lapply(response, function(elem) unlist(elem$id))
        names(elements) <- lapply(response, function(elem) {
          if (is_empty(unlist(elem$formValues))) unlist(elem$id)
          else unlist(elem$formValues)
        })

        # Fetch the rest of elements if their number exceeds the limit.
        if (total > limit) {
          offsets = seq(from = limit, to = total, by = limit)

          for (offset_ in offsets) {
            response <- cube_elements(conn, cube_id, attr_id, offset_, limit)

            chunk <- lapply(content(response), function(elem) unlist(elem$id))
            names(chunk) <- lapply(content(response), function(elem) {
              if (is_empty(unlist(elem$formValues))) unlist(elem$id)
              else unlist(elem$formValues)
            })
            elements <- c(elements, chunk)
          }
        }

        elements
      }

      #fetching a set of http requests for async downloading of attribute elements
      fetch_future_attr_elems = function(attributes, limit = 160000, conn, cookies, cube_id, offset = 0) {
        future <- list()

        all_headers <- list("X-MSTR-AuthToken" = conn@auth_token,
                            "X-MSTR-ProjectID" = conn@project_id,
                            "Cookie" = cookies)

        for (attr_id in attributes) {
          url <- paste0(conn@base_url, "/api/cubes/", cube_id, "/attributes/", attr_id, "/elements")
          respons <- HttpRequest$new(
            url = url,
            headers = all_headers
          )
          response <- respons$get(query = list(offset = format(offset, scientific=FALSE, trim=TRUE),
                            limit = format(limit, scientific=FALSE, trim=TRUE)))
          future <- append(future, response)
        }
        return(future)
      }

      #fetching attribute elements workflow
      if(self$parallel == TRUE) {
        future <- fetch_future_attr_elems(attributes = self$attributes, conn = self$connection,
                                          cookies = self$cookies, cube_id = self$cube_id)
        future_responses <- AsyncVaried$new(.list = future)
        future_responses$request()
        responses_future <- future_responses$responses()
        self$attr_elements <- lapply(responses_future, function(response) {
          #recovering attribute id of already downloaded responses, successful or failed
          if (response[["status_code"]] == 200) {
            as.list(strsplit(response[["response_headers"]][["link"]], "/")) -> link
            link[[1]][[7]] -> attr_id
          } else {
            as.list(strsplit(response[["url"]], "/")) -> link
            link[[1]][[9]] -> attr_id
          }
          list("id" = attr_id, "elements" = get_single_attr_elements(response = response,
                                                                     conn = self$connection,
                                                                     cube_id = self$cube_id,
                                                                     attr_id = attr_id,
                                                                     limit = 160000))
        })
        names(self$attr_elements) <- names(self$attributes)
      }
      else {
        self$attr_elements <- lapply(self$attributes, function(attr_id) {
        list("id" = attr_id, "elements" = get_single_attr_elements(conn = self$connection,
                                                                   cube_id = self$cube_id,
                                                                   limit = 160000,
                                                                   attr_id = attr_id))
        })
      }
    },

    multitable_definition = function() {
      # Return all tables names and collumns as a list of list

      response <- dataset_definition(connection = self$connection, dataset_id = self$cube_id)

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
    }
  )
)