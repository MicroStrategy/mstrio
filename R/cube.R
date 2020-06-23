# cube.R
# Create and interact with MicroStrategy cubes

#' @title Extract a MicroStrategy cube into a R Data.Frame
#' @description Access, filter, publish, and extract data from MicroStrategy in-memory cubes
#'
#' Create a Cube object to load basic information on a cube dataset. Specify subset of cube to
#' be fetched through apply_filters() and clear_filters(). Fetch dataset through to_dataframe() method.
#' @field connection MicroStrategy connection object
#' @field cube_id Identifier of a report.
#' @field parallel If TRUE, downloads cube data asynchronously. FALSE by default.
#' @field name Cube name.
#' @field owner_id ID of Cube owner.
#' @field path Exact path of the cube location.
#' @field last_modified Date of latest Cube modification.
#' @field size Cube size.
#' @field status Cube status.
#' @field attributes Cube attributes.
#' @field metrics Cube metrics
#' @field attr_elements Cube attribute elements.
#' @field selected_attributes Attributes selected for filtering.
#' @field selected_metrics Metrics selected for filtering.
#' @field selected_attr_elements Attribute elements selected for filtering.
#' @field dataframe Dataframe containing data fetched from the Cube.
#' @field dataframe_list List of dataframes split to match tables in Cube.
#' @field instance_id Identifier of an instance if cube instance has been already initialized.
#' @examples
#' \donttest{
#' # Create a connection object.
#' connection = Connection$new(base_url, username, password, project_name)
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

  public=list(

  #instance variables
    connection=NULL,
    cube_id=NULL,
    parallel=FALSE,
    name=NULL,
    owner_id=NULL,
    path=NULL,
    last_modified=NULL,
    size=NULL,
    status=NULL,
    attributes=NULL,
    metrics=NULL,
    attr_elements=NULL,
    selected_attributes=NULL,
    selected_metrics=NULL,
    selected_attr_elements=NULL,
    dataframe=NULL,
    dataframe_list=NULL,
    instance_id=NULL,

    #' @description
#' Initialize an instance of a cube.
#' @param connection MicroStrategy connection object. See Connection class.
#' @param cube_id Identifier of a pre-existing cube containing the required data.
#' @param instance_id Identifier of an instance if cube instance has been already initialized, NULL by default.
#' @param parallel (bool, optional):  If True, utilize optimal number of threads to increase the download
#' speed. If False (default), this feature will be disabled.
    initialize=function(connection, cube_id, instance_id=NULL, parallel=FALSE) {
      self$connection <- connection
      self$cube_id <- cube_id
      self$instance_id <- instance_id
      self$parallel <- parallel
      private$load_info()
      private$load_definition()

      private$filters <- Filter$new(attributes=self$attributes,
                                    metrics=self$metrics,
                                    attr_elements=NULL)
      self$selected_attributes <- private$filters$selected_attributes
      self$selected_metrics <- private$filters$selected_metrics
    },

#' @description Extract contents of a cube into a R Data Frame.
#' @param limit (int, optional): Used to control data extraction behaviour on cubes with a large number of rows. By
#' default the limit is calculated automatically. If TRUE, overrides automatic limit.
#' @param multi_df If True (default), returns a list of dataframes resembling the table structure of the cube. If
#' FALSE, returns one dataframe.
#' @param callback used by the GUI to extract the progress information.
#' @return Dataframe with data fetched from the given Cube.
    to_dataframe=function(limit=NULL, multi_df=FALSE, callback=function(x, y) { }) {
      #checking if given limit is valid
      auto <- TRUE
      if (is.null(limit)) {
      } else if (limit < 1 & limit != -1) {
        warning("Limit has to be larger than 0, new limit will be set automatically", immediate.=TRUE)
      } else {
        auto <- FALSE
        private$initial_limit <- limit
      }

      if (is.null(self$instance_id)) {
        response <- private$initialize_cube()
      } else {
        response <- tryCatch({
          private$get_chunk(instance_id=self$instance_id, offset=0, limit=private$initial_limit)
        },
        error=function(e) {
          private$initialize_cube()
        })
      }

      #getting size of the first response in bytes, to use in auto chunk sizing
      size_bytes <- as.integer(object.size(response))
      # Get the pagination totals from the response object
      response <- content(response)
      pagination <- response$data$paging
      instance_id <- response$instanceId
      callback(0, pagination$total)

      # initialize parser and process first response
      p <- Parser$new(response=response)
      p$parse(response=response)

      if (pagination$current != pagination$total) {
        #auto select chunk limit based on desired chunk size in bytes
        if (auto == TRUE) {
          limit <- max(1000, round(((private$initial_limit * private$size_limit) / size_bytes), digits=0))
          if (private$debug) { message(sprintf("Chunk limit set automatically to %s", limit)) }
        }
        # Create vector of offset parameters to iterate over
        offsets <- pagination$current
        while (tail(offsets, 1) + limit < pagination$total) {
          offsets <- append(offsets, tail(offsets, 1) + limit)
        }

        #asynchronous download of chunks and dataframe creation
        if (isTRUE(self$parallel)) {
          if (private$debug == TRUE) { print("Downloading Asynchronously") }
          future <- private$fetch_future(offsets, limit=limit, conn=self$connection,
                                         cookies=private$cookies, cube_id=self$cube_id, instanceId=instance_id)
          future_responses <- AsyncVaried$new(.list=future)
          future_responses$request()
          failed_chunks_idx <- which("200" != future_responses$status_code())
          future_responses <- future_responses$parse()
          private$parse_chunk(responses=future_responses,
                                       parser=p,
                                       failed_chunks_idx=failed_chunks_idx,
                                       instance_id=instance_id,
                                       limit=limit)

          callback(pagination$total, pagination$total)

        } else {
          #sequential download of chunks and dataframe creation
          if (private$debug == TRUE) { print("Downloading Sequentially") }
          sapply(c(offsets), private$fetch_chunk, p=p, instance_id=instance_id, limit=limit,
                                                       pagination=pagination, callback=callback)
          callback(pagination$total, pagination$total)
        }
      }
      self$dataframe <- p$to_dataframe()

      if (isTRUE(multi_df)) {
        self$dataframe_list <- private$split_cube_into_tables()
        return(self$dataframe_list)
      } else {
        return(self$dataframe)
      }
    },

#' @description Apply filters on the cube data so only the chosen attributes, metrics, and attribute elements are
#' retrieved from the Intelligence Server.
#' @param attributes (list or None, optional): ID numbers of attributes to be included in the filter. If list is
#' empty, no attributes will be selected and metric data will be aggregated.
#' @param metrics (list or None, optional): ID numbers of metrics to be included in the filter. If list is empty,
#' no metrics will be selected.
#' @param attr_elements (list or None, optional): Attributes' elements to be included in the filter.
#' @param operator (character, optional): Supported view filter operators are either "In" or "NotIn". This defines
#' whether data will include ("In") or exclude ("NotIn") the supplied attr_elements values.
    apply_filters=function(attributes=NULL, metrics=NULL, attr_elements=NULL, operator="In") {
      private$filters$operator <- operator

      # Clear already filtered objects from the filter object if a new filter is applied
      if (!is.null(attributes)) { private$filters$clear("attributes") }
      if (!is.null(metrics)) { private$filters$clear("metrics") }
      if (!is.null(attr_elements)) { private$filters$clear("attribute_elements") }

      # Select the objects
      if (!is.null(attributes)) { private$filters$select(attributes, "automatic") }
      if (!is.null(metrics)) { private$filters$select(metrics, "automatic") }
      if (length(attr_elements) > 0) { private$filters$select(attr_elements, "attribute_elements") }

      # Assign filter values in the cube instance
      self$selected_attributes <- private$filters$selected_attributes
      self$selected_metrics <- private$filters$selected_metrics
      self$selected_attr_elements <- private$filters$selected_attr_elements
    },

#' @description Clear previously set filters, allowing all attributes, metrics, and attribute elements to be retrieved.
    clear_filters=function() {

      private$filters$clear()
      # Select all attributes/metrics by default when no filters are specified
      private$filters$select(self$attributes, "automatic")
      private$filters$select(self$metrics, "automatic")

      self$selected_attributes <- private$filters$selected_attributes
      self$selected_metrics <- private$filters$selected_metrics
      self$selected_attr_elements <- private$filters$selected_attr_elements
    },

#' @description Load all attribute elements of the Cube. Accessible via Cube$attr_elements
#' Fetching attriubte elements will also allow for validating attriute elements by the filter object.
#' @param verbose If TRUE, displays list of attribute elements.
    get_attr_elements=function(verbose=TRUE) {
      if (is.null(self$attr_elements)) {
        private$load_attr_elements()

        # add downloaded attribute elements to the filter object
        private$filters$attr_elements=unlist(sapply(self$attr_elements, function(attr) { attr$elements }), use.names=TRUE)
      }
      if (verbose) self$attr_elements
    },

#' @description Update single-table cube easily with the data frame stored in the Cube instance (cube$dataframe).
#' Before the update, make sure that the data frame has been modified.
#' @param update_policy (character) Update operation to perform. One of 'add' (inserts new, unique rows), 'update'
#' (updates data in existing rows and columns), 'upsert' (updates existing data and inserts new rows), or 'replace'
#' (replaces the existing data with new data).
    update=function(update_policy="update") {
      # Only allow for update if cube has one table
      table_names <- names(private$multitable_definition())
      if (length(table_names) != 1) {
        stop(print("This method is only supported for single-table cubes. Please use the Dataset class to update multi-table cubes."))
      }
      ds <- Dataset$new(connection=self$connection, name=self$name,
                        dataset_id=self$cube_id, verbose=TRUE)
      ds$add_table(table_names, self$dataframe, update_policy=update_policy)
      ds$update()
    },

#' @description Creates a new single-table cube with the data frame stored in the Cube instance (cube$dataframe).
#' Before the update, make sure that the data exists.
#' @param name (character): Name of the dataset. Must be less than or equal to 250 characters.
#' @param description (character, optional): Description of the dataset. Must be less than or equal to 250 characters.
#' @param folder_id ID of the shared folder that the dataset should be created within. If `None`,
#' defaults to the user's My Reports folder.
#' @param table_name (character, optional) Name of the table. If NULL, the first table name of the original cube will
#' be used.
    save_as=function(name, description=NULL, folder_id=NULL, table_name=NULL) {
      # Only allow for update if cube has one table
      table_names <- names(private$multitable_definition())
      if (length(table_names) != 1) {
        stop(print("This method is only supported for single-table cubes. Please use the Dataset class to update multi-table cubes."))
      }
      if (is.null(table_name)) {
        table_name <- table_names
      }
      ds <- Dataset$new(connection=self$connection, name=name, description=description, verbose=TRUE)
      ds$add_table(name=table_name, data_frame=self$dataframe, update_policy="add")
      ds$create(folder_id=folder_id)
    }
  ),


  private=list(
    size_limit=10000000,
    initial_limit=1000,
    table_definition=NULL,
    cookies=NULL,
    filters=NULL,
    debug=FALSE,

    load_info=function() {
      # Get metadata for specific cubes. Implements GET /cubes to retrieve basic metadata.

      response <- cube_info(connection=self$connection, cube_id=self$cube_id, verbose=private$debug)

      private$cookies <- paste0("JSESSIONID=", response$cookies$value[[1]], "; iSession=", response$cookies$value[[2]])

      info <- content(response)$cubesInfos[[1]]

      self$name <- info$cubeName
      self$owner_id <- info$ownerId
      self$path <- info$path
      self$last_modified <- info$modificationTime
      self$size <- info$size
      self$status <- info$status
    },

    load_definition=function() {
      # Get available objects of a cube
      response <- cube(connection=self$connection, cube_id=self$cube_id, verbose=private$debug)
      objects <- content(response)$definition$availableObjects
      # TODO Check first whether there are this kind of ojects in the cube
      objects <- private$delete_row_counts(objects)

      self$attributes <- lapply(objects$attributes, function(attr) attr$id)
      self$metrics <- lapply(objects$metrics, function(metr) metr$id)
      names(self$attributes) <- lapply(objects$attributes, function(attr) attr$name)
      names(self$metrics) <- lapply(objects$metrics, function(metr) metr$name)
    },

    split_cube_into_tables=function() {
      # get the multitable_definition response
      table_definition <- private$multitable_definition()

      # split dataframe to dataframes matching tables in Cube
      dataframe_list <- list()

      table_names <- names(table_definition)
      for (name in table_names) {
        dataframe_list[[name]] <- subset(self$dataframe, select=unlist(table_definition[[name]]))
      }
      return(dataframe_list)
    },

    initialize_cube = function() {
      body <- private$filters$filter_body()
      if (length(body) == 0) { body <- c() }
      body <- toJSON(body, auto_unbox = TRUE)

      # Get first cube instance
      return(cube_instance(connection = self$connection,
                           cube_id = self$cube_id,
                           offset = 0,
                           limit = private$initial_limit,
                           body = body,
                           verbose = private$debug))
    },

    get_chunk = function(instance_id, offset, limit) {
      return(cube_instance_id(connection = self$connection,
                              cube_id = self$cube_id,
                              instance_id = instance_id,
                              offset = offset,
                              limit = limit,
                              verbose = private$debug))
    },

    fetch_chunk = function(p, instance_id, offset_, limit, pagination, callback) {
      #fetching chunk for sequential download
      response <- private$get_chunk(instance_id, offset_, limit)
      p$parse(response = content(response))
      callback(offset_, pagination$total)
    },

    fetch_future = function(offsets, limit, conn, cookies, cube_id, instanceId) {
      #fetching a set of http requests for async downloading
      future <- list()

      url <- paste0(conn$base_url, "/api/v2/cubes/", cube_id, "/instances/", instanceId)
      all_headers <- list("X-MSTR-AuthToken" = conn$auth_token,
                          "X-MSTR-ProjectID" = conn$project_id,
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

    retry_chunk = function(chunk, instance_id, offset, limit) {
      #retrying single chunk
      self$connection$renew()

      response <- cube_instance_id(connection = self$connection,
                                     cube_id = self$cube_id,
                                     instance_id = instance_id,
                                     offset = offset[[chunk]],
                                     limit = limit,
                                     verbose = private$debug)
      return(content(response))
    },

    shift=function(x) {
      # returns first element of vector and removes it from vector
      if (length(x) == 0) {
        return(NA)
      }
      shiftret <- x[1]
      assign(as.character(substitute(x)), x[2:(length(x))], parent.frame())
      return(shiftret)
    },

    parse_chunk=function(responses, parser, failed_chunks_idx, instance_id, limit) {
      index=0
      while (!is.na(responses[1])) {
        chunk <- private$shift(responses)
        index <- index + 1 #index of response currently processed
        if (index %in% failed_chunks_idx) {
          if (private$debug == TRUE) { print(paste0("Chunk ", index, " has failed to download. Retrying.")) }
          unpacked <- private$retry_chunk(index, instance_id, private$offsets, limit)
        }
        else {
          if (private$debug == TRUE) { print(paste0("Chunk ", index, " downloaded successfully.")) }
          unpacked <- fromJSON(chunk, simplifyVector=FALSE, simplifyDataFrame=FALSE, simplifyMatrix=FALSE)
        }
        parser$parse(unpacked)
      }
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
                                       limit = limit,
                                       verbose = private$debug)
          total <- as.numeric(response$headers$"x-mstr-total-count")
          response <- content(response)
        }
        else if (response[["status_code"]] != 200) {
          response <- cube_elements(connection = self$connection,
                                       cube_id = self$cube_id,
                                       attribute_id = attr_id,
                                       offset = 0,
                                       limit = limit,
                                       verbose = private$debug)
          total <- as.numeric(response$headers$"x-mstr-total-count")
          response <- content(response)
        } else if (response[["status_code"]] == 200){
          total <- as.numeric(response[["response_headers"]][["x-mstr-total-count"]])
          response <- response$parse(encoding="UTF-8")
          response <- fromJSON(response, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
       }

        elements <- lapply(response, function(elem) unlist(elem$id))
        names(elements) <- lapply(response, function(elem) {
          if (is_empty(unlist(elem$formValues))) unlist(elem$id)
          else unlist(elem$formValues)
        })

        # Fetch the rest of elements if their number exceeds the limit.
        if (total > limit) {
          offsets <- seq(from = limit, to = total, by = limit)

          for (offset_ in offsets) {
            response <- cube_elements(conn, cube_id, attr_id, offset_, limit, verbose = private$debug)

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

        all_headers <- list("X-MSTR-AuthToken" = conn$auth_token,
                            "X-MSTR-ProjectID" = conn$project_id,
                            "Cookie" = cookies)

        for (attr_id in attributes) {
          url <- paste0(conn$base_url, "/api/cubes/", cube_id, "/attributes/", attr_id, "/elements")
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
      if (self$parallel == TRUE) {
        future <- fetch_future_attr_elems(attributes = self$attributes, conn = self$connection,
                                          cookies = private$cookies, cube_id = self$cube_id)
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

    delete_row_counts = function(objects) {
      # Get table names and find row count columns
      table_names <- names(private$multitable_definition())
      row_count_col <- list()
      for (table_name in table_names) {
        row_count_col <- append(row_count_col, paste0("Row Count - ", table_name))
      }

      x <- function(x) {
        if (x$name %in% row_count_col) { FALSE }
        else { TRUE }
      }
      objects$metrics <- Filter(x, objects[["metrics"]])
      return(objects)
    },

    multitable_definition = function() {
      # Return all tables names and collumns as a list of list
      if (is.null(private$table_definition)) {
        response <- dataset_definition(connection = self$connection, dataset_id = self$cube_id, verbose = private$debug)
        response <- content(response, as = "parsed", type = "application/json")

        # Create the list of list from response
        for (table in response$result$definition$availableObjects$tables) {
          column_list <- list()
          for (column in response$result$definition$availableObjects$columns) {
            if (table$name == column$tableName) {
              column_list <- append(column_list, column$columnName)
            }
          }
          # Add another table name and column list to the table_definition list
          private$table_definition[[table$name]] <- column_list
        }
        return(private$table_definition)
      } else {
        return(private$table_definition)
      }
    }
  )
)
