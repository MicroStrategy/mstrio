# report.R
# Create and interact with MicroStrategy reports


#' @title Extract a MicroStrategy report into a R Data.Frame
#'
#' @description Access, filter, publish, and extract data from in-memory reports.
#' Create a Report object to load basic information on a report dataset. Specify subset of report
#' to be fetched through Report$apply_filters() and Report$clear_filters() . Fetch dataset through
#' Report$to_dataframe() method.
#' @field connection MicroStrategy connection object
#' @field report_id Identifier of a report.
#' @field parallel If TRUE, downloads report data asynchronously. FALSE by default.
#' @field name Report name.
#' @field attributes Report attributes.
#' @field metrics Report metrics
#' @field attr_elements Report attribute elements.
#' @field selected_attributes Attributes selected for filtering.
#' @field selected_metrics Metrics selected for filtering.
#' @field selected_attr_elements Attribute elements selected for filtering.
#' @field dataframe Dataframe containing data fetched from the Report.
#' @field cross_tab boolean for filtering cross tabbed reports logic
#' @field cross_tab_filters view filters for cross tab reports
#' @field instance_id Identifier of an instance if report instance has been already initialized.
#' @examples
#' \donttest{
#' # Create a connection object.
#' connection = Connection$new(base_url, username, password, project_name)
#'
#' # Create a report object.
#' my_report <- Report$new(connection, report_id)
#'
#' # See attributes and metrics in the report.
#' my_report$attributes
#' my_report$metrics
#' my_report$attr_elements
#'
#' # Specify attributes and metrics (columns) to be fetched.
#' my_report$apply_filters(attributes = my_report$attributes[1:2],
#'                            metrics = my_report$metrics[1:2])
#'
#' # See the selection of attributes, metrics and attribute elements.
#' my_report$selected_attributes
#' my_report$selected_metrics
#' my_report$selected_attr_elements
#'
#' # Clear filtering to load a full dataset.
#' my_report$clear_filters()
#'
#' # Fetch data from the Intelligence Server.
#' my_report$to_dataframe()
#'
#' # See the dataframe.
#' my_report$dataframe
#' }
#' @docType class
#' @importFrom R6 R6Class
#' @rawNamespace import(crul, except = handle)
#' @export
Report <- R6Class("Report",

  public = list(

#instance variables
    connection = NULL,
    report_id = NULL,
    parallel = FALSE,
    name = NULL,
    attributes = NULL,
    metrics = NULL,
    attr_elements = NULL,
    selected_attributes = NULL,
    selected_metrics = NULL,
    selected_attr_elements = NULL,
    cross_tab=FALSE,
    dataframe = NULL,
    cross_tab_filters = NULL,
    instance_id = NULL,

#' @description Initialize an instance of a report.
#' @param connection MicroStrategy connection object. See Connection class.
#' @param report_id Identifier of a pre-existing report containing the required data.
#' @param instance_id Identifier of an instance if report instance has been already initialized, NULL by default.
#' @param parallel (bool, optional):  If True, utilize optimal number of threads to increase the download
#' speed. If False (default), this feature will be disabled.
    initialize = function(connection, report_id, instance_id=NULL, parallel=FALSE) {
      # Initialize report contructor.
      self$connection <- connection
      self$report_id <- report_id
      self$instance_id <- instance_id
      self$parallel <- parallel
      private$load_definition()

      private$filters <- Filter$new(attributes = self$attributes,
                                    metrics = self$metrics,
                                    attr_elements=NULL)
      self$selected_attributes <- private$filters$selected_attributes
      self$selected_metrics <- private$filters$selected_metrics
    },

#' @description Extract contents of a Report into a R Data Frame.
#' @param limit (int, optional): Used to control data extraction behaviour on report with a large number of rows. By
#' default the limit is calculated automatically. If TRUE, overrides automatic limit.
#' @param callback used by the GUI to extract the progress information
#' @return Dataframe with data fetched from the given Report.
    to_dataframe = function(limit = NULL, callback = function(x, y) { }) {
      # Extract contents of a report into a R Data Frame.

      #checking if given limit is valid
      auto <- TRUE
      if (is.null(limit)) {
      } else if (limit < 1 & limit != -1) {
        warning("Limit has to be larger than 0, new limit will be set automatically", immediate. = TRUE)
      } else {
        auto <- FALSE
        private$initial_limit <- limit
      }

      if (is.null(self$instance_id)) {
        response <- private$initialize_report()
      } else {
        response <- tryCatch({
          private$get_chunk(instance_id=self$instance_id, offset=0, limit=private$initial_limit)
        },
        error = function(e) {
          private$initialize_report()
        })
      }
      #getting size of the first response in bytes, to use in auto chunk sizing
      size_bytes <- as.integer(object.size(response))

      # Gets the pagination totals from the response object
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
          limit <- max(1000, round(((private$initial_limit * private$size_limit) / size_bytes), digits = 0))
          message(sprintf("Chunk limit set automatically to %s", limit))
        }
        # Create vector of offset parameters to iterate over
        offsets <- pagination$current
        while (tail(offsets, 1) + limit < pagination$total) {
          offsets <- append(offsets, tail(offsets, 1) + limit)
        }

        #asynchronous download of chunks and dataframe creation
        if (isTRUE(self$parallel)) {
          future <- private$fetch_future(offsets, limit = limit, conn = self$connection,
                                         cookies = private$cookies,
                                         report_id = self$report_id, instanceId = instance_id)
          future_responses <- AsyncVaried$new(.list = future)
          future_responses$request()
          failed_chunks_idx <- which("200" != future_responses$status_code())
          future_responses <- future_responses$parse()
          future_responses <- lapply(future_responses, fromJSON, simplifyVector = FALSE,
                                     simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
          if (length(failed_chunks_idx) != 0) {
            future_responses <- private$retry_chunks(failed_chunks_idx = failed_chunks_idx,
                                                     future_responses, instance_id, offsets, limit)
          }
          sapply(future_responses, p$parse)
          callback(pagination$total, pagination$total)

        } else {
          #sequential download of chunks and dataframe creation
          sapply(c(offsets), private$fetch_chunk, p = p, instance_id = instance_id, limit = limit,
                                                    pagination = pagination, callback = callback)
          callback(pagination$total, pagination$total)
        }
      }
      self$dataframe <- p$to_dataframe()

      if (length(self$cross_tab_filters) != 0) {
        filtered <- c()
        for (elem in c(unlist(self$cross_tab_filters$attributes), unlist(self$cross_tab_filters$metrics))) {
          filtered <- append(filtered, (names(self$attributes[grep(elem, self$attributes)])))
          filtered <- append(filtered, (names(self$metrics[grep(elem, self$metrics)])))
        }

        if (!is.null(self$cross_tab_filters$attr_elements)) {
          filter_string <- "self$dataframe <- subset(self$dataframe, "
          for (elem in self$cross_tab_filters$attr_elements) {
            elem <- (strsplit(elem, ":"))[[1]]
            elem[[1]] <- (names(self$attributes[grep(elem[[1]], self$attributes)]))
            filter_string <- paste(filter_string, paste0(elem[[1]], " %in% c(", elem[[2]], ") | "))
          }
          filter_string <- paste(filter_string, "FALSE , select = c(filtered))")
          eval(parse(text=filter_string))
        } else {
          self$dataframe <- subset(self$dataframe, select = c(filtered))
        }

      }
      return(self$dataframe)
    },

#' @description Apply filters on the report data so only the chosen attributes, metrics, and attribute elements are
#' retrieved from the Intelligence Server.
#' @param attributes (list or None, optional): ID numbers of attributes to be included in the filter. If list is
#' empty, no attributes will be selected and metric data will be aggregated.
#' @param metrics (list or None, optional): ID numbers of metrics to be included in the filter. If list is empty,
#' no metrics will be selected.
#' @param attr_elements (list or None, optional): Attributes' elements to be included in the filter.
#' @param operator (character, optional): Supported view filter operators are either "In" or "NotIn". This defines
#' whether data will include ("In") or exclude ("NotIn") the supplied attr_elements values.
    apply_filters = function(attributes = NULL, metrics = NULL, attr_elements = NULL, operator = "In") {
      private$filters$operator <- operator

      if (self$cross_tab == TRUE) {
        if (is.null(attributes) & is.null(metrics) & is.null(attr_elements)) {
        } else {
          self$cross_tab_filters <- list(attributes = attributes, metrics = metrics, attr_elements = attr_elements)
        }
      } else {
        # Clear already filtered objects from the filter object if a new filter is applied
        if (!is.null(attributes)) { private$filters$clear("attributes") }
        if (!is.null(metrics)) { private$filters$clear("metrics") }
        if (!is.null(attr_elements)) { private$filters$clear("attribute_elements") }

        # Select the objects
        if (!is.null(attributes)) { private$filters$select(attributes, "automatic") }
        if (!is.null(metrics)) { private$filters$select(metrics, "automatic") }
        if (length(attr_elements) > 0) { private$filters$select(attr_elements, "attribute_elements") }

        # Assign filter values in the report instance
        self$selected_attributes <- private$filters$selected_attributes
        self$selected_metrics <- private$filters$selected_metrics
        self$selected_attr_elements <- private$filters$selected_attr_elements
      }
    },

#' @description Clear previously set filters, allowing all attributes, metrics, and attribute elements to be retrieved.
    clear_filters = function() {

      private$filters$clear()
      # Select all attributes/metrics by default when no filters are specified
      private$filters$select(self$attributes, "automatic")
      private$filters$select(self$metrics, "automatic")

      self$selected_attributes <- private$filters$selected_attributes
      self$selected_metrics <- private$filters$selected_metrics
      self$selected_attr_elements <- private$filters$selected_attr_elements
    },

#' @description Load all attribute elements of the Report. Accessible via Report$attr_elements.
#' Fetching attriubte elements will also allow for validating attriute elements by the filter object.
#' @param verbose If TRUE, displays list of attribute elements.
    get_attr_elements = function(verbose = TRUE) {
      if (is.null(self$attr_elements)) {
        private$load_attr_elements()

        # add downloaded attribute elements to the filter object
        private$filters$attr_elements = unlist(sapply(self$attr_elements, function(attr) { attr$elements }), use.names = TRUE)
      }
      if (verbose) self$attr_elements
    }
  ),


  private = list(

    size_limit = 10000000,
    initial_limit=1000,
    cookies = NULL,
    filters = NULL,
    debug = FALSE,

    load_definition = function() {
      # Get the definition of a report, including attributes and metrics.
      response <- report(connection = self$connection, report_id = self$report_id, verbose=private$debug)
      private$cookies <- paste0("JSESSIONID=", response$cookies$value[[1]], "; iSession=", response$cookies$value[[2]])

      response <- content(response)
      grid <- response$definition$grid
      available_objects <- response$definition$availableObjects
      self$name <- response$name
      self$cross_tab <- grid$crossTab

      # Check if report have custom groups or consolidations
      if (length(available_objects$customGroups) > 0) {
        stop(sprintf("Reports with custom groups are not supported.", call. = FALSE))
      }
      if (length(available_objects$consolidations) > 0){
        stop(sprintf("Reports with consolidations are not supported.", call. = FALSE))
      }

      full_attributes <- list()
      for (row in grid$rows) {
        if (row$type == 'attribute') { full_attributes <- append(full_attributes, list(row)) }
      }
      for (column in grid$columns) {
        if (column$type == 'attribute') {full_attributes <- append(full_attributes, list(column))}
      }
      self$attributes <- lapply(full_attributes, function(attr) attr$id)
      names(self$attributes) <- lapply(full_attributes, function(attr) attr$name)

      # Retrieve metrics from the report grid (metrics selected only in the report)
      if (!is.null(metrics_position <- grid$metricsPosition)) {
        full_metrics <- grid[[metrics_position$axis]][[metrics_position$index + 1]][["elements"]]
        self$metrics <- lapply(full_metrics, function(metr) metr$id)
        names(self$metrics) <- lapply(full_metrics, function(metr) metr$name)
      } else { self$metrics <- list() }
    },

    initialize_report = function() {
      body <- private$filters$filter_body()
      if (compareVersion(self$connection$iserver_version, "11.2.0100") %in% c(0, 1)){
        body[["subtotals"]][["visible"]] <- "false"
      }
      if (length(body) == 0) { body <- c() }
      body <- toJSON(body, auto_unbox = TRUE)

      # Get first report instance
      return(report_instance(connection = self$connection,
                             report_id = self$report_id,
                             body = body,
                             offset = 0,
                             limit = private$initial_limit,
                             verbose = private$debug))
    },

    get_chunk = function(instance_id, offset, limit) {
      return(report_instance_id(connection = self$connection,
                                report_id = self$report_id,
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

    fetch_future = function(offsets, limit, conn, cookies, report_id, instanceId) {
      #fetching a set of http requests for async downloading
      future <- list()

      url <- paste0(conn$base_url, "/api/v2/reports/", report_id, "/instances/", instanceId)
      all_headers <- list("X-MSTR-AuthToken" = conn$auth_token,
                   "X-MSTR-ProjectID" = conn$project_id,
                   "Cookie" = cookies)

      for (offset_ in offsets) {
        respons <- HttpRequest$new(
          url = url,
          headers = all_headers
        )
        response <- respons$get(query = list(offset = format(offset_, scientific=FALSE, trim=TRUE),
                          limit = format(limit, scientific=FALSE, trim=TRUE)))
        future <- append(future, response)
      }
      return(future)
    },

    retry_chunks = function(failed_chunks_idx, parsed_future_responses, instance_id, offsets, limit) {
      #retrying failed chunks
      for (chunk in failed_chunks_idx) {
        response <- report_instance_id(connection = self$connection,
                                       report_id = self$report_id,
                                       instance_id = instance_id,
                                       offset = offsets[[chunk]],
                                       limit = limit,
                                       verbose = private$debug)
        parsed_future_responses[[chunk]] <- content(response)
      }
      return(parsed_future_responses)
    },

    load_attr_elements = function() {

      # Get the elements of report attributes.
      is_empty <- function(x) {
        # Helper function to handle empty 'formValues'.
        if (is.na(x) || is.null(x) || nchar(x) == 0) return(TRUE)
        else return(FALSE)
      }

      get_single_attr_elements = function(response = NULL, conn, report_id, limit, attr_id) {

        if (is.null(response)) {
          response <- report_elements(connection = self$connection,
                                       report_id = self$report_id,
                                       attribute_id = attr_id,
                                       offset = 0,
                                       limit = limit,
                                       verbose = private$debug)
          total <- as.numeric(response$headers$"x-mstr-total-count")
          response <- content(response)
        }
        else if (response[["status_code"]] != 200) {
          response <- report_elements(connection = self$connection,
                                       report_id = self$report_id,
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
            response <- report_elements(conn, report_id, attr_id, offset_, limit, verbose = private$debug)

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
      fetch_future_attr_elems = function(attributes, limit = 160000, conn, cookies, report_id, offset = 0) {
        future <- list()

        all_headers <- list("X-MSTR-AuthToken" = conn$auth_token,
                    "X-MSTR-ProjectID" = conn$project_id,
                    "Cookie" = cookies)

        for (attr_id in attributes) {
          url <- paste0(conn$base_url, "/api/reports/", report_id, "/attributes/", attr_id, "/elements")
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
                                          cookies = private$cookies, report_id = self$report_id)
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
          list("id" = attr_id,
              "elements" = get_single_attr_elements(response = response, conn = self$connection,
                           report_id = self$report_id, limit = 160000, attr_id = attr_id))
        })
        names(self$attr_elements) <- names(self$attributes)
      }
      else {
        self$attr_elements <- lapply(self$attributes, function(attr_id) {
        list("id" = attr_id,
             "elements" = get_single_attr_elements(conn = self$connection, report_id = self$report_id,
                          limit = 160000, attr_id = attr_id))
        })
      }
    }
  )
)
