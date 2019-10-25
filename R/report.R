# report.R
# Create and interact with MicroStrategy reports

#' @title Extract a MicroStrategy report into a R Data.Frame
#'
#' @description Access, filter, publish, and extract data from in-memory reports.
#' Create a Report object to load basic information on a report dataset. Specify subset of report
#' to be fetched through Report.apply_filters() and Report.clear_filters() . Fetch dataset through
#' Report.to_dataframe() method.
#' @field connection MicroStrategy connection object
#' @field report_id Identifier of a pre-existing report.
#' @examples
#' \donttest{
#' my_report <- Report$new(connection=conn, report_id="...")
#' df <- my_report$to_dataframe()
#' # Use object IDs for metrics, attributes, and attribute elements to filter contents of a report.
#' my_report$metrics
#' my_report$attributes
#' my_report$attr_elements
#' 
#' # Then, choose those elements by passing their IDs to the Report.apply_filters() method. 
#' # To see the chosen elements, call my_report.filters and to clear any active filters, 
#' # call my_report.clear_filters().
#' my_report$apply_filters(
#'          attributes=list("A598372E11E9910D1CBF0080EFD54D63", "A59855D811E9910D1CC50080EFD54D63"),
#'          metrics = list("B4054F5411E9910D672E0080EFC5AE5B"),
#'          attr_elements = list("A598372E11E9910D1CBF0080EFD54D63:Los Angeles", 
#'                               "A598372E11E9910D1CBF0080EFD54D63:Seattle"))
#' df <- my_report$to_dataframe()
#' }
#' @docType class
#' @importFrom R6 R6Class
#' @export
Report <- R6Class("Report",

  public = list(

    #instance variables
    connection = NULL,
    report_id = NULL,
    name = NULL,
    attributes = NULL,
    metrics = NULL,
    attr_elements = NULL,
    selected_attributes = NULL,
    selected_metrics = NULL,
    selected_attr_elements = NULL,

    initialize = function(connection, report_id) {
      # Initialize report contructor.

      self$connection <- connection
      self$report_id <- report_id

      private$load_definition()
    },
    
    get_attr_elements = function(verbose = TRUE){
      # Load elements of all attributes to be accesibile by Report$attr_elements.

      if(is.null(self$attr_elements)){
        private$load_attr_elements()
      }

      if(verbose) self$attr_elements
    },

    apply_filters = function(attributes = NULL, metrics = NULL, attr_elements = NULL, body = NULL) {
      # Apply filters on the report data so only the chosen attributes, metrics, and attribute elements are retrieved from the Intelligence Server.
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

    to_dataframe = function(offset = 0, limit = 25000, callback = function(x,y) {}) {
      # Extract contents of a report into a Pandas Data Frame. Previously `get_report()`.

      # Get first report instance
      response <- report_instance(connection=self$connection, report_id=self$report_id, offset=offset, limit=limit, body=private$filters)

      if(http_error(response)) {  # http != 200
            private$response_handler(response, "Error getting report contents.")
      }

      # Gets the pagination totals from the response object
      response <- content(response)
      pagination <- response$result$data$paging

      # report instance id, used for fetching add'l rows
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
          # Fetch add'l rows from the report
          response <- report_instance_id(connection=self$connection, report_id=self$report_id, instance_id=instance_id, offset=offset_, limit=limit)
          response <- content(response)
          response_list[[length(response_list)+1]] <- parse_json(response = response)
          callback(offset_, pagination$total)
        }
        callback(pagination$total, pagination$total)
        return(do.call(rbind.data.frame, response_list))
      } else {
        callback(pagination$total, pagination$total)
        return(parse_json(response=response))
      }

    }

  ),

  private = list(

    filters = NULL,

    load_definition = function() {
      # Get the definition of a report, including attributes and metrics. Implements GET /reports/<report_id>.

      response <- report(connection = self$connection, report_id = self$report_id)

      if(http_error(response)) {  # http != 200
        private$response_handler(response, "Error loading Report$attributes and Report$metrics.")
      }

      self$name <- content(response)$name

      objects <- content(response)$result$definition$availableObjects

      self$attributes <- lapply(objects$attributes, function(attr) attr$id)
      self$metrics <- lapply(objects$metrics, function(metr) metr$id)
      names(self$attributes) <- lapply(objects$attributes, function(attr) attr$name)
      names(self$metrics) <- lapply(objects$metrics, function(metr) metr$name)

    },

    load_attr_elements = function() {
      # Get the elements of report attributes. Implements GET /reports/<report_id>/attributes/<attribute_id>/elements

      get_single_attr_elements = function(conn, report_id, attr_id) {
        #helper function extracting attr elements from HTTP response.
        response <- report_single_attr_elements(conn, report_id, attr_id)

        if(http_error(response)) {  # http != 200
          private$response_handler(response, "Error loading Report$attr_elements.")
        }

        elements <- lapply(content(response), function(elem) unlist(elem$id))
        names(elements) <- lapply(content(response), function(elem) unlist(elem$formValues))

        elements
      }

      self$attr_elements <- lapply(self$attributes, function(attr_id) get_single_attr_elements(self$connection, self$report_id, attr_id))

    },

    response_handler = function(response, msg) {
      # Generic error message handler for transactions against reports.

      status <- http_status(response)
      errors <- content(response)
      stop(sprintf("%s\n HTTP Error: %s %s %s\n I-Server Error: %s %s",
                   msg, response$status_code, status$reason, status$message, errors$code, errors$message),
           call.=FALSE)

    }
  )
)
