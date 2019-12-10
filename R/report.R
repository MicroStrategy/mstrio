# report.R
# Create and interact with MicroStrategy reports


#' @title Extract a MicroStrategy report into a R Data.Frame
#'
#' @description Access, filter, publish, and extract data from in-memory reports.
#' Create a Report object to load basic information on a report dataset. Specify subset of report
#' to be fetched through Report.apply_filters() and Report.clear_filters() . Fetch dataset through
#' Report.to_dataframe() method.
#' @field connection MicroStrategy connection object
#' @field report_id Identifier of a report.
#' @examples
#' \donttest{
#' # Create a connection object.
#' connection = connect_mstr(base_url, username, password, project_name)
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
#' @export
Report <- R6Class("Report",

  public = list(

    #instance variables
    connection = NULL,
    report_id = NULL,
    offset = 0,
    name = NULL,
    attributes = NULL,
    metrics = NULL,
    attr_elements = NULL,
    selected_attributes = NULL,
    selected_metrics = NULL,
    selected_attr_elements = NULL,
    dataframe = NULL,
    filters = NULL,


    initialize = function(connection, report_id) {
      # Initialize report contructor.

      self$connection <- connection
      self$report_id <- report_id

      private$load_definition()
      self$filters <- Filter$new(attributes=self$attributes,
                                          metrics=self$metrics,
                                          attr_elements=NULL)
    },
    
    to_dataframe = function(limit = 25000, callback = function(x,y) {}) {
      # Extract contents of a report into a R Data Frame. Previously `get_report()`.

      # Get first report instance
      response <- report_instance(connection=self$connection,
                                  report_id=self$report_id,
                                  offset=self$offset,
                                  limit=limit,
                                  body=self$filters$filter_body()
                                  )

      # if(response$status_code == 204) {
      #   stop(print("Data Frame looks empty."))
      # }
      if(http_error(response)) {  # http != 200
            private$response_handler(response, private$err_msg_instance)
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
          response <- report_instance_id(connection=self$connection,
                                         report_id=self$report_id,
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
      if(length(self$dataframe) == 0) {
        stop(print("Data Frame looks empty."))
      } else {
      return(self$dataframe)
    }
    },

    apply_filters = function(attributes=NULL, metrics=NULL, attr_elements=NULL) {
      # Instantiate the filter object and download all attr_elements of the report.
      # Apply filters on the report data so only the chosen attributes, metrics,
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
      # Clear previously set filters, allowing all attributes, metrics,
      # and attribute elements to be retrieved from the Intelligence Server.
      self$filters$clear()

      self$selected_attributes <- self$filters$attr_selected
      self$selected_metrics <- self$filters$metr_selected
      self$selected_attr_elements <- self$filters$attr_elem_selected
    },

    get_attr_elements = function(verbose = TRUE){
      # Load elements of all attributes to be accesibile by Report$attr_elements.

      if(is.null(self$attr_elements)){
        private$load_attr_elements()
      }
      if(verbose) self$attr_elements
    }
  ),


  private = list(

    err_msg_definition = "Error getting report definition. Check report ID.",
    err_msg_instance = "Error getting report contents.",
    err_msg_elements = "Error loading attribute elements.",

    load_definition = function() {
      # Get the definition of a report, including attributes and metrics.
      response <- report_instance(connection=self$connection,
                                  report_id=self$report_id,
                                  offset=0,
                                  limit=0
                                  )

      if(http_error(response)) {  # http != 200
        private$response_handler(response, private$err_msg_definition)
      }

      self$name <- content(response)$name
      objects <- content(response)$result$definition

      self$attributes <- lapply(objects$attributes, function(attr) attr$id)
      self$metrics <- lapply(objects$metrics, function(metr) metr$id)
      names(self$attributes) <- lapply(objects$attributes, function(attr) attr$name)
      names(self$metrics) <- lapply(objects$metrics, function(metr) metr$name)

    },

    load_attr_elements = function() {
      # Get the elements of report attributes.
      is_empty <- function(x){
      # Helper function to handle empty 'formValues'.
        if(is.na(x) || is.null(x) || nchar(x)==0) return(TRUE)
        else return(FALSE)
      }

      get_single_attr_elements = function(conn, report_id, attr_id, offset=self$offset, limit=160000) {
        # Helper function extracting attr elements from HTTP response.

        # Fetch first chunk to determine what is the total number of elements.
        response <- report_elements(connection=conn, report_id, attr_id, offset, limit)

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
            response <- report_elements(conn, report_id, attr_id, offset_, limit)

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
             "elements" = get_single_attr_elements(self$connection, self$report_id, attr_id))
      })

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
