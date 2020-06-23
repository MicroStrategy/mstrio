# utils-filter.R
# Create filters that may be passed to MicroStrategy Report/Cube objects.

#' @title Create filters that may be passed to MicroStrategy Report/Cube objects.
#'
#' @description Pass ids of all objects and selected objects (attributes, metrics and elements)
#' and the generate the requested objects and view filter body for the pull Data API.
#' @field attributes List of ids of all attribute objects of given cube/report.
#' @field metrics List of ids of all metrics objects of given cube/report.
#' @field attr_elements (optional) List of ids of all attribute elements of given cube/report.
#' @field selected_attributes List of ids for selected attributes.
#' @field selected_metrics List of ids for selected metrics.
#' @field selected_attr_elements List of ids for selected attribute elements.
#' @field operator (character, optional): Supported view filter operators are either "In" or "NotIn". This defines
#' whether data will include ("In") or exclude ("NotIn") the supplied attr_elements values.
#' @docType class
#' @keywords internal
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#' @export
Filter <- R6Class("Filter",

  public = list(

    #instance variables
    attributes = NULL,
    metrics = NULL,
    attr_elements = NULL,
    selected_attributes = list(),
    selected_metrics = list(),
    selected_attr_elements = NULL,
    operator = "In",


#' @description Initialize a filter instance
#' @param attributes (list): List of all attributes IDs in the cube/report
#' @param metrics (list): List of all metrics IDs in the cube/report
#' @param attr_elements (list, optional): List of all attributes elements IDs in the cube/report. If not #' provided the attribute element object will not be validated on selection
    initialize = function(attributes, metrics, attr_elements=NULL) {
      # Initialize filter contructor.
      self$attributes = attributes
      self$metrics = metrics
      self$attr_elements = unlist(sapply(attr_elements, function(attr) { attr$elements }), use.names = TRUE)

      # Select all attributes/metrics by default when no filters are specified
      self$select(attributes, "automatic")
      self$select(metrics, "automatic")
    },

#' @description Select attribute/metric/attr_element objects to filter data. Selecting empty lists or vectors is
#' supported and means that all objects (attributes/metrics) are deselected. This method eliminates duplicates and
#' provides type validation for attributes/metrics and optionally attribute elements (if attr_elements are fetched
#' before)
#' @param object_id (list or character vector) Object IDs to be selected. Provide empty list if the objects of the
#' chosen type are to be deselected.
#' nshould be selected
#' @param object_type (character or NULL, optional) either 'attribute_elements', 'automatic'
    select=function(object_id, object_type="automatic") {
      valid_inputs <- (typeof(object_id) == "list" | typeof(object_id) == "character") &&
                      (object_type %in% c("attribute_elements", "automatic"))
      if (!valid_inputs) {
        stop(sprintf("Object_id must be of type list or character vector. Object type must be either 'attribute_elements' or 'automatic'"))

      } else if (object_type == "attribute_elements" && is.null(self$attr_elements)) {
        self$select_attr_elements(object_id)

      } else {
        for (id in object_id) {

          if (substr(id, 33, 33) == ';') {
            id <- strsplit(toString(id), ';')
          }
          type = private$type(id)

          if(type == "invalid"){
            private$error_handler(private$err_msg_invalid, id)
          } else {

            if(type=="attribute"){
              self$selected_attributes = unique(append(self$selected_attributes, object_id))
            }
            if(type=="metric"){
              self$selected_metrics = unique(append(self$selected_metrics, object_id))
            }
            if(type=="element"){
              self$selected_attr_elements = unique(append(self$selected_attr_elements, object_id))
            }
          }
        }
      }
    },

    select_attr_elements = function(attribute_elements) {

      valid_attribute_elements <- lapply(attribute_elements, function(id) {
        attr_id <- substr(id,1,32)
        if (attr_id %in% self$attributes) {
          id
        } else {
          private$error_handler(private$err_msg_invalid_attr_element, id)
          NULL
        }
      })
      valid_attribute_elements <- Filter(Negate(is.null), valid_attribute_elements)
      self$selected_attr_elements <- unique(append(self$selected_attr_elements, valid_attribute_elements))
    },

    clear = function(object = 'all') {
      if (object == 'metrics' | object == 'all') {
        self$selected_metrics = list()
      }
      if (object == 'attributes' | object == 'all') {
        self$selected_attributes = list()
      }
      if (object == 'attribute_elements' | object == 'all') {
        self$selected_attr_elements = NULL
      }
    },

    filter_body = function() {
      fb = list()
      if (!is.null(self$selected_attributes) || !is.null(self$selected_metrics)) {
        fb[["requestedObjects"]] <- private$requested_objects()
      }
      if(length(self$selected_attr_elements) > 0) {
        fb[["viewFilter"]] <- private$view_filter()
      }
      return(fb)
    }

  ),

  private = list(
    err_msg_invalid = "Invalid object ID: %s",
    err_msg_invalid_attr_element = "Invalid attribute element ID: %s",
    err_msg_duplicated="Duplicate object ID: %s",

    requested_objects = function() {
      ro = list()
      if (!is.null(self$selected_attributes)) {
        ro[["attributes"]] <- list()
        for (attr in self$selected_attributes) {
          if (length(attr) == 1) {
            ro[["attributes"]] <- append(ro[["attributes"]], list(list("id" = toString(attr))))
          } else {
            go <- list()
            go[["id"]] <- toString(attr[[1]])
            for (form in attr[2:length(attr)]) {
              go[["forms"]] <- append(go[["forms"]], (list(list("id" = toString(form)))))
            }
            ro[["attributes"]] <- append(ro[["attributes"]], list(go))
          }
        }
      }
      if (!is.null(self$selected_metrics)) {
        ro[["metrics"]] <- lapply(self$selected_metrics, function(x) list("id" = toString(x)))
      }

      return(ro)
    },

    view_filter = function(how = self$operator) {

      # Create a nested list to sort the chosen attribute elements per attribute
      lkp <- list()
      for (elem in self$selected_attr_elements) {
        attr_id <- substr(elem, 1, 32)
        if (attr_id %in% names(lkp)) {
          lkp[[attr_id]] <- append(lkp[[attr_id]], elem)
        } else {
          lkp[[attr_id]] <- list(elem)
        }
      }
      # Create the view filter body
      opers <- list()
      for (i in 1:length(lkp)) {
        att <- list("type" = "attribute", "id" = names(lkp[i]))
        elem <- list("type" = "elements",
                      "elements" = lapply(lkp[[i]], function(x) list("id" = toString(x))))
        opers <- append(opers, list(list("operator" = how,
                                  "operands" = list(att, elem))))
      }
      if (length(opers) == 1) {
        vf = opers[[1]]
      } else {
        vf = list("operator" = "And", "operands" = opers)
      }
      return(vf)
    },

    type = function(object_id){
      # Look up and return object type from available objects.
      if(toString(object_id[[1]][1]) %in% self$attributes){
        "attribute"
      }
      else if(object_id %in% self$metrics){
        "metric"
      }
      else if(object_id %in% self$attr_elements){
        "element"
      }
      else{
        "invalid"
      }

    },

    duplicate = function(object_id){
      # Check if requested object_id is already selected.
      if(object_id %in% self$selected_attributes){
        TRUE
      }
      else if(object_id %in% self$selected_metrics){
        TRUE
      }
      else if(object_id %in% self$selected_attr_elements){
        TRUE
      }
      else{
        FALSE
      }

    },

    error_handler = function(msg, object_id) {
      # Generic error message handler.
      if (length(object_id) > 1) {
        object_id <- object_id[[1]][1]
      }
      stop(sprintf(msg, object_id, call.=FALSE))
    },

    warning_handler = function(msg, object_id) {
      # Generic warning message handler.
      warning(sprintf(msg, object_id), immediate. = TRUE)
    }
  )
)
