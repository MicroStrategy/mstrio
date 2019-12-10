# utils-filter.R
# Create filters that may be passed to MicroStrategy Report/Cube objects.

#' @title Pull MicroStrategy cubes (full or filtered)
#'
#' @description Pass ids of selected objects (attributes, metrics and elements).
#' @field attributes List of ids for selected attributes.
#' @field metrics List of ids for selected metrics.
#' @field attr_elements List of ids for selected attribute elements.
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#' @export
Filter <- R6Class("Filter",

  public = list(
    
    #instance variables
    attributes = NULL,
    metrics = NULL,
    attr_elems = NULL,
    attr_selected = c(),
    metr_selected = c(),
    attr_elem_selected = c(),

    initialize = function(attributes=NULL, metrics=NULL, attr_elements=NULL) {
      # Initialize filter contructor.

      self$attributes = attributes
      self$metrics = metrics

      elements = lapply(attr_elements, function(attr) {
          temp = lapply(as.list(names(attr$elements)), function(x) list("name" = x, "attr_id" = attr$id))
          names(temp) = attr$elements
          temp
          })
      self$attr_elems = unlist(unname(elements), recursive=FALSE)
    },

    select = function(object_id){
      if(length(object_id) > 1){
        for(i in object_id){
          self$select(i)
        }
      } else {

        typ = private$type(object_id)
        if(typ == "invalid"){
          private$error_handler(private$err_msg_invalid, object_id)
        }

        if(private$duplicate(object_id)){
          private$warning_handler(private$err_msg_duplicated, object_id)
        } else {
          
          if(typ=="attribute"){
            self$attr_selected = append(self$attr_selected, object_id)
          }
          if(typ=="metric"){
            self$metr_selected = append(self$metr_selected, object_id)
          }
          if(typ=="element"){
            self$attr_elem_selected = append(self$attr_elem_selected, object_id)
          }
        }
      }
    },

    clear = function(){
      self$attr_selected = c()
      self$metr_selected = c()
      self$attr_elem_selected = c()
    },

    requested_objects = function(){
      ro = list()
      if (!is.null(self$attr_selected)) {
        ro[["attributes"]] <- lapply(self$attr_selected, function(x) list("id" = x))
      }
      if (!is.null(self$metr_selected)) {
        ro[["metrics"]] <- lapply(self$metr_selected, function(x) list("id" = x))
      }
      
      return(ro)
    },

    view_filter = function(){

      lkp = list()
      if(!is.null(self$attr_elem_selected)){
        for (s in self$attr_elem_selected) {
                if (self$attr_elems[[s]][["attr_id"]] %in% names(lkp)) {
                  lkp[[self$attr_elems[[s]][["attr_id"]]]] <- append(lkp[[self$attr_elems[[s]][["attr_id"]]]], s)
                } else {
                  lkp[[self$attr_elems[[s]][["attr_id"]]]] <- list(s)
                }
            }

        opers <- c()
        for (i in 1:length(lkp)) {
            att <- list("type" = "attribute",  "id" = names(lkp[i]))
            elem <- list("type" = "elements",
                        "elements" = lapply(lkp[[i]], function(x) list("id"=x)))
            opers <- append(opers, list(list("operator" = "In",
                                      "operands" = list(att, elem))))
        }
                                            
            if (length(opers) == 1) {
                vf = opers[[1]]
            } else {
                vf = list("operator" = "And", "operands" = opers)
            }
            
        return(vf)
      }

    },

    filter_body = function() {
      fb = c()
      if(!is.null(self$attr_selected) || !is.null(self$metr_selected)) {
        fb[["requestedObjects"]] <- self$requested_objects()
      }
      if(!is.null(self$attr_elem_selected)) {
        fb[["viewFilter"]] <- self$view_filter()
      }

      return(toJSON(fb, auto_unbox = TRUE))
    }

  ),

  private = list(
    err_msg_invalid = "Invalid object ID: %s",
    err_msg_duplicated = "Duplicate object ID: %s",

    type = function(object_id){
      # Look up and return object type from available objects.

      # if(object_id %in% names(self$attributes)){
      if(object_id %in% self$attributes){
        "attribute"
      }
      # else if(object_id %in% names(self$metrics)){
      else if(object_id %in% self$metrics){
        "metric"
      }
      # else if(object_id %in% names(self$attr_elems)){
      else if(object_id %in% names(self$attr_elems)){
        "element"
      }
      else{
        "invalid"
      }

    },

    duplicate = function(object_id){
      # Check if requested object_id is already selected.
      if(object_id %in% self$attr_selected){
        TRUE
      }
      else if(object_id %in% self$metr_selected){
        TRUE
      }
      else if(object_id %in% self$attr_elem_selected){
        TRUE
      }
      else{
        FALSE
      }

    },

    error_handler = function(msg, object_id) {
      # Generic error message handler.
      stop(sprintf(msg, object_id, call.=FALSE))
    },

    warning_handler = function(msg, object_id) {
      # Generic warning message handler.
      sprintf(msg, object_id, call.=FALSE)
    }
  )
)
