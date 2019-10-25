# utils-composefilter.R
# Internal method for composing body of the POST request that creates a new cube/report instance.

full_body <- function(selected_attributes, selected_metrics, selected_attr_elements){

  preformat <- function(selected_objects){
    dicts = sapply(selected_objects, function(id) paste0('{"id": "', id, '"}'))
    dicts_list = paste0(dicts, sep =', ',  collapse='')
    paste0('[', substr(dicts_list, 1, nchar(dicts_list)-2), ']')
  }

  requestedObjects <- function(selected_attributes, selected_metrics){
    if(is.null(selected_attributes) && !is.null(selected_metrics)){
      # selection only on metrics
      metrs <- paste0('"metrics": ', preformat(selected_metrics))
      return(paste0('"requestedObjects": {', metrs, '}'))
    }
    if(!is.null(selected_attributes) && is.null(selected_metrics)){
      # selection only on attributes
      attrs = paste0('"attributes": ', preformat(selected_attributes))
      return(paste0('"requestedObjects": {', attrs, '}'))
    }
    if(!is.null(selected_attributes) && !is.null(selected_metrics)){
      # selection both on metrics and attributes
      attrs_metrs <- paste0('"attributes": ', preformat(selected_attributes), ', "metrics": ', preformat(selected_metrics))
      return(paste0('"requestedObjects": {', attrs_metrs, '}'))
    }
  }

  viewFilter <- function(selected_attr_elements){
    if(!is.null(selected_attr_elements)){
      return(paste0('"viewFilter": {', selected_attr_elements, '}'))
    }
  }

  composed_body <- function(selected_attributes, selected_metrics, selected_attr_elements){
    requestedObjects = requestedObjects(selected_attributes, selected_metrics)
    viewFilter = viewFilter(selected_attr_elements)

    if(!is.null(requestedObjects) && !is.null(viewFilter)){
      # selection on attributes or metrics, viewfilter on attribute elements
      return(paste0('{', requestedObjects, ', ', viewFilter, '}'))
    }
    if(!is.null(requestedObjects) && is.null(viewFilter)){
      # selection on attributes or metrics, no viewfilter on attribute elements
      return(paste0('{', requestedObjects, '}'))
    }
    if(is.null(requestedObjects) && !is.null(viewFilter)){
      # no selection on attributes or metrics, viewfilter on attribute elements
      return(paste0('{', viewFilter, '}'))
    }
    NULL
    }

  composed_body(selected_attributes, selected_metrics, selected_attr_elements)
}
