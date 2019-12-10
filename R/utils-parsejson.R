# utils-parsejson.R
# Used for parsing the JSON response and converting it into a R data frame

parse_json <- function(response){

  # Detects existence of a child node
  has_child <- function(node){
    if("children" %in% names(node)){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  # Returns a named vector of attribute names from the response object
  get_attribute_names <- function(response){
    return(sapply(response$result$definition$attributes, function(x){
      x$name
    }))
  }

  # Returns named vector containing the name and the attribute element
  get_attribute <- function(node, attribute_names){
    attr_element <- node$element$name
    names(attr_element) <- attribute_names[node$element$attributeIndex + 1]
    return(attr_element)
  }

  # Returns a named vector of metric names from the response object
  get_metric_names <- function(response){
    return(sapply(response$result$definition$metrics, function(x){
      x$name
    }))
  }

  # Returns named vector containing metric name and metric value
  get_metrics <- function(node){
    return(sapply(node$metrics, function(c){
      c$rv
    }))
  }

  # simply extracts the attribute element and/or metrics and returns them as a vector
  extract_node <- function(node, attribute_names){
    if("element" %in% names(node)){
      ## There is an attribute element to extract, so extract it
      extract <- get_attribute(node, attribute_names = attribute_names)
    }
    if("metrics" %in% names(node)){
      ## There are metrics to extract, so extract them
      extract <- c(extract, get_metrics(node))
    }
    return(extract)
  }

  # Recursively extract attributes and metrics
  parse_node <- function(node, attribute_names, row_data=NULL){
    rows <- lapply(node$children, function(child){
      row_data <- c(row_data, extract_node(child, attribute_names=attribute_names))
      if(has_child(node=child)){
        parse_node(node=child, attribute_names=attribute_names, row_data=row_data)
      }
      else {
        return(row_data)
      }
    })
    return(rows)
  }

  if(is.null(response$result$data$root)){
    return(NULL)
  }

  attribute_names <- get_attribute_names(response)
  metric_names <- get_metric_names(response)
  parsed_rows <- parse_node(node=response$result$data$root, attribute_names = attribute_names)

  if(length(attribute_names)>0){
    df <- data.frame(matrix(unlist(parsed_rows), ncol=length(c(attribute_names, metric_names)), byrow=TRUE), stringsAsFactors=FALSE)
    names(df) <- c(attribute_names, metric_names)
  }else{
    metrics <- get_metrics(node=response$result$data$root)
    df <- data.frame(matrix(metrics, ncol=length(metrics), byrow=TRUE), stringsAsFactors=FALSE)
    names(df) <- metric_names
  }


  df
}
