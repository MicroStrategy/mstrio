# parsejson.R
# Used for parsing the JSON response and converting it into a R data frame

parse_json <- function(response){

  # Detects existence of a child node
  child_exists <- function(node){
    if("children" %in% names(node)){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  # simply extracts the attribute element and/or metrics and returns them as a vector
  extract_node <- function(node, attribute_names){
    if("element" %in% names(node)){
      ## There is an attribute element to extract, so extract it
      extract <- extract_attribute(node, attribute_names = attribute_names)
    }
    if("metrics" %in% names(node)){
      ## There are metrics to extract, so extract them
      extract <- c(extract, extract_metrics(node))
    }
    return(extract)
  }

  # Returns named vector containing metric name and metric value
  extract_metrics <- function(node){
    return(sapply(node$metrics, function(c){
      c$rv
    }))
  }

  # Returns named vector containing the name and the attribute element
  extract_attribute <- function(node, attribute_names){
    attr_element <- node$element$name
    names(attr_element) <- attribute_names[node$element$attributeIndex + 1]
    return(attr_element)
  }

  # Returns a named vector of attribute names from the response object
  extract_attribute_names <- function(response){
    return(sapply(response$result$definition$attributes, function(x){
      x$name
    }))
  }

  # Returns a named vector of metric names from the response object
  extract_metric_names <- function(response){
    return(sapply(response$result$definition$metrics, function(x){
      x$name
    }))
  }

  # Recursively extract attributes and metrics
  drill <- function(node, attribute_names, row_data=NULL){
    rows <- lapply(node$children, function(child){
      row_data <- c(row_data, extract_node(child, attribute_names = attribute_names))
      if(child_exists(child)){
        drill(node=child, attribute_names = attribute_names, row_data=row_data)
      }
      else {
        return(row_data)
      }
    })
    return(rows)
  }

  attribute_names <- extract_attribute_names(response)
  metric_names <- extract_metric_names(response)
  parsed_rows <- drill(node=response$result$data$root, attribute_names = attribute_names)

  df <- data.frame(matrix(unlist(parsed_rows), ncol=length(c(attribute_names, metric_names)), byrow=TRUE), stringsAsFactors=FALSE)
  names(df) <- c(attribute_names, metric_names)

  df
}
