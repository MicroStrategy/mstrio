# formjson.R
# Utility for forming the JSON objects that are required to create and edit MicroStrategy datasets

form_json <- function(df, table_name, to_metric=NULL, to_attribute=NULL){

  # Internal functions here...
  convert_datatype <- function(datatype){

    ## treated as metrics
    if(class(datatype) %in% c('double', 'numeric'))              return('DOUBLE')
    if(class(datatype) %in% c('integer'))                        return('INTEGER')

    ## treated as attributes
    if(class(datatype) %in% c('string', 'factor', 'character'))  return('STRING')
    if(class(datatype) %in% c('logical'))                        return("BOOL")
    if(class(datatype) %in% c('Date'))                           return("DATE")
    if(any(grepl("POSIX",class(datatype))))                      return("DATETIME")
    # TODO: Add TIME
  }

  form_column_headers <- function(columns){
    columnHeaders <- lapply(seq(length(columns)), function(x){
      list(name = names(columns)[x],
           dataType = as.character(columns[x]))
    })
    return(columnHeaders)
  }

  form_attribute_list <- function(attributes){
    attrList <- lapply(attributes, function(attribute){
      list(name = attribute,
           attributeForms = list(list(category = "ID",
                                      expressions = list(list(
                                        formula = paste0(table_name, ".", attribute))))))
    })
    return(attrList)
  }

  form_metric_list <- function(metrics){
    metricList <- lapply(metrics, function(metric){
      list(name = metric,
           expressions = list(list(
             formula = paste0(table_name, ".", metric))))
    })
    return(metricList)
  }

  # Map data types from the dataframe to microstrategy data types
  columns <- lapply(df, convert_datatype)

  # Initialize lists to contain attribute and metric elements
  attributes <- list()
  metrics <- list()

  for(i in 1:length(columns)){
    if(columns[i] %in% c("DOUBLE", "INTEGER")){

      ## Metrics
      if(names(columns[i]) %in% to_attribute){  ## If this element is in the attribute override, add it to attributes instead
        attributes <- c(attributes, names(columns[i]))
      } else {
        metrics <- c(metrics, names(columns[i]))
      }

    } else {

      ## Attributes
      if(names(columns[i]) %in% to_metric){  ## If this element is in the metric override, add it to metrics instead
        metrics <- c(metrics, names(columns[i]))
      } else {
        attributes <- c(attributes, names(columns[i]))
      }
    }
  }

  list("columnHeaders" = form_column_headers(columns = columns),
       "attributeList" = form_attribute_list(attributes = attributes),
       "metricList" = form_metric_list(metrics = metrics))
}

