# api-reports.R
#' @import httr


# Get the definition of a specific report, including attributes and metrics. The report can
# be either an Intelligent Cube or a Direct Data Access (DDA)/MDX report. The in-memory report
# definition provides information about all available objects without actually running any
# data query/report. The results can be used by other requests to help filter large datasets
# and retrieve values dynamically, helping with performance and scalability.
# Returns complete HTTP response object.
report <- function(connection, report_id, verbose = FALSE) {
  response <- httr::GET(url = paste0(connection$base_url, "/api/v2/reports/", report_id),
                        add_headers("X-MSTR-AuthToken" = connection$auth_token,
                                    "X-MSTR-ProjectID" = connection$project_id),
                        set_cookies(connection$cookies))
  if (verbose) {
    print(response$url)
  }
  error_msg <- "Error getting report definition. Check report ID."
  response_handler(response, error_msg)
  return(response)
}

# Get the results of a newly created report instance.
# This in-memory report instance can be used by other requests.
# Returns complete HTTP response object.
report_instance <- function(connection, report_id, body = NULL, offset = 0, limit = 1000, verbose = FALSE) {
  query <- list(offset = format(offset, scientific = FALSE, trim = TRUE),
                                limit = format(limit, scientific = FALSE, trim = TRUE))
  # filtering of extra and formatted metric data is available from version 11.2.2 and higher
  if (compareVersion(connection$iserver_version, "11.2.0200") %in% c(0, 1)){
    query <- c(query, fields  = '-data.metricValues.extras,-data.metricValues.formatted')
  }

  response <- httr::POST(url = paste0(connection$base_url, "/api/v2/reports/", report_id, "/instances"),
                         add_headers("X-MSTR-AuthToken" = connection$auth_token,
                                     "X-MSTR-ProjectID" = connection$project_id),
                         body = body,
                         content_type_json(),
                         query = query,
                         set_cookies(connection$cookies))
  if (verbose) {
    print(response$url)
  }
  error_msg <- "Error getting report contents."
  response_handler(response, error_msg)
  return(response)
}

# Get the results of a previously created report instance, using the in-memory report instance
# created by a POST /v2/reports/{reportId}/instances request.
# Returns complete HTTP response object.
report_instance_id <- function(connection, report_id, instance_id, offset = 0, limit = 1000, verbose = FALSE) {
  query <- list(offset = format(offset, scientific = FALSE, trim = TRUE),
                                limit = format(limit, scientific = FALSE, trim = TRUE))
  # filtering of extra and formatted metric data is available from version 11.2.2 and higher
  if (compareVersion(connection$iserver_version, "11.2.0200") %in% c(0, 1)){
    query <- c(query, fields  = '-data.metricValues.extras,-data.metricValues.formatted')
  }

  response <- httr::GET(url = paste0(connection$base_url, "/api/v2/reports/", report_id, "/instances/", instance_id),
                        add_headers("X-MSTR-AuthToken" = connection$auth_token,
                                    "X-MSTR-ProjectID" = connection$project_id),
                        query = query,
                        set_cookies(connection$cookies))
  if (verbose) {
    print(response$url)
  }
  error_msg <- sprintf("Error fetching report chunk with limit: %d and offset: %d.", limit, offset)
  response_handler(response, error_msg)
  return(response)
}


# Get elements of a specific attribute of a specific report.
# Returns complette HTTP response object.
report_elements <- function(connection, report_id, attribute_id, offset = 0, limit = 25000, verbose = FALSE) {
  response <- httr::GET(url = paste0(connection$base_url, "/api/reports/", report_id, "/attributes/",
                        attribute_id, "/elements"),
                        add_headers("X-MSTR-AuthToken" = connection$auth_token,
                                    "X-MSTR-ProjectID" = connection$project_id),
                        query = list(offset = format(offset, scientific = FALSE, trim = TRUE),
                                   limit = format(limit, scientific = FALSE, trim = TRUE)),
                        set_cookies(connection$cookies))
  if (verbose) {
    print(response$url)
  }
  error_msg <- "Error loading attribute elements."
  response_handler(response, error_msg)
  return(response)
}

# Launch an async HTTP request and return the future, for async downloading of attribute elements
report_elements_async <- function(connection, report_id, attribute_id, offset = 0, limit = 50000, verbose = FALSE) {
  all_headers <- list("X-MSTR-AuthToken" = connection$auth_token,
                      "X-MSTR-ProjectID" = connection$project_id,
                      "Cookie" = connection$cookies)
  url <- paste0(connection$base_url, "/api/reports/", report_id, "/attributes/", attribute_id, "/elements")
  request <- HttpRequest$new(
    url = url,
    headers = all_headers
  )
  future <- request$get(query = list(offset = format(offset, scientific=FALSE, trim=TRUE),
                                     limit = format(limit, scientific=FALSE, trim=TRUE)))
  return(future)
}
