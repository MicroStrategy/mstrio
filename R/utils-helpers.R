#' @importFrom utils compareVersion
#' @importFrom stats setNames
#' @importFrom httr http_status http_error content

myView <- function(x, title) get("View", envir = as.environment("package:utils"))(x, title) # nocov

getEnvironment <- function() {
  return(.GlobalEnv) # nocov
}

mstrio_env <- getEnvironment()
mstrio_temp_env <- new.env()

firstUp <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

stringIntersects <- function(string1, string2) {
  grepl(string1, string2, fixed = TRUE)
}

# 'cols' must be a named vector, e.g. c("col.name"=1)
arrange.col <- function(data, cols) {
  data.names <- names(data)
  col.nr <- length(data.names)
  col.names <- names(cols)
  col.pos <- cols

  out.vec <- character(col.nr)
  out.vec[col.pos] <- col.names
  out.vec[-col.pos] <- data.names[!(data.names %in% col.names)]

  data <- data[, out.vec]
  return(data)
}

# Request the versioning information from server
check_version <- function(base_url, supported_ver) {

  response <- server_status(base_url)

  # Raises server error message
  if (httr::http_error(response)) {
    # http != 204
    status <- httr::http_status(response)
    errors <- httr::content(response)
    usrmsg <- "Check REST API URL and try again."

    stop(sprintf("%s\n HTTP Error: %s %s %s\n I-Server Error: %s %s",
                 usrmsg, response$status_code, status$reason, status$message, errors$code, errors$message),
         call. = FALSE)
  }
  response <- httr::content(response)
  web_version <- substr(response$webVersion, 1, 9)
  iserver_version <- substr(response$iServerVersion, 1, 9)
  temp_web_version_ok <- utils::compareVersion(web_version, supported_ver)
  temp_iserver_version_ok <- utils::compareVersion(iserver_version, supported_ver)

  version_ok <- FALSE
  # Convert the return value of compareVersion into logical
  if ((temp_web_version_ok >= 0) && (temp_iserver_version_ok >= 0)) {
    version_ok <- TRUE
  }
  info <- list("is_ok" = version_ok, "web_version" = web_version, "iserver_version" = iserver_version)

  return(info)
}

verifyColumnsNames <- function(dataframe_name, proper_columns) {
  dataset <- get(dataframe_name, mstrio_env)
  colNames <- names(dataset)
  if (!identical(colNames, proper_columns)) { # nocov start
    assign(dataframe_name, stats::setNames(dataset, proper_columns), mstrio_env)
    tryCatch({
      myView(x=get(dataframe_name, mstrio_env), title=dataframe_name)
    }, error = function(e) { # nocov end

    })
  }
}

url_check <- function(url) {
  if (!grepl("https?://.+$", url)) {
    stop("Please check the validity of the base_url parameter. Typically of the form 'https://<<MSTR Domain>>/MicroStrategyLibrary/'")
  }
  url <- sub("/?api/?$", "", url)
  url <- sub("/$", "", url)
  return(url)
}

parse_error_message <- function(input_str) {
  parsed <- input_str
  # removes \n, \r and ' symbols from the input string
  parsed <- gsub("[\r\n]", " ", parsed)
  parsed <- gsub("[\']", "`", parsed)
  return(parsed)
}

response_handler <- function(response, msg, throw_error = TRUE) {
  # Generic error message handler for transactions against I-Server.
  if (http_error(response)) {
    status <- http_status(response)
    content <- content(response, encoding = "UTF-8")
    error_code <- content$code
    error_msg <- content$message

    if (isTRUE(throw_error)) {
      if (is.null(error_code) | is.null(error_msg)) {
        # If no information from I-Server
        stop(sprintf("%s\n HTTP Error: %s %s %s", msg, response$status_code, status$reason, status$message),
            call. = FALSE)
      } else {
        # Raises I-Server error message
        stop(sprintf("%s\n HTTP Error: %s %s %s\n I-Server Error: %s %s",
            msg, response$status_code, status$reason, status$message, error_code, error_msg),
        call. = FALSE)
      }
    } else {
      if (is.null(error_code) | is.null(error_msg)) {
        # If no information from I-Server
        print(sprintf("%s\n HTTP Error: %s %s %s", msg, response$status_code, status$reason, status$message))
      } else {
        # Raises I-Server error message
        print(sprintf("%s\n HTTP Error: %s %s %s\n I-Server Error: %s %s",
            msg, response$status_code, status$reason, status$message, error_code, error_msg))
      }
    }
  }
}

