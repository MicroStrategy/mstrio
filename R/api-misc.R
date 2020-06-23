# api-misc.R
#' @importFrom httr GET

# Get information about the version of iServer and web.
server_status <- function(base_url, verbose = FALSE) {
  response <- httr::GET(url = paste0(base_url, "/api/status"))

  if (verbose) {
    print(response$url)
  }
  response_handler(response, "Failed to check server status")

  return(response)
}
